module FsLAC.Parser.Frame.Metadata

open FsLAC.Parser
open FsLAC.Types
open Common

let private checkSyncCode =
    parse {
        let! syncCode = Parser.readBits 14 |> Parser.map int

        if syncCode <> 0x3FFE then
            return! Parser.error $"Invalid sync code: %04X{syncCode}"
    }

let private parseBlockingStrategy =
    parse {
        let! strategyBit = Parser.readBits 1
        if strategyBit = 0UL then return Fixed else return Variable
    }

let private parseUncommon bits =
    parse {
        let! value = Parser.readBits bits |> Parser.map uint
        return value
    }

let private createBlockSizeParser =
    parse {
        let! blockSizeSelector = Parser.readBits 4 |> Parser.map int

        if blockSizeSelector = 0x0 then
            return! Parser.error "Reserved block size"
        elif blockSizeSelector = 0x1 then
            return Parser.ok 192u
        elif blockSizeSelector <= 0x5 then
            return Parser.ok (144u <<< blockSizeSelector)
        elif blockSizeSelector = 0x6 then
            return parseUncommon 8 |> Parser.map ((+) 1u)
        elif blockSizeSelector = 0x7 then
            return parseUncommon 16 |> Parser.map ((+) 1u)
        else
            return Parser.ok (1u <<< blockSizeSelector)
    }

let private createSampleRateParser (streamInfo: StreamInfo) =
    parse {
        match! Parser.readBits 4 |> Parser.map int with
        | 0x00 -> return Parser.ok streamInfo.SampleRate
        | 0x01 -> return Parser.ok 88200u
        | 0x02 -> return Parser.ok 176400u
        | 0x03 -> return Parser.ok 192000u
        | 0x04 -> return Parser.ok 8000u
        | 0x05 -> return Parser.ok 16000u
        | 0x06 -> return Parser.ok 22050u
        | 0x07 -> return Parser.ok 24000u
        | 0x08 -> return Parser.ok 32000u
        | 0x09 -> return Parser.ok 44100u
        | 0x0A -> return Parser.ok 48000u
        | 0x0B -> return Parser.ok 96000u
        | 0x0C -> return parseUncommon 8 |> Parser.map (fun x -> x * 1000u)
        | 0x0D -> return parseUncommon 16 |> Parser.map (fun x -> x * 1000u)
        | 0x0E -> return parseUncommon 16 |> Parser.map (fun x -> x * 100u)
        | _ -> return! Parser.error "Invalid sample rate in frame header"
    }

let private parseChannelAssignment =
    parse {
        match! Parser.readBits 4 |> Parser.map int with
        | 0x00 -> return Mono
        | 0x01 -> return LeftRight
        | 0x08 -> return LeftSide
        | 0x09 -> return RightSide
        | 0x0A -> return MidSide
        | x -> return! Parser.error $"Unsupported channel assignment selector: %X{x}"
    }

let private parseBitDepth (streamInfo: StreamInfo) =
    parse {
        match! Parser.readBits 3 |> Parser.map int with
        | 0x00 -> return streamInfo.BitDepth
        | 0x01 -> return 8u
        | 0x02 -> return 12u
        | 0x04 -> return 16u
        | 0x05 -> return 20u
        | 0x06 -> return 24u
        | 0x07 -> return 32u
        | x -> return! Parser.error $"Unsupported bit depth selector: %X{x}"
    }

let private parseUtf8Number =
    let parseContinuationByte =
        parse {
            let! byte = Parser.readByte

            if byte &&& 0xC0uy = 0x80uy then
                return uint64 (byte &&& 0x3Fuy)
            else
                return! Parser.error $"Invalid UTF-8 continuation byte %X{byte}"
        }

    let rec parseContinuationBytes count =
        parse {
            if count = 0 then
                return 0UL
            else
                let! continuationByte = parseContinuationByte
                let! continuationBytes = parseContinuationBytes (count - 1)
                return (continuationByte <<< 6 * count) ||| (continuationBytes <<< 6)
        }

    parse {
        let! firstByte = Parser.readByte

        if firstByte &&& 0x80uy = 0uy then
            return uint64 firstByte
        elif firstByte &&& 0xE0uy = 0xC0uy then
            let! continuationBytes = parseContinuationBytes 1
            return (uint64 firstByte &&& 0x1FUL) <<< 6 ||| continuationBytes
        elif firstByte &&& 0xF0uy = 0xE0uy then
            let! continuationBytes = parseContinuationBytes 2
            return (uint64 firstByte &&& 0x0FUL) <<< 12 ||| continuationBytes
        elif firstByte &&& 0xF8uy = 0xF0uy then
            let! continuationBytes = parseContinuationBytes 3
            return (uint64 firstByte &&& 0x07UL) <<< 18 ||| continuationBytes
        elif firstByte &&& 0xFCuy = 0xF8uy then
            let! continuationBytes = parseContinuationBytes 4
            return (uint64 firstByte &&& 0x03UL) <<< 24 ||| continuationBytes
        elif firstByte &&& 0xFEuy = 0xFCuy then
            let! continuationBytes = parseContinuationBytes 5
            return (uint64 firstByte &&& 0x01UL) <<< 30 ||| continuationBytes
        elif firstByte = 0xFEuy then
            return! parseContinuationBytes 6
        else
            return! Parser.error $"Invalid UTF-8 first byte %X{firstByte}"
    }

// Checks that the frame header matches the stream info
let checkFrameHeader (header: FrameHeader) (streamInfo: StreamInfo) =
    let checkBlockingStrategy =
        if streamInfo.MinBlockSize = streamInfo.MaxBlockSize then
            match header.BlockingStrategy with
            | Fixed -> Parser.ok ()
            | _ -> Parser.error "Variable blocking strategy in frame does not match stream info"
        else
            match header.BlockingStrategy with
            | Variable -> Parser.ok ()
            | _ -> Parser.error "Fixed blocking strategy in frame does not match stream info"

    let checkBlockSize =
        if
            streamInfo.MinBlockSize <= header.BlockSize
            && header.BlockSize <= streamInfo.MaxBlockSize
        then
            Parser.ok ()
        else
            Parser.error
                $"Block size {header.BlockSize} in frame does not match stream info {streamInfo.MinBlockSize} - {streamInfo.MaxBlockSize}"

    let checkSampleRate =
        if header.SampleRate = streamInfo.SampleRate then
            Parser.ok ()
        else
            Parser.error $"Sample rate {header.SampleRate} in frame does not match stream info {streamInfo.SampleRate}"

    let checkChannelAssignment =
        match header.ChannelAssignment with
        | Mono ->
            if streamInfo.Channels = 1u then
                Parser.ok ()
            else
                Parser.error $"Expected mono frame, got {header.ChannelAssignment}"
        | _ ->
            if streamInfo.Channels = 2u then
                Parser.ok ()
            else
                Parser.error $"Expected stereo frame, got {header.ChannelAssignment}"

    let checkBitDepth =
        if header.BitDepth = streamInfo.BitDepth then
            Parser.ok ()
        else
            Parser.error $"Bit depth {header.BitDepth} in frame does not match stream info {streamInfo.BitDepth}"

    // TODO: Check CRC
    parse {
        do! checkBlockingStrategy
        do! checkBlockSize
        do! checkSampleRate
        do! checkChannelAssignment
        do! checkBitDepth
        return ()
    }

let parseFrameHeader (streamInfo: StreamInfo) =
    parse {
        do! checkSyncCode
        do! checkZeroBit
        let! blockingStrategy = parseBlockingStrategy
        let! parseBlockSize = createBlockSizeParser
        let! parseSampleRate = createSampleRateParser streamInfo
        let! channelAssignment = parseChannelAssignment
        let! bitDepth = parseBitDepth streamInfo
        do! checkZeroBit

        let! frameOrSampleNumber = parseUtf8Number
        let! blockSize = parseBlockSize
        let! sampleRate = parseSampleRate
        let! crc = Parser.readByte |> Parser.map uint

        let header =
            { BlockingStrategy = blockingStrategy
              BlockSize = blockSize
              SampleRate = sampleRate
              ChannelAssignment = channelAssignment
              BitDepth = bitDepth
              FrameOrSampleNumber = frameOrSampleNumber
              CRC = crc }

        do! checkFrameHeader header streamInfo
        return header
    }
