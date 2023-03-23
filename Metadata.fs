module FsLAC.Metadata

open FsLAC.Types
open Decoder

let readStreamInfo =
    decode {
        let! minBlockSize = readUInt16
        let! maxBlockSize = readUInt16
        let! minFrameSize = readUInt24
        let! maxFrameSize = readUInt24
        let! sampleRateChannelsBitsAndTotalSamples = readUInt64
        // Format of chunk is:
        // 20 bits sample rate
        // 3 bits channels (-1)
        // 5 bits bits per sample (-1)
        // 36 bits total samples
        let sampleRate =
            (sampleRateChannelsBitsAndTotalSamples &&& 0xFFFFF00000000000uL) >>> 44 |> uint

        let channels =
            ((sampleRateChannelsBitsAndTotalSamples &&& 0x00000E0000000000uL) >>> 41)
            |> uint
            |> (+) 1u

        let bitsPerSample =
            ((sampleRateChannelsBitsAndTotalSamples &&& 0x000001F000000000uL) >>> 36)
            |> uint
            |> (+) 1u

        let totalSamples =
            (sampleRateChannelsBitsAndTotalSamples &&& 0x0000000FFFFFFFFFuL) |> uint64

        let! audioMD5 = readBytes 16

        return
            Ok(
                StreamInfo
                    { MinBlockSize = uint minBlockSize
                      MaxBlockSize = uint maxBlockSize
                      MinFrameSize = minFrameSize
                      MaxFrameSize = maxFrameSize
                      SampleRate = sampleRate
                      Channels = channels
                      BitsPerSample = bitsPerSample
                      TotalSamples = totalSamples
                      AudioMD5 = audioMD5 }
            )
    }

let readSeekTable length =
    decode {
        let table = List.empty
        let length = length / 18 // Number of entries

        let rec loop table i =
            decode {
                if i = length then
                    return Ok table
                else
                    let! sampleNumber = readUInt64
                    let! streamOffset = readUInt64
                    let! frameSamples = readUInt16
                    // Skip placeholder seek points
                    if sampleNumber = 0xFFFFFFFFFFFFFFFFuL then
                        return! loop table (i + 1)
                    else
                        let entry =
                            { SampleNumber = sampleNumber
                              StreamOffset = streamOffset
                              FrameSamplesCount = (uint frameSamples) }

                        return! loop (entry :: table) (i + 1)
            }

        let! table = loop table 0
        return Ok(SeekTable table)
    }

let readMetadataBlock =
    decode {
        let! lastAndType = readByte
        // If the first bit is set, this is the last metadata block
        let isLast = (lastAndType &&& 0x80uy) = 0x80uy
        // The remaining 7 bits are the type of the metadata block
        let blockType = lastAndType &&& 0x7Fuy
        let! length = readUInt24

        let! data =
            match blockType with
            | 0uy -> readStreamInfo
            | 3uy -> readSeekTable (int length)
            | _ -> decode { return Error $"Unknown metadata block type: {blockType}" }

        return
            Ok
                { Data = data
                  Length = length
                  IsLast = isLast }
    }

let readStreamInfoBlock =
    decode {
        let! block = readMetadataBlock

        match block.Data with
        | StreamInfo data -> return Ok(block, data)
        | _ -> return Error "Expected stream info block"
    }

let readBlocks =
    let rec loop blocks =
        decode {
            let! block = readMetadataBlock
            printfn $"Read block {block.Data}"
            let blocks = block :: blocks

            if block.IsLast then
                return Ok(List.rev blocks)
            else
                return! loop blocks
        }

    loop []

let loadMetadata =
    decode {
        // The stream info block is always the first block
        let! streamInfoBlock, streamInfo = readStreamInfoBlock

        let! extraBlocks =
            if streamInfoBlock.IsLast then
                decode.Return <| Ok []
            else
                // Read the rest of the blocks
                readBlocks

        let metadata =
            { StreamInfo = streamInfo
              Blocks = streamInfoBlock :: extraBlocks }

        do! setMetadata metadata
        return Ok()
    }
