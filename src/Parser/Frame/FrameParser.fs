module FsLAC.Frame.FrameParser

open FsLAC.Parser
open Frame.Metadata
open FsLAC.Parser.Frame
open FsLAC.Types
open FsLAC.Types.Frame.Subframe
open Frame.Common

let private readWastedBits =
    parse {
        let! wastedBitsFlag = Parser.readBits 1 |> Parser.map uint

        if wastedBitsFlag = 1u then
            return! Parser.readUnary |> Parser.map ((+) 1u)
        else
            return 0u
    }

let private parseSubframeData subframeTypeSelector frameHeader bitsPerSample =
    if subframeTypeSelector = 0 then
        Subframes.parseConstant bitsPerSample |> Parser.map Constant
    elif subframeTypeSelector = 1 then
        Subframes.parseVerbatim (int frameHeader.BlockSize) bitsPerSample
        |> Parser.map Verbatim
    elif subframeTypeSelector >= 8 && subframeTypeSelector <= 12 then
        Subframes.parseFixed (subframeTypeSelector - 8) (int frameHeader.BlockSize) bitsPerSample
        |> Parser.map Fixed
    elif subframeTypeSelector >= 32 then
        Subframes.parseLpc (subframeTypeSelector - 31) (int frameHeader.BlockSize) bitsPerSample
        |> Parser.map Lpc
    else
        Parser.error "Unsupported subframe type"

let private parseSubframe frameHeader isSide =
    let bitDepth =
        if isSide then
            frameHeader.BitDepth + 1u
        else
            frameHeader.BitDepth

    parse {
        do! checkZeroBit
        let! subframeTypeSelector = Parser.readBits 6 |> Parser.map int
        let! wastedBits = readWastedBits
        let bitsPerSample = int (bitDepth - wastedBits)
        let! subframeData = parseSubframeData subframeTypeSelector frameHeader bitsPerSample

        return
            { WastedBits = wastedBits
              Data = subframeData }
    }

let parseFrame streamInfo =
    parse {
        let! header = parseFrameHeader streamInfo
        let parseSubframe = parseSubframe header

        let! subframes =
            match header.ChannelAssignment with
            | Mono -> parseSubframe false |> Parser.map (fun x -> [ x ])
            | LeftRight ->
                parse {
                    let! left = parseSubframe false
                    let! right = parseSubframe false
                    return [ left; right ]
                }
            | MidSide
            | LeftSide ->
                parse {
                    let! mid = parseSubframe false
                    let! side = parseSubframe true
                    return [ mid; side ]
                }
            | RightSide ->
                parse {
                    let! side = parseSubframe true
                    let! right = parseSubframe false
                    return [ side; right ]
                }

        do! Parser.skipToAlignment
        let! crc = Parser.readUInt16

        return
            { Header = header
              Subframes = subframes
              CRC = uint crc }
    }
