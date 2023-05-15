module FsLAC.Parser.Metadata.Blocks.SeekTable

open FsLAC.Parser
open FsLAC.Types.Metadata

let private parseSeekPoint =
    parse {
        let! sampleNumber = Parser.readUInt64
        let! streamOffset = Parser.readUInt64
        let! frameSamples = Parser.readUInt16

        return
            { SampleNumber = sampleNumber
              StreamOffset = streamOffset
              FrameSamplesCount = (uint frameSamples) }
    }

let parseSeekTable (length: uint) =
    parse {
        let length = length / 18u // Number of entries

        return!
            List.init (int length) (fun _ -> parseSeekPoint)
            |> List.sequenceParser
            |> Parser.map (List.filter (fun x -> x.SampleNumber <> 0xFFFFFFFFFFFFFFFFuL))
    }
