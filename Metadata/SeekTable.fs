module FsLAC.MetadataBlocks.SeekTable

open FsLAC
open Decoder

let readSeekPoint =
    decode {
        let! sampleNumber = readUInt64
        let! streamOffset = readUInt64
        let! frameSamples = readUInt16

        return
            { SampleNumber = sampleNumber
              StreamOffset = streamOffset
              FrameSamplesCount = (uint frameSamples) }
    }

let readSeekTable (length: uint) =
    decode {
        let length = length / 18u // Number of entries

        return!
            List.init (int length) (fun _ -> readSeekPoint)
            |> List.sequenceDecoder
            |> map (List.filter (fun x -> x.SampleNumber <> 0xFFFFFFFFFFFFFFFFuL))
    }
