module FsLAC.Metadata.Block.SeekTable

open FsLAC

let readSeekPoint =
    decode {
        let! sampleNumber = Decoder.readUInt64
        let! streamOffset = Decoder.readUInt64
        let! frameSamples = Decoder.readUInt16

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
            |> Decoder.map (List.filter (fun x -> x.SampleNumber <> 0xFFFFFFFFFFFFFFFFuL))
    }
