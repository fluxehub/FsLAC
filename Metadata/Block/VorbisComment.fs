module FsLAC.Metadata.Block.VorbisComment

open System
open FsLAC

// Used to read the Vorbis string lengths
let private readUInt32LE =
    decode {
        let! bytes = Decoder.readBytes 4

        if BitConverter.IsLittleEndian then
            return BitConverter.ToUInt32(bytes, 0)
        else
            return BitConverter.ToUInt32(bytes |> Array.rev, 0)
    }

let parseComment (comment: string) =
    match comment.Split('=') with
    | [| key; value |] -> Decoder.ok (key, value)
    | _ -> Decoder.error $"Invalid comment in Vorbis comment block: {comment}"

let readComment =
    decode {
        let! commentLength = readUInt32LE |> Decoder.map int
        let! comment = Decoder.readString commentLength
        return! parseComment comment
    }

let readVorbisComment =
    decode {
        let! vendorLength = readUInt32LE |> Decoder.map int
        let! vendorString = Decoder.readString vendorLength
        let! commentCount = readUInt32LE |> Decoder.map int

        let! comments =
            List.init commentCount (fun _ -> readComment)
            |> List.sequenceDecoder
            |> Decoder.map Map.ofList

        return
            { Vendor = vendorString
              Comments = comments }
    }
