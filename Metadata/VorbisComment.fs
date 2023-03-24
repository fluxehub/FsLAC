module FsLAC.MetadataBlocks.VorbisComment

open System
open FsLAC
open Decoder

// Used to read the Vorbis string lengths
let private readUInt32LE =
    decode {
        let! bytes = readBytes 4

        if BitConverter.IsLittleEndian then
            return BitConverter.ToUInt32(bytes, 0)
        else
            return BitConverter.ToUInt32(bytes |> Array.rev, 0)
    }

let parseComment (comment: string) =
    match comment.Split('=') with
    | [| key; value |] -> decodeReturn (key, value)
    | _ -> decodeError $"Invalid comment in Vorbis comment block: {comment}"

let readComment =
    decode {
        let! commentLength = readUInt32LE |> map int
        let! comment = readString commentLength
        return! parseComment comment
    }

let readVorbisComment =
    decode {
        let! vendorLength = readUInt32LE |> map int
        let! vendorString = readString vendorLength
        let! commentCount = readUInt32LE |> map int

        let! comments =
            List.init commentCount (fun _ -> readComment)
            |> List.sequenceDecoder
            |> map Map.ofList

        return
            { Vendor = vendorString
              Comments = comments }
    }
