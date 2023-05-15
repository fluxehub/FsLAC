module FsLAC.Parser.Metadata.Blocks.VorbisComment

open System
open FsLAC.Parser
open FsLAC.Types.Metadata

// Used to read the Vorbis string lengths
let private readUInt32LE =
    parse {
        let! bytes = Parser.readBytes 4

        if BitConverter.IsLittleEndian then
            return BitConverter.ToUInt32(bytes, 0)
        else
            return BitConverter.ToUInt32(bytes |> Array.rev, 0)
    }

let private splitComment (comment: string) =
    match comment.Split('=') with
    | [| key; value |] -> Parser.ok (key, value)
    | _ -> Parser.error $"Invalid comment in Vorbis comment block: {comment}"

let private parseComment =
    parse {
        let! commentLength = readUInt32LE |> Parser.map int
        let! comment = Parser.readString commentLength
        return! splitComment comment
    }

let parseVorbisComment =
    parse {
        let! vendorLength = readUInt32LE |> Parser.map int
        let! vendorString = Parser.readString vendorLength
        let! commentCount = readUInt32LE |> Parser.map int

        let! comments =
            List.init commentCount (fun _ -> parseComment)
            |> List.sequenceParser
            |> Parser.map Map.ofList

        return
            { Vendor = vendorString
              Comments = comments }
    }
