open System.IO
open System.Text
open FsLAC
open Decoder
open Metadata

let checkMagic =
    decode {
        let! magic = readBytes 4

        if magic <> "fLaC"B then
            return! decodeError "Not a valid FLAC file"
    }

let openFile name =
    let file = File.OpenRead(name)
    new BinaryReader(file, Encoding.UTF8, false)

let decodeFile filename =
    use reader = openFile filename

    // TODO: This should be disposable rather than having to create a reader first
    let stream = Stream.create reader

    let decodeFlac =
        decode {
            do! checkMagic
            return! readMetadata
        }

    stream |> run decodeFlac

match decodeFile "circle.flac" with
| Ok data -> printfn $"Metadata: {data}"
| Error e -> printfn $"Error: {e}"
