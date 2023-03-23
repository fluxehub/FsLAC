open System.IO
open System.Text
open FsLAC
open Decoder
open Metadata

let readMagic =
    decode {
        let! magic = readBytes 4

        if magic <> "fLaC"B then
            return Error "Not a valid FLAC file"
    }

let openFile name =
    let file = File.OpenRead(name)
    new BinaryReader(file, Encoding.UTF8, false)

let stream = openFile "circle.flac"

let decodeFile filename =
    let stream = openFile filename
    let decoder = { Stream = stream; Metadata = None }

    let decodeSteps =
        decode {
            do! readMagic
            do! loadMetadata
            let! metadata = getMetadata
            printfn $"Metadata: {metadata}"
        }

    decoder |> runDecode decodeSteps

match decodeFile "circle.flac" with
| Ok _, _ -> printfn "Success"
| Error e, _ -> printfn $"Error: {e}"
