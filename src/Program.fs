open System.IO
open System.Threading.Tasks
open FsLAC
open FsLAC.Decoder
open FsLAC.Frame
open FsLAC.Parser
open FsLAC.Parser.Metadata
open FsLAC.Player
open FsLAC.Types

let checkMagic =
    parse {
        let! magic = Parser.readBytes 4

        if magic <> "fLaC"B then
            return! Parser.error "Not a valid FLAC file"
    }

let openFile name =
    let file = File.OpenRead(name)
    BitStream(new BinaryReader(file))

let decodeFile filename =
    let stream = openFile filename

    let decodeFrame metadata =
        parse {
            return!
                FrameParser.parseFrame metadata.StreamInfo
                |> Parser.map (Decoder.decodeFrame true)
        }

    let decodeFlac =
        parse {
            do! checkMagic
            let! metadata = MetadataParser.parseMetadata
            return! decodeFrame metadata
        }

    stream |> Parser.run decodeFlac


let callback requestedSamples (left, right) =
    let leftSamples, leftTail = List.splitAt requestedSamples left
    let rightSamples, rightTail = List.splitAt requestedSamples right
    [ leftSamples; rightSamples ], (leftTail, rightTail)

let format =
    { SampleRate = 44100.0
      Channels = 2u
      BitDepth = 16u }

let [ left; right ] = WaveFile.getWaveBytes "circle.wav"

let player = new Player<int list * int list>(format, 2048u, callback, (left, right))

printfn "Playing..."
player.Start()
Task.Delay((3 * 60 + 53) * 1000).Wait()
printfn "Stopping..."
player.Stop()

match decodeFile "circle.flac" with
| Ok data -> printfn $"Frame: %A{data}"
| Error e -> printfn $"Error: {e}"
