open System.IO
open FsLAC
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

    let decodeFlac =
        parse {
            do! checkMagic
            let! metadata = MetadataParser.parseMetadata
            return! FrameParser.parseFrame metadata.StreamInfo
        }

    stream |> Parser.run decodeFlac

let callback (waveBuffer: byte array) requestedBytes pos =
    let startPos = pos * requestedBytes
    let endPos = startPos + requestedBytes
    waveBuffer[startPos .. endPos - 1], pos + 1

let format =
    { SampleRate = 44100.0
      Channels = 2u
      BitDepth = 16u }

let player =
    new Player<int>(format, 2048u, callback (WaveFile.getWaveBytes "circle.wav"), 0)

// printfn "Playing..."
// player.Start()
// // Task.Delay((3 * 60 + 53) * 1000).Wait()
// printfn "Stopping..."
// player.Stop()

match decodeFile "circle.flac" with
| Ok data -> printfn $"Frame: {data}"
| Error e -> printfn $"Error: {e}"
