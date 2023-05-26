open System.IO
open System.Threading.Tasks
open FsLAC
open FsLAC.Decoder
open FsLAC.Frame
open FsLAC.Parser
open FsLAC.Parser.Metadata
open FsLAC.Player
open FsLAC.Types

let openFile name =
    let file = File.OpenRead(name)
    BitStream(new BinaryReader(file))

let stream = openFile "output.flac"

let metadata =
    match Parser.run MetadataParser.parseMetadata stream with
    | Ok metadata -> metadata
    | Error e -> failwith e

let mutex = new System.Threading.Mutex()

let parseFrame stream =
    match Parser.run (FrameParser.parseFrame metadata.StreamInfo) stream with
    | Ok frame -> frame
    | Error e -> failwith e

let callback requestedSamples _ =
    mutex.WaitOne() |> ignore
    let frame = parseFrame stream

    let decoded = Decoder.decodeFrame true frame
    mutex.ReleaseMutex()
    decoded, ()

let format =
    { SampleRate = float metadata.StreamInfo.SampleRate
      Channels = metadata.StreamInfo.Channels
      BitDepth = metadata.StreamInfo.BitDepth }

// Variable block size is not implemented yet
assert (metadata.StreamInfo.MaxBlockSize = metadata.StreamInfo.MinBlockSize)

let player =
    new Player<unit>(format, metadata.StreamInfo.MaxBlockSize, callback, ())

printfn $"{metadata.StreamInfo}"
printfn "Playing..."
player.Start()
Task.Delay(-1).Wait()
printfn "Stopping..."
player.Stop()
