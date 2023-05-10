open System
open System.IO
open FsLAC
open FsLAC.Metadata

let checkMagic =
    decode {
        let! magic = Decoder.readBytes 4

        if magic <> "fLaC"B then
            return! Decoder.error "Not a valid FLAC file"
    }

let openFile name =
    let file = File.OpenRead(name)
    BitStream(new BinaryReader(file))

let decodeFile filename =
    let stream = openFile filename

    let decodeFlac =
        decode {
            do! checkMagic
            let! metadata = MetadataDecoder.readMetadata
            let! sync = Decoder.readBits 14
            let! resv = Decoder.readBits 1
            let! block = Decoder.readBits 1
            let! blockSize = Decoder.readBits 4
            let! sr = Decoder.readBits 4
            let! ch = Decoder.readBits 4
            let! sample = Decoder.readBits 3
            let! resv2 = Decoder.readBits 1
            let! utf8_first = Decoder.readByte
            let! crc = Decoder.readByte
            let! pad = Decoder.readBits 1
            let! subframeType = Decoder.readBits 6
            let! wasted = Decoder.readBits 1
            printfn $"Sync: {Convert.ToString(int sync, 2)}"
            printfn $"Reserved: {resv}"
            printfn $"Block: {Convert.ToString(int block, 2)}"
            printfn $"Block size: {Convert.ToString(int blockSize, 2)}"
            printfn $"Sample rate: {Convert.ToString(int sr, 2)}"
            printfn $"Channels: {Convert.ToString(int ch, 2)}"
            printfn $"Sample size: {Convert.ToString(int sample, 2)}"
            printfn $"Reserved: {resv2}"
            printfn $"UTF8 first: {utf8_first}"
            printfn $"CRC: {crc}"
            printfn $"Padding: {pad}"
            printfn $"Subframe type: {Convert.ToString(int subframeType, 2)}"
            printfn $"Wasted: {wasted}"
        }

    stream |> Decoder.run decodeFlac

match decodeFile "circle.flac" with
| Ok data -> printfn $"Metadata: {data}"
| Error e -> printfn $"Error: {e}"
