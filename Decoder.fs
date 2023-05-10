namespace FsLAC

open System.IO
open System.Text
open Microsoft.FSharp.Core

// TODO: IDisposable
type BitStream(reader: BinaryReader) =
    let mutable bitBuffer = 0UL
    let mutable bitCount = 0

    // Based off https://fgiesen.wordpress.com/2018/02/20/reading-bits-in-far-too-many-ways-part-2/
    member _.ReadBits(count) =
        assert (count >= 1 && count <= 57)

        while bitCount < count do
            let byte: uint64 = reader.ReadByte() |> uint64
            bitBuffer <- bitBuffer ||| (byte <<< (56 - bitCount))
            bitCount <- bitCount + 8

        let result = bitBuffer >>> (64 - count)
        bitBuffer <- bitBuffer <<< count
        bitCount <- bitCount - count
        result

    member _.Skip(byteCount: uint) =
        // Skips only happen on byte boundaries, so we can just reset the bit buffer
        assert (bitCount % 8 = 0)
        let bytesToSkip = byteCount - (uint (bitCount / 8))
        reader.BaseStream.Seek(int64 bytesToSkip, SeekOrigin.Current) |> ignore

        bitBuffer <- 0UL
        bitCount <- 0


type Decoder<'ok, 'err> = Decoder of (BitStream -> Result<'ok, 'err>)

module Decoder =
    let run (Decoder(f)) stream = f stream
    let inline ok value = Decoder(fun _ -> Ok value)
    let inline error err = Decoder(fun _ -> Error err)

    let map f (Decoder(decoder)) =
        Decoder(fun stream -> decoder stream |> Result.map f)

    type DecodeBuilder() =
        member this.Zero() = Decoder(fun _ -> Ok())

        member this.Return value = ok value

        member inline this.ReturnFrom(d: Decoder<_, _>) = d

        member this.Bind(decoder: Decoder<'a, _>, f: 'a -> Decoder<'b, _>) : Decoder<'b, 'err> =
            Decoder(fun stream ->
                let result = stream |> run decoder

                match result with
                | Ok value -> stream |> run (f value)
                | Error err -> Error err)

    let private decode = DecodeBuilder()

    let readByte =
        Decoder(fun stream ->
            try
                Ok(stream.ReadBits(8) |> byte)
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let readBytes (count: int) =
        Decoder(fun stream ->
            try
                Ok(Array.init count (fun _ -> stream.ReadBits(8) |> byte))
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let readBits count =
        Decoder(fun stream ->
            try
                Ok(stream.ReadBits(count))
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let skip bytesToSkip =
        Decoder(fun stream ->
            stream.Skip(bytesToSkip)
            Ok())

    // let seek position =
    //     Decoder(fun stream ->
    //         ignore (stream.Reader.BaseStream.Seek(position, SeekOrigin.Begin))
    //         Ok())

    let private readInt size converter =
        decode {
            let! bytes = readBits size
            return converter bytes
        }

    let readUInt16 = readInt 16 uint16

    let readUInt24 = readInt 24 uint32

    let readUInt32 = readInt 32 uint32

    let readUInt64 =
        decode {
            let! high = readUInt32
            let! low = readUInt32
            return (uint64 high <<< 32) ||| uint64 low
        }

    let readString length =
        decode {
            let! bytes = readBytes length
            return Encoding.UTF8.GetString bytes
        }

[<AutoOpen>]
module DecoderCE =
    let decode = Decoder.DecodeBuilder()

module List =
    open Decoder

    let traverseDecoder decoder list =
        let (>>=) f x = decode.Bind(f, x)
        let init = ok []

        let folder head tail =
            decoder head >>= (fun head -> tail >>= (fun tail -> ok (head :: tail)))

        List.foldBack folder list init

    let sequenceDecoder list = traverseDecoder id list
