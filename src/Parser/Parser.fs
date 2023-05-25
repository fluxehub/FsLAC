namespace FsLAC.Parser

open System.IO
open System.Text
open FsLAC
open Microsoft.FSharp.Core

// TODO: IDisposable
type BitStream(reader: BinaryReader) =
    let mutable bitBuffer = 0UL
    let mutable bitCount = 0

    // Based off https://fgiesen.wordpress.com/2018/02/20/reading-bits-in-far-too-many-ways-part-2/
    member this.ReadBits(count) =
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

    member this.SkipToAlignment() =
        if bitCount % 8 <> 0 then
            this.ReadBits(8 - (bitCount % 8)) |> ignore

type Parser<'ok, 'err> = Parser of (BitStream -> Result<'ok, 'err>)

module Parser =
    let run (Parser(f)) stream = f stream
    let inline ok value = Parser(fun _ -> Ok value)
    let inline error err = Parser(fun _ -> Error err)

    let map f (Parser(decoder)) =
        Parser(fun stream -> decoder stream |> Result.map f)

    type ParserBuilder() =
        member this.Zero() = Parser(fun _ -> Ok())

        member this.Return value = ok value

        member inline this.ReturnFrom(d: Parser<_, _>) = d

        member this.Bind(decoder: Parser<'a, _>, f: 'a -> Parser<'b, _>) : Parser<'b, 'err> =
            Parser(fun stream ->
                let result = stream |> run decoder

                match result with
                | Ok value -> stream |> run (f value)
                | Error err -> Error err)

    let private parse = ParserBuilder()

    let readByte =
        Parser(fun stream ->
            try
                Ok(stream.ReadBits(8) |> byte)
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let readBytes (count: int) =
        Parser(fun stream ->
            try
                Ok(Array.init count (fun _ -> stream.ReadBits(8) |> byte))
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let readBits count =
        Parser(fun stream ->
            try
                Ok(stream.ReadBits(count))
            with :? EndOfStreamException ->
                Error "End of stream reached")

    let readBitsSigned count =
        parse {
            let! bits = readBits count
            return Utility.signExtend (int64 bits) count
        }

    let skip bytesToSkip =
        Parser(fun stream ->
            stream.Skip(bytesToSkip)
            Ok())

    let skipToAlignment =
        Parser(fun stream ->
            stream.SkipToAlignment()
            Ok())

    // let seek position =
    //     Decoder(fun stream ->
    //         ignore (stream.Reader.BaseStream.Seek(position, SeekOrigin.Begin))
    //         Ok())

    let private readInt size converter =
        parse {
            let! bytes = readBits size
            return converter bytes
        }

    let readUInt16 = readInt 16 uint16

    let readUInt24 = readInt 24 uint32

    let readUInt32 = readInt 32 uint32

    let readUInt64 =
        parse {
            let! high = readUInt32
            let! low = readUInt32
            return (uint64 high <<< 32) ||| uint64 low
        }

    // Let it be known that I had a better system for this using CPU intrinsics,
    // but reaching the end of the stream made it a lot harder to implement
    let readUnary =
        let rec loop count =
            parse {
                let! bit = readBits 1

                if bit = 1UL then return count else return! loop (count + 1)
            }

        loop 0 |> map uint

    let readString length =
        parse {
            let! bytes = readBytes length
            return Encoding.UTF8.GetString bytes
        }

[<AutoOpen>]
module ParseCE =
    let parse = Parser.ParserBuilder()

module List =
    open Parser

    let traverseParser decoder list =
        let (>>=) f x = parse.Bind(f, x)
        let init = ok []

        let folder head tail =
            decoder head >>= (fun head -> tail >>= (fun tail -> ok (head :: tail)))

        List.foldBack folder list init

    let sequenceParser list = traverseParser id list
