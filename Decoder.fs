namespace FsLAC

open System
open System.IO
open System.Text

type Stream =
    { Reader: BinaryReader
      Metadata: Metadata option }

module Stream =
    let create reader = { Reader = reader; Metadata = None }

type Decoder<'ok, 'err> = Decoder of (Stream -> Result<'ok, 'err>)

module Decoder =
    let run (Decoder(f)) stream = f stream
    let inline decodeReturn value = Decoder(fun _ -> Ok value)
    let inline decodeError err = Decoder(fun _ -> Error err)

    let map f (Decoder(decoder)) =
        Decoder(fun stream -> decoder stream |> Result.map f)

    type DecodeBuilder() =
        member this.Zero() = Decoder(fun _ -> Ok())

        member this.Return value = decodeReturn value

        member inline this.ReturnFrom(d: Decoder<_, _>) = d

        member this.Bind(decoder: Decoder<'a, _>, f: 'a -> Decoder<'b, _>) : Decoder<'b, 'err> =
            Decoder(fun stream ->
                let result = stream |> run decoder

                match result with
                | Ok value -> stream |> run (f value)
                | Error err -> Error err)

    let decode = DecodeBuilder()

    let readByte = Decoder(fun stream -> Ok(stream.Reader.ReadByte()))

    let readBytes bytesToRead =
        Decoder(fun stream ->
            let bytes = stream.Reader.ReadBytes(bytesToRead)

            if bytes.Length = bytesToRead then
                Ok bytes
            else
                Error "Could not read enough bytes")

    let skip bytesToSkip =
        Decoder(fun stream ->
            ignore (stream.Reader.BaseStream.Seek(bytesToSkip, SeekOrigin.Current))
            Ok())

    let seek position =
        Decoder(fun stream ->
            ignore (stream.Reader.BaseStream.Seek(position, SeekOrigin.Begin))
            Ok())

    let private readBigEndian bytesToRead =
        decode {
            let! bytes = readBytes bytesToRead

            if BitConverter.IsLittleEndian then
                return Array.rev bytes
            else
                return bytes
        }

    let private readInt size converter =
        decode {
            let! bytes = readBigEndian size
            return converter bytes
        }

    let readUInt16 = readInt sizeof<uint16> BitConverter.ToUInt16

    let readUInt24 =
        decode {
            let! readBytes = readBigEndian 3

            let bytes =
                if BitConverter.IsLittleEndian then
                    [| 0uy |] |> Array.append readBytes
                else
                    readBytes |> Array.append [| 0uy |]

            return BitConverter.ToUInt32 bytes
        }

    let readUInt32 = readInt sizeof<uint32> BitConverter.ToUInt32

    let readUInt64 = readInt sizeof<uint64> BitConverter.ToUInt64

    let readString length =
        decode {
            let! bytes = readBytes length
            return Encoding.UTF8.GetString bytes
        }

module List =
    open Decoder

    let traverseDecoder decoder list =
        let (>>=) f x = decode.Bind(f, x)
        let init = decodeReturn []

        let folder head tail =
            decoder head
            >>= (fun head -> tail >>= (fun tail -> decodeReturn (head :: tail)))

        List.foldBack folder list init

    let sequenceDecoder list = traverseDecoder id list
