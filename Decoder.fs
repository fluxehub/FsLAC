namespace FsLAC

open System
open System.IO

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
