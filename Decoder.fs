namespace FsLAC

open System
open System.IO
open FsLAC.Types

type StreamInfo =
    { MinBlockSize: uint
      MaxBlockSize: uint
      MinFrameSize: uint
      MaxFrameSize: uint
      SampleRate: uint
      Channels: uint
      BitsPerSample: uint
      TotalSamples: uint64
      AudioMD5: byte[] }

type Decoder =
    { Stream: BinaryReader
      Metadata: Metadata option }

type DecoderState<'ok, 'err> = DecoderState of (Decoder -> Result<'ok, 'err> * Decoder)

module Decoder =
    let inline runDecode x decoder = let (DecoderState(f)) = x in f decoder

    let getMetadata =
        DecoderState(fun decoder ->
            match decoder.Metadata with
            | Some metadata -> Ok metadata, decoder
            | None -> Error "Attempted to retrieve metadata when no metadata was loaded", decoder)

    let setMetadata metadata =
        DecoderState(fun decoder ->
            Ok(),
            { decoder with
                Metadata = Some metadata })

    // This is basically a combo of the state monad and the result monad
    type DecoderBuilder() =
        member this.Zero() =
            DecoderState(fun decoder -> Ok(), decoder)

        member this.Return result =
            DecoderState(fun decoder -> result, decoder)

        member inline this.ReturnFrom(state: DecoderState<_, _>) = state

        member this.Bind(x, f) : DecoderState<_, _> =
            DecoderState(fun decoder ->
                let result, decoder = decoder |> runDecode x

                match result with
                | Ok value -> decoder |> runDecode (f value)
                | Error err -> Error err, decoder)

        member this.Combine x1 x2 =
            DecoderState(fun decoder ->
                let result1, decoder = decoder |> runDecode x1
                let result2, decoder = decoder |> runDecode x2

                match result1, result2 with
                | Ok value1, Ok value2 -> Ok(value1, value2), decoder
                | Error err, _ -> Error err, decoder
                | _, Error err -> Error err, decoder)

        member this.Delay f : DecoderState<_, _> = f ()
        member this.Yield result = this.Return result

    let decode = DecoderBuilder()

    let readByte = DecoderState(fun decoder -> Ok(decoder.Stream.ReadByte()), decoder)

    let readBytes bytesToRead =
        DecoderState(fun decoder ->
            let bytes = decoder.Stream.ReadBytes(bytesToRead)

            if bytes.Length = bytesToRead then
                Ok bytes, decoder
            else
                Error "Could not read enough bytes", decoder)

    let private readBigEndian bytesToRead =
        decode {
            let! bytes = readBytes bytesToRead

            if BitConverter.IsLittleEndian then
                return Ok <| Array.rev bytes
            else
                return Ok bytes
        }

    let private readInt size converter =
        decode {
            let! bytes = readBigEndian size
            return Ok <| converter bytes
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

            return Ok <| BitConverter.ToUInt32 bytes
        }

    let readUInt32 = readInt sizeof<uint32> BitConverter.ToUInt32

    let readUInt64 = readInt sizeof<uint64> BitConverter.ToUInt64
