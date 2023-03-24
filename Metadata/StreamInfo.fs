module FsLAC.MetadataBlocks.StreamInfo

open FsLAC
open Decoder

let readStreamInfo =
    decode {
        let! minBlockSize = readUInt16
        let! maxBlockSize = readUInt16
        let! minFrameSize = readUInt24
        let! maxFrameSize = readUInt24

        let! samplesDescChunk = readUInt64

        // Format of chunk is:
        // 20 bits for sample rate
        // 3 bits for channels (-1)
        // 5 bits for bits per sample (-1)
        // 36 bits for total samples
        let sampleRate = (samplesDescChunk &&& 0xFFFFF00000000000uL) >>> 44 |> uint

        let channels =
            ((samplesDescChunk &&& 0x00000E0000000000uL) >>> 41) |> uint |> (+) 1u

        let bitsPerSample =
            ((samplesDescChunk &&& 0x000001F000000000uL) >>> 36) |> uint |> (+) 1u

        let totalSamples = (samplesDescChunk &&& 0x0000000FFFFFFFFFuL) |> uint64

        let! audioMD5 = readBytes 16

        return
            { MinBlockSize = uint minBlockSize
              MaxBlockSize = uint maxBlockSize
              MinFrameSize = minFrameSize
              MaxFrameSize = maxFrameSize
              SampleRate = sampleRate
              Channels = channels
              BitsPerSample = bitsPerSample
              TotalSamples = totalSamples
              AudioMD5 = audioMD5 }
    }
