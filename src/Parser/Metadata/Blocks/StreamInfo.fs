module FsLAC.Parser.Metadata.Blocks.StreamInfo

open FsLAC.Parser
open FsLAC.Types.Metadata

let parseStreamInfo =
    parse {
        let! minBlockSize = Parser.readUInt16
        let! maxBlockSize = Parser.readUInt16
        let! minFrameSize = Parser.readUInt24
        let! maxFrameSize = Parser.readUInt24

        let! samplesDescChunk = Parser.readUInt64

        // Format of chunk is:
        // 20 bits for sample rate
        // 3 bits for channels (-1)
        // 5 bits for bits per sample (-1)
        // 36 bits for total samples
        // TODO: Use the new bitstream class
        let sampleRate = (samplesDescChunk &&& 0xFFFFF00000000000uL) >>> 44 |> uint

        let channels =
            ((samplesDescChunk &&& 0x00000E0000000000uL) >>> 41) |> uint |> (+) 1u

        let bitsPerSample =
            ((samplesDescChunk &&& 0x000001F000000000uL) >>> 36) |> uint |> (+) 1u

        let totalSamples = (samplesDescChunk &&& 0x0000000FFFFFFFFFuL) |> uint64

        let! audioMD5 = Parser.readBytes 16

        return
            { MinBlockSize = uint minBlockSize
              MaxBlockSize = uint maxBlockSize
              MinFrameSize = minFrameSize
              MaxFrameSize = maxFrameSize
              SampleRate = sampleRate
              Channels = channels
              BitDepth = bitsPerSample
              TotalSamples = totalSamples
              AudioMD5 = audioMD5 }
    }
