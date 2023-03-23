module FsLAC.Metadata

open FsLAC.Types
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
            StreamInfo
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

let readSeekPoint =
    decode {
        let! sampleNumber = readUInt64
        let! streamOffset = readUInt64
        let! frameSamples = readUInt16

        return
            { SampleNumber = sampleNumber
              StreamOffset = streamOffset
              FrameSamplesCount = (uint frameSamples) }
    }

let readSeekTable length =
    decode {
        let table = List.empty
        let length = length / 18 // Number of entries

        let rec readLoop table i =
            if i = length then
                decodeReturn (List.rev table)
            else
                decode {
                    let! seekPoint = readSeekPoint
                    // Skip placeholder seek points
                    if seekPoint.SampleNumber = 0xFFFFFFFFFFFFFFFFuL then
                        return! readLoop table (i + 1)
                    else
                        return! readLoop (seekPoint :: table) (i + 1)
                }

        let! table = readLoop table 0
        return (SeekTable table)
    }

let readMetadataBlock =
    decode {
        let! lastAndType = readByte
        // If the first bit is set, this is the last metadata block
        let isLast = (lastAndType &&& 0x80uy) = 0x80uy
        // The remaining 7 bits are the type of the metadata block
        let blockType = lastAndType &&& 0x7Fuy
        let! length = readUInt24

        let! data =
            match blockType with
            | 0uy -> readStreamInfo
            | 3uy -> readSeekTable (int length)
            | _ -> decodeError $"Unknown metadata block type: {blockType}"

        return
            { Data = data
              Length = length
              IsLast = isLast }
    }

let readStreamInfoBlock =
    decode {
        let! block = readMetadataBlock

        return!
            match block.Data with
            | StreamInfo data -> decodeReturn (block, data)
            | _ -> decodeError "Expected stream info block"
    }

let readBlocks =
    let rec loop blocks =
        decode {
            let! block = readMetadataBlock
            printfn $"Read block {block.Data}"
            let blocks = block :: blocks

            if block.IsLast then
                return List.rev blocks
            else
                return! loop blocks
        }

    loop []

let readMetadata =
    decode {
        // The stream info block is always the first block
        let! streamInfoBlock, streamInfo = readStreamInfoBlock

        let! extraBlocks =
            if streamInfoBlock.IsLast then
                decodeReturn []
            else
                // Read the rest of the blocks
                readBlocks

        return
            { StreamInfo = streamInfo
              Blocks = streamInfoBlock :: extraBlocks }
    }
