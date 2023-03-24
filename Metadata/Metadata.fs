module FsLAC.Metadata.MetadataDecoder

open FsLAC
open Block.StreamInfo
open Block.Padding
open Block.Application
open Block.SeekTable
open Block.VorbisComment
open Block.Unknown

let readMetadataBlock =
    decode {
        let! lastAndType = Decoder.readByte
        // If the first bit is set, this is the last metadata block
        let isLast = (lastAndType &&& 0x80uy) = 0x80uy
        // The remaining 7 bits are the type of the metadata block
        let blockType = lastAndType &&& 0x7Fuy
        let! length = Decoder.readUInt24

        let! data =
            match blockType with
            | 0uy -> readStreamInfo |> Decoder.map StreamInfo
            | 1uy -> readPadding length |> Decoder.map Padding
            | 2uy -> readApplication length |> Decoder.map Application
            | 3uy -> readSeekTable length |> Decoder.map SeekTable
            | 4uy -> readVorbisComment |> Decoder.map VorbisComment
            | 127uy -> Decoder.error "Invalid metadata block type found (127)"
            | t -> readUnknown t length |> Decoder.map Unknown

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
            | StreamInfo data -> Decoder.ok (block, data)
            | _ -> Decoder.error "Expected stream info block"
    }

let readBlocks =
    let rec loop blocks =
        decode {
            let! block = readMetadataBlock
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
                Decoder.ok []
            else
                // Read the rest of the blocks
                readBlocks

        return
            { StreamInfo = streamInfo
              Blocks = streamInfoBlock :: extraBlocks }
    }
