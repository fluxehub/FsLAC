module FsLAC.Metadata

open Decoder
open MetadataBlocks.StreamInfo
open MetadataBlocks.Padding
open MetadataBlocks.SeekTable
open MetadataBlocks.VorbisComment

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
            | 0uy -> readStreamInfo |> map StreamInfo
            | 1uy -> readPadding length |> map Padding
            | 3uy -> readSeekTable length |> map SeekTable
            | 4uy -> readVorbisComment |> map VorbisComment
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
