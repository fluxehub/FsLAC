module FsLAC.Parser.Metadata.MetadataParser

open FsLAC.Parser
open FsLAC.Types.Metadata
open Blocks.StreamInfo
open Blocks.Padding
open Blocks.Application
open Blocks.SeekTable
open Blocks.VorbisComment
open Blocks.Picture
open Blocks.Unknown

let private parseMetadataBlock =
    parse {
        let! lastAndType = Parser.readByte
        // If the first bit is set, this is the last metadata block
        let isLast = (lastAndType &&& 0x80uy) = 0x80uy
        // The remaining 7 bits are the type of the metadata block
        let blockType = lastAndType &&& 0x7Fuy
        let! length = Parser.readUInt24

        let! data =
            match blockType with
            | 0uy -> parseStreamInfo |> Parser.map StreamInfo
            | 1uy -> parsePadding length |> Parser.map Padding
            | 2uy -> parseApplication length |> Parser.map Application
            | 3uy -> parseSeekTable length |> Parser.map SeekTable
            | 4uy -> parseVorbisComment |> Parser.map VorbisComment
            | 6uy -> parsePicture |> Parser.map Picture
            | 127uy -> Parser.error "Invalid metadata block type found (127)"
            | t -> parseUnknown t length |> Parser.map Unknown

        return
            { Data = data
              Length = length
              IsLast = isLast }
    }

let private parseStreamInfoBlock =
    parse {
        let! block = parseMetadataBlock

        return!
            match block.Data with
            | StreamInfo data -> Parser.ok (block, data)
            | _ -> Parser.error "Expected stream info block"
    }

let private parseBlocks =
    let rec loop blocks =
        parse {
            let! block = parseMetadataBlock
            let blocks = block :: blocks

            if block.IsLast then
                return List.rev blocks
            else
                return! loop blocks
        }

    loop []

let parseMetadata =
    parse {
        // The stream info block is always the first block
        let! streamInfoBlock, streamInfo = parseStreamInfoBlock

        let! extraBlocks =
            if streamInfoBlock.IsLast then
                Parser.ok []
            else
                // Read the rest of the blocks
                parseBlocks

        return
            { StreamInfo = streamInfo
              Blocks = streamInfoBlock :: extraBlocks }
    }