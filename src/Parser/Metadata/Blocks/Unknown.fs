module FsLAC.Parser.Metadata.Blocks.Unknown

open FsLAC.Parser
open FsLAC.Types.Metadata

let parseUnknown blockType (length: uint) =
    parse {
        let! data = Parser.readBytes (int length)
        return { Type = blockType; Data = data }
    }
