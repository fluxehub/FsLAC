module FsLAC.Parser.Metadata.Blocks.Application

open FsLAC.Parser
open FsLAC.Types.Metadata

let parseApplication (length: uint) =
    parse {
        let! id = Parser.readBytes 4
        let! data = Parser.readBytes (int length - 4)
        return { Id = id; Data = data }
    }
