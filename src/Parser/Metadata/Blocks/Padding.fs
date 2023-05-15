module FsLAC.Parser.Metadata.Blocks.Padding

open FsLAC.Parser

let parsePadding length =
    parse {
        do! Parser.skip length
        return length
    }
