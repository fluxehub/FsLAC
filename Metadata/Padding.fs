module FsLAC.MetadataBlocks.Padding

open FsLAC
open Decoder

let readPadding (length: uint) =
    decode {
        do! skip (int64 length)
        return length
    }
