module FsLAC.Metadata.Block.Padding

open FsLAC

let readPadding (length: uint) =
    decode {
        do! Decoder.skip (int64 length)
        return length
    }
