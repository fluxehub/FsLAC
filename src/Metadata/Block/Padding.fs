module FsLAC.Metadata.Block.Padding

open FsLAC

let readPadding length =
    decode {
        do! Decoder.skip length
        return length
    }
