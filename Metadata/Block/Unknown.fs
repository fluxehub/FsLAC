module FsLAC.Metadata.Block.Unknown

open FsLAC

let readUnknown blockType (length: uint) =
    decode {
        let! data = Decoder.readBytes (int length)
        return { Type = blockType; Data = data }
    }
