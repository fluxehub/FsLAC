module FsLAC.Metadata.Block.Application

open FsLAC

let readApplication (length: uint) =
    decode {
        let! id = Decoder.readBytes 4
        let! data = Decoder.readBytes (int length - 4)
        return { Id = id; Data = data }
    }
