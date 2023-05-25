module FsLAC.Utility

let signExtend value bitCount =
    let mask = 1L <<< (bitCount - 1)
    (value ^^^ mask) - mask
