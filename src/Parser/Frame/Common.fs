module FsLAC.Parser.Frame.Common

open FsLAC.Parser

let checkZeroBit =
    parse {
        let! zeroBit = Parser.readBits 1

        if zeroBit <> 0UL then
            return! Parser.error "Expected zero bit but got 1"
    }
