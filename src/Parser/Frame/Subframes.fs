module FsLAC.Parser.Frame.Subframes

open FsLAC.Parser
open FsLAC.Types.Frame.Subframe

let parseConstant bitsPerSample = Parser.readBitsSigned bitsPerSample

let parseVerbatim sampleCount bitsPerSample =
    List.init sampleCount (fun _ -> Parser.readBitsSigned bitsPerSample)
    |> List.sequenceParser

type private ResidualParameter =
    | Rice of int
    | Escape of int

let private parseRiceParameterBits =
    parse {
        match! Parser.readBits 2 with
        | 0UL -> return 4
        | 1UL -> return 5
        | b -> return! Parser.error $"Invalid Rice parameter bits: {b}"
    }

let private parseResidualParameter parameterBits =
    assert (parameterBits = 4 || parameterBits = 5)

    parse {
        let! parameter = Parser.readBits parameterBits |> Parser.map uint

        if
            (parameterBits = 4 && parameter = 0b1111u)
            || (parameterBits = 5 && parameter = 0b11111u)
        then
            let! bitsPerResidual = Parser.readBits 5 |> Parser.map uint
            return Escape(int bitsPerResidual)
        else
            return Rice(int parameter)
    }

let private parseRiceResidual parameter =
    parse {
        let! quotient = Parser.readUnary

        if parameter = 0 then
            return { Quotient = quotient; Remainder = 0u }
        else
            let! remainder = Parser.readBits parameter |> Parser.map uint

            return
                { Quotient = quotient
                  Remainder = uint remainder }
    }

let private parseResidualPartition parameterBits samples =
    parse {
        match! parseResidualParameter parameterBits with
        | Rice parameter ->
            return!
                List.init samples (fun _ -> parseRiceResidual parameter)
                |> List.sequenceParser
                |> Parser.map (fun codes ->
                    ResidualPartition.Rice
                        { Parameter = uint parameter
                          Codes = codes })
        | Escape parameter ->
            return!
                List.init samples (fun _ -> Parser.readBitsSigned parameter |> Parser.map int)
                |> List.sequenceParser
                |> Parser.map ResidualPartition.Unencoded
    }


let private parseResiduals blockSize predictorOrder =
    parse {
        let! riceParameterBits = parseRiceParameterBits
        let! partitionOrder = Parser.readBits 4 |> Parser.map int
        let partitionCount = 1 <<< partitionOrder

        return!
            List.init partitionCount (fun partitionIndex ->
                let sampleCount =
                    if partitionIndex = 0 then
                        (blockSize >>> partitionOrder) - predictorOrder
                    else
                        blockSize >>> partitionOrder

                parseResidualPartition riceParameterBits sampleCount)
            |> List.sequenceParser
    }

let parseFixed order blockSize bitsPerSample =
    parse {
        let! warmUp = parseVerbatim order bitsPerSample
        let! residuals = parseResiduals blockSize order

        return
            { WarmUp = warmUp
              Order = uint order
              ResidualPartitions = residuals }
    }

let private parseLpcPrecision =
    parse {
        match! Parser.readBits 4 with
        | 0b1111UL -> return! Parser.error "Invalid LPC precision"
        | b -> return (int b) + 1
    }

let parseLpc order blockSize bitsPerSample =
    parse {
        let! warmUp = parseVerbatim order bitsPerSample
        let! precision = parseLpcPrecision
        let! rightShift = Parser.readBitsSigned 5 |> Parser.map int

        let! coefficients =
            List.init order (fun _ -> Parser.readBitsSigned precision |> Parser.map int)
            |> List.sequenceParser

        let! residuals = parseResiduals blockSize order

        return
            { WarmUp = warmUp
              Order = uint order
              RightShift = rightShift
              Coefficients = coefficients
              ResidualPartitions = residuals }
    }
