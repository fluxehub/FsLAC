module FsLAC.Decoder.Decoder

open FSharpx.Collections
open FsLAC
open FsLAC.Types
open FsLAC.Types.Frame.Subframe

let private addWastedBits wastedBitCount sample = sample <<< (int32 wastedBitCount)

let private decodeResidualPartition partition =
    match partition with
    | Rice ricePartition ->
        let parameter = ricePartition.Parameter
        let codes = ricePartition.Codes

        let decodeRice code =
            let value = (code.Quotient <<< int32 parameter) + code.Remainder

            if value % 2u = 0u then value >>> 1 else ~~~(value >>> 1)
            |> int

        List.map decodeRice codes
    | Unencoded residuals -> residuals

let private decodeResiduals residuals =
    List.map decodeResidualPartition residuals |> List.concat

let private predictSample previous coefficients =
    assert (List.length previous = List.length coefficients)
    List.fold2 (fun acc sample coefficient -> acc + (int64 sample * int64 coefficient)) 0L previous coefficients

let maskSample bitDepth sample =
    let bitDepth = int32 bitDepth
    let masked = sample &&& ((1L <<< bitDepth) - 1L)
    Utility.signExtend masked bitDepth

let private decodeSamples bitDepth warmUp coefficients shift residuals useResiduals =
    // Accumulator style because decode needs to be tail recursive.
    let rec decodeLoop samples previous residuals =
        match residuals with
        | [] -> List.rev samples
        | residual :: residuals ->
            let predictedSample = (predictSample previous coefficients) >>> shift

            let sample =
                if useResiduals then
                    predictedSample + int64 residual
                else
                    predictedSample

            // Mask out the bits that are not part of the sample.
            let sample = maskSample bitDepth sample

            // This is anti-FP, but O(n) doesn't matter that much when n is max 4.
            let previous = List.tail previous @ [ sample ]
            decodeLoop (sample :: samples) previous residuals

    decodeLoop (List.rev warmUp) warmUp residuals

let private getFixedCoefficients =
    function
    | 0u -> [ 0 ] // Realistically, there should be no coefficient as there's no prediction in order 0.
    // This makes it easier though.
    | 1u -> [ 1 ]
    | 2u -> [ -1; 2 ]
    | 3u -> [ 1; -3; 3 ]
    | 4u -> [ -1; 4; -6; 4 ]
    | order -> failwith $"Invalid fixed order: {order}"

let private decodeSubframe useResiduals frameHeader isSide subframe =
    let bitDepth =
        if isSide then
            frameHeader.BitDepth + 1u
        else
            frameHeader.BitDepth

    let data =
        match subframe.Data with
        | Constant sample -> List.init (int frameHeader.BlockSize) (fun _ -> sample)
        | Verbatim samples -> samples
        | Fixed subframe ->
            let coefficients = getFixedCoefficients subframe.Order
            let warmUp = if subframe.Order = 0u then [ 0L ] else subframe.WarmUp
            let residuals = decodeResiduals subframe.ResidualPartitions
            let samples = decodeSamples bitDepth warmUp coefficients 0 residuals useResiduals
            if subframe.Order = 0u then List.tail samples else samples
        | Lpc subframe ->
            let coefficients = List.rev subframe.Coefficients // LPC coefficients are stored in reverse order
            let residuals = decodeResiduals subframe.ResidualPartitions
            decodeSamples bitDepth subframe.WarmUp coefficients subframe.RightShift residuals useResiduals

    List.map (addWastedBits subframe.WastedBits) data

let decodeFrame useResiduals (frame: Frame) =
    let decodeSubframe = decodeSubframe useResiduals frame.Header

    match frame.Header.ChannelAssignment with
    | Mono -> [ decodeSubframe false frame.Subframes[0] |> List.map int32 ]
    | LeftRight ->
        [ decodeSubframe false frame.Subframes[0] |> List.map int32
          decodeSubframe false frame.Subframes[1] |> List.map int32 ]
    | LeftSide ->
        let left = decodeSubframe false frame.Subframes[0]
        let side = decodeSubframe true frame.Subframes[1]

        let right =
            List.map2 (fun left side -> maskSample frame.Header.BitDepth (left - side)) left side

        [ left |> List.map int32; right |> List.map int32 ]
    | RightSide ->
        let side = decodeSubframe true frame.Subframes[0]
        let right = decodeSubframe false frame.Subframes[1]

        let left =
            List.map2 (fun side right -> maskSample frame.Header.BitDepth (right + side)) side right

        [ left |> List.map int32; right |> List.map int32 ]
    | MidSide ->
        let mid = decodeSubframe false frame.Subframes[0]
        let side = decodeSubframe true frame.Subframes[1]

        let decorrelate operation (mid: int64) (side: int64) =
            let mid = (mid <<< 1) ||| (side &&& 1)
            let sample = operation mid side
            sample >>> 1

        let left = List.map2 (decorrelate (+)) mid side
        let right = List.map2 (decorrelate (-)) mid side

        [ left |> List.map int32; right |> List.map int32 ]
