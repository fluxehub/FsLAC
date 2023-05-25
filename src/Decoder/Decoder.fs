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

let private decodeSamples bitDepth warmUp coefficients shift residuals useResiduals =
    let maskSample sample =
        let bitDepth = int32 bitDepth
        let masked = sample &&& ((1L <<< bitDepth) - 1L)
        Utility.signExtend masked bitDepth

    let rec decodeLoop previous residuals =
        match residuals with
        | [] -> []
        | residual :: residuals ->
            let predictedSample = (predictSample previous coefficients) >>> shift

            let sample =
                if useResiduals then
                    predictedSample + int64 residual
                else
                    predictedSample

            // Mask out the bits that are not part of the sample.
            let sample = maskSample sample

            // This is anti-FP, but O(n) doesn't matter that much when n is max 4.
            let previous = List.tail previous @ [ sample ]
            sample :: decodeLoop previous residuals

    decodeLoop warmUp residuals

let private getFixedCoefficients =
    function
    | 0u -> [ 0 ] // Realistically, there should be no coefficient as there's no prediction in order 0.
    // This makes it easier though.
    | 1u -> [ 1 ]
    | 2u -> [ 2; -1 ]
    | 3u -> [ 3; -3; 1 ]
    | 4u -> [ 4; -6; 4; -1 ]
    | order -> failwith $"Invalid fixed order: {order}"

let private decodeSubframe frameHeader bitDepth useResiduals subframe =
    let data =
        match subframe.Data with
        | Constant sample -> List.init (int frameHeader.BlockSize) (fun _ -> sample)
        | Verbatim samples -> samples
        | Fixed subframe ->
            let coefficients = getFixedCoefficients subframe.Order
            let warmUp = if subframe.Order = 0u then [ 0L ] else subframe.WarmUp
            let residuals = decodeResiduals subframe.ResidualPartitions
            decodeSamples bitDepth warmUp coefficients 0 residuals useResiduals
        | Lpc subframe ->
            let coefficients = List.rev subframe.Coefficients // LPC coefficients are stored in reverse order
            let residuals = decodeResiduals subframe.ResidualPartitions
            decodeSamples bitDepth subframe.WarmUp coefficients subframe.RightShift residuals useResiduals

    List.map (addWastedBits subframe.WastedBits) data

let decodeFrame useResiduals (frame: Frame) =
    decodeSubframe frame.Header frame.Header.BitDepth useResiduals frame.Subframes[0]
