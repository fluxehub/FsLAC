[<AutoOpen>]
module FsLAC.Types.Frame

type BlockingStrategy =
    | Variable
    | Fixed

type ChannelAssignment =
    | Mono
    | LeftRight
    | MidSide
    | LeftSide
    | RightSide

type FrameHeader =
    { BlockingStrategy: BlockingStrategy
      BlockSize: uint
      SampleRate: uint
      ChannelAssignment: ChannelAssignment
      BitDepth: uint
      FrameOrSampleNumber: uint64
      CRC: uint }

module Subframe =
    type RiceCode = { Quotient: uint; Remainder: uint }

    type RicePartition =
        { Parameter: uint
          Codes: RiceCode list }

    type ResidualPartition =
        | Rice of RicePartition
        | Unencoded of int list

    type Constant = Constant of int64
    type Verbatim = Verbatim of int64 list

    type Fixed =
        { Order: uint
          WarmUp: int64 list
          ResidualPartitions: ResidualPartition list }

    type Lpc =
        { Order: uint
          WarmUp: int64 list
          Coefficients: int list
          RightShift: int
          ResidualPartitions: ResidualPartition list }

    type SubframeData =
        | Constant of int64
        | Verbatim of int64 list
        | Fixed of Fixed
        | Lpc of Lpc

    type Subframe =
        { WastedBits: uint; Data: SubframeData }

type Frame =
    { Header: FrameHeader
      Subframes: Subframe.Subframe list
      CRC: uint }
