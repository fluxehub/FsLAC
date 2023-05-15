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
      ChannelMode: ChannelAssignment
      BitDepth: uint
      FrameOrSampleNumber: uint64
      CRC: uint }

type Subframe 