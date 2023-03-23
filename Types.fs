module FsLAC.Types

type StreamInfo =
    { MinBlockSize: uint
      MaxBlockSize: uint
      MinFrameSize: uint
      MaxFrameSize: uint
      SampleRate: uint
      Channels: uint
      BitsPerSample: uint
      TotalSamples: uint64
      AudioMD5: byte[] }

type SeekPoint =
    { SampleNumber: uint64
      StreamOffset: uint64
      FrameSamplesCount: uint }

type BlockData =
    | StreamInfo of StreamInfo
    | SeekTable of SeekPoint list

type MetadataBlock =
    { Data: BlockData
      Length: uint
      IsLast: bool }

type Metadata =
    { StreamInfo: StreamInfo
      Blocks: MetadataBlock list }
