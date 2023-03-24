[<AutoOpen>]
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

type Application = { Id: byte[]; Data: byte[] }

type SeekPoint =
    { SampleNumber: uint64
      StreamOffset: uint64
      FrameSamplesCount: uint }

type VorbisComment =
    { Vendor: string
      Comments: Map<string, string> }

type PictureType =
    | Other
    | FileIcon
    | OtherFileIcon
    | FrontCover
    | BackCover
    | LeafletPage
    | Media
    | LeadArtist
    | Artist
    | Conductor
    | Band
    | Composer
    | Lyricist
    | RecordingLocation
    | DuringRecording
    | DuringPerformance
    | MovieScreenCapture
    | ColouredFish
    | Illustration
    | BandLogo
    | PublisherLogo

type Picture =
    { Type: PictureType
      MimeType: string
      Description: string
      Width: uint
      Height: uint
      ColorDepth: uint
      ColorCount: uint
      Data: byte[] }

type UnknownBlock = { Type: byte; Data: byte[] }

type BlockData =
    | StreamInfo of StreamInfo
    | Padding of uint
    | Application of Application
    | SeekTable of SeekPoint list
    | VorbisComment of VorbisComment
    | Picture of Picture
    | Unknown of UnknownBlock

type MetadataBlock =
    { Data: BlockData
      Length: uint
      IsLast: bool }

type Metadata =
    { StreamInfo: StreamInfo
      Blocks: MetadataBlock list }
