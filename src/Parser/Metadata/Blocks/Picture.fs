module FsLAC.Parser.Metadata.Blocks.Picture

open FsLAC.Parser
open FsLAC.Types.Metadata

let private parsePictureType =
    parse {
        let! pictureType = Parser.readUInt32

        match pictureType with
        | 0u -> return PictureType.Other
        | 1u -> return PictureType.FileIcon
        | 2u -> return PictureType.OtherFileIcon
        | 3u -> return PictureType.FrontCover
        | 4u -> return PictureType.BackCover
        | 5u -> return PictureType.LeafletPage
        | 6u -> return PictureType.Media
        | 7u -> return PictureType.LeadArtist
        | 8u -> return PictureType.Artist
        | 9u -> return PictureType.Conductor
        | 10u -> return PictureType.Band
        | 11u -> return PictureType.Composer
        | 12u -> return PictureType.Lyricist
        | 13u -> return PictureType.RecordingLocation
        | 14u -> return PictureType.DuringRecording
        | 15u -> return PictureType.DuringPerformance
        | 16u -> return PictureType.MovieScreenCapture
        | 17u -> return PictureType.ColouredFish
        | 18u -> return PictureType.Illustration
        | 19u -> return PictureType.BandLogo
        | 20u -> return PictureType.PublisherLogo
        | t -> return! Parser.error $"Unknown picture type ({t})"
    }

let parsePicture =
    parse {
        let! pictureType = parsePictureType
        let! mimeLength = Parser.readUInt32 |> Parser.map int
        let! mimeType = Parser.readString mimeLength
        let! descriptionLength = Parser.readUInt32 |> Parser.map int
        let! description = Parser.readString descriptionLength
        let! width = Parser.readUInt32
        let! height = Parser.readUInt32
        let! colorDepth = Parser.readUInt32
        let! colorCount = Parser.readUInt32
        let! pictureLength = Parser.readUInt32 |> Parser.map int
        let! picture = Parser.readBytes pictureLength

        return
            { Type = pictureType
              MimeType = mimeType
              Description = description
              Width = width
              Height = height
              ColorDepth = colorDepth
              ColorCount = colorCount
              Data = picture }
    }
