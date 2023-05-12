module FsLAC.Metadata.Block.Picture

open FsLAC

let private readPictureType =
    decode {
        let! pictureType = Decoder.readUInt32

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
        | t -> return! Decoder.error $"Unknown picture type ({t})"
    }

let readPicture =
    decode {
        let! pictureType = readPictureType
        let! mimeLength = Decoder.readUInt32 |> Decoder.map int
        let! mimeType = Decoder.readString mimeLength
        let! descriptionLength = Decoder.readUInt32 |> Decoder.map int
        let! description = Decoder.readString descriptionLength
        let! width = Decoder.readUInt32
        let! height = Decoder.readUInt32
        let! colorDepth = Decoder.readUInt32
        let! colorCount = Decoder.readUInt32
        let! pictureLength = Decoder.readUInt32 |> Decoder.map int
        let! picture = Decoder.readBytes pictureLength

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
