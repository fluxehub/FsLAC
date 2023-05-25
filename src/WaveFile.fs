// Only used for testing Player.fs
module FsLAC.WaveFile

open System.IO

let getWaveBytes fileName =
    // Open the file
    let file = File.OpenRead(fileName)
    use reader = new BinaryReader(file)
    reader.BaseStream.Seek(64L, SeekOrigin.Begin) |> ignore

    // Read the samples
    let rec loop left right position =
        if position >= 5000000L then
            [ List.rev left; List.rev right ]
        else
            let leftSample = reader.ReadInt16() |> int
            let rightSample = reader.ReadInt16() |> int
            loop (leftSample :: left) (rightSample :: right) (position + 1L)

    loop [] [] 0L
