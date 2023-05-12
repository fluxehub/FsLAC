// Only used for testing Player.fs
module FsLAC.WaveFile

open System.IO

let getWaveBytes fileName =
    // Open the file
    let file = File.OpenRead(fileName)
    use reader = new BinaryReader(file)
    let mutable samples = Array.zeroCreate <| int file.Length

    for i in 0 .. (int file.Length) - 1 do
        samples[i] <- reader.ReadByte()

    samples
