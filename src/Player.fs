module FsLAC.Player

open System
open System.Runtime.InteropServices

[<Literal>]
let private AudioToolboxFramework =
    "/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox"

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type private AudioStreamBasicDescription =
    { SampleRate: float
      FormatID: uint32
      FormatFlags: uint32
      BytesPerPacket: uint32
      FramesPerPacket: uint32
      BytesPerFrame: uint32
      ChannelsPerFrame: uint32
      BitsPerChannel: uint32
      Reserved: uint32 }

[<StructLayout(LayoutKind.Sequential)>]
type private AudioQueueBuffer =
    struct
        val mutable AudioDataBytesCapacity: uint32
        val mutable AudioData: nativeint
        val mutable AudioDataByteSize: uint32
        val mutable UserData: nativeint
        val mutable PacketDescriptionCapacity: uint32
        val mutable PacketDescriptions: nativeint
        val mutable PacketDescriptionCount: uint32
    end

type private AudioQueueOutputCallback = delegate of nativeint * nativeint * nativeint -> unit

[<DllImport(AudioToolboxFramework, CallingConvention = CallingConvention.Cdecl)>]
extern int32 private AudioQueueNewOutput(
    AudioStreamBasicDescription& format,
    AudioQueueOutputCallback callback,
    nativeint userData,
    nativeint runLoop,
    nativeint runLoopMode,
    uint32 flags,
    nativeint& queue
)

[<DllImport(AudioToolboxFramework, CallingConvention = CallingConvention.Cdecl)>]
extern int32 private AudioQueueAllocateBuffer(nativeint queue, uint32 size, nativeint& buffer)

[<DllImport(AudioToolboxFramework, CallingConvention = CallingConvention.Cdecl)>]
extern int32 private AudioQueueEnqueueBuffer(
    nativeint queue,
    nativeint buffer,
    uint32 numPacketDescs,
    nativeint packetDescs
)

[<DllImport(AudioToolboxFramework, CallingConvention = CallingConvention.Cdecl)>]
extern int32 private AudioQueueStart(nativeint queue, nativeint startTime)

[<DllImport(AudioToolboxFramework, CallingConvention = CallingConvention.Cdecl)>]
extern int32 private AudioQueueDispose(nativeint queue, [<MarshalAs(UnmanagedType.I1)>] bool immediate)

let private checkError method error =
    if error <> 0 then
        failwith $"{error} in {method}"

type private Buffer = Buffer of nativeint

module private Buffer =
    let getBufferSize (Buffer bufferPtr) =
        // Get the buffer struct from the pointer
        let mutable buffer = Marshal.PtrToStructure<AudioQueueBuffer>(bufferPtr)

        // Return the buffer size
        int buffer.AudioDataByteSize

    let copyData (data: byte array) (Buffer bufferPtr) =
        // Get the buffer struct from the pointer
        let mutable buffer = Marshal.PtrToStructure<AudioQueueBuffer>(bufferPtr)

        // Copy the data into the buffer
        Marshal.Copy(data, 0, buffer.AudioData, int buffer.AudioDataByteSize)

        // Copy back to the pointer
        Marshal.StructureToPtr(buffer, bufferPtr, true)

    let setBufferSize (size: uint32) (Buffer bufferPtr) =
        // Get the buffer struct from the pointer
        let mutable buffer = Marshal.PtrToStructure<AudioQueueBuffer>(bufferPtr)

        // Set the buffer size
        buffer.AudioDataByteSize <- size

        // Copy back to the pointer
        Marshal.StructureToPtr(buffer, bufferPtr, true)
        Buffer bufferPtr

    let ofPointer (ptr: nativeint) = Buffer ptr

type Format =
    { SampleRate: float
      Channels: uint32
      BitDepth: uint32 }

// We need to keep the callback around so it doesn't get garbage collected
type private Queue = Queue of nativeint * AudioQueueOutputCallback
type private QueueCallback = Buffer -> unit

module private Queue =
    let create format (callback: QueueCallback) =
        let mutable queuePtr = 0n

        let mutable queueFormat =
            { SampleRate = format.SampleRate
              FormatID = 1819304813u // Linear PCM
              FormatFlags = 4u ||| (1u <<< 3) // Signed integer, packed
              BitsPerChannel = format.BitDepth
              ChannelsPerFrame = format.Channels
              FramesPerPacket = 1u
              BytesPerFrame = format.BitDepth / 8u * format.Channels
              BytesPerPacket = format.BitDepth / 8u * format.Channels
              Reserved = 0u }

        let callback =
            AudioQueueOutputCallback(fun _ _ bufferPtr ->
                let buffer = Buffer.ofPointer bufferPtr
                callback buffer)

        // Create the queue
        AudioQueueNewOutput(&queueFormat, callback, 0n, 0n, 0n, 0u, &queuePtr)
        |> checkError "AudioQueueNewOutput"

        Queue(queuePtr, callback)

    let createBuffer size (Queue(queuePtr, _)) =
        let mutable bufferPtr = 0n

        // Allocate the buffer
        AudioQueueAllocateBuffer(queuePtr, size, &bufferPtr)
        |> checkError "AudioQueueAllocateBuffer"

        Buffer.ofPointer bufferPtr |> Buffer.setBufferSize size

    let start (Queue(queuePtr, _)) =
        AudioQueueStart(queuePtr, 0n) |> checkError "AudioQueueStart"

    let dispose (Queue(queuePtr, _)) =
        AudioQueueDispose(queuePtr, false) |> checkError "AudioQueueDispose"

    let enqueue (Queue(queuePtr, _)) (Buffer bufferPtr) =
        AudioQueueEnqueueBuffer(queuePtr, bufferPtr, 0u, 0n)
        |> checkError "AudioQueueEnqueueBuffer"

type private PlayerMessage =
    | Start of Queue
    | FillBuffer of Buffer
    | Stop of AsyncReplyChannel<unit>

type PlayerCallback<'State> = int -> 'State -> int32 list list * 'State

type Player<'State>(format: Format, bufferSize: uint32, callback: PlayerCallback<'State>, state: 'State) =
    let bufferByteSize = bufferSize * format.Channels * (format.BitDepth / 8u)
    let bufferSize = int bufferSize
    let mutable disposed = false

    let samplesToByteArray (samples: int32 list) =
        let bytesPerSample = int format.BitDepth / 8

        let toByteArray (sample: int32) =
            let bytes = BitConverter.GetBytes(sample)

            if BitConverter.IsLittleEndian then
                bytes[0 .. bytesPerSample - 1]
            else
                bytes[bytesPerSample..]

        // I have no clue why this type annotation is needed
        List.map toByteArray samples |> Seq.concat |> Seq.toArray

    let interleaveSamples (channels: int32 list list) =
        match channels with
        | [ mono ] -> mono
        | [ left; right ] -> List.map2 (fun left right -> [ left; right ]) left right |> List.concat
        | _ -> failwith $"Unsupported number of channels: {List.length channels}"

    let fillBuffer state queue buffer =
        // Get the new audio data (and new state)
        let audio, newState = callback bufferSize state
        let bytes = audio |> interleaveSamples |> samplesToByteArray

        // Copy the data into the buffer
        Buffer.copyData bytes buffer

        // Enqueue the buffer
        Queue.enqueue queue buffer

        // Return the new state
        newState

    let playerProcessor (inbox: MailboxProcessor<PlayerMessage>) =
        let rec loop queue state =
            async {
                match! inbox.Receive() with
                | Start queue ->
                    let buffers = List.init 3 (fun _ -> Queue.createBuffer bufferByteSize queue)
                    let state = List.fold (fun data -> fillBuffer data queue) state buffers
                    Queue.start queue
                    return! loop (Some queue) state
                | FillBuffer buffer ->
                    let newState = fillBuffer state (Option.get queue) buffer
                    return! loop queue newState
                | Stop notifyStopped ->
                    Queue.dispose (Option.get queue)
                    notifyStopped.Reply()
            }

        loop None state

    let player = MailboxProcessor.Start(playerProcessor)

    member this.Start() =
        // Create the queue
        let queue = Queue.create format (fun buffer -> player.Post(FillBuffer buffer))
        player.Post(Start queue)

    member this.Stop() = player.PostAndReply(Stop)

    member private this.Close() =
        if not disposed then
            this.Stop()
            disposed <- true

    interface IDisposable with
        member this.Dispose() =
            this.Close()
            GC.SuppressFinalize(this)

    override this.Finalize() = this.Close()
