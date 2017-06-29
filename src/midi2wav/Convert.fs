module Convert

open System
open System.IO
open NAudio.Wave
open NAudio.Midi
open NAudio.SoundFont
open Units

[<Measure>] type s
[<Measure>] type Hz = /s
[<Measure>] type beat
[<Measure>] type tick
[<Measure>] type sample
[<Measure>] type timecent
[<Measure>] type cB
[<Measure>] type midikey
[<Measure>] type midivel

let toSeconds (t : float<timecent>) = (2.0 ** -1200.0) ** float t * 1.0<s>

let toAddress (index : int<sample>) = int index * 2
    
let toInt16Array (bytes : byte[]) =
    // Ignores last element if bytes.Length is odd.
    let shorts = Array.zeroCreate<int16> (bytes.Length / 2)
    for i in 0 .. shorts.Length-1 do
        shorts.[i] <- (int16 bytes.[i*2 + 1] <<< 8) ||| int16 bytes.[i*2]
    shorts

// TODO: Implement an equoation which is more consist with other synthesizers.
// Reference: The Interpretation of MIDI Velocity by Roger B. Dannenberg
let toAmplitude (v : int<midivel>) =
    (float v / 127.0) ** 2.0

type Progress =
    | Start
    | Change of float<percent>
    | Finish

type ProgressCallback = Progress -> Async<unit>

type Sample =
    { PreLoop  : int16[]
      Loop     : int16[]
      PostLoop : int16[] }

type SampleIndices =
    { StartIndex     : int<sample>
      StartLoopIndex : int<sample> 
      EndLoopIndex   : int<sample>
      EndIndex       : int<sample> }

type VolumeEnvelopeInfo =
    { DelayLength            : int<timecent>
      AttackLength           : int<timecent>
      HoldLength             : int<timecent>
      DecayLength            : int<timecent>
      SustainAttenuation     : int<cB>
      ReleaseLength          : int<timecent>
      KeyNumberToHoldLength  : int<midikey>
      KeyNumberToDecayLength : int<midikey> }

let keyToFrequency key =
    let n = float (key - 69)
    440.0<Hz> * (2.0 ** (n / 12.0))
        
type MidiToWaveConverter(soundFont : SoundFont, midiFile : MidiFile, outStream : Stream) =
    let track = 0
    let events = midiFile.Events.[track]
    let tempo = 0.5<s/beat> // 500,000 microseconds
    let ticksPerBeat = float midiFile.DeltaTicksPerQuarterNote * 1.0<tick/beat>
    let instrumentName = "Yamaha Grand Piano"
    let instrument = soundFont.Instruments |> Seq.find (fun i -> i.Name = instrumentName)
    let sampleRate = 32000<Hz>
    let numChannels = 1

    let writeSamples (samples : int16[]) (count : int<sample>) (writer : WaveFileWriter) =
        writer.WriteSamples(samples, 0, int count)

    let skip (n : int<sample>) =
        writeSamples (Array.zeroCreate<int16> (int n)) n

    let findZoneByKey key =
        instrument.Zones
        |> Array.filter (fun zone ->
            zone.Generators
            |> Array.exists (fun gen -> gen.GeneratorType = GeneratorEnum.SampleID))
        |> Array.find (fun zone ->
            zone.Generators
            |> Array.exists (fun gen ->
                gen.GeneratorType = GeneratorEnum.KeyRange &&
                int gen.LowByteAmount <= key && key <= int gen.HighByteAmount))
        
    let interpolate delta (src : int16[]) =
        let dst = Array.zeroCreate (int (float src.Length / delta))
        ({ 0 .. dst.Length-1 }, { 0.0 .. delta .. float (src.Length-1) })
        ||> Seq.iter2 (fun dstIndex srcIndex ->
            let r1 = srcIndex % 1.0
            let r2 = 1.0 - r1
            let src = (float src.[int srcIndex] * r1) + (float src.[int (ceil srcIndex)] * r2)
            dst.[dstIndex] <- int16 src)
        dst

    let ticksToSamples (ticks : int<tick>) =
        let noteLength = float ticks * tempo / ticksPerBeat
        int (noteLength * float sampleRate) * 1<sample>
    
    let computeSampleIndices (sampleHeader : SampleHeader) (generators : Generator seq) =
        let mutable startIndex     = int sampleHeader.Start     * 1<sample>
        let mutable startLoopIndex = int sampleHeader.StartLoop * 1<sample>
        let mutable endLoopIndex   = int sampleHeader.EndLoop   * 1<sample>
        let mutable endIndex       = int sampleHeader.End       * 1<sample>
        
        for gen in generators do
            let offset       index = index + int gen.Int16Amount * 1<sample>
            let coarseOffset index = index + int gen.Int16Amount * 32768<sample>

            match gen.GeneratorType with
            | GeneratorEnum.StartAddressOffset ->
                startIndex     <- offset startIndex
            | GeneratorEnum.EndAddressOffset ->
                endIndex       <- offset endIndex
            | GeneratorEnum.StartLoopAddressOffset ->
                startLoopIndex <- offset startLoopIndex
            | GeneratorEnum.EndLoopAddressOffset ->
                endLoopIndex   <- offset endLoopIndex
            | GeneratorEnum.StartAddressCoarseOffset ->
                startIndex     <- coarseOffset startIndex
            | GeneratorEnum.EndAddressCoarseOffset ->
                endIndex       <- coarseOffset endIndex
            | GeneratorEnum.StartLoopAddressCoarseOffset ->
                startLoopIndex <- coarseOffset startLoopIndex
            | GeneratorEnum.EndLoopAddressCoarseOffset ->
                endLoopIndex   <- coarseOffset endLoopIndex
            | _ -> ()

        { StartIndex     = startIndex
          StartLoopIndex = startLoopIndex
          EndLoopIndex   = endLoopIndex
          EndIndex       = endIndex }

    // TODO: Implement volume envelope.
    let computeVolumeEnvelopes (generators : Generator seq) =
        for gen in generators do
            match gen.GeneratorType with
            | GeneratorEnum.DelayVolumeEnvelope ->
                let t = float gen.Int16Amount * 1.0<timecent>
                printfn "%.20f" <| toSeconds t
            | GeneratorEnum.AttackVolumeEnvelope -> ()
            | GeneratorEnum.HoldVolumeEnvelope -> ()
            | GeneratorEnum.DecayVolumeEnvelope -> ()
            | GeneratorEnum.SustainVolumeEnvelope -> ()
            | GeneratorEnum.ReleaseVolumeEnvelope -> ()
            | GeneratorEnum.KeyNumberToVolumeEnvelopeHold -> ()
            | GeneratorEnum.KeyNumberToVolumeEnvelopeDecay -> ()
            | _ -> ()
    
    let computeRootKey (sampleHeader : SampleHeader) (generators : Generator seq) =
        let mutable rootKey = int sampleHeader.OriginalPitch
        for gen in generators do
            match gen.GeneratorType with
            | GeneratorEnum.OverridingRootKey ->
                if 0s <= gen.Int16Amount && gen.Int16Amount <= 127s then
                    rootKey <- int gen.Int16Amount
            | _ -> ()
        rootKey

    let recode (event : NoteOnEvent) ticks writer =
        let sampleCount = ticksToSamples ticks
        let key = event.NoteNumber
        let zone = findZoneByKey key
        let generators = zone.Generators
        let sampleId = generators |> Array.find (fun gen -> gen.GeneratorType = GeneratorEnum.SampleID)
        let sampleHeader = sampleId.SampleHeader
        let sampleIndices = computeSampleIndices sampleHeader generators
        let rootKey = computeRootKey sampleHeader generators
        let frequency = keyToFrequency key
        let rootFrequency = keyToFrequency rootKey
        let delta = frequency / rootFrequency
        let amplitude = event.Velocity * 1<midivel> |> toAmplitude

        let mapSampleData sampleData =
            sampleData
            |> toInt16Array
            |> interpolate delta
            |> Array.map (float >> ((*) amplitude) >> int16)

        let sample = {
            PreLoop  = mapSampleData soundFont.SampleData.[toAddress sampleIndices.StartIndex .. (toAddress sampleIndices.StartLoopIndex)-1]
            Loop     = mapSampleData soundFont.SampleData.[toAddress sampleIndices.StartLoopIndex .. (toAddress sampleIndices.EndLoopIndex)+1]
            PostLoop = mapSampleData soundFont.SampleData.[toAddress (sampleIndices.EndLoopIndex+1<sample>) .. (toAddress sampleIndices.EndIndex)+1] }
        
        let preLoopLength  = min sampleCount (sample.PreLoop.Length * 1<sample>)
        let postLoopLength = max 0<sample> (min (sampleCount - preLoopLength) (sample.PostLoop.Length * 1<sample>))
        let loopLength     = max 0<sample> (min (sampleCount- preLoopLength - postLoopLength) (sample.Loop.Length * 1<sample>))

        writer |> writeSamples sample.PreLoop preLoopLength

        if loopLength > 0<sample> then
            for _ in 1 .. (sampleCount / loopLength) do
                writer |> writeSamples sample.Loop loopLength
            writer |> writeSamples sample.Loop (sampleCount % loopLength)

        writer |> writeSamples sample.PostLoop postLoopLength

        printfn "write: note = %s (%d), duration = %d (%d ticks = %d samples)" event.NoteName event.NoteNumber event.NoteLength ticks sampleCount

    let recodeNoteOn (event : NoteOnEvent) length writer =
        let noteOnLength = event.NoteLength * 1<tick>
        writer |> recode event noteOnLength
        writer |> skip (ticksToSamples (length - noteOnLength))
    
    member __.Convert(callback : ProgressCallback) = async {
        let! token = Async.StartChild <| callback Start
        do! token

        use writer = new WaveFileWriter(outStream, WaveFormat(int sampleRate, numChannels))
        
        let noteOnEvents =
            events
            |> Seq.filter (fun event -> event.CommandCode = MidiCommandCode.NoteOn)
            |> Seq.map (fun event -> event :?> NoteOnEvent)
            |> Seq.toArray

        for (i, (event1, event2)) in noteOnEvents |> Seq.pairwise |> Seq.indexed do
            let noteLength = int (event2.AbsoluteTime - event1.AbsoluteTime) * 1<tick>
            writer |> recodeNoteOn event1 noteLength
            let! token = Async.StartChild <| callback (Change (float i / float noteOnEvents.Length * 100.0<percent>))
            do! token

        let lastMidiEvent = Seq.last events
        let lastNoteOnEvent = Seq.last noteOnEvents
        let ticks = int (lastMidiEvent.AbsoluteTime - lastNoteOnEvent.AbsoluteTime) * 1<tick>
        writer |> recodeNoteOn lastNoteOnEvent ticks
        
        let! token = Async.StartChild <| callback Finish
        do! token }
    
let convertMidiToWave (soundFontPath : string) (midiFilePath : string) (waveFilePath : string) (callback : ProgressCallback) =
    let soundFont = SoundFont(soundFontPath)
    let midiFile = MidiFile(midiFilePath)
    let outStream = new FileStream(waveFilePath, FileMode.Create, FileAccess.Write, FileShare.Read)
    MidiToWaveConverter(soundFont, midiFile, outStream).Convert(callback)
