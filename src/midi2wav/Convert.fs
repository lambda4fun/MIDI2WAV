module Convert

open System
open NAudio.Wave
open NAudio.Midi
open NAudio.SoundFont

[<Measure>]
type s

[<Measure>]
type Hz = /s

[<Measure>]
type beat

[<Measure>]
type tick

[<Measure>]
type sample

type Sample = {
    PreLoop  : int16[]
    Loop     : int16[]
    PostLoop : int16[] }

let frequencies = [|
    8.175<Hz>; 8.661<Hz>; 9.177<Hz>; 9.722<Hz>; 10.300<Hz>; 10.913<Hz>; 11.562<Hz>; 12.249<Hz>; 12.978<Hz>;
    13.75<Hz>; 14.567<Hz>; 15.433<Hz>; 16.351<Hz>; 17.323<Hz>; 18.354<Hz>; 19.445<Hz>; 20.601<Hz>; 21.826<Hz>; 23.124<Hz>; 24.499<Hz>; 25.956<Hz>;
    27.5<Hz>; 29.135<Hz>; 30.867<Hz>; 32.703<Hz>; 34.647<Hz>; 36.708<Hz>; 38.890<Hz>; 41.203<Hz>; 43.653<Hz>; 46.249<Hz>; 48.999<Hz>; 51.913<Hz>;
    55.0<Hz>; 58.270<Hz>; 61.735<Hz>; 65.406<Hz>; 69.295<Hz>; 73.416<Hz>; 77.781<Hz>; 82.406<Hz>; 87.307<Hz>; 92.498<Hz>; 97.998<Hz>; 103.826<Hz>; 
    110.0<Hz>; 116.540<Hz>; 123.470<Hz>; 130.812<Hz>; 138.591<Hz>; 146.832<Hz>; 155.563<Hz>; 164.813<Hz>; 174.614<Hz>; 184.997<Hz>; 195.997<Hz>; 207.652<Hz>;
    220.0<Hz>; 233.081<Hz>; 246.941<Hz>; 261.625<Hz>; 277.182<Hz>; 293.664<Hz>; 311.126<Hz>; 329.627<Hz>; 349.228<Hz>; 369.994<Hz>; 391.995<Hz>; 415.304<Hz>;
    440.0<Hz>; 466.163<Hz>; 493.883<Hz>; 523.251<Hz>; 554.365<Hz>; 587.329<Hz>; 622.253<Hz>; 659.255<Hz>; 698.456<Hz>; 739.988<Hz>; 783.990<Hz>; 830.609<Hz>;
    880.0<Hz>; 932.327<Hz>; 987.766<Hz>; 1046.502<Hz>; 1108.730<Hz>; 1174.659<Hz>; 1244.507<Hz>; 1318.510<Hz>; 1396.912<Hz>; 1479.977<Hz>; 1567.981<Hz>; 1661.218<Hz>;
    1760.0<Hz>; 1864.655<Hz>; 1975.533<Hz>; 2093.004<Hz>; 2217.461<Hz>; 2349.318<Hz>; 2489.015<Hz>; 2637.020<Hz>; 2793.825<Hz>; 2959.955<Hz>; 3135.963<Hz>; 3322.437<Hz>;
    3520.0<Hz>; 3729.310<Hz>; 3951.066<Hz>; 4186.009<Hz>; 4434.922<Hz>; 4698.636<Hz>; 4978.031<Hz>; 5274.040<Hz>; 5587.651<Hz>; 5919.910<Hz>; 6271.926<Hz>; 6644.875<Hz>;
    7040.0<Hz>; 7458.620<Hz>; 7902.132<Hz>; 8372.018<Hz>; 8869.844<Hz>; 9397.272<Hz>; 9956.063<Hz>; 10548.081<Hz>; 11175.303<Hz>; 11839.821<Hz>; 12543.853<Hz> |] |> Array.AsReadOnly

let keyToFrequency key = frequencies.[key]

let convertMidiToWave (soundFontPath : string) (instrumentName : string) (midiFilePath : string) (waveFilePath : string) =
    let midiFile = MidiFile(midiFilePath)
    let track = 0
    let events = midiFile.Events.[track]
    let tempo = 0.5<s/beat> // 500,000 microseconds
    let ticksPerBeat = float midiFile.DeltaTicksPerQuarterNote * 1.0<tick/beat>

    let soundFont = SoundFont(soundFontPath)
    let instrument = soundFont.Instruments |> Seq.find (fun i -> i.Name = instrumentName)
    let sampleRate = 32000<Hz>
    let numChannels = 1
    
    use writer = new WaveFileWriter(waveFilePath, WaveFormat(int sampleRate, numChannels))
    
    let writeSamples (samples : int16[]) (count : int<sample>) =
        writer.WriteSamples(samples, 0, int count)

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

    let ticksToSamples (ticks : int<tick>) =
        let noteLength = float ticks * tempo / ticksPerBeat
        int (noteLength * float sampleRate) * 1<sample>
        
    let recode (event : NoteOnEvent) ticks =
        let sampleCount = ticksToSamples ticks
        let key = event.NoteNumber
        let zone = findZoneByKey key
        let generators = zone.Generators
        let sampleId = generators |> Array.find (fun gen -> gen.GeneratorType = GeneratorEnum.SampleID)
        let sampleHeader = sampleId.SampleHeader
        let mutable startIndex     = int sampleHeader.Start
        let mutable startLoopIndex = int sampleHeader.StartLoop
        let mutable endLoopIndex   = int sampleHeader.EndLoop
        let mutable endIndex       = int sampleHeader.End
        let mutable rootKey = int sampleHeader.OriginalPitch

        for gen in generators do
            match gen.GeneratorType with
            | GeneratorEnum.OverridingRootKey ->
                if 0s <= gen.Int16Amount && gen.Int16Amount <= 127s then
                    rootKey <- int gen.Int16Amount
            // TODO: modify startIndex and endIndex
            | GeneratorEnum.StartAddressCoarseOffset -> ()
            | GeneratorEnum.StartAddressOffset -> ()
            | GeneratorEnum.StartLoopAddressCoarseOffset -> ()
            | GeneratorEnum.StartLoopAddressOffset -> ()
            | GeneratorEnum.EndAddressCoarseOffset -> ()
            | GeneratorEnum.EndAddressOffset -> ()
            | GeneratorEnum.EndLoopAddressCoarseOffset -> ()
            | GeneratorEnum.EndLoopAddressOffset -> ()
            | _ -> ()
        
        let frequency = keyToFrequency key
        let rootFrequency = keyToFrequency rootKey
        let delta = frequency / rootFrequency
        
        let toInt16s (bytes : byte[]) =
            bytes
            |> Seq.chunkBySize 2
            |> Seq.map (fun pair -> BitConverter.ToInt16(pair, 0))
            |> Seq.toArray
        
        let interpolate (src : int16[]) delta =
            let dst = Array.zeroCreate (int (float src.Length / delta))
            ({ 0 .. dst.Length-1 }, { 0.0 .. delta .. float (src.Length-1) })
            ||> Seq.iter2 (fun dstIndex srcIndex ->
                let r1 = srcIndex % 1.0
                let r2 = 1.0 - r1
                let src = (float src.[int srcIndex] * r1) + (float src.[int (ceil srcIndex)] * r2)
                dst.[dstIndex] <- int16 src)
            dst
            
        let sample = {
            PreLoop  = interpolate (toInt16s soundFont.SampleData.[startIndex*2 .. (startLoopIndex*2)-1]) delta
            Loop     = interpolate (toInt16s soundFont.SampleData.[startLoopIndex*2 .. (endLoopIndex*2)+1]) delta
            PostLoop = interpolate (toInt16s soundFont.SampleData.[(endLoopIndex+1)*2 .. (endIndex*2)+1]) delta }
        
        let preLoopLength  = min sampleCount (sample.PreLoop.Length * 1<sample>)
        let postLoopLength = max 0<sample> (min (sampleCount - preLoopLength) (sample.PostLoop.Length * 1<sample>))
        let loopLength     = max 0<sample> (min (sampleCount- preLoopLength - postLoopLength) (sample.Loop.Length * 1<sample>))

        writeSamples sample.PreLoop preLoopLength

        if loopLength > 0<sample> then
            for _ in 1 .. (sampleCount / loopLength) do
                writeSamples sample.Loop loopLength
            writeSamples sample.Loop (sampleCount % loopLength)

        writeSamples sample.PostLoop postLoopLength

        printfn "write: note = %s (%d), duration = %d (%d ticks = %d samples)" event.NoteName event.NoteNumber event.NoteLength ticks sampleCount

    let noteOnEvents =
        events
        |> Seq.filter (fun event -> event.CommandCode = MidiCommandCode.NoteOn)
        |> Seq.map (fun event -> event :?> NoteOnEvent)

    let skip (n : int<sample>) =
        writeSamples (Array.zeroCreate<int16> (int n)) n

    let recodeNoteOn (event : NoteOnEvent) length =
        let noteOnLength = event.NoteLength * 1<tick>
        recode event noteOnLength
        skip (ticksToSamples (length - noteOnLength))

    noteOnEvents
    |> Seq.pairwise
    |> Seq.iter (fun (event1, event2) ->
        let noteLength = int (event2.AbsoluteTime - event1.AbsoluteTime) * 1<tick>
        recodeNoteOn event1 noteLength)

    let lastMidiEvent = Seq.last events
    let lastNoteOnEvent = Seq.last noteOnEvents
    let ticks = int (lastMidiEvent.AbsoluteTime - lastNoteOnEvent.AbsoluteTime) * 1<tick>
    recodeNoteOn lastNoteOnEvent ticks
