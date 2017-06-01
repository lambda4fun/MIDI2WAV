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
type min

let tryFindGeneratorByType t (generators : Generator[]) =
    generators
    |> Seq.tryFind (fun g -> g.GeneratorType = t)

let findGeneratorsByKey key (instrument : Instrument) =
    let find (gs : Generator[]) =
        let keyRangeOption = gs |> tryFindGeneratorByType GeneratorEnum.KeyRange
        let sampleIdOption = gs |> tryFindGeneratorByType GeneratorEnum.SampleID
        let check (keyRange : Generator) (sampleId : Generator) =
            if keyRange.LowByteAmount <= key && key <= keyRange.HighByteAmount
            then
                Some (sampleId.SampleHeader, (keyRange.LowByteAmount, keyRange.HighByteAmount))
            else
                None
        
        Option.map2 check keyRangeOption sampleIdOption
        |> Option.bind id
    
    instrument.Zones
    |> Seq.map (fun zone -> zone.Generators)
    |> Seq.tryPick find

let findSampleHeader key (instrument : Instrument) =
    instrument
    |> findGeneratorsByKey key
    |> Option.map fst

let frequencies = [|
    8.17579891564368; 8.66195721802723; 9.17702399741896; 9.722718241315; 10.3008611535272; 10.9133822322813; 11.5623257097385; 12.2498573744296; 12.9782717993733;
    13.75; 14.5676175474403; 15.4338531642538; 16.3515978312874; 17.3239144360545; 18.3540479948379; 19.44543648263; 20.6017223070543; 21.8267644645627; 23.1246514194771; 24.4997147488593; 25.9565435987465;
    27.5; 29.1352350948806; 30.8677063285077; 32.7031956625748; 34.6478288721089; 36.7080959896759; 38.89087296526; 41.2034446141087; 43.6535289291254; 46.2493028389542; 48.9994294977186; 51.9130871974931;
    55.0; 58.2704701897612; 61.7354126570154; 65.4063913251496; 69.2956577442179; 73.4161919793518; 77.7817459305201; 82.4068892282174; 87.3070578582509; 92.4986056779085; 97.9988589954372; 103.826174394986; 
    110.0; 116.540940379522; 123.470825314031; 130.812782650299; 138.591315488436; 146.832383958704; 155.56349186104; 164.813778456435; 174.614115716502; 184.997211355817; 195.997717990875; 207.652348789972;
    220.0; 233.081880759045; 246.941650628062; 261.625565300599; 277.182630976872; 293.664767917407; 311.126983722081; 329.62755691287; 349.228231433004; 369.994422711634; 391.995435981749; 415.304697579945;
    440.0; 466.16376151809; 493.883301256124; 523.251130601197; 554.365261953744; 587.329535834815; 622.253967444162; 659.25511382574; 698.456462866008; 739.988845423269; 783.990871963499; 830.609395159891;
    880.0; 932.32752303618; 987.766602512249; 1046.5022612024; 1108.73052390749; 1174.65907166963; 1244.50793488832; 1318.51022765148; 1396.91292573202; 1479.97769084654; 1567.981743927; 1661.21879031978;
    1760.0; 1864.65504607236; 1975.5332050245; 2093.00452240479; 2217.46104781498; 2349.31814333926; 2489.01586977665; 2637.02045530296; 2793.82585146403; 2959.95538169308; 3135.963487854; 3322.43758063957;
    3520.0; 3729.31009214472; 3951.066410049; 4186.00904480959; 4434.92209562996; 4698.63628667853; 4978.0317395533; 5274.04091060593; 5587.65170292807; 5919.91076338616; 6271.926975708; 6644.87516127914;
    7040.0; 7458.62018428945; 7902.132820098; 8372.01808961917; 8869.84419125993; 9397.27257335706; 9956.06347910661; 10548.0818212119; 11175.3034058562; 11839.8215267723; 12543.853951416 |] |> Array.AsReadOnly

let noteToFrequency note = frequencies.[note]

// TODO: Consider using BufferedWaveProvider or MultiplexingWaveProvider.
type SoundFontWaveProvider(soundFont : SoundFont, instrument : Instrument) as this =
    inherit WaveProvider16()
    
    let mutable key = 60
    // TODO: Process Option.get error.
    let mutable h = findSampleHeader (byte key) instrument |> Option.get
    let mutable startKey = 60

    let changeKey k =
        key <- k
        findGeneratorsByKey (byte key) instrument
        |> Option.iter (fun (sampleHeader, (sk, _)) ->
            h <- sampleHeader
            startKey <- int sk)
        // TODO: Use Result discriminated union instead of exception.
        let currentSampleRate = (this :> WaveProvider16).WaveFormat.SampleRate
        if int h.SampleRate <> currentSampleRate
        then failwithf "Changing sample rate after initialization is not supported. (%dHz -> %dHz)" currentSampleRate h.SampleRate

    do
        (this :> WaveProvider16).SetWaveFormat(int h.SampleRate, 1)
    
    member __.Key with get() = key
                  and  set k = changeKey k
        
    override __.Read(buffer : int16[], offset : int, sampleCount : int) =
        let mutable sampleIndex = 0u
        let deltaIndex = float (noteToFrequency startKey) / float (noteToFrequency this.Key)

        // TODO: Shift sampleIndex instead of i so that buffer is filled correctly.
        { 0.0 .. deltaIndex .. (float sampleCount - 1.0) }
        |> Seq.iter (fun i ->
            let sample = BitConverter.ToInt16(soundFont.SampleData, int (h.Start + sampleIndex) * 2)
            if BitConverter.IsLittleEndian
            then buffer.[int i] <- sample
            else buffer.[int i] <- (sample <<< 8) ||| (sample >>> 8)

            if sampleIndex >= h.EndLoop - h.Start && uint32 i < uint32 sampleCount - (h.End - h.EndLoop)
            then sampleIndex <- h.StartLoop - h.Start
            else sampleIndex <- sampleIndex + 1u)
        sampleCount

let soundFontPath = "FluidR3 GM2-2.SF2"
let soundFont = SoundFont(soundFontPath)
let instrumentName = "Yamaha Grand Piano"
let instrument = soundFont.Instruments
                 |> Seq.find (fun i -> i.Name = instrumentName)
let waveProvider = SoundFontWaveProvider(soundFont, instrument)

let convertMidiToWave midiPath wavePath =
    let midiFile = NAudio.Midi.MidiFile(midiPath)
    let track = 0
    let events = midiFile.Events.[track]
    let tempo = 0.5<s/beat> // 500,000 microseconds
    let ticksPerBeat = float midiFile.DeltaTicksPerQuarterNote * 1.0<tick/beat>

    use recorder = new WaveRecorder(waveProvider, wavePath)
    let buffer = ref <| Array.zeroCreate 1024

    let recode (event1 : NoteOnEvent) ticks =
        let noteLength = ticks * tempo / ticksPerBeat
        let sampleCount = int (noteLength * (float waveProvider.WaveFormat.SampleRate * 1.0<Hz>))
        let byteCount = sampleCount * 2
        Array.Resize(buffer, byteCount)

        waveProvider.Key <- event1.NoteNumber
        recorder.Read(!buffer, 0, byteCount) |> ignore

    let noteOnEvents =
        events
        |> Seq.filter (fun event -> event.CommandCode = MidiCommandCode.NoteOn)
        |> Seq.map (fun event -> event :?> NoteOnEvent)

    noteOnEvents
    |> Seq.pairwise
    |> Seq.iter (fun (event1, event2) ->
        let ticks = float (event2.AbsoluteTime - event1.AbsoluteTime) * 1.0<tick>
        recode event1 ticks)
    let lastMidiEvent = Seq.last events
    let lastNoteOnEvent = Seq.last noteOnEvents
    let ticks = float (lastMidiEvent.AbsoluteTime - lastNoteOnEvent.AbsoluteTime) * 1.0<tick>
    recode lastNoteOnEvent ticks
