open System
open Convert

let convertTest() =
    let convert = convertMidiToWave "FluidR3_GM.sf2"
    do convert "../../../../resources/3midi.mid" "3midi-new.wav" (fun _ -> async {()}) |> Async.RunSynchronously
    do convert "../../../../resources/12midi.mid" "12midi-new.wav" (fun _ -> async {()}) |> Async.RunSynchronously

let convertTimecentsToSecondsTest() =
    // 2^(-t/1200)
    let toSeconds0 (t : float<timecent>) = 2.0 ** (-t / 1200.0<timecent>) * 1.0<s>
    // Precomputed: 2^(-1/1200) ~= 0.9994225441413808
    let toSeconds1 (t : float<timecent>) = 0.9994225441413808 ** float t * 1.0<s>
    // Precomputed: 2^(1/1200) ~= 1.0005777895065548
    let toSeconds2 (t : float<timecent>) = 1.0005777895065548 ** float -t * 1.0<s>
    // 1 / 2^(t/1200)
    let toSeconds3 (t : float<timecent>) =  1.0 / (2.0 ** (t / 1200.0<timecent>)) * 1.0<s>
    // 2^7973 overflows float.
    let toSeconds4 (t : float<timecent>) = (2.0 ** float t) ** (-1.0 / 1200.0) * 1.0<s>
    let toSeconds5 (t : float<timecent>) = 1.0 / ((2.0 ** float t) ** (1.0 / 1200.0)) * 1.0<s>

    let t = 7973.0<timecent>
    let answer = 0.01<s>

    [toSeconds0; toSeconds1; toSeconds2; toSeconds3; toSeconds4; toSeconds5]
    |> List.mapi (fun i f ->(i, (f t, answer - f t)))
    |> List.sortBy (snd >> snd >> abs)
    |> List.iter (fun (i, (x, delta)) ->
        printfn "toSeconds%d : %.20f, delta=%.20f" i x delta )

[<EntryPoint>]
let main argv =
    //convertTest()
    convertTimecentsToSecondsTest()

    printfn "Press any key to exit..."
    Console.ReadKey(intercept = true) |> ignore
    0
