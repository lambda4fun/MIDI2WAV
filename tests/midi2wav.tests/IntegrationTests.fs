open System
open Convert

[<EntryPoint>]
let main argv =
    let convert = convertMidiToWave "FluidR3_GM.sf2" "Yamaha Grand Piano"
    convert "../../../../resources/3midi.mid" "3midi-new.wav"
    convert "../../../../resources/12midi.mid" "12midi-new.wav"

    printfn "Press any key to exit..."
    Console.ReadKey(intercept = true) |> ignore
    0
