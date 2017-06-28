open System
open Convert

let convertTest() =
    let convert = convertMidiToWave "FluidR3_GM.sf2"
    do convert "../../../../resources/3midi.mid" "3midi-new.wav"
    do convert "../../../../resources/12midi.mid" "12midi-new.wav"

[<EntryPoint>]
let main argv =
    convertTest()

    printfn "Press any key to exit..."
    Console.ReadKey(intercept = true) |> ignore
    0
