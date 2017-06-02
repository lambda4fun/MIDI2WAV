namespace ViewModels

open System
open System.Windows
open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators
open NAudio.Midi
open CommandUtils
open Views

type EventRow = {
    Index : int
    AbsoluteTime : int64
    DeltaTime : int
    Channel : int
    CommandCode : MidiCommandCode
}

type MainViewModel(window : MainWindow) =
    let loadMidiFile path =
        let midiFile = NAudio.Midi.MidiFile(path)
        let track = 0
        let events = midiFile.Events.[track]
        let rows = events |> Seq.mapi (fun i e ->
            { Index=i; AbsoluteTime=e.AbsoluteTime; DeltaTime=e.DeltaTime; Channel=e.Channel; CommandCode=e.CommandCode })
        window.EventsGrid.DataContext <- rows

    member __.OpenCommand = functionCommand(fun () ->
        let dialog = OpenFileDialog(Filter = "MIDI Files|*.mid;*.midi|All Files|*.*")
        let result = dialog.ShowDialog()
        if result ?= true then
            loadMidiFile dialog.FileName)

    member __.ExitCommand = functionCommand(fun () ->
        let result = MessageBox.Show("Do you really want to exit?", "", MessageBoxButton.YesNo)
        if result = MessageBoxResult.Yes then
            Application.Current.Shutdown())
    
    member __.AboutCommand = functionCommand(fun () ->
        MessageBox.Show("MIDI File Editor", "About"))
        