namespace ViewModels

open System
open System.Windows
open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators
open CommandUtils

type MainViewModel(window : Window) =
    do
        window.MouseLeftButtonDown.AddHandler(fun _ _ ->
            MessageBox.Show("Greeting from midi-editor!") |> ignore)

    member __.OpenCommand = functionCommand(fun () ->
        let dialog = OpenFileDialog(Filter = "MIDI Files|*.mid;*.midi|All Files|*.*")
        let result = dialog.ShowDialog()
        if result ?= true then
            MessageBox.Show(dialog.FileName) |> ignore)

    member __.ExitCommand = functionCommand(fun () ->
        let result = MessageBox.Show("Do you really want to exit?", "", MessageBoxButton.YesNo)
        if result = MessageBoxResult.Yes then
            Application.Current.Shutdown())
    
    member __.AboutCommand = functionCommand(fun () ->
        MessageBox.Show("MIDI File Editor", "About"))
        