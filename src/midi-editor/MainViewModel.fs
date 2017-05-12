namespace ViewModels

open System.Windows
open System
open System.Windows.Input
open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators

type OpenCommand() =
    interface ICommand with
        [<CLIEvent>]
        member __.CanExecuteChanged = Event<EventHandler, EventArgs>().Publish
        member __.CanExecute _ = true
        member __.Execute _ =
            let dialog = OpenFileDialog(Filter = "MIDI Files|*.mid;*.midi|All Files|*.*")
            let result = dialog.ShowDialog()
            if result ?= true then
                MessageBox.Show(dialog.FileName) |> ignore
            ()

type FunctionCommand(execute : unit -> unit) =
    interface ICommand with
        [<CLIEvent>]
        member __.CanExecuteChanged = Event<EventHandler, EventArgs>().Publish
        member __.CanExecute _ = true
        member __.Execute _ = execute()

type MainViewModel(window : Window) =
    do
        window.MouseLeftButtonDown.AddHandler(fun _ _ ->
            MessageBox.Show("Greeting from midi-editor!") |> ignore)

    member __.OpenCommand = OpenCommand()

    member __.ExitCommand = FunctionCommand(fun () ->
        let result = MessageBox.Show("Do you really want to exit?", "", MessageBoxButton.YesNo)
        if result = MessageBoxResult.Yes then
            Application.Current.Shutdown())

    member __.AboutCommand = FunctionCommand(fun () ->
        MessageBox.Show("MIDI File Editor", "About") |> ignore)
