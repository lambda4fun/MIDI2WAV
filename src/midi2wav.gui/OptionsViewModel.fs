namespace ViewModels

open System
open System.Configuration
open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators
open CommandUtils
open Views

type OptionsViewModel(view : OptionsDialog) as self =
    inherit ViewModelBase()

    let mutable soundFontPath = ""
    let config = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)

    do
        view.Loaded.AddHandler(fun _ _ ->
            self.SoundFontPath <- config.AppSettings.Settings.["SoundFontPath"].Value)

    member __.SoundFontPath with get() = soundFontPath
                            and  set v = soundFontPath <- v
                                         self.NotifyPropertyChanged("SoundFontPath")

    member __.BrowseSoundFontFileCommand = functionCommand(fun () ->
        let dialog = OpenFileDialog()
        let result = dialog.ShowDialog(view)
        if result ?= true then
            self.SoundFontPath <- dialog.FileName)
    
    member __.OkCommand = functionCommand(fun () ->
        config.AppSettings.Settings.["SoundFontPath"].Value <- soundFontPath
        config.Save()
        view.DialogResult <- Nullable(true))

    member __.CancelCommand = functionCommand(fun () ->
        view.DialogResult <- Nullable(false)
        view.Close())
