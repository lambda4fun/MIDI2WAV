namespace ViewModels

open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators
open CommandUtils
open Views
open System.ComponentModel

type ViewModelBase() =
    let propertyChangedEvent = Event<_, _>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChangedEvent.Publish
    
    member this.NotifyPropertyChanged(name : string) =
        propertyChangedEvent.Trigger(this, PropertyChangedEventArgs(name))

type MainViewModel(window : MainWindow) as self =
    inherit ViewModelBase()

    let mutable inputFilePath = ""

    member __.InputFilePath with get() = inputFilePath
                            and  set v = inputFilePath <- v
                                         self.NotifyPropertyChanged("InputFilePath")

    member __.BrowseInputFileCommand = functionCommand(fun () ->
        let dialog = OpenFileDialog()
        let result = dialog.ShowDialog(window)
        if result ?= true then
            self.InputFilePath <- dialog.FileName)
