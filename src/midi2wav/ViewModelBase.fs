namespace ViewModels

open System.ComponentModel

type ViewModelBase() =
    let propertyChangedEvent = Event<_, _>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChangedEvent.Publish
    
    member this.NotifyPropertyChanged(name : string) =
        propertyChangedEvent.Trigger(this, PropertyChangedEventArgs(name))
