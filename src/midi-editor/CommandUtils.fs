module CommandUtils

open System
open System.Windows.Input

let functionCommand (f : unit -> 'a) = 
    { new ICommand with
        [<CLIEvent>]
        member __.CanExecuteChanged = Event<EventHandler, EventArgs>().Publish
        member __.CanExecute _ = true
        member __.Execute _ = f() |> ignore }
