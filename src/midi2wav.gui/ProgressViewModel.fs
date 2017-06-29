namespace ViewModels

open System
open System.Threading
open System.Windows.Forms
open Units
open CommandUtils
open Views

type ProgressViewModel(view : ProgressDialog) as this =
    inherit ViewModelBase()

    // TODO: When to dispose this object?
    let cancellationToken = new CancellationTokenSource()
    
    let mutable message = ""
    let mutable progress = 0.0<percent>
    let mutable buttonText = "Cancel"

    do view.Closing.Add(fun e ->
        view.DialogResult <- Nullable(this.Completed)
        if not this.Completed then
            let result = MessageBox.Show("Do you want to cancel the job?", "Cancel",
                                         MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            if result = DialogResult.Yes then
                cancellationToken.Cancel()
            else
                e.Cancel <- true)
   
    member __.Message with get() = message
                      and  set v = message <- v
                                   this.NotifyPropertyChanged("Message")

    member __.Progress with get() = progress
                       and  set v = progress <- v
                                    this.NotifyPropertyChanged("Progress")
                                    if this.Completed then
                                        this.ButtonText <- "Close"

    member __.Completed with get() = progress = 100.0<percent>

    member __.ButtonText with get() = buttonText
                         and  set v = buttonText <- v
                                      this.NotifyPropertyChanged("ButtonText")

    member __.StartTask(task : Async<unit>) =
        Async.Start(task, cancellationToken.Token)

    member __.CloseOrCancelCommand = functionCommand(fun () ->
        view.Close())
    