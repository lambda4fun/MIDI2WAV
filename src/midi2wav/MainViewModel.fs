namespace ViewModels

open System.Windows

type MainViewModel(window : Window) =
    do
        window.MouseLeftButtonDown.AddHandler(fun _ _ ->
            MessageBox.Show("MouseLeftButtonDown") |> ignore)
