open System
open FsXaml
open Views
open ViewModels

type App = XAML<"App.xaml">

[<STAThread; EntryPoint>]
let main _ =
    Windows.Forms.Application.EnableVisualStyles()

    let window = MainWindow()
    window.DataContext <- MainViewModel(window)

    App().Run(window)
