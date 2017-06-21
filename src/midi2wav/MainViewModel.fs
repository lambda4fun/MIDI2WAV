namespace ViewModels

open System.Configuration
open System.IO
open System.Windows.Forms
open Microsoft.Win32
open Microsoft.FSharp.Linq.NullableOperators
open CommandUtils
open Convert
open Views

type MainViewModel(window : MainWindow) as self =
    inherit ViewModelBase()
    
    let mutable inputFilePath = ""
    let mutable outputDirectory = ""
    
    let (|FileDrop|_|) (e : System.Windows.DragEventArgs) =
        if e.Data.GetDataPresent(DataFormats.FileDrop)
        then e.Data.GetData(DataFormats.FileDrop) :?> string array |> Array.tryHead
        else None
    
    let (|FileExists|_|) (path : string) =
        if File.Exists(path)
        then Some FileExists
        else None
    
    let (|DirectoryExists|_|) (path : string) =
        if Directory.Exists(path)
        then Some DirectoryExists
        else None

    do
        window.DragEnter.AddHandler(fun _ e ->
            match e with
            | FileDrop _ -> ()
            | _ -> e.Effects <- System.Windows.DragDropEffects.None)

        window.Drop.AddHandler(fun _ e ->
            match e with
            | FileDrop path ->
                match path with
                | FileExists ->
                    self.InputFilePath <- path
                | DirectoryExists ->
                    self.OutputDirectory <- path
                | _ -> ()
            | _ -> ())

    member __.InputFilePath with get() = inputFilePath
                            and  set v = inputFilePath <- v
                                         self.NotifyPropertyChanged("InputFilePath")

    member __.OutputDirectory with get() = outputDirectory
                              and  set v = outputDirectory <- v
                                           self.NotifyPropertyChanged("OutputDirectory")
    
    member __.BrowseInputFileCommand = functionCommand(fun () ->
        let dialog = OpenFileDialog()
        let result = dialog.ShowDialog(window)
        if result ?= true then
            self.InputFilePath <- dialog.FileName)
    
    member __.BrowseOutputDirectoryCommand = functionCommand(fun () ->
        use dialog = new FolderBrowserDialog()
        let result = dialog.ShowDialog()
        if result = DialogResult.OK then
            self.OutputDirectory <- dialog.SelectedPath)
    
    member __.OptionsCommand = functionCommand(fun () ->
        let dialog = OptionsDialog()
        dialog.DataContext <- OptionsViewModel(dialog)
        dialog.Owner <- window
        dialog.ShowDialog())

    member __.ConvertCommand = functionCommand(fun () ->
        try
            if not (File.Exists(inputFilePath))
            then failwith "Input file is not found."
            if not (Directory.Exists(outputDirectory))
            then failwith "Output directory is not found."
            
            let config = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)
            let soundFontPath = config.AppSettings.Settings.["SoundFontPath"].Value

            let outputFileName = Path.ChangeExtension(Path.GetFileName(inputFilePath), "wav")
            let outputFilePath = Path.Combine(outputDirectory, outputFileName)
            convertMidiToWave soundFontPath "Yamaha Grand Piano" inputFilePath outputFilePath
        with err ->
            MessageBox.Show(err.Message, "Conversion failed", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore)
