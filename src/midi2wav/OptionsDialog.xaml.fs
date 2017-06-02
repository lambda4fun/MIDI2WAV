namespace Views

open FsXaml

type OptionsDialogBase = XAML<"OptionsDialog.xaml">

type OptionsDialog() =
    inherit OptionsDialogBase()
    