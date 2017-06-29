namespace Views

open FsXaml

type ProgressDialogBase = XAML<"ProgressDialog.xaml">

type ProgressDialog() =
    inherit ProgressDialogBase()
    