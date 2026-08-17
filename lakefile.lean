import Lake

open System Lake DSL

package LeanTui where version := v!"0.1.0"

@[default_target] lean_lib LeanTui

@[default_target]
lean_exe «screen-edit-demo» where root := `Demo.ScreenEdit.ScreenEditDemo

lean_lib Demo.ScreenEdit

@[default_target]
lean_lib Demo.Chess

@[default_target]
lean_exe «chess-demo» where root := `Demo.Chess.Main
