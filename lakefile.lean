import Lake

open System Lake DSL

package LeanTui where version := v!"0.1.0"

lean_lib LeanEffects

@[default_target] lean_lib LeanTui

@[default_target]
lean_exe «screen-edit-demo» where root := `Demo.ScreenEditMain

lean_lib Demo.ScreenEdit

@[default_target]
lean_lib Demo.Editor

@[default_target]
lean_exe «editor-demo» where root := `Demo.EditorMain

@[default_target]
lean_lib Demo.Chess

@[default_target]
lean_exe «chess-demo» where root := `Demo.ChessMain
