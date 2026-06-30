import Lake

open System Lake DSL

package LeanTui where version := v!"0.1.0"

@[default_target] lean_lib LeanTui

lean_lib Demo.Chess

lean_exe «screen-edit-demo» where root := `Demo.ScreenEdit.ScreenEditDemo

lean_exe «chess-demo» where root := `Demo.Chess.ChessDemo
