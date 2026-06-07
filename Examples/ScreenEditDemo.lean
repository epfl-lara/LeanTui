import LeanTui

open LeanTui

structure DemoModel where
  size : Size
  typed : List (Pos × Char) := []
  row : Nat := 2
  col : Nat := 4
  message : String := "Type printable keys, move with cursor keys, Home, and End. Press Esc to quit."

def fallbackSize : Size :=
  { rows := 12, cols := 72 }

def clampTo (limit value : Nat) : Nat :=
  if limit == 0 then 0 else if value >= limit then limit - 1 else value

def initModel (size : Size) : DemoModel :=
  { size := size, row := clampTo size.rows 2, col := clampTo size.cols 4 }

def upsertTyped (pos : Pos) (ch : Char) : List (Pos × Char) -> List (Pos × Char)
  | [] => [(pos, ch)]
  | (oldPos, oldCh) :: rest =>
      if oldPos == pos then
        (oldPos, ch) :: rest
      else
        (oldPos, oldCh) :: upsertTyped pos ch rest

def advanceCursor (model : DemoModel) : DemoModel :=
  if model.size.rows == 0 || model.size.cols == 0 then
    model
  else if model.col + 1 < model.size.cols then
    { model with col := model.col + 1 }
  else if model.row + 1 < model.size.rows then
    { model with row := model.row + 1, col := 0 }
  else
    model

def insertTyped (ch : Char) (model : DemoModel) : DemoModel :=
  let pos := { row := model.row, col := model.col }
  let model := { model with typed := upsertTyped pos ch model.typed, message := "typed " ++ String.singleton ch }
  advanceCursor model

def moveDemo (key : Key) (model : DemoModel) : Step DemoModel :=
    match key with
    | Key.escape => Step.quit { model with message := "Goodbye." }
    | Key.left =>
        Step.continue { model with col := model.col - 1, message := "left" }
    | Key.right =>
        Step.continue { model with col := clampTo model.size.cols (model.col + 1), message := "right" }
    | Key.up =>
        Step.continue { model with row := model.row - 1, message := "up" }
    | Key.down =>
        Step.continue { model with row := clampTo model.size.rows (model.row + 1), message := "down" }
    | Key.home => Step.continue { model with col := 0, message := "home" }
    | Key.endKey => Step.continue {
        model with col := clampTo model.size.cols model.size.cols, message := "end"
      }
    | other =>
        match other.toInputChar? with
        | some ch => Step.continue (insertTyped ch model)
        | none => Step.continue { model with message := "Unhandled key: " ++ reprStr other }

def renderTyped (screen : Screen) : List (Pos × Char) -> Screen
  | [] => screen
  | (pos, ch) :: rest => renderTyped (screen.putChar pos.row pos.col ch) rest

def renderDemo (model : DemoModel) : Screen :=
  let titleStyle : Style := { fg := some Color.cyan, bold := true }
  let cursorStyle : Style := { fg := some Color.black, bg := some Color.white, bold := true }
  let screen := Screen.blank model.size
  let screen := screen.putString 0 0 "LeanTui demo: ESC to quit" titleStyle
  let screen := screen.putString 1 0 model.message
  let screen := screen.putString 3 0 "The highlighted cell is redrawn incrementally."
  let screen := screen.putString 5 2 "Cursor keys move the highlighted terminal-style cell."
  let screen := screen.putString 7 2 "Typing symbols replaces current character"
  let screen := renderTyped screen model.typed
  let baseCell := screen.get { row := model.row, col := model.col }
  screen.putChar model.row model.col baseCell.ch cursorStyle

def demoApp (size : Size) : App DemoModel :=
  { init := initModel size
    view := renderDemo
    update := moveDemo }

def main : IO Unit := do
  let size := (← Terminal.size?).getD fallbackSize
  App.run (demoApp size)
