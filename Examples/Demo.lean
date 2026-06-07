import LeanTui

open LeanTui

structure DemoModel where
  row : Nat := 2
  col : Nat := 4
  message : String := "Use cursor keys, Home, and End. Press q to quit."

def demoSize : Size :=
  { rows := 12, cols := 72 }

def clampTo (limit value : Nat) : Nat :=
  if value >= limit then limit - 1 else value

def moveDemo (key : Key) (model : DemoModel) : Step DemoModel :=
  if key == Key.home then
    Step.continue { model with col := 0, message := "home" }
  else if key == Key.endKey then
    Step.continue { model with col := demoSize.cols - 1, message := "end" }
  else
    match key with
    | Key.char 'q' => Step.quit { model with message := "Goodbye." }
    | Key.left =>
        Step.continue { model with col := model.col - 1, message := "left" }
    | Key.right =>
        Step.continue { model with col := clampTo demoSize.cols (model.col + 1), message := "right" }
    | Key.up =>
        Step.continue { model with row := model.row - 1, message := "up" }
    | Key.down =>
        Step.continue { model with row := clampTo demoSize.rows (model.row + 1), message := "down" }
    | other =>
        Step.continue { model with message := "Unhandled key: " ++ reprStr other }

def renderDemo (model : DemoModel) : Screen :=
  let titleStyle : Style := { fg := some Color.cyan, bold := true }
  let cursorStyle : Style := { fg := some Color.black, bg := some Color.white, bold := true }
  let screen := Screen.blank demoSize
  let screen := screen.putString 0 0 "LeanTui demo" titleStyle
  let screen := screen.putString 1 0 model.message
  let screen := screen.putString 3 0 "The highlighted cell is redrawn incrementally."
  let screen := screen.putString 5 2 "Cursor keys move the highlighted terminal-style cell."
  let baseCell := screen.get { row := model.row, col := model.col }
  screen.putChar model.row model.col baseCell.ch cursorStyle

def demoApp : App DemoModel :=
  { init := {}
    view := renderDemo
    update := moveDemo }

def main : IO Unit :=
  App.run demoApp
