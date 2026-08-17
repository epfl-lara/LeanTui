import LeanTui

/-!
# Screen edit demo

A minimal interactive LeanTui example that displays the first page of a text file contents in a terminal screen and allows emphemeral edits on it, without scrolling.
The purpose is to showcase how to write LeanTui applications.
-/

open LeanTui

--! The state of our application
structure DemoModel where
  size : Size
  screen : Screen
  row : Nat := 2
  col : Nat := 4
  fileLines : Array String := #[]
  scrollRow : Nat := 0
  terminalOutputChars : Nat := 0

def fallbackSize : Size :=
  { rows := 12, cols := 72 }

def clampTo (limit value : Nat) : Nat :=
  if limit == 0 then 0 else if value >= limit then limit - 1 else value

def blankRowFuel (screen : Screen) (row col fuel : Nat) : Screen :=
  match fuel with
  | 0 => screen
  | fuel + 1 =>
  if col >= screen.size.cols then
    screen
  else
    blankRowFuel (screen.putChar row col ' ') row (col + 1) fuel

def blankRow (screen : Screen) (row col : Nat) : Screen :=
  blankRowFuel screen row col (screen.size.cols - col + 1)

def setMessage (model : DemoModel) (message : String) : DemoModel :=
  let screen := blankRow model.screen 1 0
  let messageStyle: Style := { fg := some Color.black, bg := some Color.yellow, bold := true }
  let screen := screen.putString 1 0 message messageStyle
  { model with screen := screen }

def baseScreen (size : Size) (fileLines : Array String) : Screen :=
  let titleStyle : Style := { fg := some Color.cyan, bold := true }
  let screen := Screen.blank size
  let screen := screen.putString 0 0 "Screen Edit Demo for LeanTui. Press ESC to quit" titleStyle
  let visibleRows := if size.rows <= 2 then 0 else size.rows - 2
  let visibleLines := fileLines.extract 0 visibleRows
  screen.putStrings 2 0 visibleLines

def initModel (size : Size) (fileLines : Array String) : DemoModel :=
  { size := size
    screen := baseScreen size fileLines
    row := clampTo size.rows 2
    col := clampTo size.cols 4
    fileLines := fileLines }

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
  let model := setMessage model ("typed " ++ String.singleton ch)
  let screen := model.screen.putChar model.row model.col ch
  let model := { model with screen := screen }
  advanceCursor model

def moveDemo (key : Key) (model : DemoModel) : Step DemoModel :=
    match key with
    | Key.ctrl 'l' => Step.redrawFull (setMessage model "full render")
    | Key.escape => Step.quit (setMessage model "Goodbye.")
    | Key.left =>
        Step.continue (setMessage { model with col := model.col - 1 } "left")
    | Key.right =>
        Step.continue (setMessage { model with col := clampTo model.size.cols (model.col + 1) } "right")
    | Key.up =>
        Step.continue (setMessage { model with row := model.row - 1 } "up")
    | Key.down =>
        Step.continue (setMessage { model with row := clampTo model.size.rows (model.row + 1) } "down")
    | Key.home => Step.continue (setMessage { model with col := 0 } "home")
    | Key.endKey =>
        Step.continue (setMessage { model with col := clampTo model.size.cols model.size.cols } "end")
    | other =>
        match other.toInputChar? with
        | some ch => Step.continue (insertTyped ch model)
        | none => Step.continue (setMessage model ("Unhandled key: " ++ reprStr other))

def renderDemo (model : DemoModel) : Screen :=
  let cursorStyle : Style := { fg := some Color.black, bg := some Color.orange, bold := true }
  let screen := model.screen
  let screen := screen.putString 0 0
    ("Screen Edit Demo. ESC quits. out " ++ toString model.terminalOutputChars)
    { fg := some Color.cyan, bold := true }
  let baseCell := screen.get { row := model.row, col := model.col }
  screen.putChar model.row model.col baseCell.ch cursorStyle

def demoApp (size : Size) (fileLines : Array String) : App DemoModel :=
  { init := initModel size fileLines
    view := renderDemo
    update := moveDemo
    terminalOutput := fun count model => { model with terminalOutputChars := count } }

def parseArgs (args : List String) : Option String :=
  match args with
  | [] => none
  | path :: _ => some path

def readFileLines (path : String) : IO (Array String) := do
  let content ← IO.FS.readFile path
  pure ((content.split '\n').map String.Slice.toString).toArray

--! Application entry is a monadic program that invokes `App.run`
def main (args : List String) : IO Unit := do
  let size := (← Terminal.size?).getD fallbackSize
  match parseArgs args with
  | some path =>
      let lines ← readFileLines path
      App.run (demoApp size lines)
  | none => IO.Process.exit 1
