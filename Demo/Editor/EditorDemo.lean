import LeanTui
set_option doc.verso true
set_option doc.verso.module true
/-!
# Editor: A demo
A minimal interactive LeanTui demo. Displays the first page of a text file contents in a terminal screen and allows emphemeral edits to it, without scrolling or saving.
-/
open LeanTui

namespace Editor
  /-- The state of our editor application -/
  structure EditorState where
    /-- Path of the file being edited, used when saving on exit. -/
    path : String
    /-- The lines of the file being edited. -/
    lines : Array String
    /-- Current screen content being shown and modified, including cursor display. -/
    screen : Screen
    screenSize : Size
    /-- Cursor position -/
    pos: Pos
    /-- {name}`Cell` underneath the cursor. -/
    cellAtCursor: Cell
    /-- Whether there are unsaved edits to the file. -/
    dirty : Bool := false

  /-- Load a text file and split it into an array of lines. -/
  def readFileLines (path : String) : IO (Array String) := do
    let content ← IO.FS.readFile path
    pure ((content.split '\n').map String.Slice.toString).toArray

  /-- Clamp a terminal coordinate to the valid range below {lit}`limit`. -/
  def clampTo (limit value : Nat) : Nat :=
    if value >= limit then limit - 1 else value

  /-- Clears the remainder of a screen row starting at the given column. -/
  def blankRow (screen : Screen) (row col : Nat) : Screen :=
    (screen.size.cols - col + 1).fold
      (fun (i:Nat) _ (screen: Screen) => screen.putChar row (col+i) ' ')
      screen

  def messageStyle: Style := { fg := some Color.black, bg := some Color.yellow, bold := true }

  /-- Replace the status row with a highlighted message for the user. -/
  def setMessage (model : EditorState) (message : String) : EditorState :=
    let screen := blankRow model.screen 1 0
    let screen := screen.putString 1 0 message messageStyle
    { model with screen := screen }

  def cursorStyle : Style := { fg := some Color.black, bg := some Color.orange, bold := true }

  /-- Set cursor to a new position (assumed to be in screen) and update the screen. -/
  def updateCursor (newPos: Pos) (state: EditorState): EditorState :=
    let prevCell := state.cellAtCursor
    let screen := state.screen.set state.pos prevCell
    let newCell := screen.get newPos
    let screen := screen.set newPos { newCell with style := cursorStyle}
    { state with screen := screen, pos := newPos, cellAtCursor := newCell }

  /-- Advance the cursor one cell in row-major order without leaving the screen. -/
  def advanceCursorOnTyping (model : EditorState) : EditorState :=
    let pos := model.pos
    let newPos :=
      if model.screenSize.rows == 0 || model.screenSize.cols == 0 then pos
      else if pos.col + 1 < model.screenSize.cols then {pos with col := pos.col + 1}
      else if pos.row + 1 < model.screenSize.rows then {pos with row := pos.row + 1}
      else pos
    updateCursor newPos model

  /-- The initial screen with a title and the file lines that fit in the terminal. -/
  def baseScreen (size : Size) (fileLines : Array String) : Screen :=
    let titleStyle : Style := { fg := some Color.cyan, bold := true }
    let screen := Screen.blank size
    let screen := screen.putString 0 0 "Editor Demo for LeanTui. ESC quits." titleStyle
    let visibleRows := if size.rows <= 2 then 0 else size.rows - 2
    let visibleLines := fileLines.extract 0 visibleRows
    screen.putStrings 2 0 visibleLines

    /-- The initial application model with screen displaying the cursor. -/
  def initModel (path : String) (size : Size) (fileLines : Array String) : EditorState :=
    let initScreen: Screen := baseScreen size fileLines
    let cursorPos := { row := clampTo size.rows 2, col := clampTo size.cols 0 }
    let cell := initScreen.get cursorPos
    { path := path
      lines := fileLines
      screenSize := size
      screen := initScreen.set cursorPos {cell with style := cursorStyle}
      pos := cursorPos
      cellAtCursor := cell
    }

  /-- Inserts a character into a string at a column, padding with spaces if needed. -/
  def insertAt (line : String) (col : Nat) (ch : Char) : String :=
    let chars := line.toList
    if col >= chars.length then
      line ++ String.ofList (List.replicate (col - chars.length) ' ' ++ [ch])
    else
      let (before, after) := chars.splitAt col
      String.ofList (before ++ ch :: after)

  /-- Renders a single file line into a blanked screen row. -/
  def renderLine (screen : Screen) (row : Nat) (line : String) : Screen :=
    let screen := blankRow screen row 0
    screen.putString row 0 line Style.plain

  /-- Write a typed character into the file buffer and screen at the cursor, then advance. -/
  def insertTyped (ch : Char) (model : EditorState) : EditorState :=
    let model := setMessage model ("typed " ++ String.singleton ch)
    let lineIdx := model.pos.row - 2
    if model.pos.row < 2 || lineIdx >= model.lines.size then
      setMessage model "Cannot edit outside the file lines."
    else
      let line := model.lines.getD lineIdx ""
      let newLine := insertAt line model.pos.col ch
      let lines := model.lines.set! lineIdx newLine
      let screen := renderLine model.screen model.pos.row newLine
      let model := { model with lines := lines, screen := screen, dirty := true }
      advanceCursorOnTyping { model with cellAtCursor := screen.get model.pos }

  /-- React to a key press: translate a key event into the next state. -/
  def updateRule (key : Key) (model : EditorState) : Step EditorState :=
      match key with
      | Key.ctrl 'l' => Step.redrawFull (setMessage model "full render")
      | Key.escape => Step.quit (setMessage model "Saving and exiting...")
      | Key.left =>
          setMessage model "left" |>
          updateCursor {model.pos with col := model.pos.col - 1} |>
          Step.continue
      | Key.right =>
          setMessage model "right" |>
          updateCursor {model.pos with col := clampTo model.screenSize.cols (model.pos.col + 1)} |>
          Step.continue
      | Key.up =>
          setMessage model "up" |>
          updateCursor {model.pos with row := model.pos.row - 1} |>
          Step.continue
      | Key.down =>
          setMessage model "down" |>
          updateCursor {model.pos with row := clampTo model.screenSize.rows (model.pos.row + 1)} |>
          Step.continue
      | other =>
          match other.toInputChar? with
          | some ch => insertTyped ch model |> Step.continue
          | none => setMessage model ("Unhandled key: " ++ reprStr other) |> Step.continue

  /-- Render {name}`state` on the screen. Here we maintain a {name}`Screen` inside {name}`state`, so we return it. -/
  def viewRule (state : EditorState) : Screen := state.screen

  /-- Writes the edited lines back to the file when the app exits. -/
  def saveFile (state : EditorState) : IO Unit := do
    let content := String.intercalate "\n" state.lines.toList
    IO.FS.writeFile state.path content

  /-- Creates an App for our Editor application. Behavior given by {name}`viewRule`, {name}`updateRule`, and {name}`saveFile` on exit. -/
  def editorApp (size : Size) (path : String) (fileLines : Array String) : App EditorState :=
    { init := initModel path size fileLines
      view := viewRule
      update := updateRule
      onExit := saveFile }

  /-- App entry: load the requested file and use it as initial screen content, saving edits on exit. -/
  def main (cmdLineArgs : List String) : IO Unit := do
    let sizeOpt <- Terminal.size?
    let size := sizeOpt.getD { rows := 12, cols := 72 }
    match cmdLineArgs with
    | [path] => App.run (editorApp size path (←readFileLines path))
    | _ => IO.Process.exit 1

end Editor
