import LeanTui
set_option doc.verso true
set_option doc.verso.module true
/-!
# Editor: A demo
A minimal interactive LeanTui demo. Displays the first page of a text file contents in a terminal screen and allows emphemeral edits to it, without scrolling or saving.
-/
open LeanTui

namespace Editor
  /-- The state of our screen edit application -/
  structure EditorState where
    /-- Current screen content being shown and modified, including cursor display. -/
    screen : Screen
    screenSize : Size
    /-- Cursor position -/
    pos: Pos
    /-- {name}`Cell` underneath the cursor. -/
    cellAtCursor: Cell

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
  def initModel (size : Size) (fileLines : Array String) : EditorState :=
    let initScreen: Screen := baseScreen size fileLines
    let cursorPos := { row := clampTo size.rows 2, col := clampTo size.cols 0 }
    let cell := initScreen.get cursorPos
    { screenSize := size
      screen := initScreen.set cursorPos {cell with style := cursorStyle}
      pos := cursorPos
      cellAtCursor := cell
    }

  /-- Write a typed character at the cursor and advance to the next cell. -/
  def insertTyped (ch : Char) (model : EditorState) : EditorState :=
    let model := setMessage model ("typed " ++ String.singleton ch)
    let typedCell := {ch := ch, style := Style.plain}
    let screen := model.screen.set model.pos typedCell
    let model := { model with screen := screen, cellAtCursor := typedCell }
    advanceCursorOnTyping model

  /-- React to a key press: translate a key event into the next state. -/
  def updateRule (key : Key) (model : EditorState) : Step EditorState :=
      match key with
      | Key.ctrl 'l' => Step.redrawFull (setMessage model "full render")
      | Key.escape => Step.quit (setMessage model "Goodbye.")
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

  /-- Creates an App for out Editor application. Behavior given by {name}`viewRule` and {name}`updateRule`. -/
  def editorApp (size : Size) (fileLines : Array String) : App EditorState :=
    { init := initModel size fileLines
      view := viewRule
      update := updateRule }

  /-- App entry: load the requested file and use it as inital screen content, allowing ephemeral edits. -/
  def main (cmdLineArgs : List String) : IO Unit := do
    let sizeOpt <- Terminal.size?
    let size := sizeOpt.getD { rows := 12, cols := 72 }
    match cmdLineArgs with
    | [path] => App.run (editorApp size (←readFileLines path))
    | _ => IO.Process.exit 1

end Editor
