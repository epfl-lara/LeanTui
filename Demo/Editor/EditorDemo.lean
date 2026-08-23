import LeanTui
import LeanEffects.FileSystem
set_option doc.verso true
set_option doc.verso.module true
/-!
# Editor: A demo
A minimal interactive LeanTui demo. Displays a scrollable view of a text file in a terminal screen, allows edits to it, and saves the changes back to the file on exit.
-/
open LeanTui
open LeanEffects

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
    /-- The index of the first file line currently visible at the top of the viewport. -/
    scrollTop : Nat := 0

  /-- Load a text file and split it into an array of lines. -/
  def readFileLines (path : String) : Command FileSystem.Effect (Array String) := do
    let content ← FileSystem.readFile path
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

  /-- Number of content rows available below the title and status rows. -/
  def contentRows (size : Size) : Nat :=
    if size.rows <= 2 then 0 else size.rows - 2

  /-- The file line index shown at a screen row, or none when the row is out of the viewport. -/
  def lineAt (model : EditorState) (row : Nat) : Option Nat :=
    if row < 2 || row >= model.screenSize.rows then
      none
    else
      let idx := model.scrollTop + (row - 2)
      if idx < model.lines.size then some idx else none

  /-- Renders one file line into a blanked screen row. -/
  def renderLine (screen : Screen) (row : Nat) (line : String) : Screen :=
    let screen := blankRow screen row 0
    screen.putString row 0 line Style.plain

  /-- Re-renders every visible content row from the current viewport, skipping the title and status rows. -/
  def renderRowsLoop (model : EditorState) (row : Nat) (screen : Screen) : Screen :=
    if row >= model.screenSize.rows then
      screen
    else if row < 2 then
      renderRowsLoop model (row + 1) screen
    else
      let screen :=
        match lineAt model row with
        | some lineIdx => renderLine screen row (model.lines.getD lineIdx "")
        | none => blankRow screen row 0
      renderRowsLoop model (row + 1) screen
  termination_by model.screenSize.rows - row

  /-- Re-renders the visible file viewport and re-applies the cursor highlight. -/
  def renderViewport (model : EditorState) : EditorState :=
    let screen := renderRowsLoop model 0 model.screen
    let cell := screen.get model.pos
    { model with screen := screen.set model.pos { cell with style := cursorStyle }, cellAtCursor := cell }

  /-- Fully renders the title, status, and file viewport onto a fresh screen. -/
  def renderScreen (model : EditorState) : EditorState :=
    let titleStyle : Style := { fg := some Color.cyan, bold := true }
    let screen := Screen.blank model.screenSize
    let screen := screen.putString 0 0 "Editor Demo for LeanTui. ESC quits." titleStyle
    renderViewport { model with screen := screen }

  /-- Set cursor to a new position (assumed to be in screen) and update the screen. -/
  def updateCursor (newPos: Pos) (state: EditorState): EditorState :=
    let prevCell := state.cellAtCursor
    let screen := state.screen.set state.pos prevCell
    let newCell := screen.get newPos
    let screen := screen.set newPos { newCell with style := cursorStyle}
    { state with screen := screen, pos := newPos, cellAtCursor := newCell }

  /-- Advance the cursor one cell in row-major order, scrolling down when past the bottom row. -/
  def advanceCursorOnTyping (model : EditorState) : EditorState :=
    let pos := model.pos
    let newPos :=
      if model.screenSize.rows == 0 || model.screenSize.cols == 0 then pos
      else if pos.col + 1 < model.screenSize.cols then {pos with col := pos.col + 1}
      else if pos.row + 1 < model.screenSize.rows then {pos with row := pos.row + 1}
      else pos
    if newPos == pos && pos.row == model.screenSize.rows - 1 &&
        model.scrollTop + contentRows model.screenSize < model.lines.size then
      renderViewport { model with scrollTop := model.scrollTop + 1 }
    else
      updateCursor newPos model

  /-- The initial application model with the cursor on the first content row. -/
  def initModel (path : String) (size : Size) (fileLines : Array String) : EditorState :=
    renderScreen
      { path := path
        lines := fileLines
        screenSize := size
        screen := Screen.blank size
        pos := { row := 2, col := 0 }
        cellAtCursor := Cell.blank
        scrollTop := 0 }

  /-- Inserts a character into a string at a column, padding with spaces if needed. -/
  def insertAt (line : String) (col : Nat) (ch : Char) : String :=
    let chars := line.toList
    if col >= chars.length then
      line ++ String.ofList (List.replicate (col - chars.length) ' ' ++ [ch])
    else
      let (before, after) := chars.splitAt col
      String.ofList (before ++ ch :: after)

  /-- Removes the character before a column, returning the line unchanged at column zero. -/
  def deleteBefore (line : String) (col : Nat) : String :=
    if col == 0 then
      line
    else
      let chars := line.toList
      let suffix := if col >= chars.length then [] else chars.drop col
      String.ofList ((chars.take (col - 1)) ++ suffix)

  /-- Write a typed character into the file buffer and screen at the cursor, then advance. -/
  def insertTyped (ch : Char) (model : EditorState) : EditorState :=
    let model := setMessage model ("typed " ++ String.singleton ch)
    match lineAt model model.pos.row with
    | none => setMessage model "Cannot edit outside the file lines."
    | some lineIdx =>
        let line := model.lines.getD lineIdx ""
        let newLine := insertAt line model.pos.col ch
        let lines := model.lines.set! lineIdx newLine
        let screen := renderLine model.screen model.pos.row newLine
        let model := { model with lines := lines, screen := screen, dirty := true }
        advanceCursorOnTyping { model with cellAtCursor := screen.get model.pos }

  /-- Deletes the character before the cursor in the file buffer and on screen, moving the cursor left. -/
  def backspaceTyped (model : EditorState) : EditorState :=
    let model := setMessage model "backspace"
    if model.pos.col == 0 then
      model
    else
      match lineAt model model.pos.row with
      | none => model
      | some lineIdx =>
          let line := model.lines.getD lineIdx ""
          let newLine := deleteBefore line model.pos.col
          let lines := model.lines.set! lineIdx newLine
          let screen := renderLine model.screen model.pos.row newLine
          let model := { model with lines := lines, screen := screen, dirty := true }
          let model := { model with cellAtCursor := screen.get model.pos }
          updateCursor { model.pos with col := model.pos.col - 1 } model

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
          let s := setMessage model "up"
          if s.pos.row <= 2 then
            if s.scrollTop > 0 then
              Step.continue (renderViewport { s with scrollTop := s.scrollTop - 1 })
            else
              Step.continue s
          else
            Step.continue (updateCursor { s.pos with row := s.pos.row - 1 } s)
      | Key.down =>
          let s := setMessage model "down"
          let bottom := s.screenSize.rows - 1
          if s.pos.row >= bottom then
            if s.scrollTop + contentRows s.screenSize < s.lines.size then
              Step.continue (renderViewport { s with scrollTop := s.scrollTop + 1 })
            else
              Step.continue s
          else
            Step.continue (updateCursor { s.pos with row := s.pos.row + 1 } s)
      | Key.backspace =>
          Step.continue (backspaceTyped model)
      | other =>
          match other.toInputChar? with
          | some ch => insertTyped ch model |> Step.continue
          | none => setMessage model ("Unhandled key: " ++ reprStr other) |> Step.continue

  /-- Render {name}`state` on the screen. Here we maintain a {name}`Screen` inside {name}`state`, so we return it. -/
  def viewRule (state : EditorState) : Screen := state.screen

  /-- Writes the edited lines back to the file when the app exits. -/
  def saveFile (state : EditorState) : Command FileSystem.Effect Unit := do
    let content := String.intercalate "\n" state.lines.toList
    FileSystem.writeFile state.path content

  /-- Creates an App for our Editor application. Behavior given by {name}`viewRule`, {name}`updateRule`, and {name}`saveFile` on exit. -/
  def editorApp (size : Size) (path : String) (fileLines : Array String) :
      App EditorState FileSystem.Effect :=
    { init := initModel path size fileLines
      view := viewRule
      update := fun key model => pure (updateRule key model)
      onExit := saveFile }

  /-- App entry: load the requested file and use it as initial screen content, saving edits on exit. -/
  def main (cmdLineArgs : List String) : IO Unit := do
    let sizeOpt <- Terminal.size?
    let size := sizeOpt.getD { rows := 12, cols := 72 }
    match cmdLineArgs with
    | [path] =>
        let fileLines ← (readFileLines path).run FileSystem.interpret
        App.run FileSystem.interpret (editorApp size path fileLines)
    | _ => IO.Process.exit 1

end Editor
