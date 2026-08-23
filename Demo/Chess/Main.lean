import LeanTui
import LeanEffects.Command
import Demo.Chess.Text

open LeanTui
open LeanEffects

structure ChessModel where
  size : Size
  state : ChessState := initialState
  terminalOutputChars : Nat := 0

/-- Provides a usable terminal size when the actual terminal size cannot be read. -/
def fallbackSize : Size :=
  { rows := 18, cols := 27 }

/-- Converts a terminal mouse event over the rendered board into a chess square. -/
def mouseSquare? (event : MouseEvent) : Option Square :=
  if !event.pressed then
    none
  else if event.row < 3 || event.row >= 11 || event.col < 2 || event.col >= 26 then
    none
  else
    some { row := event.row - 3, col := (event.col - 2) / 3 }

/-- Applies a mouse click as moving the cursor and pressing Enter on a board square. -/
def clickSquare (event : MouseEvent) (model : ChessModel) : ChessModel :=
  match mouseSquare? event with
  | none => model
  | some sq => { model with state := selectSquare { model.state with cursor := sq } }

/-- Creates the initial chess model for a terminal size. -/
def initModel (size : Size) : ChessModel :=
  { size := size }

/-- Handles one key press and returns the app-loop step for the chess demo. -/
def updateChess (key : Key) (model : ChessModel) : Step ChessModel :=
  match key with
  | Key.escape | Key.char 'q' => Step.quit model
  | Key.ctrl 'l' => Step.redrawFull model
  | Key.char 'r' => Step.redrawFull { model with state := initialState }
  | _ =>
      match model.state.winner with
      | some _ => Step.continue model
      | none =>
          let state := model.state
          match key with
          | Key.left => Step.continue { model with state := moveCursor 0 (-1) state }
          | Key.right => Step.continue { model with state := moveCursor 0 1 state }
          | Key.up => Step.continue { model with state := moveCursor (-1) 0 state }
          | Key.down => Step.continue { model with state := moveCursor 1 0 state }
          | Key.mouse event => Step.continue (clickSquare event model)
          | Key.enter | Key.char ' ' => Step.continue { model with state := selectSquare state }
          | _ => Step.continue model

/-- Style for the top title and control hint. -/
def titleStyle : Style := { fg := some Color.cyan, bold := true }
/-- Style for row labels, column labels, and status labels. -/
def labelStyle : Style := { fg := some Color.yellow, bold := true }
/-- Style for light board squares. -/
def lightStyle : Style := { fg := some Color.black, bg := some Color.white, bold := false }
/-- Style for dark board squares. -/
def darkStyle : Style := { fg := some Color.black, bg := some Color.cyan, bold := false }
/-- Style used to highlight the current cursor square. -/
def cursorStyle : Style := { fg := some Color.black, bg := some Color.orange, bold := true }
/-- Style used to highlight the currently selected square. -/
def selectedStyle : Style := { fg := some Color.black, bg := some Color.yellow, bold := true }

/-- Chooses the visible style for a square from cursor, selection, and board parity. -/
def squareStyle (sq : Square) (state : ChessState) : Style :=
  if state.cursor == sq then
    cursorStyle
  else
    match state.selected with
    | some selected => if selected == sq then selectedStyle else base
    | none => base
where
  /-- Computes the ordinary checkerboard style before overlays are applied. -/
  base : Style :=
    if (sq.row + sq.col) % 2 == 0 then lightStyle else darkStyle

/-- Writes wrapped lines starting at a row and returns the next unused row. -/
def putWrappedLines
    (screen : Screen) (row col index : Nat) (lines : Array String) (style : Style) :
    Screen × Nat :=
  if index >= lines.size then
    (screen, row + index)
  else
    let screen := screen.putString (row + index) col (lines.getD index "") style
    putWrappedLines screen row col (index + 1) lines style
termination_by lines.size - index

/-- Writes wrapped text starting at a row and returns the next unused row. -/
def putWrapped
    (screen : Screen) (row col width : Nat) (text : String) (style : Style) : Screen × Nat :=
  let lines := wrapText width text
  putWrappedLines screen row col 0 lines style

/-- Writes a three-column board cell with the piece centered inside it. -/
def putCell (screen : Screen) (row col : Nat) (ch : Char) (style : Style) : Screen :=
  let screen := screen.putChar row col ' ' style
  let screen := screen.putChar row (col + 1) ch style
  screen.putChar row (col + 2) ' ' style

/-- Renders one board row into the screen buffer. -/
def renderBoardRow (screen : Screen) (state : ChessState) (boardRow col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let sq := { row := boardRow, col := col }
    let ch :=
      match boardGet state.board sq with
      | some piece => pieceGlyph piece
      | none => ' '
    let screen := putCell screen (3 + boardRow) (2 + col * 3) ch (squareStyle sq state)
    renderBoardRow screen state boardRow (col + 1)
termination_by 8 - col

/-- Renders all eight rows of the chess board. -/
def renderBoard (screen : Screen) (state : ChessState) (row : Nat) : Screen :=
  if row >= 8 then
    screen
  else
    let screen := screen.putString (3 + row) 1 (rowName row) labelStyle
    let screen := renderBoardRow screen state row 0
    renderBoard screen state (row + 1)
termination_by 8 - row

/-- Renders the column labels below the board. -/
def renderColumns (screen : Screen) (col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let screen := screen.putString 11 (3 + col * 3) (columnName col) labelStyle
    renderColumns screen (col + 1)
termination_by 8 - col

/-- Converts the current chess model into a full terminal screen. -/
def renderChess (model : ChessModel) : Screen :=
  let screen := Screen.blank model.size
  let width := model.size.cols
  let state := model.state
  let currentLine :=
    match state.winner with
    | some winner => winner.name ++ " wins"
    | none =>
        match selectedEntry? state with
        | some selected => state.turn.name ++ " moves " ++ selected
        | none => state.turn.name ++ " moves"
  let screen := screen.putString 0 0 "Chess Demo" titleStyle
  let (screen, _) := putWrapped screen 1 0 width "Click, arrows, Enter, ESC."
    Style.plain
  let screen := renderBoard screen state 0
  let screen := renderColumns screen 0
  let screen := screen.putString 12 0 currentLine Style.plain
  let (history1, history2) := historyLines width state.moveHistory
  let screen := screen.putString 13 0 history1 Style.plain
  let screen := screen.putString 14 0 history2 Style.plain
  screen.putString 15 0 ("chars printed: " ++ toString model.terminalOutputChars) Style.plain

/-- Assembles the LeanTui app record for the chess demo. -/
def chessApp (size : Size) : App ChessModel NoEffect :=
  { init := initModel size
    view := renderChess
    update := fun key model => pure (updateChess key model)
    resize := fun size model => { model with size := size }
    terminalOutput := fun count model => { model with terminalOutputChars := count }
    mouse := true }

/-- Detects the terminal size and runs the chess demo. -/
def chessMain (_args : List String) : IO Unit := do
  let size := (← Terminal.size?).getD fallbackSize
  App.run NoEffect.interpret (chessApp size)
