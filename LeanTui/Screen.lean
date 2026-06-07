import LeanTui.Style

namespace LeanTui

/-- A zero-based position on a screen. -/
structure Pos where
  /-- Zero-based row. -/
  row : Nat
  /-- Zero-based column. -/
  col : Nat
deriving BEq, Repr

/-- The dimensions of a terminal screen in rows and columns. -/
structure Size where
  /-- Number of rows. -/
  rows : Nat
  /-- Number of columns. -/
  cols : Nat
deriving BEq, Repr

/-- A single screen cell with a character and style. -/
structure Cell where
  /-- Character displayed in the cell. -/
  ch : Char := ' '
  /-- Style used to render the character. -/
  style : Style := Style.plain
deriving BEq, Repr

namespace Cell

/-- A blank cell containing a space with the plain style. -/
def blank : Cell := {}

/-- Renders a cell as styled terminal text followed by an ANSI reset. -/
def render (cell : Cell) : String :=
  cell.style.render ++ String.singleton cell.ch ++ Ansi.reset

end Cell

/-- A rectangular terminal screen buffer. -/
structure Screen where
  /-- Screen dimensions. -/
  size : Size
  /-- Row-major cell storage. -/
  cells : Array Cell
deriving BEq, Repr

namespace Screen

/-- Creates a blank screen buffer of the requested size. -/
def blank (size : Size) : Screen :=
  { size := size, cells := Array.replicate (size.rows * size.cols) Cell.blank }

/-- Converts a position into a row-major cell index for the given size. -/
def index (size : Size) (pos : Pos) : Nat :=
  pos.row * size.cols + pos.col

/-- Returns whether a position lies inside the screen bounds. -/
def inBounds (screen : Screen) (pos : Pos) : Bool :=
  pos.row < screen.size.rows && pos.col < screen.size.cols

/-- Returns the cell at a position, or `Cell.blank` when out of bounds. -/
def get (screen : Screen) (pos : Pos) : Cell :=
  if screen.inBounds pos then
    screen.cells.getD (index screen.size pos) Cell.blank
  else
    Cell.blank

/-- Sets a cell at a position, leaving the screen unchanged when out of bounds. -/
def set (screen : Screen) (pos : Pos) (cell : Cell) : Screen :=
  if screen.inBounds pos then
    { screen with cells := screen.cells.set! (index screen.size pos) cell }
  else
    screen

/-- Writes one character at a zero-based row and column. -/
def putChar (screen : Screen) (row col : Nat) (ch : Char) (style : Style := Style.plain) : Screen :=
  screen.set { row := row, col := col } { ch := ch, style := style }

/-- Worker for writing a list of characters across a row. -/
partial def putStringLoop (screen : Screen) (row col : Nat) (chars : List Char) (style : Style) : Screen :=
  match chars with
  | [] => screen
  | ch :: rest =>
      putStringLoop (screen.putChar row col ch style) row (col + 1) rest style

/-- Writes a string starting at a zero-based row and column. -/
def putString (screen : Screen) (row col : Nat) (text : String) (style : Style := Style.plain) : Screen :=
  putStringLoop screen row col text.toList style

/-- Worker for rendering every row of a screen into ANSI text. -/
partial def renderFullRows (screen : Screen) (row : Nat) (acc : String) : String :=
  if row >= screen.size.rows then
    acc
  else
    let lineStart := Ansi.moveCursor row 0
    let rec renderCols (col : Nat) (line : String) : String :=
      if col >= screen.size.cols then
        line
      else
        renderCols (col + 1) (line ++ (screen.get { row := row, col := col }).render)
    renderFullRows screen (row + 1) (acc ++ lineStart ++ renderCols 0 "")

/-- Renders the entire screen as ANSI text, including cursor hide and clear commands. -/
def renderFull (screen : Screen) : String :=
  Ansi.hideCursor ++ Ansi.clearScreen ++ renderFullRows screen 0 "" ++ Ansi.reset

/-- Worker for rendering a contiguous run of changed cells sharing one style. -/
partial def renderStyleRun
    (old next : Screen) (i rowEnd : Nat) (style : Style) (text : String) : Nat × String :=
  if i >= rowEnd || i >= next.cells.size then
    (i, style.render ++ text ++ Ansi.reset)
  else
    let oldCell := old.cells.getD i Cell.blank
    let nextCell := next.cells.getD i Cell.blank
    if oldCell == nextCell || !(nextCell.style == style) then
      (i, style.render ++ text ++ Ansi.reset)
    else
      renderStyleRun old next (i + 1) rowEnd style (text ++ String.singleton nextCell.ch)

/-- Worker for rendering changed cells until the end of a row. -/
partial def renderChangedRun (old next : Screen) (i rowEnd : Nat) (acc : String) : Nat × String :=
  if i >= rowEnd || i >= next.cells.size then
    (i, acc)
  else
    let oldCell := old.cells.getD i Cell.blank
    let nextCell := next.cells.getD i Cell.blank
    if oldCell == nextCell then
      (i, acc)
    else
      let (j, rendered) := renderStyleRun old next i rowEnd nextCell.style ""
      renderChangedRun old next j rowEnd (acc ++ rendered)

/-- Worker for rendering an ANSI diff from one screen to another. -/
partial def renderDiffLoop (old next : Screen) (i : Nat) (acc : String) : String :=
  if i >= next.cells.size then
    acc ++ Ansi.reset
  else if next.size.cols == 0 then
    acc ++ Ansi.reset
  else
    let oldCell := old.cells.getD i Cell.blank
    let nextCell := next.cells.getD i Cell.blank
    if oldCell == nextCell then
      renderDiffLoop old next (i + 1) acc
    else
      let row := i / next.size.cols
      let col := i % next.size.cols
      let rowEnd := Nat.min next.cells.size ((row + 1) * next.size.cols)
      let (j, rendered) := renderChangedRun old next i rowEnd ""
      renderDiffLoop old next j (acc ++ Ansi.moveCursor row col ++ rendered)

/--
Renders the ANSI text needed to update `old` into `next`.
Falls back to a full render when the screen size changed.
-/
def renderDiff (old next : Screen) : String :=
  if old.size == next.size then
    renderDiffLoop old next 0 Ansi.hideCursor
  else
    next.renderFull

end Screen

end LeanTui
