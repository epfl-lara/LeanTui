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

/-- A rectangular terminal screen buffer of `Cell`s with operations to manipulate it. -/
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

  /-- Writes a string starting at a zero-based row and column. -/
  def putString (screen : Screen) (row col : Nat) (text : String) (style : Style := Style.plain) : Screen :=
    (text.foldl
      (fun (state : Screen × Nat) ch =>
        let (screen, col) := state
        (screen.putChar row col ch style, col + 1))
      (screen, col)).fst

  /-- Worker for writing multiple strings in consecutive rows. -/
  def putStringsLoop (screen : Screen) (row col index fuel : Nat) (lines : Array String) (style : Style) : Screen :=
    match fuel with
    | 0 => screen
    | fuel + 1 =>
        if index >= lines.size then
          screen
        else
          let line := lines.getD index ""
          let screen := screen.putString (row + index) col line style
          putStringsLoop screen row col (index + 1) fuel lines style

  /-- Writes strings into consecutive rows starting at a zero-based top-left corner. `putString` is the single-line special case of this function. -/
  def putStrings (screen : Screen) (row col : Nat) (lines : Array String) (style : Style := Style.plain) : Screen :=
    putStringsLoop screen row col 0 (lines.size + 1) lines style

  /-- Worker for rendering columns in one row into ANSI text. -/
  def renderFullCols (screen : Screen) (row col fuel : Nat) (line : String) : String :=
    match fuel with
    | 0 => line
    | fuel + 1 =>
        if col >= screen.size.cols then
          line
        else
          renderFullCols screen row (col + 1) fuel
            (line ++ (screen.get { row := row, col := col }).render)

  /-- Worker for rendering every row of a screen into ANSI text. -/
  def renderFullRows (screen : Screen) (row fuel : Nat) (acc : String) : String :=
    match fuel with
    | 0 => acc
    | fuel + 1 =>
        if row >= screen.size.rows then
          acc
        else
          let lineStart := Ansi.moveCursor row 0
          let line := renderFullCols screen row 0 (screen.size.cols + 1) ""
          renderFullRows screen (row + 1) fuel (acc ++ lineStart ++ line)

  /-- Renders the entire screen as ANSI text, including cursor hide and clear commands. -/
  def renderFull (screen : Screen) : String :=
    Ansi.hideCursor ++ Ansi.clearScreen ++ renderFullRows screen 0 (screen.size.rows + 1) "" ++
      Ansi.reset

  /-- Worker for rendering a contiguous run of changed cells sharing one style. -/
  def renderStyleRunFuel
      (old next : Screen) (i rowEnd fuel : Nat) (style : Style) (text : String) : Nat × String :=
    match fuel with
    | 0 => (i, style.render ++ text ++ Ansi.reset)
    | fuel + 1 =>
    if i >= rowEnd || i >= next.cells.size then
      (i, style.render ++ text ++ Ansi.reset)
    else
      let oldCell := old.cells.getD i Cell.blank
      let nextCell := next.cells.getD i Cell.blank
      if oldCell == nextCell || !(nextCell.style == style) then
        (i, style.render ++ text ++ Ansi.reset)
      else
        renderStyleRunFuel old next (i + 1) rowEnd fuel style (text ++ String.singleton nextCell.ch)

  /-- Worker for rendering a contiguous run of changed cells sharing one style. -/
  def renderStyleRun
      (old next : Screen) (i rowEnd : Nat) (style : Style) (text : String) : Nat × String :=
    renderStyleRunFuel old next i rowEnd (rowEnd - i + 1) style text

  /-- Worker for rendering changed cells until the end of a row. -/
  def renderChangedRunFuel (old next : Screen) (i rowEnd fuel : Nat) (acc : String) :
      Nat × String :=
    match fuel with
    | 0 => (i, acc)
    | fuel + 1 =>
    if i >= rowEnd || i >= next.cells.size then
      (i, acc)
    else
      let oldCell := old.cells.getD i Cell.blank
      let nextCell := next.cells.getD i Cell.blank
      if oldCell == nextCell then
        (i, acc)
      else
        let (j, rendered) := renderStyleRun old next i rowEnd nextCell.style ""
        renderChangedRunFuel old next j rowEnd fuel (acc ++ rendered)

  /-- Worker for rendering changed cells until the end of a row. -/
  def renderChangedRun (old next : Screen) (i rowEnd : Nat) (acc : String) : Nat × String :=
    renderChangedRunFuel old next i rowEnd (rowEnd - i + 1) acc

  /-- Worker for rendering an ANSI diff from one screen to another. -/
  def renderDiffLoopFuel (old next : Screen) (i fuel : Nat) (acc : String) : String :=
    match fuel with
    | 0 => acc ++ Ansi.reset
    | fuel + 1 =>
    if i >= next.cells.size then
      acc ++ Ansi.reset
    else if next.size.cols == 0 then
      acc ++ Ansi.reset
    else
      let oldCell := old.cells.getD i Cell.blank
      let nextCell := next.cells.getD i Cell.blank
      if oldCell == nextCell then
        renderDiffLoopFuel old next (i + 1) fuel acc
      else
        let row := i / next.size.cols
        let col := i % next.size.cols
        let rowEnd := Nat.min next.cells.size ((row + 1) * next.size.cols)
        let (j, rendered) := renderChangedRun old next i rowEnd ""
        renderDiffLoopFuel old next j fuel (acc ++ Ansi.moveCursor row col ++ rendered)

  /-- Worker for rendering an ANSI diff from one screen to another. -/
  def renderDiffLoop (old next : Screen) (i : Nat) (acc : String) : String :=
    renderDiffLoopFuel old next i (next.cells.size - i + 1) acc

  /-- Renders the ANSI text needed to update `old` into `next`. Falls back to a full render when the screen size changed. -/
  def renderDiff (old next : Screen) : String :=
    if old.size == next.size then
      renderDiffLoop old next 0 Ansi.hideCursor
    else
      next.renderFull

end Screen

end LeanTui
