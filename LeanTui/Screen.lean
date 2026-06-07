import LeanTui.Style

namespace LeanTui

structure Pos where
  row : Nat
  col : Nat
deriving BEq, Repr

structure Size where
  rows : Nat
  cols : Nat
deriving BEq, Repr

structure Cell where
  ch : Char := ' '
  style : Style := Style.plain
deriving BEq, Repr

namespace Cell

def blank : Cell := {}

def render (cell : Cell) : String :=
  cell.style.render ++ String.singleton cell.ch ++ Ansi.reset

end Cell

structure Screen where
  size : Size
  cells : Array Cell
deriving BEq, Repr

namespace Screen

def blank (size : Size) : Screen :=
  { size := size, cells := Array.replicate (size.rows * size.cols) Cell.blank }

def index (size : Size) (pos : Pos) : Nat :=
  pos.row * size.cols + pos.col

def inBounds (screen : Screen) (pos : Pos) : Bool :=
  pos.row < screen.size.rows && pos.col < screen.size.cols

def get (screen : Screen) (pos : Pos) : Cell :=
  if screen.inBounds pos then
    screen.cells.getD (index screen.size pos) Cell.blank
  else
    Cell.blank

def set (screen : Screen) (pos : Pos) (cell : Cell) : Screen :=
  if screen.inBounds pos then
    { screen with cells := screen.cells.set! (index screen.size pos) cell }
  else
    screen

def putChar (screen : Screen) (row col : Nat) (ch : Char) (style : Style := Style.plain) : Screen :=
  screen.set { row := row, col := col } { ch := ch, style := style }

partial def putStringLoop (screen : Screen) (row col : Nat) (chars : List Char) (style : Style) : Screen :=
  match chars with
  | [] => screen
  | ch :: rest =>
      putStringLoop (screen.putChar row col ch style) row (col + 1) rest style

def putString (screen : Screen) (row col : Nat) (text : String) (style : Style := Style.plain) : Screen :=
  putStringLoop screen row col text.toList style

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

def renderFull (screen : Screen) : String :=
  Ansi.hideCursor ++ Ansi.clearScreen ++ renderFullRows screen 0 "" ++ Ansi.reset

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

def renderDiff (old next : Screen) : String :=
  if old.size == next.size then
    renderDiffLoop old next 0 Ansi.hideCursor
  else
    next.renderFull

end Screen

end LeanTui
