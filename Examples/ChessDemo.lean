import LeanTui

open LeanTui

inductive Side where
  | white
  | black
deriving BEq, Repr

namespace Side

def other : Side -> Side
  | white => black
  | black => white

def name : Side -> String
  | white => "White"
  | black => "Black"

end Side

inductive PieceKind where
  | king
  | queen
  | rook
  | bishop
  | knight
  | pawn
deriving BEq, Repr

structure Piece where
  side : Side
  kind : PieceKind
deriving BEq, Repr

structure Square where
  row : Nat
  col : Nat
deriving BEq, Repr

abbrev Board := Array (Option Piece)

structure ChessModel where
  size : Size
  board : Board
  cursor : Square := { row := 6, col := 4 }
  selected : Option Square := none
  turn : Side := Side.white
  message : String := "White to move"
  moveCount : Nat := 1

def fallbackSize : Size :=
  { rows := 18, cols := 52 }

def clampTo (limit value : Nat) : Nat :=
  if limit == 0 then 0 else if value >= limit then limit - 1 else value

def boardIndex (sq : Square) : Nat :=
  sq.row * 8 + sq.col

def onBoard (sq : Square) : Bool :=
  sq.row < 8 && sq.col < 8

def boardGet (board : Board) (sq : Square) : Option Piece :=
  if onBoard sq then board.getD (boardIndex sq) none else none

def boardSet (board : Board) (sq : Square) (piece : Option Piece) : Board :=
  if onBoard sq then board.set! (boardIndex sq) piece else board

def backRank (side : Side) : Array PieceKind :=
  match side with
  | Side.white => #[PieceKind.rook, PieceKind.knight, PieceKind.bishop, PieceKind.queen,
      PieceKind.king, PieceKind.bishop, PieceKind.knight, PieceKind.rook]
  | Side.black => #[PieceKind.rook, PieceKind.knight, PieceKind.bishop, PieceKind.queen,
      PieceKind.king, PieceKind.bishop, PieceKind.knight, PieceKind.rook]

partial def fillBackRank (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := (backRank side).getD col PieceKind.pawn }
    fillBackRank (boardSet board { row := row, col := col } (some piece)) side row (col + 1)

partial def fillPawns (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := PieceKind.pawn }
    fillPawns (boardSet board { row := row, col := col } (some piece)) side row (col + 1)

def initialBoard : Board :=
  let board := Array.replicate 64 none
  let board := fillBackRank board Side.black 0 0
  let board := fillPawns board Side.black 1 0
  let board := fillPawns board Side.white 6 0
  fillBackRank board Side.white 7 0

def absDiff (a b : Nat) : Nat :=
  if a >= b then a - b else b - a

def sameLine (src dst : Square) : Bool :=
  src.row == dst.row || src.col == dst.col

def sameDiagonal (src dst : Square) : Bool :=
  absDiff src.row dst.row == absDiff src.col dst.col

def stepToward (src dst : Nat) : Int :=
  if src < dst then 1 else if src > dst then -1 else 0

def squareFromInt? (row col : Int) : Option Square :=
  if row < 0 || row >= 8 || col < 0 || col >= 8 then
    none
  else
    some { row := row.toNat, col := col.toNat }

partial def clearPathLoop
    (board : Board) (row col targetRow targetCol rowStep colStep : Int) : Bool :=
  if row == targetRow && col == targetCol then
    true
  else
    match squareFromInt? row col with
    | none => false
    | some sq =>
        if boardGet board sq == none then
          clearPathLoop board (row + rowStep) (col + colStep) targetRow targetCol rowStep colStep
        else
          false

def clearPath (board : Board) (src dst : Square) : Bool :=
  let rowStep := stepToward src.row dst.row
  let colStep := stepToward src.col dst.col
  clearPathLoop board (Int.ofNat src.row + rowStep) (Int.ofNat src.col + colStep)
    (Int.ofNat dst.row) (Int.ofNat dst.col) rowStep colStep

def pawnMoveLegal (board : Board) (side : Side) (src dst : Square) : Bool :=
  let target := boardGet board dst
  match side with
  | Side.white =>
      let one := src.row == dst.row + 1 && src.col == dst.col && target == none
      let two := src.row == 6 && dst.row == 4 && src.col == dst.col &&
        target == none && boardGet board { row := 5, col := src.col } == none
      let capture := src.row == dst.row + 1 && absDiff src.col dst.col == 1 && target != none
      one || two || capture
  | Side.black =>
      let one := dst.row == src.row + 1 && src.col == dst.col && target == none
      let two := src.row == 1 && dst.row == 3 && src.col == dst.col &&
        target == none && boardGet board { row := 2, col := src.col } == none
      let capture := dst.row == src.row + 1 && absDiff src.col dst.col == 1 && target != none
      one || two || capture

def pieceMoveLegal (board : Board) (piece : Piece) (src dst : Square) : Bool :=
  if src == dst then
    false
  else
    match boardGet board dst with
    | some target =>
        if target.side == piece.side then
          false
        else
          legalShape
    | none => legalShape
where
  legalShape : Bool :=
    match piece.kind with
    | PieceKind.king => absDiff src.row dst.row <= 1 && absDiff src.col dst.col <= 1
    | PieceKind.queen => (sameLine src dst || sameDiagonal src dst) && clearPath board src dst
    | PieceKind.rook => sameLine src dst && clearPath board src dst
    | PieceKind.bishop => sameDiagonal src dst && clearPath board src dst
    | PieceKind.knight =>
        (absDiff src.row dst.row == 2 && absDiff src.col dst.col == 1) ||
        (absDiff src.row dst.row == 1 && absDiff src.col dst.col == 2)
    | PieceKind.pawn => pawnMoveLegal board piece.side src dst

def promoteIfNeeded (piece : Piece) (to : Square) : Piece :=
  match piece.kind, piece.side with
  | PieceKind.pawn, Side.white =>
      if to.row == 0 then { piece with kind := PieceKind.queen } else piece
  | PieceKind.pawn, Side.black =>
      if to.row == 7 then { piece with kind := PieceKind.queen } else piece
  | _, _ => piece

def movePiece (board : Board) (src dst : Square) (piece : Piece) : Board :=
  let board := boardSet board src none
  boardSet board dst (some (promoteIfNeeded piece dst))

def pieceGlyph : Piece -> Char
  | { side := Side.white, kind := PieceKind.king } => '♔'
  | { side := Side.white, kind := PieceKind.queen } => '♕'
  | { side := Side.white, kind := PieceKind.rook } => '♖'
  | { side := Side.white, kind := PieceKind.bishop } => '♗'
  | { side := Side.white, kind := PieceKind.knight } => '♘'
  | { side := Side.white, kind := PieceKind.pawn } => '♙'
  | { side := Side.black, kind := PieceKind.king } => '♚'
  | { side := Side.black, kind := PieceKind.queen } => '♛'
  | { side := Side.black, kind := PieceKind.rook } => '♜'
  | { side := Side.black, kind := PieceKind.bishop } => '♝'
  | { side := Side.black, kind := PieceKind.knight } => '♞'
  | { side := Side.black, kind := PieceKind.pawn } => '♟'

def pieceName (piece : Piece) : String :=
  piece.side.name ++ " " ++
    match piece.kind with
    | PieceKind.king => "king"
    | PieceKind.queen => "queen"
    | PieceKind.rook => "rook"
    | PieceKind.bishop => "bishop"
    | PieceKind.knight => "knight"
    | PieceKind.pawn => "pawn"

def fileName (col : Nat) : String :=
  String.singleton (Char.ofNat ('a'.toNat + col))

def rankName (row : Nat) : String :=
  toString (8 - row)

def squareName (sq : Square) : String :=
  fileName sq.col ++ rankName sq.row

def selectSquare (model : ChessModel) : ChessModel :=
  match model.selected with
  | none =>
      match boardGet model.board model.cursor with
      | none => { model with message := "Choose one of " ++ model.turn.name ++ "'s pieces" }
      | some piece =>
          if piece.side == model.turn then
            let model := { model with selected := some (model.cursor) }
            { model with message := pieceName piece ++ " selected on " ++ squareName model.cursor }
          else
            { model with message := "It is " ++ model.turn.name ++ "'s turn" }
  | some src =>
      if src == model.cursor then
        { model with selected := none, message := "Selection cleared" }
      else
        match boardGet model.board src with
        | none => { model with selected := none, message := "Selection cleared" }
        | some piece =>
            if pieceMoveLegal model.board piece src model.cursor then
              let nextTurn := model.turn.other
              let message := pieceName piece ++ " moved " ++ squareName src ++ " to " ++
                squareName model.cursor ++ ". " ++ nextTurn.name ++ " to move"
              let model := { model with board := movePiece model.board src model.cursor piece }
              let model := { model with selected := none }
              let model := { model with turn := nextTurn }
              let model := { model with moveCount := model.moveCount + 1 }
              { model with message := message }
            else
              { model with message := "Illegal move from " ++ squareName src ++ " to " ++
                  squareName model.cursor }

def moveCursor (dRow dCol : Int) (model : ChessModel) : ChessModel :=
  let row := clampTo 8 ((Int.ofNat model.cursor.row + dRow).toNat)
  let col := clampTo 8 ((Int.ofNat model.cursor.col + dCol).toNat)
  { model with cursor := { row := row, col := col } }

def updateChess (key : Key) (model : ChessModel) : Step ChessModel :=
  match key with
  | Key.escape | Key.char 'q' => Step.quit { model with message := "Goodbye." }
  | Key.ctrl 'l' => Step.redrawFull model
  | Key.char 'r' => Step.redrawFull { initModel model.size with message := "Board reset" }
  | Key.left => Step.continue (moveCursor 0 (-1) model)
  | Key.right => Step.continue (moveCursor 0 1 model)
  | Key.up => Step.continue (moveCursor (-1) 0 model)
  | Key.down => Step.continue (moveCursor 1 0 model)
  | Key.enter | Key.char ' ' => Step.continue (selectSquare model)
  | _ => Step.continue model
where
  initModel (size : Size) : ChessModel :=
    { size := size, board := initialBoard }

def titleStyle : Style := { fg := some Color.cyan, bold := true }
def labelStyle : Style := { fg := some Color.yellow, bold := true }
def lightStyle : Style := { fg := some Color.black, bg := some Color.white, bold := true }
def darkStyle : Style := { fg := some Color.white, bg := some Color.blue, bold := true }
def cursorStyle : Style := { fg := some Color.black, bg := some Color.orange, bold := true }
def selectedStyle : Style := { fg := some Color.black, bg := some Color.yellow, bold := true }

def squareStyle (sq : Square) (model : ChessModel) : Style :=
  if model.cursor == sq then
    cursorStyle
  else
    match model.selected with
    | some selected => if selected == sq then selectedStyle else base
    | none => base
where
  base : Style :=
    if (sq.row + sq.col) % 2 == 0 then lightStyle else darkStyle

def putCell (screen : Screen) (row col : Nat) (ch : Char) (style : Style) : Screen :=
  let screen := screen.putChar row col ' ' style
  let screen := screen.putChar row (col + 1) ch style
  screen.putChar row (col + 2) ' ' style

partial def renderBoardRow (screen : Screen) (model : ChessModel) (boardRow col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let sq := { row := boardRow, col := col }
    let ch :=
      match boardGet model.board sq with
      | some piece => pieceGlyph piece
      | none => ' '
    let screen := putCell screen (2 + boardRow) (4 + col * 3) ch (squareStyle sq model)
    renderBoardRow screen model boardRow (col + 1)

partial def renderBoard (screen : Screen) (model : ChessModel) (row : Nat) : Screen :=
  if row >= 8 then
    screen
  else
    let screen := screen.putString (2 + row) 1 (rankName row) labelStyle
    let screen := renderBoardRow screen model row 0
    renderBoard screen model (row + 1)

partial def renderFiles (screen : Screen) (col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let screen := screen.putString 10 (5 + col * 3) (fileName col) labelStyle
    renderFiles screen (col + 1)

def renderChess (model : ChessModel) : Screen :=
  let screen := Screen.blank model.size
  let screen := screen.putString 0 0
    "Chess Demo for LeanTui. Arrows move, Enter/Space selects, r resets, q/ESC quits"
    titleStyle
  let screen := renderBoard screen model 0
  let screen := renderFiles screen 0
  let screen := screen.putString 12 0 ("Turn: " ++ model.turn.name) labelStyle
  let screen := screen.putString 13 0 ("Move: " ++ toString model.moveCount) Style.plain
  let screen := screen.putString 14 0 model.message Style.plain
  screen.putString 16 0 "Note: normal piece movement is enforced; check, castling, and en passant are omitted."
    Style.plain

def initModel (size : Size) : ChessModel :=
  { size := size, board := initialBoard }

def chessApp (size : Size) : App ChessModel :=
  { init := initModel size
    view := renderChess
    update := updateChess }

def main (_args : List String) : IO Unit := do
  let size := (← Terminal.size?).getD fallbackSize
  App.run (chessApp size)
