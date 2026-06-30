/-- The two players in a chess game. -/
inductive Side where
  | white
  | black
deriving BEq, Repr

namespace Side

/-- Returns the side whose turn follows the current side. -/
def other : Side -> Side
  | white => black
  | black => white

end Side

/-- The role of a chess piece, independent of which side owns it. -/
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

structure MoveRecord where
  piece : Piece
  dst : Square
deriving BEq, Repr

abbrev Board := Array (Option Piece)

structure ChessState where
  board : Board
  cursor : Square := { row := 6, col := 4 }
  selected : Option Square := none
  turn : Side := Side.white
  moveCount : Nat := 1
  moveHistory : Array MoveRecord := #[]
  whiteKingMoved : Bool := false
  blackKingMoved : Bool := false
  whiteQueensideRookMoved : Bool := false
  whiteKingsideRookMoved : Bool := false
  blackQueensideRookMoved : Bool := false
  blackKingsideRookMoved : Bool := false
  enPassantTarget : Option Square := none
  winner : Option Side := none

/-- Clamps a natural number into the zero-based range below `limit`. -/
def clampTo (limit value : Nat) : Nat :=
  if limit == 0 then 0 else if value >= limit then limit - 1 else value

/-- Converts a square coordinate into the board array's row-major index. -/
def boardIndex (sq : Square) : Nat :=
  sq.row * 8 + sq.col

/-- Checks whether a square is inside the 8 by 8 chess board. -/
def onBoard (sq : Square) : Bool :=
  sq.row < 8 && sq.col < 8

/-- Looks up the piece on a square, returning `none` for empty or out-of-bounds squares. -/
def boardGet (board : Board) (sq : Square) : Option Piece :=
  if onBoard sq then board.getD (boardIndex sq) none else none

/-- Writes an optional piece to a square, leaving the board unchanged out of bounds. -/
def boardSet (board : Board) (sq : Square) (piece : Option Piece) : Board :=
  if onBoard sq then board.set! (boardIndex sq) piece else board

/-- Lists the piece order used for a player's home row. -/
def homeRow (side : Side) : Array PieceKind :=
  match side with
  | Side.white => #[PieceKind.rook, PieceKind.knight, PieceKind.bishop, PieceKind.queen,
      PieceKind.king, PieceKind.bishop, PieceKind.knight, PieceKind.rook]
  | Side.black => #[PieceKind.rook, PieceKind.knight, PieceKind.bishop, PieceKind.queen,
      PieceKind.king, PieceKind.bishop, PieceKind.knight, PieceKind.rook]

/-- Places one side's non-pawn pieces across a home row. -/
def fillHomeRow (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := (homeRow side).getD col PieceKind.pawn }
    fillHomeRow (boardSet board { row := row, col := col } (some piece)) side row (col + 1)
termination_by 8 - col

/-- Places one side's pawns across a row. -/
def fillPawns (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := PieceKind.pawn }
    fillPawns (boardSet board { row := row, col := col } (some piece)) side row (col + 1)
termination_by 8 - col

/-- Builds the standard starting chess position. -/
def initialBoard : Board :=
  let board := Array.replicate 64 none
  let board := fillHomeRow board Side.black 0 0
  let board := fillPawns board Side.black 1 0
  let board := fillPawns board Side.white 6 0
  fillHomeRow board Side.white 7 0

/-- Builds a fresh game state in the standard starting position. -/
def initialState : ChessState :=
  { board := initialBoard }

/-- Returns the board row where a side's king and rooks begin. -/
def homeRowIndex : Side -> Nat
  | Side.white => 7
  | Side.black => 0

/-- Returns whether the king for a side has moved. -/
def kingMoved (state : ChessState) : Side -> Bool
  | Side.white => state.whiteKingMoved
  | Side.black => state.blackKingMoved

/-- Returns whether the relevant rook for a side has moved. -/
def rookMoved (state : ChessState) (side : Side) (kingside : Bool) : Bool :=
  match side, kingside with
  | Side.white, true => state.whiteKingsideRookMoved
  | Side.white, false => state.whiteQueensideRookMoved
  | Side.black, true => state.blackKingsideRookMoved
  | Side.black, false => state.blackQueensideRookMoved

/-- Marks a side's king as having moved. -/
def setKingMoved (state : ChessState) : Side -> ChessState
  | Side.white => { state with whiteKingMoved := true }
  | Side.black => { state with blackKingMoved := true }

/-- Marks one of a side's rooks as having moved. -/
def setRookMoved (state : ChessState) (side : Side) (kingside : Bool) : ChessState :=
  match side, kingside with
  | Side.white, true => { state with whiteKingsideRookMoved := true }
  | Side.white, false => { state with whiteQueensideRookMoved := true }
  | Side.black, true => { state with blackKingsideRookMoved := true }
  | Side.black, false => { state with blackQueensideRookMoved := true }

/-- Computes the absolute difference between two natural numbers. -/
def absDiff (a b : Nat) : Nat :=
  if a >= b then a - b else b - a

/-- Checks whether two squares share a row or column. -/
def sameLine (src dst : Square) : Bool :=
  src.row == dst.row || src.col == dst.col

/-- Checks whether two squares lie on the same diagonal. -/
def sameDiagonal (src dst : Square) : Bool :=
  absDiff src.row dst.row == absDiff src.col dst.col

/-- Returns the signed unit step from one coordinate toward another. -/
def stepToward (src dst : Nat) : Int :=
  if src < dst then 1 else if src > dst then -1 else 0

/-- Converts signed row and column coordinates into an in-bounds square. -/
def squareFromInt? (row col : Int) : Option Square :=
  if row < 0 || row >= 8 || col < 0 || col >= 8 then
    none
  else
    some { row := row.toNat, col := col.toNat }

/-- Walks a ray across the board to decide whether intervening squares are empty. -/
def clearPathLoopFuel
    (board : Board) (row col targetRow targetCol rowStep colStep : Int)
    (isLine: (∃ k: Int, k > 0 &&
                       row + k*rowStep = targetRow &&
                       col + k*colStep = targetCol)): Bool :=
  if row == targetRow && col == targetCol then
    true
  else
    match squareFromInt? row col with
    | none => false
    | some sq =>
        if boardGet board sq == none then
          clearPathLoopFuel board (row + rowStep) (col + colStep) targetRow targetCol rowStep colStep
          (sorry)
        else
          false

/-- Walks a ray across the board to decide whether intervening squares are empty. -/
def clearPathLoop
    (board : Board) (row col targetRow targetCol rowStep colStep : Int) : Bool :=
  clearPathLoopFuel board row col targetRow targetCol rowStep colStep (sorry)


/-- Checks whether every square between two aligned squares is empty. -/
def clearPath (board : Board) (src dst : Square) : Bool :=
  let rowStep := stepToward src.row dst.row
  let colStep := stepToward src.col dst.col
  clearPathLoop board (Int.ofNat src.row + rowStep) (Int.ofNat src.col + colStep)
    (Int.ofNat dst.row) (Int.ofNat dst.col) rowStep colStep

/-- Checks pawn-specific movement, including double moves and ordinary captures. -/
def pawnMoveLegal (state : ChessState) (side : Side) (src dst : Square) : Bool :=
  let board := state.board
  let target := boardGet board dst
  let enPassant :=
    target == none && absDiff src.col dst.col == 1 && state.enPassantTarget == some dst &&
      match boardGet board { row := src.row, col := dst.col } with
      | some piece => piece.side != side && piece.kind == PieceKind.pawn
      | none => false
  match side with
  | Side.white =>
      let one := src.row == dst.row + 1 && src.col == dst.col && target == none
      let two := src.row == 6 && dst.row == 4 && src.col == dst.col &&
        target == none && boardGet board { row := 5, col := src.col } == none
      let capture := src.row == dst.row + 1 && absDiff src.col dst.col == 1 && target != none
      one || two || capture || (src.row == dst.row + 1 && enPassant)
  | Side.black =>
      let one := dst.row == src.row + 1 && src.col == dst.col && target == none
      let two := src.row == 1 && dst.row == 3 && src.col == dst.col &&
        target == none && boardGet board { row := 2, col := src.col } == none
      let capture := dst.row == src.row + 1 && absDiff src.col dst.col == 1 && target != none
      one || two || capture || (dst.row == src.row + 1 && enPassant)

/-- Checks whether a castle move has clear path and retained castling rights. -/
def castleMoveLegal (state : ChessState) (piece : Piece) (src dst : Square) : Bool :=
  let row := homeRowIndex piece.side
  let kingside := dst.col == 6
  let queenside := dst.col == 2
  let rookCol := if kingside then 7 else 0
  let rookSq := { row := row, col := rookCol }
  let rookOk :=
    match boardGet state.board rookSq with
    | some rook => rook.side == piece.side && rook.kind == PieceKind.rook
    | none => false
  let pathOk :=
    if kingside then
      boardGet state.board { row := row, col := 5 } == none &&
      boardGet state.board { row := row, col := 6 } == none
    else
      boardGet state.board { row := row, col := 1 } == none &&
      boardGet state.board { row := row, col := 2 } == none &&
      boardGet state.board { row := row, col := 3 } == none
  piece.kind == PieceKind.king && src.row == row && src.col == 4 &&
    dst.row == row && (kingside || queenside) &&
    !kingMoved state piece.side && !rookMoved state piece.side kingside &&
    rookOk && pathOk

/-- Checks whether a piece may move from one square to another on the current board. -/
def pieceMoveLegal (state : ChessState) (piece : Piece) (src dst : Square) : Bool :=
  if src == dst then
    false
  else
    match boardGet state.board dst with
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
        || castleMoveLegal state piece src dst
    | PieceKind.queen => (sameLine src dst || sameDiagonal src dst) && clearPath state.board src dst
    | PieceKind.rook => sameLine src dst && clearPath state.board src dst
    | PieceKind.bishop => sameDiagonal src dst && clearPath state.board src dst
    | PieceKind.knight =>
        (absDiff src.row dst.row == 2 && absDiff src.col dst.col == 1) ||
        (absDiff src.row dst.row == 1 && absDiff src.col dst.col == 2)
    | PieceKind.pawn => pawnMoveLegal state piece.side src dst

/-- Promotes a pawn to a queen when it reaches the far row. -/
def promoteIfNeeded (piece : Piece) (to : Square) : Piece :=
  match piece.kind, piece.side with
  | PieceKind.pawn, Side.white =>
      if to.row == 0 then { piece with kind := PieceKind.queen } else piece
  | PieceKind.pawn, Side.black =>
      if to.row == 7 then { piece with kind := PieceKind.queen } else piece
  | _, _ => piece

/-- Moves a piece between squares and applies promotion if needed. -/
def movePiece (board : Board) (src dst : Square) (piece : Piece) : Board :=
  let board := boardSet board src none
  boardSet board dst (some (promoteIfNeeded piece dst))

/-- Returns whether a move is a two-square pawn advance. -/
def isDoublePawnMove (piece : Piece) (src dst : Square) : Bool :=
  piece.kind == PieceKind.pawn && src.col == dst.col && absDiff src.row dst.row == 2

/-- Returns the en passant target square created by a two-square pawn advance. -/
def nextEnPassantTarget? (piece : Piece) (src dst : Square) : Option Square :=
  if isDoublePawnMove piece src dst then
    match piece.side with
    | Side.white => some { row := src.row - 1, col := src.col }
    | Side.black => some { row := src.row + 1, col := src.col }
  else
    none

/-- Returns whether a legal king move is castling. -/
def isCastleMove (piece : Piece) (src dst : Square) : Bool :=
  piece.kind == PieceKind.king && src.row == dst.row && absDiff src.col dst.col == 2

/-- Returns whether a legal pawn move captures en passant. -/
def isEnPassantMove (state : ChessState) (piece : Piece) (src dst : Square) : Bool :=
  piece.kind == PieceKind.pawn && boardGet state.board dst == none &&
    absDiff src.col dst.col == 1 && state.enPassantTarget == some dst

/-- Applies any rook movement needed for castling. -/
def moveCastleRook (board : Board) (side : Side) (dst : Square) : Board :=
  let row := homeRowIndex side
  if dst.col == 6 then
    match boardGet board { row := row, col := 7 } with
    | some rook =>
        let board := boardSet board { row := row, col := 7 } none
        boardSet board { row := row, col := 5 } (some rook)
    | none => board
  else if dst.col == 2 then
    match boardGet board { row := row, col := 0 } with
    | some rook =>
        let board := boardSet board { row := row, col := 0 } none
        boardSet board { row := row, col := 3 } (some rook)
    | none => board
  else
    board

/-- Removes the pawn captured by en passant. -/
def removeEnPassantPawn (board : Board) (src dst : Square) : Board :=
  boardSet board { row := src.row, col := dst.col } none

/-- Updates castling rights after a king or rook move. -/
def markCastlingRights (state : ChessState) (piece : Piece) (src : Square) : ChessState :=
  match piece.kind with
  | PieceKind.king => setKingMoved state piece.side
  | PieceKind.rook =>
      let row := homeRowIndex piece.side
      if src.row == row && src.col == 0 then
        setRookMoved state piece.side false
      else if src.row == row && src.col == 7 then
        setRookMoved state piece.side true
      else
        state
  | _ => state

/-- Applies a legal move, including castling, en passant, and move-right updates. -/
def applyMove (state : ChessState) (src dst : Square) (piece : Piece) : ChessState :=
  let enPassant := isEnPassantMove state piece src dst
  let castle := isCastleMove piece src dst
  let board := if enPassant then removeEnPassantPawn state.board src dst else state.board
  let board := movePiece board src dst piece
  let board := if castle then moveCastleRook board piece.side dst else board
  let state := { state with board := board }
  let state := markCastlingRights state piece src
  let state := if castle then setRookMoved state piece.side (dst.col == 6) else state
  { state with enPassantTarget := nextEnPassantTarget? piece src dst }

/-- Selects a piece or attempts to move the selected piece to the cursor square. -/
def selectSquare (state : ChessState) : ChessState :=
  match state.winner with
  | some _ => state
  | none =>
  match state.selected with
  | none =>
      match boardGet state.board state.cursor with
      | none => state
      | some piece =>
          if piece.side == state.turn then
            { state with selected := some state.cursor }
          else
            state
  | some src =>
      if src == state.cursor then
        { state with selected := none }
      else
        match boardGet state.board src with
        | none => { state with selected := none }
        | some piece =>
            if pieceMoveLegal state piece src state.cursor then
              let nextTurn := state.turn.other
              let captured := boardGet state.board state.cursor
              let won :=
                match captured with
                | some target => target.kind == PieceKind.king
                | none => false
              let state := applyMove state src state.cursor piece
              let state := { state with moveHistory := state.moveHistory.push { piece := piece, dst := state.cursor } }
              let state := { state with selected := none }
              let state := if won then { state with winner := some piece.side } else { state with turn := nextTurn }
              { state with moveCount := state.moveCount + 1 }
            else
              match boardGet state.board state.cursor with
              | some target =>
                  if target.side == piece.side then
                    { state with selected := some state.cursor }
                  else
                    state
              | none => state

/-- Moves the board cursor by a signed row and column delta. -/
def moveCursor (dRow dCol : Int) (state : ChessState) : ChessState :=
  let row := clampTo 8 ((Int.ofNat state.cursor.row + dRow).toNat)
  let col := clampTo 8 ((Int.ofNat state.cursor.col + dCol).toNat)
  { state with cursor := { row := row, col := col } }
