import LeanTui

open LeanTui

inductive Side where
  | white
  | black
deriving BEq, Repr

namespace Side

/-- Returns the side whose turn follows the current side. -/
def other : Side -> Side
  | white => black
  | black => white

/-- Renders a side name for status messages. -/
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
  moveHistory : Array String := #[]
  whiteKingMoved : Bool := false
  blackKingMoved : Bool := false
  whiteQueensideRookMoved : Bool := false
  whiteKingsideRookMoved : Bool := false
  blackQueensideRookMoved : Bool := false
  blackKingsideRookMoved : Bool := false
  enPassantTarget : Option Square := none
  winner : Option Side := none

/-- Provides a usable terminal size when the actual terminal size cannot be read. -/
def fallbackSize : Size :=
  { rows := 18, cols := 27 }

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
partial def fillHomeRow (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := (homeRow side).getD col PieceKind.pawn }
    fillHomeRow (boardSet board { row := row, col := col } (some piece)) side row (col + 1)

/-- Places one side's pawns across a row. -/
partial def fillPawns (board : Board) (side : Side) (row col : Nat) : Board :=
  if col >= 8 then
    board
  else
    let piece := { side := side, kind := PieceKind.pawn }
    fillPawns (boardSet board { row := row, col := col } (some piece)) side row (col + 1)

/-- Builds the standard starting chess position. -/
def initialBoard : Board :=
  let board := Array.replicate 64 none
  let board := fillHomeRow board Side.black 0 0
  let board := fillPawns board Side.black 1 0
  let board := fillPawns board Side.white 6 0
  fillHomeRow board Side.white 7 0

/-- Returns the board row where a side's king and rooks begin. -/
def homeRowIndex : Side -> Nat
  | Side.white => 7
  | Side.black => 0

/-- Returns whether the king for a side has moved. -/
def kingMoved (model : ChessModel) : Side -> Bool
  | Side.white => model.whiteKingMoved
  | Side.black => model.blackKingMoved

/-- Returns whether the relevant rook for a side has moved. -/
def rookMoved (model : ChessModel) (side : Side) (kingside : Bool) : Bool :=
  match side, kingside with
  | Side.white, true => model.whiteKingsideRookMoved
  | Side.white, false => model.whiteQueensideRookMoved
  | Side.black, true => model.blackKingsideRookMoved
  | Side.black, false => model.blackQueensideRookMoved

/-- Marks a side's king as having moved. -/
def setKingMoved (model : ChessModel) : Side -> ChessModel
  | Side.white => { model with whiteKingMoved := true }
  | Side.black => { model with blackKingMoved := true }

/-- Marks one of a side's rooks as having moved. -/
def setRookMoved (model : ChessModel) (side : Side) (kingside : Bool) : ChessModel :=
  match side, kingside with
  | Side.white, true => { model with whiteKingsideRookMoved := true }
  | Side.white, false => { model with whiteQueensideRookMoved := true }
  | Side.black, true => { model with blackKingsideRookMoved := true }
  | Side.black, false => { model with blackQueensideRookMoved := true }

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

/-- Checks whether every square between two aligned squares is empty. -/
def clearPath (board : Board) (src dst : Square) : Bool :=
  let rowStep := stepToward src.row dst.row
  let colStep := stepToward src.col dst.col
  clearPathLoop board (Int.ofNat src.row + rowStep) (Int.ofNat src.col + colStep)
    (Int.ofNat dst.row) (Int.ofNat dst.col) rowStep colStep

/-- Checks pawn-specific movement, including double moves and ordinary captures. -/
def pawnMoveLegal (model : ChessModel) (side : Side) (src dst : Square) : Bool :=
  let board := model.board
  let target := boardGet board dst
  let enPassant :=
    target == none && absDiff src.col dst.col == 1 && model.enPassantTarget == some dst &&
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
def castleMoveLegal (model : ChessModel) (piece : Piece) (src dst : Square) : Bool :=
  let row := homeRowIndex piece.side
  let kingside := dst.col == 6
  let queenside := dst.col == 2
  let rookCol := if kingside then 7 else 0
  let rookSq := { row := row, col := rookCol }
  let rookOk :=
    match boardGet model.board rookSq with
    | some rook => rook.side == piece.side && rook.kind == PieceKind.rook
    | none => false
  let pathOk :=
    if kingside then
      boardGet model.board { row := row, col := 5 } == none &&
      boardGet model.board { row := row, col := 6 } == none
    else
      boardGet model.board { row := row, col := 1 } == none &&
      boardGet model.board { row := row, col := 2 } == none &&
      boardGet model.board { row := row, col := 3 } == none
  piece.kind == PieceKind.king && src.row == row && src.col == 4 &&
    dst.row == row && (kingside || queenside) &&
    !kingMoved model piece.side && !rookMoved model piece.side kingside &&
    rookOk && pathOk

/-- Checks whether a piece may move from one square to another on the current board. -/
def pieceMoveLegal (model : ChessModel) (piece : Piece) (src dst : Square) : Bool :=
  if src == dst then
    false
  else
    match boardGet model.board dst with
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
        || castleMoveLegal model piece src dst
    | PieceKind.queen => (sameLine src dst || sameDiagonal src dst) && clearPath model.board src dst
    | PieceKind.rook => sameLine src dst && clearPath model.board src dst
    | PieceKind.bishop => sameDiagonal src dst && clearPath model.board src dst
    | PieceKind.knight =>
        (absDiff src.row dst.row == 2 && absDiff src.col dst.col == 1) ||
        (absDiff src.row dst.row == 1 && absDiff src.col dst.col == 2)
    | PieceKind.pawn => pawnMoveLegal model piece.side src dst

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
def isEnPassantMove (model : ChessModel) (piece : Piece) (src dst : Square) : Bool :=
  piece.kind == PieceKind.pawn && boardGet model.board dst == none &&
    absDiff src.col dst.col == 1 && model.enPassantTarget == some dst

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
def markCastlingRights (model : ChessModel) (piece : Piece) (src : Square) : ChessModel :=
  match piece.kind with
  | PieceKind.king => setKingMoved model piece.side
  | PieceKind.rook =>
      let row := homeRowIndex piece.side
      if src.row == row && src.col == 0 then
        setRookMoved model piece.side false
      else if src.row == row && src.col == 7 then
        setRookMoved model piece.side true
      else
        model
  | _ => model

/-- Applies a legal move, including castling, en passant, and move-right updates. -/
def applyMove (model : ChessModel) (src dst : Square) (piece : Piece) : ChessModel :=
  let enPassant := isEnPassantMove model piece src dst
  let castle := isCastleMove piece src dst
  let board := if enPassant then removeEnPassantPawn model.board src dst else model.board
  let board := movePiece board src dst piece
  let board := if castle then moveCastleRook board piece.side dst else board
  let model := { model with board := board }
  let model := markCastlingRights model piece src
  let model := if castle then setRookMoved model piece.side (dst.col == 6) else model
  { model with enPassantTarget := nextEnPassantTarget? piece src dst }

/-- Chooses the Unicode chess glyph for a piece. -/
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

/-- Builds a human-readable piece name for status messages. -/
def pieceName (piece : Piece) : String :=
  piece.side.name ++ " " ++
    match piece.kind with
    | PieceKind.king => "king"
    | PieceKind.queen => "queen"
    | PieceKind.rook => "rook"
    | PieceKind.bishop => "bishop"
    | PieceKind.knight => "knight"
    | PieceKind.pawn => "pawn"

/-- Converts a zero-based board row into a one-based display row. -/
def rowName (row : Nat) : String :=
  toString (8 - row)

/-- Converts a zero-based board column into a display column letter. -/
def columnName (col : Nat) : String :=
  String.singleton (Char.ofNat ('a'.toNat + col))

/-- Formats a square using a column letter and row number, such as `e2`. -/
def squareName (sq : Square) : String :=
  columnName sq.col ++ rowName sq.row

/-- Formats one history entry as the moved piece glyph followed by the target square. -/
def moveEntry (piece : Piece) (dst : Square) : String :=
  String.singleton (pieceGlyph piece) ++ squareName dst

/-- Formats the currently selected piece and source square, if any. -/
def selectedEntry? (model : ChessModel) : Option String :=
  match model.selected with
  | none => none
  | some sq =>
      match boardGet model.board sq with
      | some piece => some (moveEntry piece sq)
      | none => none

/-- Counts display cells for the ASCII status and help text used by this demo. -/
def textWidth (text : String) : Nat :=
  text.toList.length

/-- Adds a word to a line with a separating space when needed. -/
def appendWord (line word : String) : String :=
  if line == "" then word else line ++ " " ++ word

/-- Wraps words greedily into lines no wider than the requested width. -/
partial def wrapWordsLoop (width : Nat) (words : List String) (line : String) (lines : Array String) :
    Array String :=
  match words with
  | [] =>
      if line == "" then lines else lines.push line
  | word :: rest =>
      let candidate := appendWord line word
      if textWidth candidate <= width || line == "" then
        wrapWordsLoop width rest candidate lines
      else
        wrapWordsLoop width rest word (lines.push line)

/-- Wraps a space-separated string to the available screen width. -/
def wrapText (width : Nat) (text : String) : Array String :=
  if width == 0 then #[] else wrapWordsLoop width (text.splitOn " ") "" #[]

/-- Tries to append a compact move entry to a history line. -/
def tryAppendMove (width : Nat) (line entry : String) : Option String :=
  let candidate := appendWord line entry
  if textWidth candidate <= width then some candidate else none

/-- Packs chronological move entries into at most two display lines. -/
partial def packHistoryLoop
    (width : Nat) (moves : List String) (line1 line2 : String) : Option (String × String) :=
  match moves with
  | [] => some (line1, line2)
  | move :: rest =>
      if line2 == "" then
        match tryAppendMove width line1 move with
        | some line1 => packHistoryLoop width rest line1 ""
        | none =>
            match tryAppendMove width "" move with
            | some line2 => packHistoryLoop width rest line1 line2
            | none => none
      else
        match tryAppendMove width line2 move with
        | some line2 => packHistoryLoop width rest line1 line2
        | none => none

/-- Drops oldest moves until the rest fit in two history lines. -/
partial def historyLinesFrom (width : Nat) (moves : List String) : String × String :=
  match packHistoryLoop width moves "" "" with
  | some lines => lines
  | none =>
      match moves with
      | [] => ("", "")
      | _ :: rest => historyLinesFrom width rest

/-- Returns the most recent move history that fits in two lines. -/
def historyLines (width : Nat) (moves : Array String) : String × String :=
  historyLinesFrom width moves.toList

/-- Writes wrapped text starting at a row and returns the next unused row. -/
partial def putWrapped
    (screen : Screen) (row col width : Nat) (text : String) (style : Style) : Screen × Nat :=
  let lines := wrapText width text
  let rec loop (screen : Screen) (index : Nat) : Screen × Nat :=
    if index >= lines.size then
      (screen, row + lines.size)
    else
      let screen := screen.putString (row + index) col (lines.getD index "") style
      loop screen (index + 1)
  loop screen 0

/-- Selects a piece or attempts to move the selected piece to the cursor square. -/
def selectSquare (model : ChessModel) : ChessModel :=
  match model.winner with
  | some winner => { model with message := winner.name ++ " wins" }
  | none =>
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
            if pieceMoveLegal model piece src model.cursor then
              let nextTurn := model.turn.other
              let captured := boardGet model.board model.cursor
              let won :=
                match captured with
                | some target => target.kind == PieceKind.king
                | none => false
              let message :=
                if won then
                  piece.side.name ++ " wins"
                else
                  pieceName piece ++ " " ++ squareName src ++ " to " ++
                    squareName model.cursor ++ "; " ++ nextTurn.name ++ " turn"
              let model := applyMove model src model.cursor piece
              let model := { model with moveHistory := model.moveHistory.push (moveEntry piece model.cursor) }
              let model := { model with selected := none }
              let model := if won then { model with winner := some piece.side } else { model with turn := nextTurn }
              let model := { model with moveCount := model.moveCount + 1 }
              { model with message := message }
            else
              match boardGet model.board model.cursor with
              | some target =>
                  if target.side == piece.side then
                    let model := { model with selected := some (model.cursor) }
                    { model with message := pieceName target ++ " selected on " ++ squareName model.cursor }
                  else
                    { model with message := "Illegal move from " ++ squareName src ++ " to " ++
                        squareName model.cursor }
              | none =>
                  { model with message := "Illegal move from " ++ squareName src ++ " to " ++
                      squareName model.cursor }

/-- Moves the board cursor by a signed row and column delta. -/
def moveCursor (dRow dCol : Int) (model : ChessModel) : ChessModel :=
  let row := clampTo 8 ((Int.ofNat model.cursor.row + dRow).toNat)
  let col := clampTo 8 ((Int.ofNat model.cursor.col + dCol).toNat)
  { model with cursor := { row := row, col := col } }

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
  | some sq => selectSquare { model with cursor := sq }

/-- Handles one key press and returns the app-loop step for the chess demo. -/
def updateChess (key : Key) (model : ChessModel) : Step ChessModel :=
  match key with
  | Key.escape | Key.char 'q' => Step.quit { model with message := "Goodbye." }
  | Key.ctrl 'l' => Step.redrawFull model
  | Key.char 'r' => Step.redrawFull { initModel model.size with message := "Board reset" }
  | _ =>
      match model.winner with
      | some winner => Step.continue { model with message := winner.name ++ " wins" }
      | none =>
          match key with
          | Key.left => Step.continue (moveCursor 0 (-1) model)
          | Key.right => Step.continue (moveCursor 0 1 model)
          | Key.up => Step.continue (moveCursor (-1) 0 model)
          | Key.down => Step.continue (moveCursor 1 0 model)
          | Key.mouse event =>
              let next := clickSquare event model
              if next.moveCount == model.moveCount then
                Step.continue next
              else
                Step.redrawFull next
          | Key.enter | Key.char ' ' =>
              let next := selectSquare model
              if next.moveCount == model.moveCount then
                Step.continue next
              else
                Step.redrawFull next
          | _ => Step.continue model
where
  /-- Builds a fresh model used by the reset key. -/
  initModel (size : Size) : ChessModel :=
    { size := size, board := initialBoard }

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
def squareStyle (sq : Square) (model : ChessModel) : Style :=
  if model.cursor == sq then
    cursorStyle
  else
    match model.selected with
    | some selected => if selected == sq then selectedStyle else base
    | none => base
where
  /-- Computes the ordinary checkerboard style before overlays are applied. -/
  base : Style :=
    if (sq.row + sq.col) % 2 == 0 then lightStyle else darkStyle

/-- Writes a three-column board cell with the piece centered inside it. -/
def putCell (screen : Screen) (row col : Nat) (ch : Char) (style : Style) : Screen :=
  let screen := screen.putChar row col ' ' style
  let screen := screen.putChar row (col + 1) ch style
  screen.putChar row (col + 2) ' ' style

/-- Renders one board row into the screen buffer. -/
partial def renderBoardRow (screen : Screen) (model : ChessModel) (boardRow col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let sq := { row := boardRow, col := col }
    let ch :=
      match boardGet model.board sq with
      | some piece => pieceGlyph piece
      | none => ' '
    let screen := putCell screen (3 + boardRow) (2 + col * 3) ch (squareStyle sq model)
    renderBoardRow screen model boardRow (col + 1)

/-- Renders all eight rows of the chess board. -/
partial def renderBoard (screen : Screen) (model : ChessModel) (row : Nat) : Screen :=
  if row >= 8 then
    screen
  else
    let screen := screen.putString (3 + row) 1 (rowName row) labelStyle
    let screen := renderBoardRow screen model row 0
    renderBoard screen model (row + 1)

/-- Renders the column labels below the board. -/
partial def renderColumns (screen : Screen) (col : Nat) : Screen :=
  if col >= 8 then
    screen
  else
    let screen := screen.putString 11 (3 + col * 3) (columnName col) labelStyle
    renderColumns screen (col + 1)

/-- Converts the current chess model into a full terminal screen. -/
def renderChess (model : ChessModel) : Screen :=
  let screen := Screen.blank model.size
  let width := model.size.cols
  let currentLine :=
    match model.winner with
    | some winner => winner.name ++ " wins"
    | none =>
        match selectedEntry? model with
        | some selected => model.turn.name ++ " moves " ++ selected
        | none => model.turn.name ++ " moves"
  let screen := screen.putString 0 0 "Chess Demo" titleStyle
  let (screen, _) := putWrapped screen 1 0 width "Click, arrows, Enter, ESC."
    Style.plain
  let screen := renderBoard screen model 0
  let screen := renderColumns screen 0
  let screen := screen.putString 12 0 currentLine Style.plain
  -- let screen := screen.putString 13 0 ("Move: " ++ toString model.moveCount) Style.plain
  -- let (screen, _) := putWrapped screen 14 0 width model.message Style.plain
  let (history1, history2) := historyLines width model.moveHistory
  let screen := screen.putString 13 0 history1 Style.plain
  let screen := screen.putString 14 0 history2 Style.plain
  screen

/-- Creates the initial chess model for the current terminal size. -/
def initModel (size : Size) : ChessModel :=
  { size := size, board := initialBoard }

/-- Assembles the LeanTui app record for the chess demo. -/
def chessApp (size : Size) : App ChessModel :=
  { init := initModel size
    view := renderChess
    update := updateChess
    resize := fun size model => { model with size := size }
    mouse := true }

/-- Entry point that detects terminal size and runs the chess demo. -/
def main (_args : List String) : IO Unit := do
  let size := (← Terminal.size?).getD fallbackSize
  App.run (chessApp size)
