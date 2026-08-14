import Demo.Chess.Logic

namespace Side

/-- Renders a side name for status messages. -/
def name : Side -> String
  | white => "White"
  | black => "Black"

end Side

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
def moveEntry (record : MoveRecord) : String :=
  String.singleton (pieceGlyph record.piece) ++ squareName record.dst

/-- Formats the currently selected piece and source square, if any. -/
def selectedEntry? (state : ChessState) : Option String :=
  match state.selected with
  | none => none
  | some sq =>
      match boardGet state.board sq with
      | some piece => some (moveEntry { piece := piece, dst := sq })
      | none => none

/-- Counts display cells for the ASCII status and help text used by this demo. -/
def textWidth (text : String) : Nat :=
  text.toList.length

/-- Adds a word to a line with a separating space when needed. -/
def appendWord (line word : String) : String :=
  if line == "" then word else line ++ " " ++ word

/-- Wraps words greedily into lines no wider than the requested width. -/
def wrapWordsLoop (width : Nat) (words : List String) (line : String) (lines : Array String) :
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
def packHistoryLoop
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
def historyLinesFrom (width : Nat) (moves : List String) : String × String :=
  match packHistoryLoop width moves "" "" with
  | some lines => lines
  | none =>
      match moves with
      | [] => ("", "")
      | _ :: rest => historyLinesFrom width rest

/-- Returns the most recent move history that fits in two lines. -/
def historyLines (width : Nat) (moves : Array MoveRecord) : String × String :=
  historyLinesFrom width (moves.map moveEntry).toList
