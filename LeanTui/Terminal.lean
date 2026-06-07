import LeanTui.Screen
import LeanTui.Key

namespace LeanTui

namespace Terminal

/-- Which terminal file descriptor `stty` commands should target. -/
inductive SttyTarget where
  /-- Use `/dev/tty`, the process controlling terminal. -/
  | controllingTerminal
  /-- Use the process inherited standard input. -/
  | inheritedStdin
deriving BEq, Repr

/-- A captured terminal mode that can later be restored. -/
structure Mode where
  /-- The encoded mode returned by `stty -g`. -/
  encoded : String
  /-- The target where this mode was captured. -/
  target : SttyTarget
deriving Repr

/-- Writes text to standard output without adding a newline. -/
def write (text : String) : IO Unit :=
  IO.print text

/-- Flushes standard output. -/
def flush : IO Unit := do
  (← IO.getStdout).flush

/-- Removes carriage-return and newline characters from a string. -/
def stripLineEnd (text : String) : String :=
  String.ofList (text.toList.filter (fun ch => ch != '\n' && ch != '\r'))

/-- Adds the correct target flags to a `stty` argument list. -/
def sttyArgs (target : SttyTarget) (args : Array String) : Array String :=
  match target with
  | SttyTarget.controllingTerminal => #["-f", "/dev/tty"] ++ args
  | SttyTarget.inheritedStdin => args

/-- Runs `stty` for a target and ignores its output. -/
def runSttyOn (target : SttyTarget) (args : Array String) : IO Unit := do
  let _ ← IO.Process.output { cmd := "stty", args := sttyArgs target args }
  pure ()

/-- Runs `stty` for a target and returns captured output on success. -/
def runSttyCaptureOn (target : SttyTarget) (args : Array String) : IO (Option String) := do
  try
    let output ← IO.Process.run { cmd := "stty", args := sttyArgs target args }
    pure (some output)
  catch _ =>
    pure none

/-- Parses `stty size` output into a nonzero terminal size. -/
def parseSize (text : String) : Option Size :=
  let parts := (stripLineEnd text).splitOn " " |>.filter (fun part => part != "")
  match parts with
  | rowsText :: colsText :: _ =>
      match rowsText.toNat?, colsText.toNat? with
      | some rows, some cols =>
          if rows == 0 || cols == 0 then none else some { rows := rows, cols := cols }
      | _, _ => none
  | _ => none

/-- Reads the terminal size for one `stty` target. -/
def sizeOn? (target : SttyTarget) : IO (Option Size) := do
  match (← runSttyCaptureOn target #["size"]) with
  | some output => pure (parseSize output)
  | none => pure none

/--
Reads the current terminal size.
Tries the controlling terminal first, then inherited standard input.
-/
def size? : IO (Option Size) := do
  match (← sizeOn? SttyTarget.controllingTerminal) with
  | some size => pure (some size)
  | none => sizeOn? SttyTarget.inheritedStdin

/-- Captures the current terminal mode for one `stty` target. -/
def captureModeOn (target : SttyTarget) : IO (Option Mode) := do
  try
    let encoded ← IO.Process.run { cmd := "stty", args := sttyArgs target #["-g"] }
    pure (some { encoded := stripLineEnd encoded, target := target })
  catch _ =>
    pure none

/--
Captures the current terminal mode.
Tries the controlling terminal first, then inherited standard input.
-/
def captureMode : IO (Option Mode) := do
  match (← captureModeOn SttyTarget.controllingTerminal) with
  | some mode => pure (some mode)
  | none => captureModeOn SttyTarget.inheritedStdin

/--
Enables raw, non-echoing terminal input and returns the previous mode.
Returns `none` when the terminal mode cannot be captured or changed.
-/
def enableRaw : IO (Option Mode) := do
  let mode? ← captureMode
  match mode? with
  | none => pure none
  | some mode =>
      try
        runSttyOn mode.target #["raw", "-echo", "min", "0", "time", "0"]
        pure (some mode)
      catch _ =>
        pure none

/-- Restores a terminal mode previously returned by `enableRaw` or `captureMode`. -/
def restore (mode? : Option Mode) : IO Unit := do
  match mode? with
  | none => pure ()
  | some mode =>
      try
        runSttyOn mode.target #[mode.encoded]
      catch _ =>
        pure ()

/-- Enters the alternate screen, hides the cursor, and clears the screen. -/
def enter : IO Unit := do
  write (Ansi.enterAlternateScreen ++ Ansi.hideCursor ++ Ansi.clearScreen)
  flush

/-- Resets styling, shows the cursor, and leaves the alternate screen. -/
def leave : IO Unit := do
  write (Ansi.reset ++ Ansi.showCursor ++ Ansi.leaveAlternateScreen)
  flush

/-- Draws a full screen render to standard output. -/
def drawFull (screen : Screen) : IO Unit := do
  write screen.renderFull
  flush

/-- Draws the ANSI diff needed to update one screen to another. -/
def drawDiff (old next : Screen) : IO Unit := do
  write (Screen.renderDiff old next)
  flush

/-- Reads one line from standard input and decodes it as a key. -/
def readKeyLine : IO Key := do
  let line ← (← IO.getStdin).getLine
  pure (Key.decode (stripLineEnd line))

/-- Converts one input byte to the corresponding Unicode scalar value. -/
def byteToChar (byte : UInt8) : Char :=
  Char.ofNat byte.toNat

/-- Result of parsing one key from a buffered character stream. -/
inductive ParseResult where
  /-- A complete key and the unconsumed buffer. -/
  | key (key : Key) (rest : List Char)
  /-- The buffer may become a key after more input arrives. -/
  | incomplete

/-- Returns whether a character byte represents backspace. -/
def isBackspaceByte (ch : Char) : Bool :=
  ch == '\x7f' || ch == '\x08'

/-- Converts one non-escape character into a key. -/
def keyFromChar (ch : Char) : Key :=
  if ch == '\r' || ch == '\n' then
    Key.enter
  else if ch == '\t' then
    Key.tab
  else if isBackspaceByte ch then
    Key.backspace
  else
    Key.char ch

/-- Parses one key from the front of a character buffer. -/
def parseOne : List Char -> ParseResult
  | [] => ParseResult.incomplete
  | ['\x1b'] => ParseResult.incomplete
  | '\x1b' :: '[' :: [] => ParseResult.incomplete
  | '\x1b' :: '[' :: '3' :: [] => ParseResult.incomplete
  | '\x1b' :: '[' :: 'A' :: rest => ParseResult.key Key.up rest
  | '\x1b' :: '[' :: 'B' :: rest => ParseResult.key Key.down rest
  | '\x1b' :: '[' :: 'C' :: rest => ParseResult.key Key.right rest
  | '\x1b' :: '[' :: 'D' :: rest => ParseResult.key Key.left rest
  | '\x1b' :: '[' :: 'H' :: rest => ParseResult.key Key.home rest
  | '\x1b' :: '[' :: 'F' :: rest => ParseResult.key Key.endKey rest
  | '\x1b' :: '[' :: '1' :: '~' :: rest => ParseResult.key Key.home rest
  | '\x1b' :: '[' :: '4' :: '~' :: rest => ParseResult.key Key.endKey rest
  | '\x1b' :: 'O' :: 'H' :: rest => ParseResult.key Key.home rest
  | '\x1b' :: 'O' :: 'F' :: rest => ParseResult.key Key.endKey rest
  | '\x1b' :: '[' :: '3' :: '~' :: rest => ParseResult.key Key.delete rest
  | '\x1b' :: '[' :: ch :: rest =>
      ParseResult.key (Key.unknown (String.ofList ['\x1b', '[', ch])) rest
  | '\x1b' :: rest => ParseResult.key Key.escape rest
  | ch :: rest => ParseResult.key (keyFromChar ch) rest

/-- A raw input reader with buffered partial escape sequences. -/
structure InputReader where
  /-- Buffered characters that did not yet form a complete key. -/
  buffer : IO.Ref (List Char)

namespace InputReader

/-- Reads currently available bytes from standard input as characters. -/
def readAvailable : IO (List Char) := do
  let bytes ← (← IO.getStdin).read 64
  pure (bytes.toList.map byteToChar)

/-- Parses as many complete keys as possible from a character buffer. -/
partial def parseBuffered (keys : List Key) : List Char -> List Key × List Char
  | chars =>
      match parseOne chars with
      | ParseResult.key key rest => parseBuffered (key :: keys) rest
      | ParseResult.incomplete => (keys.reverse, chars)

/-- Polls standard input and returns all complete keys currently available. -/
def pollKeys (reader : InputReader) : IO (List Key) := do
  let buffered ← reader.buffer.get
  let available ← readAvailable
  let (keys, rest) := parseBuffered [] (buffered ++ available)
  reader.buffer.set rest
  pure keys

/-- Waits until a key is available and returns it. -/
partial def readKey (reader : InputReader) : IO Key := do
  match (← reader.pollKeys) with
  | key :: _ => pure key
  | [] =>
      IO.sleep 1
      readKey reader

end InputReader

/-- Creates a raw input reader with an empty buffer. -/
def mkInputReader : IO InputReader := do
  pure { buffer := (← IO.mkRef []) }

/-- Reads one key using a fresh raw input reader. -/
def readKeyRaw : IO Key := do
  let reader ← mkInputReader
  reader.readKey

end Terminal

end LeanTui
