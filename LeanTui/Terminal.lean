import LeanTui.Screen
import LeanTui.Key

namespace LeanTui

namespace Terminal

/-- Which terminal file descriptor `stty` commands should target. -/
inductive SttyTarget where
  /-- Use `/dev/tty` with BSD/macOS `stty -f`. -/
  | controllingTerminalBsd
  /-- Use `/dev/tty` with GNU/Linux `stty -F`. -/
  | controllingTerminalGnu
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
private def write (text : String) : IO Unit :=
  IO.print text

/-- Counts the characters in text that will be sent to the terminal. -/
def outputCharCount (text : String) : Nat :=
  text.toList.length

/-- Writes text to standard output and returns the number of characters written. -/
def writeMeasured (text : String) : IO Nat := do
  write text
  pure text.length

/-- Flushes standard output. -/
def flush : IO Unit := do
  (← IO.getStdout).flush

/-- Removes carriage-return and newline characters from a string. -/
def stripLineEnd (text : String) : String :=
  text.foldl
    (fun acc ch =>
      if ch == '\n' || ch == '\r' then
        acc
      else
        acc.push ch)
    ""

/-- Adds the correct target flags to a `stty` argument list. -/
def sttyArgs (target : SttyTarget) (args : Array String) : Array String :=
  match target with
  | SttyTarget.controllingTerminalBsd => #["-f", "/dev/tty"] ++ args
  | SttyTarget.controllingTerminalGnu => #["-F", "/dev/tty"] ++ args
  | SttyTarget.inheritedStdin => args

/-- Runs `stty` for a target and ignores its output. -/
def runSttyOn (target : SttyTarget) (args : Array String) : IO Unit := do
  let output ← IO.Process.output { cmd := "stty", args := sttyArgs target args }
  if output.exitCode == 0 then
    pure ()
  else
    throw (IO.userError output.stderr)

/-- Runs `stty` for a target and returns captured output on success. -/
def runSttyCaptureOn (target : SttyTarget) (args : Array String) : IO (Option String) := do
  try
    let output ← IO.Process.output { cmd := "stty", args := sttyArgs target args }
    if output.exitCode == 0 then
      pure (some output.stdout)
    else
      pure none
  catch _ =>
    pure none

/-- Targets to try when asking `stty` about the active terminal. -/
def sttyTargets : List SttyTarget :=
  [ SttyTarget.controllingTerminalBsd
  , SttyTarget.controllingTerminalGnu
  , SttyTarget.inheritedStdin
  ]

/-- Tries an `stty` operation on targets until one succeeds. -/
def firstStty? {α : Type} (op : SttyTarget -> IO (Option α)) : List SttyTarget ->
    IO (Option α)
  | [] => pure none
  | target :: rest => do
      match (← op target) with
      | some value => pure (some value)
      | none => firstStty? op rest

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
Tries BSD/macOS `/dev/tty`, GNU/Linux `/dev/tty`, then inherited standard input.
-/
def size? : IO (Option Size) := do
  firstStty? sizeOn? sttyTargets

/-- Captures the current terminal mode for one `stty` target. -/
def captureModeOn (target : SttyTarget) : IO (Option Mode) := do
  try
    let encoded ← IO.Process.run { cmd := "stty", args := sttyArgs target #["-g"] }
    pure (some { encoded := stripLineEnd encoded, target := target })
  catch _ =>
    pure none

/--
Captures the current terminal mode.
Tries BSD/macOS `/dev/tty`, GNU/Linux `/dev/tty`, then inherited standard input.
-/
def captureMode : IO (Option Mode) := do
  firstStty? captureModeOn sttyTargets

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

/-- Draws a full screen render and returns the number of characters written. -/
def drawFullMeasured (screen : Screen) : IO Nat := do
  let text := screen.renderFull
  let count ← writeMeasured text
  flush
  pure count

/-- Draws the ANSI diff needed to update one screen to another. -/
def drawDiff (old next : Screen) : IO Unit := do
  write (Screen.renderDiff old next)
  flush

/-- Draws a screen diff and returns the number of characters written. -/
def drawDiffMeasured (old next : Screen) : IO Nat := do
  let text := Screen.renderDiff old next
  let count ← writeMeasured text
  flush
  pure count

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

/-- Result of scanning an SGR mouse escape sequence. -/
inductive SgrMouseScan where
  /-- A complete payload, final event character, and remaining input. -/
  | complete (payload : List Char) (final : Char) (rest : List Char)
  /-- More input is needed before the mouse sequence can be decoded. -/
  | incomplete
  /-- The bytes are not a valid SGR mouse sequence. -/
  | invalid
deriving Nonempty

/-- Scans characters after `ESC [ <` until the SGR mouse final byte. -/
def scanSgrMouse (payload : List Char) : List Char -> SgrMouseScan
  | [] => SgrMouseScan.incomplete
  | 'M' :: rest => SgrMouseScan.complete payload.reverse 'M' rest
  | 'm' :: rest => SgrMouseScan.complete payload.reverse 'm' rest
  | ch :: rest =>
      if ch.isDigit || ch == ';' then
        scanSgrMouse (ch :: payload) rest
      else
        SgrMouseScan.invalid

/-- Parses an unsigned natural number from a list of decimal digits. -/
def parseDigitsNat? (chars : List Char) : Option Nat :=
  match chars with
  | [] => none
  | _ =>
      if chars.all Char.isDigit then
        String.toNat? (String.ofList chars)
      else
        none

/-- Splits a list of characters on semicolon separators. -/
def splitSemicolonLoop (current : List Char) (parts : List (List Char)) : List Char ->
    List (List Char)
  | [] => (current.reverse :: parts).reverse
  | ';' :: rest => splitSemicolonLoop [] (current.reverse :: parts) rest
  | ch :: rest => splitSemicolonLoop (ch :: current) parts rest

/-- Splits a list of characters on semicolon separators. -/
def splitSemicolon (chars : List Char) : List (List Char) :=
  splitSemicolonLoop [] [] chars

/-- Decodes an SGR mouse payload into a LeanTui mouse event. -/
def decodeSgrMouse? (payload : List Char) (final : Char) : Option MouseEvent :=
  match splitSemicolon payload with
  | [buttonChars, colChars, rowChars] =>
      match parseDigitsNat? buttonChars, parseDigitsNat? colChars, parseDigitsNat? rowChars with
      | some button, some col, some row =>
          if row == 0 || col == 0 then
            none
          else
            some { row := row - 1, col := col - 1, button := button, pressed := final == 'M' }
      | _, _, _ => none
  | _ => none

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
    match Key.fromControlChar? ch with
    | some key => key
    | none => Key.fromPrintableChar ch

/-- Parses one key from the front of a character buffer. -/
def parseOne : List Char -> ParseResult
  | [] => ParseResult.incomplete
  | ['\x1b'] => ParseResult.incomplete
  | '\x1b' :: '[' :: [] => ParseResult.incomplete
  | '\x1b' :: '[' :: '<' :: rest =>
      match scanSgrMouse [] rest with
      | SgrMouseScan.incomplete => ParseResult.incomplete
      | SgrMouseScan.invalid =>
          ParseResult.key (Key.unknown (String.ofList ['\x1b', '[', '<'])) rest
      | SgrMouseScan.complete payload final rest =>
          match decodeSgrMouse? payload final with
          | some event => ParseResult.key (Key.mouse event) rest
          | none => ParseResult.key (Key.unknown (String.ofList (['\x1b', '[', '<'] ++ payload ++ [final]))) rest
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
def parseBufferedFuel (keys : List Key) (chars : List Char) : Nat -> List Key × List Char
  | 0 => (keys.reverse, chars)
  | fuel + 1 =>
      match parseOne chars with
      | ParseResult.key key rest => parseBufferedFuel (key :: keys) rest fuel
      | ParseResult.incomplete => (keys.reverse, chars)

/-- Parses as many complete keys as possible from a character buffer. -/
def parseBuffered (keys : List Key) (chars : List Char) : List Key × List Char :=
  parseBufferedFuel keys chars (chars.length + 1)

/-- Finishes a buffered lone Escape key when no more input bytes are available. -/
def flushLoneEscape (keys : List Key) : List Char -> List Key × List Char
  | ['\x1b'] => (keys ++ [Key.escape], [])
  | rest => (keys, rest)

/-- Polls standard input and returns all complete keys currently available. -/
def pollKeys (reader : InputReader) : IO (List Key) := do
  let buffered ← reader.buffer.get
  let available ← readAvailable
  let (keys, rest) := parseBuffered [] (buffered ++ available)
  let (keys, rest) :=
    if available.isEmpty then
      flushLoneEscape keys rest
    else
      (keys, rest)
  reader.buffer.set rest
  pure keys

/-- Waits until a key is available and returns it. -/
partial def readKey (reader : InputReader) : IO Key := do
  match (← reader.pollKeys) with
  | key :: _ => pure key
  | [] =>
      IO.sleep 16
      readKey reader

/-- Waits until at least one key is available and returns the full available batch. -/
partial def readKeys (reader : InputReader) : IO (List Key) := do
  match (← reader.pollKeys) with
  | [] =>
      IO.sleep 16
      readKeys reader
  | keys => pure keys

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
