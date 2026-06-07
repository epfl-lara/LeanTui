import LeanTui.Screen
import LeanTui.Key

namespace LeanTui

namespace Terminal

inductive SttyTarget where
  | controllingTerminal
  | inheritedStdin
deriving BEq, Repr

structure Mode where
  encoded : String
  target : SttyTarget
deriving Repr

def write (text : String) : IO Unit :=
  IO.print text

def flush : IO Unit := do
  (← IO.getStdout).flush

def stripLineEnd (text : String) : String :=
  String.ofList (text.toList.filter (fun ch => ch != '\n' && ch != '\r'))

def sttyArgs (target : SttyTarget) (args : Array String) : Array String :=
  match target with
  | SttyTarget.controllingTerminal => #["-f", "/dev/tty"] ++ args
  | SttyTarget.inheritedStdin => args

def runSttyOn (target : SttyTarget) (args : Array String) : IO Unit := do
  let _ ← IO.Process.output { cmd := "stty", args := sttyArgs target args }
  pure ()

def captureModeOn (target : SttyTarget) : IO (Option Mode) := do
  try
    let encoded ← IO.Process.run { cmd := "stty", args := sttyArgs target #["-g"] }
    pure (some { encoded := stripLineEnd encoded, target := target })
  catch _ =>
    pure none

def captureMode : IO (Option Mode) := do
  match (← captureModeOn SttyTarget.controllingTerminal) with
  | some mode => pure (some mode)
  | none => captureModeOn SttyTarget.inheritedStdin

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

def restore (mode? : Option Mode) : IO Unit := do
  match mode? with
  | none => pure ()
  | some mode =>
      try
        runSttyOn mode.target #[mode.encoded]
      catch _ =>
        pure ()

def enter : IO Unit := do
  write (Ansi.enterAlternateScreen ++ Ansi.hideCursor ++ Ansi.clearScreen)
  flush

def leave : IO Unit := do
  write (Ansi.reset ++ Ansi.showCursor ++ Ansi.leaveAlternateScreen)
  flush

def drawFull (screen : Screen) : IO Unit := do
  write screen.renderFull
  flush

def drawDiff (old next : Screen) : IO Unit := do
  write (Screen.renderDiff old next)
  flush

def readKeyLine : IO Key := do
  let line ← (← IO.getStdin).getLine
  pure (Key.decode (stripLineEnd line))

def byteToChar (byte : UInt8) : Char :=
  Char.ofNat byte.toNat

inductive ParseResult where
  | key (key : Key) (rest : List Char)
  | incomplete

def isBackspaceByte (ch : Char) : Bool :=
  ch == '\x7f' || ch == '\x08'

def keyFromChar (ch : Char) : Key :=
  if ch == '\r' || ch == '\n' then
    Key.enter
  else if ch == '\t' then
    Key.tab
  else if isBackspaceByte ch then
    Key.backspace
  else
    Key.char ch

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

structure InputReader where
  buffer : IO.Ref (List Char)

namespace InputReader

def readAvailable : IO (List Char) := do
  let bytes ← (← IO.getStdin).read 64
  pure (bytes.toList.map byteToChar)

partial def parseBuffered (keys : List Key) : List Char -> List Key × List Char
  | chars =>
      match parseOne chars with
      | ParseResult.key key rest => parseBuffered (key :: keys) rest
      | ParseResult.incomplete => (keys.reverse, chars)

def pollKeys (reader : InputReader) : IO (List Key) := do
  let buffered ← reader.buffer.get
  let available ← readAvailable
  let (keys, rest) := parseBuffered [] (buffered ++ available)
  reader.buffer.set rest
  pure keys

partial def readKey (reader : InputReader) : IO Key := do
  match (← reader.pollKeys) with
  | key :: _ => pure key
  | [] =>
      IO.sleep 1
      readKey reader

end InputReader

def mkInputReader : IO InputReader := do
  pure { buffer := (← IO.mkRef []) }

def readKeyRaw : IO Key := do
  let reader ← mkInputReader
  reader.readKey

end Terminal

end LeanTui
