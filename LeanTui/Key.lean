namespace LeanTui

/-- A decoded terminal mouse event position and button payload. -/
structure MouseEvent where
  /-- Zero-based terminal row. -/
  row : Nat
  /-- Zero-based terminal column. -/
  col : Nat
  /-- Raw terminal mouse button/modifier code. -/
  button : Nat
  /-- Whether this event is a button press rather than a release. -/
  pressed : Bool
deriving BEq, Repr

/-- A decoded terminal key press. -/
inductive Key where
  /-- A printable character key. -/
  | char (ch : Char)
  /-- A Ctrl-modified letter key, stored as a lowercase character. -/
  | ctrl (ch : Char)
  /-- The Enter or Return key. -/
  | enter
  /-- The Tab key. -/
  | tab
  /-- Backspace or Delete-as-backspace. -/
  | backspace
  /-- The Escape key. -/
  | escape
  /-- Up arrow. -/
  | up
  /-- Down arrow. -/
  | down
  /-- Right arrow. -/
  | right
  /-- Left arrow. -/
  | left
  /-- Home key. -/
  | home
  /-- End key. -/
  | endKey
  /-- Forward delete key. -/
  | delete
  /-- A mouse event decoded from an SGR mouse escape sequence. -/
  | mouse (event : MouseEvent)
  /-- An unrecognized escape sequence or byte sequence. -/
  | unknown (bytes : String)
deriving BEq, Repr

namespace Key

/-- Returns whether a character is printable ASCII. -/
def isPrintableAscii (ch : Char) : Bool :=
  ' '.toNat <= ch.toNat && ch.toNat <= '~'.toNat

/-- Converts a control-byte value from Ctrl-A through Ctrl-Z into its letter. -/
def controlLetter? (value : Nat) : Option Char :=
  if 1 <= value && value <= 26 then
    some (Char.ofNat ('a'.toNat + value - 1))
  else
    none

/-- Converts an ASCII control character into a Ctrl-modified key when possible. -/
def fromControlChar? (ch : Char) : Option Key :=
  match controlLetter? ch.toNat with
  | some letter => some (ctrl letter)
  | none => none

/-- Converts an ordinary typed character into a key. -/
def fromPrintableChar (ch : Char) : Key :=
  if isPrintableAscii ch then
    char ch
  else
    unknown (String.singleton ch)

/-- Returns the character inserted by a printable key. -/
def toInputChar? : Key -> Option Char
  | char ch => some ch
  | _ => none

/-- Decodes a single command character into a key. -/
def fromCommandChar : Char -> Key
  | '\n' => enter
  | '\t' => tab
  | ch =>
      match fromControlChar? ch with
      | some key => key
      | none => fromPrintableChar ch

/-- Decodes a complete escape sequence into a key. -/
def decodeEscape : List Char -> Key
  | ['\x1b'] => escape
  | ['\x1b', '[', 'A'] => up
  | ['\x1b', '[', 'B'] => down
  | ['\x1b', '[', 'C'] => right
  | ['\x1b', '[', 'D'] => left
  | ['\x1b', '[', 'H'] => home
  | ['\x1b', '[', 'F'] => endKey
  | ['\x1b', '[', '1', '~'] => home
  | ['\x1b', '[', '4', '~'] => endKey
  | ['\x1b', 'O', 'H'] => home
  | ['\x1b', 'O', 'F'] => endKey
  | ['\x1b', '[', '3', '~'] => delete
  | chars => unknown (String.ofList chars)

/-- Decodes a complete input string into a single key. -/
def decode (input : String) : Key :=
  match input.toList with
  | [] => enter
  | [ch] => fromCommandChar ch
  | chars => decodeEscape chars

end Key

end LeanTui
