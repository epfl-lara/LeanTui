namespace LeanTui

/-- A decoded terminal key press. -/
inductive Key where
  /-- A printable character key. -/
  | char (ch : Char)
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
  /-- An unrecognized escape sequence or byte sequence. -/
  | unknown (bytes : String)
deriving BEq, Repr

namespace Key

/-- Decodes a single command character into a key. -/
def fromCommandChar : Char -> Key
  | '\n' => enter
  | '\t' => tab
  | ch => char ch

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
