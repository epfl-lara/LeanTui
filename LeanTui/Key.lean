namespace LeanTui

inductive Key where
  | char (ch : Char)
  | enter
  | tab
  | backspace
  | escape
  | up
  | down
  | right
  | left
  | home
  | endKey
  | delete
  | unknown (bytes : String)
deriving BEq, Repr

namespace Key

def fromCommandChar : Char -> Key
  | '\n' => enter
  | '\t' => tab
  | ch => char ch

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

def decode (input : String) : Key :=
  match input.toList with
  | [] => enter
  | [ch] => fromCommandChar ch
  | chars => decodeEscape chars

end Key

end LeanTui
