import LeanTui.Ansi

namespace LeanTui

inductive Color where
  | black
  | red
  | green
  | yellow
  | blue
  | magenta
  | cyan
  | white
deriving BEq, Repr

namespace Color

def fgCode : Color -> String
  | black => "30"
  | red => "31"
  | green => "32"
  | yellow => "33"
  | blue => "34"
  | magenta => "35"
  | cyan => "36"
  | white => "37"

def bgCode : Color -> String
  | black => "40"
  | red => "41"
  | green => "42"
  | yellow => "43"
  | blue => "44"
  | magenta => "45"
  | cyan => "46"
  | white => "47"

end Color

structure Style where
  fg : Option Color := none
  bg : Option Color := none
  bold : Bool := false
  inverse : Bool := false
deriving BEq, Repr

namespace Style

def plain : Style := {}

partial def joinWith (sep : String) : List String -> String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ joinWith sep xs

def codes (style : Style) : List String :=
  let codes := ([] : List String)
  let codes := if style.bold then "1" :: codes else codes
  let codes := if style.inverse then "7" :: codes else codes
  let codes :=
    match style.fg with
    | some color => color.fgCode :: codes
    | none => codes
  let codes :=
    match style.bg with
    | some color => color.bgCode :: codes
    | none => codes
  codes.reverse

def render (style : Style) : String :=
  match style.codes with
  | [] => Ansi.reset
  | codes => Ansi.csi (joinWith ";" codes ++ "m")

end Style

end LeanTui
