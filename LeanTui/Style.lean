import LeanTui.Ansi

namespace LeanTui

/-- ANSI terminal colors supported by LeanTui styles. -/
inductive Color where
  | black
  | red
  | green
  | yellow
  | blue
  | magenta
  | cyan
  | white
  | orange
deriving BEq, Repr

namespace Color

/-- Returns the ANSI foreground color code for a color. -/
def fgCode : Color -> String
  | black => "30"
  | red => "31"
  | green => "32"
  | yellow => "33"
  | blue => "34"
  | magenta => "35"
  | cyan => "36"
  | white => "37"
  | orange => "38;5;208"

/-- Returns the ANSI background color code for a color. -/
def bgCode : Color -> String
  | black => "40"
  | red => "41"
  | green => "42"
  | yellow => "43"
  | blue => "44"
  | magenta => "45"
  | cyan => "46"
  | white => "47"
  | orange => "48;5;208"

end Color

/-- Text styling attributes for a terminal cell. -/
structure Style where
  /-- Optional foreground color. -/
  fg : Option Color := none
  /-- Optional background color. -/
  bg : Option Color := none
  /-- Whether to render text using bold/intense styling. -/
  bold : Bool := false
  /-- Whether to render text with inverse foreground/background colors. -/
  inverse : Bool := false
deriving BEq, Repr

namespace Style

/-- The default style with no colors or extra attributes. -/
def plain : Style := {}

/-- Joins strings with a separator. -/
def joinWith (sep : String) : List String -> String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ joinWith sep xs

/-- Returns the ANSI SGR codes needed to render a style. -/
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

/-- Renders a style as an ANSI SGR escape sequence. -/
def render (style : Style) : String :=
  match style.codes with
  | [] => Ansi.reset
  | codes => Ansi.csi (joinWith ";" codes ++ "m")

end Style

end LeanTui
