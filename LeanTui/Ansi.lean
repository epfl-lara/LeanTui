namespace LeanTui

namespace Ansi

def esc : String := "\x1b["

def csi (code : String) : String :=
  esc ++ code

def clearScreen : String :=
  csi "2J"

def clearLine : String :=
  csi "2K"

def hideCursor : String :=
  csi "?25l"

def showCursor : String :=
  csi "?25h"

def enterAlternateScreen : String :=
  csi "?1049h"

def leaveAlternateScreen : String :=
  csi "?1049l"

def reset : String :=
  csi "0m"

def moveCursor (row col : Nat) : String :=
  csi (toString (row + 1) ++ ";" ++ toString (col + 1) ++ "H")

def setCursor (row col : Nat) : IO Unit :=
  IO.print (moveCursor row col)

end Ansi

end LeanTui
