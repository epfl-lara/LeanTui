namespace LeanTui

namespace Ansi

/-- The Control Sequence Introducer prefix used by ANSI escape sequences. -/
def esc : String := "\x1b["

/-- Builds an ANSI CSI escape sequence from a terminal command suffix. -/
def csi (code : String) : String :=
  esc ++ code

/-- ANSI sequence that clears the entire screen. -/
def clearScreen : String :=
  csi "2J"

/-- ANSI sequence that clears the current line. -/
def clearLine : String :=
  csi "2K"

/-- ANSI sequence that hides the terminal cursor. -/
def hideCursor : String :=
  csi "?25l"

/-- ANSI sequence that shows the terminal cursor. -/
def showCursor : String :=
  csi "?25h"

/-- ANSI sequence that switches to the alternate terminal screen buffer. -/
def enterAlternateScreen : String :=
  csi "?1049h"

/-- ANSI sequence that returns from the alternate terminal screen buffer. -/
def leaveAlternateScreen : String :=
  csi "?1049l"

/-- ANSI sequence that disables automatic wrapping at the right screen edge. -/
def disableAutoWrap : String :=
  csi "?7l"

/-- ANSI sequence that enables automatic wrapping at the right screen edge. -/
def enableAutoWrap : String :=
  csi "?7h"

/-- ANSI sequence that enables basic mouse press reporting. -/
def enableMouseTracking : String :=
  csi "?1000h"

/-- ANSI sequence that disables basic mouse press reporting. -/
def disableMouseTracking : String :=
  csi "?1000l"

/-- ANSI sequence that enables SGR mouse coordinate encoding. -/
def enableSgrMouse : String :=
  csi "?1006h"

/-- ANSI sequence that disables SGR mouse coordinate encoding. -/
def disableSgrMouse : String :=
  csi "?1006l"

/-- ANSI sequence that resets text style and colors. -/
def reset : String :=
  csi "0m"

/--
Builds an ANSI sequence that moves the cursor to a zero-based row and column.
-/
def moveCursor (row col : Nat) : String :=
  csi (toString (row + 1) ++ ";" ++ toString (col + 1) ++ "H")

/-- Writes a cursor movement sequence for a zero-based row and column. -/
def setCursor (row col : Nat) : IO Unit :=
  IO.print (moveCursor row col)

end Ansi

end LeanTui
