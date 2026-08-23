import LeanTui
import LeanEffects.FileSystem
import LeanEffects.Clipboard
import LeanEffects.Sum
set_option doc.verso true
set_option doc.verso.module true
/-!
# Editor: A demo
A small text editor with VS Code-style navigation, selection, clipboard commands, and explicit Ctrl-S saving.
-/
open LeanTui
open LeanEffects

namespace Editor

  /-- A zero-based position in the document. -/
  structure TextPos where
    line : Nat
    col : Nat
  deriving BEq, Repr

  /-- Filesystem and clipboard capabilities used by the editor. -/
  abbrev EditorEffect := Sum FileSystem.Effect Clipboard.Effect

  /-- The state of the editor application. -/
  structure EditorState where
    path : String
    lines : Array String
    screenSize : Size
    cursor : TextPos := { line := 0, col := 0 }
    selectionAnchor : Option TextPos := none
    scrollTop : Nat := 0
    scrollLeft : Nat := 0
    dirty : Bool := false
    message : String := "Ctrl-S saves. Shift+arrows select. Ctrl-C/V copy/paste."

  def messageStyle : Style :=
    { fg := some Color.black, bg := some Color.yellow, bold := true }

  def titleStyle : Style :=
    { fg := some Color.cyan, bold := true }

  def cursorStyle : Style :=
    { fg := some Color.black, bg := some Color.orange, bold := true }

  def selectionStyle : Style :=
    { fg := some Color.black, bg := some Color.cyan, bold := false }

  /-- Lifts a filesystem command into the editor's combined effect signature. -/
  def withFileEffects (command : Command FileSystem.Effect α) : Command EditorEffect α :=
    command.hoist fun effect => Sum.left effect

  /-- Lifts a clipboard command into the editor's combined effect signature. -/
  def withClipboardEffects (command : Command Clipboard.Effect α) : Command EditorEffect α :=
    command.hoist fun effect => Sum.right effect

  /-- Loads a text file and splits it into lines. -/
  def readFileLines (path : String) : Command FileSystem.Effect (Array String) := do
    let content ← FileSystem.readFile path
    let lines := ((content.split '\n').map String.Slice.toString).toArray
    pure (if lines.isEmpty then #[""] else lines)

  def contentRows (size : Size) : Nat :=
    if size.rows <= 2 then 0 else size.rows - 2

  def lineLength (model : EditorState) (line : Nat) : Nat :=
    (model.lines.getD line "").toList.length

  def lastLine (model : EditorState) : Nat :=
    if model.lines.isEmpty then 0 else model.lines.size - 1

  def clampPosition (model : EditorState) (pos : TextPos) : TextPos :=
    let line := min pos.line (lastLine model)
    { line := line, col := min pos.col (lineLength model line) }

  def positionLt (left right : TextPos) : Bool :=
    left.line < right.line || (left.line == right.line && left.col < right.col)

  def positionLe (left right : TextPos) : Bool :=
    left == right || positionLt left right

  /-- Returns the ordered, nonempty selected range. -/
  def selectionRange? (model : EditorState) : Option (TextPos × TextPos) :=
    match model.selectionAnchor with
    | none => none
    | some anchor =>
        if anchor == model.cursor then none
        else if positionLt anchor model.cursor then some (anchor, model.cursor)
        else some (model.cursor, anchor)

  def positionSelected (model : EditorState) (pos : TextPos) : Bool :=
    match selectionRange? model with
    | none => false
    | some (startPos, endPos) => positionLe startPos pos && positionLt pos endPos

  /-- Keeps the document cursor visible in both viewport dimensions. -/
  def ensureVisible (model : EditorState) : EditorState :=
    let cursor := clampPosition model model.cursor
    let rows := contentRows model.screenSize
    let scrollTop :=
      if rows == 0 then 0
      else if cursor.line < model.scrollTop then cursor.line
      else if cursor.line >= model.scrollTop + rows then cursor.line - rows + 1
      else model.scrollTop
    let cols := model.screenSize.cols
    let scrollLeft :=
      if cols == 0 then 0
      else if cursor.col < model.scrollLeft then cursor.col
      else if cursor.col >= model.scrollLeft + cols then cursor.col - cols + 1
      else model.scrollLeft
    { model with cursor := cursor, scrollTop := scrollTop, scrollLeft := scrollLeft }

  def setMessage (model : EditorState) (message : String) : EditorState :=
    { model with message := message }

  /-- Moves the cursor, optionally extending the current selection. -/
  def moveTo (model : EditorState) (pos : TextPos) (selecting : Bool := false) : EditorState :=
    let selectionAnchor :=
      if selecting then some (model.selectionAnchor.getD model.cursor) else none
    ensureVisible { model with cursor := clampPosition model pos, selectionAnchor := selectionAnchor }

  def moveLeft (model : EditorState) (selecting : Bool := false) : EditorState :=
    if !selecting then
      match selectionRange? model with
      | some (startPos, _) => moveTo model startPos
      | none => moveNormally
    else moveNormally
  where
    moveNormally : EditorState :=
      let cursor := model.cursor
      let next :=
        if cursor.col > 0 then { cursor with col := cursor.col - 1 }
        else if cursor.line > 0 then
          let line := cursor.line - 1
          { line := line, col := lineLength model line }
        else cursor
      moveTo model next selecting

  def moveRight (model : EditorState) (selecting : Bool := false) : EditorState :=
    if !selecting then
      match selectionRange? model with
      | some (_, endPos) => moveTo model endPos
      | none => moveNormally
    else moveNormally
  where
    moveNormally : EditorState :=
      let cursor := model.cursor
      let next :=
        if cursor.col < lineLength model cursor.line then { cursor with col := cursor.col + 1 }
        else if cursor.line < lastLine model then { line := cursor.line + 1, col := 0 }
        else cursor
      moveTo model next selecting

  def moveVertical (model : EditorState) (line : Nat) (selecting : Bool := false) : EditorState :=
    let targetLine := min line (lastLine model)
    moveTo model { line := targetLine, col := min model.cursor.col (lineLength model targetLine) } selecting

  def movePageUp (model : EditorState) (selecting : Bool := false) : EditorState :=
    let amount := max 1 (contentRows model.screenSize)
    moveVertical model (model.cursor.line - amount) selecting

  def movePageDown (model : EditorState) (selecting : Bool := false) : EditorState :=
    let amount := max 1 (contentRows model.screenSize)
    moveVertical model (model.cursor.line + amount) selecting

  def stringTake (text : String) (count : Nat) : String :=
    String.ofList (text.toList.take count)

  def stringDrop (text : String) (count : Nat) : String :=
    String.ofList (text.toList.drop count)

  def stringSlice (text : String) (startPos endPos : Nat) : String :=
    String.ofList ((text.toList.drop startPos).take (endPos - startPos))

  def replaceLineRange
      (lines : Array String) (startLine endLine : Nat) (replacement : List String) : Array String :=
    (lines.toList.take startLine ++ replacement ++ lines.toList.drop endLine).toArray

  /-- Returns the selected text, retaining newlines in a multiline selection. -/
  def selectedText (model : EditorState) : String :=
    match selectionRange? model with
    | none => ""
    | some (startPos, endPos) =>
        if startPos.line == endPos.line then
          stringSlice (model.lines.getD startPos.line "") startPos.col endPos.col
        else
          let first := stringDrop (model.lines.getD startPos.line "") startPos.col
          let middle := (model.lines.toList.drop (startPos.line + 1)).take (endPos.line - startPos.line - 1)
          let last := stringTake (model.lines.getD endPos.line "") endPos.col
          String.intercalate "\n" (first :: middle ++ [last])

  /-- Deletes the selection and places the cursor at its beginning. -/
  def deleteSelection (model : EditorState) : EditorState :=
    match selectionRange? model with
    | none => model
    | some (startPos, endPos) =>
        let startLine := model.lines.getD startPos.line ""
        let endLine := model.lines.getD endPos.line ""
        let replacement := stringTake startLine startPos.col ++ stringDrop endLine endPos.col
        let lines := replaceLineRange model.lines startPos.line (endPos.line + 1) [replacement]
        ensureVisible
          { model with lines := lines, cursor := startPos, selectionAnchor := none, dirty := true }

  /-- Inserts text at the cursor, replacing the selection and supporting embedded newlines. -/
  def insertText (text : String) (model : EditorState) : EditorState :=
    if text.isEmpty then model
    else
      let model := deleteSelection model
      let cursor := model.cursor
      let line := model.lines.getD cursor.line ""
      let linePrefix := stringTake line cursor.col
      let suffix := stringDrop line cursor.col
      let pieces := ((text.split '\n').map String.Slice.toString).toArray.toList
      match pieces with
      | [] => model
      | [piece] =>
          let lines := model.lines.set! cursor.line (linePrefix ++ piece ++ suffix)
          let nextCursor : TextPos :=
            { line := cursor.line, col := cursor.col + piece.toList.length }
          ensureVisible
            { model with lines := lines, cursor := nextCursor, selectionAnchor := none, dirty := true }
      | first :: rest =>
          let last := rest.getLast!
          let middle := rest.dropLast
          let replacement := (linePrefix ++ first) :: middle ++ [last ++ suffix]
          let lines := replaceLineRange model.lines cursor.line (cursor.line + 1) replacement
          let nextCursor : TextPos :=
            { line := cursor.line + replacement.length - 1, col := last.toList.length }
          ensureVisible
            { model with lines := lines, cursor := nextCursor, selectionAnchor := none, dirty := true }

  def backspace (model : EditorState) : EditorState :=
    if (selectionRange? model).isSome then
      setMessage (deleteSelection model) "Deleted selection."
    else if model.cursor.col > 0 then
      let line := model.lines.getD model.cursor.line ""
      let col := model.cursor.col
      let replacement := stringTake line (col - 1) ++ stringDrop line col
      let lines := model.lines.set! model.cursor.line replacement
      let nextCursor : TextPos := { line := model.cursor.line, col := col - 1 }
      setMessage (ensureVisible { model with lines := lines, cursor := nextCursor, dirty := true }) "Backspace."
    else if model.cursor.line > 0 then
      let previousLine := model.cursor.line - 1
      let previous := model.lines.getD previousLine ""
      let current := model.lines.getD model.cursor.line ""
      let lines := replaceLineRange model.lines previousLine (model.cursor.line + 1) [previous ++ current]
      let nextCursor : TextPos := { line := previousLine, col := previous.toList.length }
      setMessage (ensureVisible { model with lines := lines, cursor := nextCursor, dirty := true }) "Joined lines."
    else model

  /-- Deletes the character in front of the cursor, or joins the next line at end-of-line. -/
  def deleteForward (model : EditorState) : EditorState :=
    if (selectionRange? model).isSome then
      setMessage (deleteSelection model) "Deleted selection."
    else
      let line := model.lines.getD model.cursor.line ""
      let chars := line.toList
      if model.cursor.col < chars.length then
        let deleted := chars.getD model.cursor.col ' '
        let replacement := stringTake line model.cursor.col ++ stringDrop line (model.cursor.col + 1)
        let lines := model.lines.set! model.cursor.line replacement
        setMessage { model with lines := lines, dirty := true }
          ("Deleted " ++ reprStr deleted ++ ".")
      else if model.cursor.line < lastLine model then
        let next := model.lines.getD (model.cursor.line + 1) ""
        let lines := replaceLineRange model.lines model.cursor.line (model.cursor.line + 2) [line ++ next]
        setMessage { model with lines := lines, dirty := true } "Joined next line."
      else model

  def selectAll (model : EditorState) : EditorState :=
    let endLine := lastLine model
    let anchor : TextPos := { line := 0, col := 0 }
    let cursor : TextPos := { line := endLine, col := lineLength model endLine }
    ensureVisible { model with selectionAnchor := some anchor, cursor := cursor, message := "Selected all." }

  def insertTab (model : EditorState) : EditorState :=
    let count := 4 - model.cursor.col % 4
    setMessage (insertText (String.ofList (List.replicate count ' ')) model) "Tab."

  /-- Handles editing and navigation commands that do not request effects. -/
  def updatePure (key : Key) (model : EditorState) : Step EditorState :=
    match key with
    | .ctrl 'l' => Step.redrawFull model
    | .escape => Step.quit (setMessage model "Goodbye.")
    | _ =>
      let next :=
        match key with
        | .ctrl 'a' => selectAll model
        | .left => moveLeft model
        | .right => moveRight model
        | .up => moveVertical model (model.cursor.line - 1)
        | .down => moveVertical model (model.cursor.line + 1)
        | .home => moveTo model { model.cursor with col := 0 }
        | .endKey => moveTo model { model.cursor with col := lineLength model model.cursor.line }
        | .pageUp => movePageUp model
        | .pageDown => movePageDown model
        | .shiftLeft => moveLeft model true
        | .shiftRight => moveRight model true
        | .shiftUp => moveVertical model (model.cursor.line - 1) true
        | .shiftDown => moveVertical model (model.cursor.line + 1) true
        | .shiftHome => moveTo model { model.cursor with col := 0 } true
        | .shiftEnd => moveTo model { model.cursor with col := lineLength model model.cursor.line } true
        | .shiftPageUp => movePageUp model true
        | .shiftPageDown => movePageDown model true
        | .enter => setMessage (insertText "\n" model) "New line."
        | .tab => insertTab model
        | .backspace => backspace model
        | .delete => deleteForward model
        | other =>
            match other.toInputChar? with
            | some ch => setMessage (insertText (String.singleton ch) model) ("Typed " ++ reprStr ch ++ ".")
            | none => setMessage model ("Unhandled key: " ++ reprStr other)
      Step.continue next

  /-- Writes the edited lines to the configured path. -/
  def saveFile (state : EditorState) : Command EditorEffect Unit :=
    withFileEffects (FileSystem.writeFile state.path (String.intercalate "\n" state.lines.toList))

  /-- Handles effectful save and clipboard commands around the pure editor rules. -/
  def updateEditor (key : Key) (model : EditorState) : Command EditorEffect (Step EditorState) := do
    match key with
    | .ctrl 's' =>
        saveFile model
        pure (.continue (setMessage { model with dirty := false } "Saved."))
    | .ctrl 'c' =>
        match selectionRange? model with
        | none => pure (.continue (setMessage model "Nothing selected."))
        | some _ =>
            withClipboardEffects (Clipboard.setText (selectedText model))
            pure (.continue (setMessage model "Copied."))
    | .ctrl 'x' =>
        match selectionRange? model with
        | none => pure (.continue (setMessage model "Nothing selected."))
        | some _ =>
            withClipboardEffects (Clipboard.setText (selectedText model))
            pure (.continue (setMessage (deleteSelection model) "Cut."))
    | .ctrl 'v' =>
        let text ← withClipboardEffects Clipboard.getText
        pure (.continue (setMessage (insertText text model) "Pasted."))
    | _ => pure (updatePure key model)

  def renderLineChars
      (model : EditorState) (screen : Screen) (documentLine screenRow screenCol fuel : Nat) : Screen :=
    match fuel with
    | 0 => screen
    | fuel + 1 =>
        if screenCol >= model.screenSize.cols then screen
        else
          let documentCol := model.scrollLeft + screenCol
          let chars := (model.lines.getD documentLine "").toList
          let ch := chars.getD documentCol ' '
          let style := if positionSelected model { line := documentLine, col := documentCol }
            then selectionStyle else Style.plain
          renderLineChars model (screen.putChar screenRow screenCol ch style)
            documentLine screenRow (screenCol + 1) fuel

  def renderRows (model : EditorState) (screen : Screen) (screenRow fuel : Nat) : Screen :=
    match fuel with
    | 0 => screen
    | fuel + 1 =>
        if screenRow >= model.screenSize.rows then screen
        else
          let documentLine := model.scrollTop + screenRow - 2
          let screen := renderLineChars model screen documentLine screenRow 0 (model.screenSize.cols + 1)
          renderRows model screen (screenRow + 1) fuel

  /-- Renders the document, selection, status, and cursor into a screen buffer. -/
  def viewRule (model : EditorState) : Screen :=
    let screen := Screen.blank model.screenSize
    let dirtyMark := if model.dirty then " *" else ""
    let screen := screen.putString 0 0 ("Editor Demo — Ctrl-S saves, Esc quits" ++ dirtyMark) titleStyle
    let screen := screen.putString 1 0 model.message messageStyle
    let screen := renderRows model screen 2 (contentRows model.screenSize + 1)
    if model.screenSize.rows <= 2 || model.screenSize.cols == 0 ||
        model.cursor.line < model.scrollTop ||
        model.cursor.line >= model.scrollTop + contentRows model.screenSize then screen
    else
      let cursorRow := 2 + model.cursor.line - model.scrollTop
      let cursorCol := model.cursor.col - model.scrollLeft
      let pos : Pos := { row := cursorRow, col := cursorCol }
      let cell := screen.get pos
      screen.set pos { cell with style := cursorStyle }

  def resizeEditor (size : Size) (model : EditorState) : EditorState :=
    ensureVisible { model with screenSize := size }

  def initModel (path : String) (size : Size) (fileLines : Array String) : EditorState :=
    let lines := if fileLines.isEmpty then #[""] else fileLines
    ensureVisible { path := path, lines := lines, screenSize := size }

  def editorApp (size : Size) (path : String) (fileLines : Array String) :
      App EditorState EditorEffect :=
    { init := initModel path size fileLines
      view := viewRule
      update := updateEditor
      resize := resizeEditor }

  /-- Loads a file, creates the clipboard interpreter, and runs the editor. -/
  def main (cmdLineArgs : List String) : IO Unit := do
    let size := (← Terminal.size?).getD { rows := 12, cols := 72 }
    match cmdLineArgs with
    | [path] =>
        let fileLines ← (readFileLines path).run FileSystem.interpret
        let clipboard ← Clipboard.Store.create
        let interpret : Command.Interpreter EditorEffect :=
          Sum.interpret FileSystem.interpret clipboard.interpret
        App.run interpret (editorApp size path fileLines)
    | _ => IO.Process.exit 1

end Editor
