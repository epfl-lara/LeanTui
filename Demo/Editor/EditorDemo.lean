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

  /-- A reversible text splice plus the editor state needed to restore either side. -/
  structure EditDiff where
    startPos : TextPos
    removedText : String
    insertedText : String
    cursorBefore : TextPos
    selectionBefore : Option TextPos
    cursorAfter : TextPos
    selectionAfter : Option TextPos
    revisionBefore : Nat
    revisionAfter : Nat
  deriving Repr

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
    revision : Nat := 0
    savedRevision : Nat := 0
    nextRevision : Nat := 1
    undoStack : List EditDiff := []
    redoStack : List EditDiff := []
    message : String := "Ctrl-S saves. Shift+arrows select. Ctrl-C/V copy/paste."
    /-- Whether the terminal supports truecolor, enabling the light current-line highlight. -/
    richColor : Bool := true

  /-- Shared full-width style for the help header and status footer. -/
  def chromeStyle : Style :=
    { fg := some Color.white, bg := some Color.blue, bold := true }

  def cursorStyle : Style :=
    { fg := some Color.black, bg := some Color.orange, bold := true }

  def selectionStyle : Style :=
    { fg := some Color.black, bg := some Color.cyan, bold := false }

  /-- Soft light highlight for the line containing the cursor (truecolor terminals only). -/
  def currentLineStyle : Style :=
    { fg := some Color.black, bg := some (Color.rgb 255 246 178), bold := false }

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

  /-- Returns the final path component for display in the status line. -/
  def fileName (path : String) : String :=
    let name := (path.toList.reverse.takeWhile fun ch => ch != '/' && ch != '\\').reverse
    if name.isEmpty then path else String.ofList name

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

  /-- Returns whether a character starts or belongs to a word (non-whitespace). -/
  def isWordChar (ch : Char) : Bool :=
    !ch.isWhitespace

  /--
  Scans `chars` from reversed-index `i` while `p` holds, returning the index
  just past the last satisfying character (or `chars.length`).
  -/
  def skipWhile : List Char -> Nat -> (Char -> Bool) -> Nat
    | [], i, _ => i
    | c :: rest, i, p =>
        if p c then skipWhile rest (i + 1) p else i

  /--
  Returns the column of the first character of the next word at or after `col`,
  or `none` when the line has no further word. Whitespace separates words.
  -/
  def nextWordStart? (text : String) (col : Nat) : Option Nat :=
    let chars := text.toList
    let len := chars.length
    if col >= len then none
    else
      -- Past the rest of the current word if we start inside one.
      let afterCurrent :=
        if (chars.getD col ' ').isWhitespace then col
        else skipWhile (chars.drop col) (col + 1) isWordChar
      let afterGap := skipWhile (chars.drop afterCurrent) afterCurrent (fun ch => ch.isWhitespace)
      if afterGap < len then some afterGap else none

  /--
  Returns the column where the word ending just before `col` begins,
  or `none` when nothing but whitespace precedes `col`. Whitespace separates words.
  -/
  def prevWordStart? (text : String) (col : Nat) : Option Nat :=
    if col == 0 then none
    else
      let reversedPrefix := (text.toList.take col).reverse
      let trailingWs := skipWhile reversedPrefix 0 (fun ch => ch.isWhitespace)
      let wordRun := skipWhile (reversedPrefix.drop trailingWs) trailingWs isWordChar
      if wordRun == trailingWs then none
      else some (col - wordRun)

  /-- Moves the cursor to the start of the next word, or the next line at a boundary. -/
  def moveWordRight (model : EditorState) (selecting : Bool := false) : EditorState :=
    let cursor := model.cursor
    let atLineEnd := cursor.col >= lineLength model cursor.line
    let target : TextPos :=
      if atLineEnd then
        if cursor.line < lastLine model then { line := cursor.line + 1, col := 0 } else cursor
      else
        match nextWordStart? (model.lines.getD cursor.line "") cursor.col with
        | some col => { cursor with col := col }
        | none =>
            if cursor.line < lastLine model then { line := cursor.line + 1, col := 0 } else cursor
    moveTo model target selecting

  /-- Moves the cursor to the start of the current or previous word, or the previous line at a boundary. -/
  def moveWordLeft (model : EditorState) (selecting : Bool := false) : EditorState :=
    let cursor := model.cursor
    let target : TextPos :=
      if cursor.col == 0 then
        if cursor.line > 0 then { line := cursor.line - 1, col := lineLength model (cursor.line - 1) } else cursor
      else
        match prevWordStart? (model.lines.getD cursor.line "") cursor.col with
        | some col => { cursor with col := col }
        | none =>
            if cursor.line > 0 then { line := cursor.line - 1, col := lineLength model (cursor.line - 1) } else cursor
    moveTo model target selecting

  def stringTake (text : String) (count : Nat) : String :=
    String.ofList (text.toList.take count)

  def stringDrop (text : String) (count : Nat) : String :=
    String.ofList (text.toList.drop count)

  def stringSlice (text : String) (startPos endPos : Nat) : String :=
    String.ofList ((text.toList.drop startPos).take (endPos - startPos))

  def replaceLineRange
      (lines : Array String) (startLine endLine : Nat) (replacement : List String) : Array String :=
    (lines.toList.take startLine ++ replacement ++ lines.toList.drop endLine).toArray

  def splitText (text : String) : List String :=
    ((text.split '\n').map String.Slice.toString).toArray.toList

  /-- Returns the text in a half-open document range. -/
  def textInRange (model : EditorState) (startPos endPos : TextPos) : String :=
    if startPos.line == endPos.line then
      stringSlice (model.lines.getD startPos.line "") startPos.col endPos.col
    else
      let first := stringDrop (model.lines.getD startPos.line "") startPos.col
      let middle := (model.lines.toList.drop (startPos.line + 1)).take (endPos.line - startPos.line - 1)
      let last := stringTake (model.lines.getD endPos.line "") endPos.col
      String.intercalate "\n" (first :: middle ++ [last])

  /-- Returns the selected text, retaining newlines in a multiline selection. -/
  def selectedText (model : EditorState) : String :=
    match selectionRange? model with
    | none => ""
    | some (startPos, endPos) => textInRange model startPos endPos

  /-- Computes the end position of text inserted at a document position. -/
  def endAfterText (startPos : TextPos) (text : String) : TextPos :=
    match splitText text with
    | [] => startPos
    | [piece] => { startPos with col := startPos.col + piece.toList.length }
    | _ :: rest =>
        { line := startPos.line + rest.length, col := rest.getLast!.toList.length }

  /-- Applies a text splice without adding it to either history stack. -/
  def replaceRangeRaw
      (model : EditorState) (startPos endPos : TextPos) (insertedText : String) : EditorState :=
    let startLine := model.lines.getD startPos.line ""
    let endLine := model.lines.getD endPos.line ""
    let linePrefix := stringTake startLine startPos.col
    let suffix := stringDrop endLine endPos.col
    match splitText insertedText with
    | [] => model
    | [piece] =>
        let lines := replaceLineRange model.lines startPos.line (endPos.line + 1)
          [linePrefix ++ piece ++ suffix]
        let cursor := endAfterText startPos insertedText
        ensureVisible { model with lines := lines, cursor := cursor, selectionAnchor := none }
    | first :: rest =>
        let last := rest.getLast!
        let middle := rest.dropLast
        let replacement := (linePrefix ++ first) :: middle ++ [last ++ suffix]
        let lines := replaceLineRange model.lines startPos.line (endPos.line + 1) replacement
        let cursor := endAfterText startPos insertedText
        ensureVisible { model with lines := lines, cursor := cursor, selectionAnchor := none }

  /-- Records and applies one reversible text splice. -/
  def commitEdit
      (model : EditorState) (startPos endPos : TextPos) (insertedText : String) : EditorState :=
    let removedText := textInRange model startPos endPos
    if removedText == insertedText then model
    else
      let revisionAfter := model.nextRevision
      let after := replaceRangeRaw model startPos endPos insertedText
      let diff : EditDiff :=
        { startPos := startPos
          removedText := removedText
          insertedText := insertedText
          cursorBefore := model.cursor
          selectionBefore := model.selectionAnchor
          cursorAfter := after.cursor
          selectionAfter := after.selectionAnchor
          revisionBefore := model.revision
          revisionAfter := revisionAfter }
      let nextRevision := revisionAfter + 1
      let undoStack := diff :: model.undoStack
      let dirty := revisionAfter != model.savedRevision
      let after := { after with revision := revisionAfter }
      let after := { after with nextRevision := nextRevision }
      let after := { after with undoStack := undoStack }
      let after := { after with redoStack := [] }
      { after with dirty := dirty }

  /-- Deletes the selection and places the cursor at its beginning. -/
  def deleteSelection (model : EditorState) : EditorState :=
    match selectionRange? model with
    | none => model
    | some (startPos, endPos) => commitEdit model startPos endPos ""

  /-- Inserts text at the cursor, replacing the selection and supporting embedded newlines. -/
  def insertText (text : String) (model : EditorState) : EditorState :=
    if text.isEmpty then model
    else
      match selectionRange? model with
      | some (startPos, endPos) => commitEdit model startPos endPos text
      | none => commitEdit model model.cursor model.cursor text

  def backspace (model : EditorState) : EditorState :=
    if (selectionRange? model).isSome then
      setMessage (deleteSelection model) "Deleted selection."
    else if model.cursor.col > 0 then
      let startPos : TextPos := { model.cursor with col := model.cursor.col - 1 }
      setMessage (commitEdit model startPos model.cursor "") "Backspace."
    else if model.cursor.line > 0 then
      let previousLine := model.cursor.line - 1
      let startPos : TextPos := { line := previousLine, col := lineLength model previousLine }
      setMessage (commitEdit model startPos model.cursor "") "Joined lines."
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
        let endPos : TextPos := { model.cursor with col := model.cursor.col + 1 }
        setMessage (commitEdit model model.cursor endPos "") ("Deleted " ++ reprStr deleted ++ ".")
      else if model.cursor.line < lastLine model then
        let endPos : TextPos := { line := model.cursor.line + 1, col := 0 }
        setMessage (commitEdit model model.cursor endPos "") "Joined next line."
      else model

  /-- Reverses the latest edit by applying the inverse splice. -/
  def undoEdit (model : EditorState) : EditorState :=
    match model.undoStack with
    | [] => setMessage model "Nothing to undo."
    | diff :: rest =>
        let insertedEnd := endAfterText diff.startPos diff.insertedText
        let restored := replaceRangeRaw model diff.startPos insertedEnd diff.removedText
        let redoStack := diff :: model.redoStack
        let dirty := diff.revisionBefore != model.savedRevision
        let cursor := diff.cursorBefore
        let selectionAnchor := diff.selectionBefore
        let revision := diff.revisionBefore
        let restored := { restored with cursor := cursor }
        let restored := { restored with selectionAnchor := selectionAnchor }
        let restored := { restored with revision := revision }
        let restored := { restored with undoStack := rest }
        let restored := { restored with redoStack := redoStack }
        let restored := { restored with dirty := dirty }
        setMessage (ensureVisible restored) "Undo."

  /-- Reapplies the latest undone edit. -/
  def redoEdit (model : EditorState) : EditorState :=
    match model.redoStack with
    | [] => setMessage model "Nothing to redo."
    | diff :: rest =>
        let removedEnd := endAfterText diff.startPos diff.removedText
        let restored := replaceRangeRaw model diff.startPos removedEnd diff.insertedText
        let undoStack := diff :: model.undoStack
        let dirty := diff.revisionAfter != model.savedRevision
        let cursor := diff.cursorAfter
        let selectionAnchor := diff.selectionAfter
        let revision := diff.revisionAfter
        let restored := { restored with cursor := cursor }
        let restored := { restored with selectionAnchor := selectionAnchor }
        let restored := { restored with revision := revision }
        let restored := { restored with undoStack := undoStack }
        let restored := { restored with redoStack := rest }
        let restored := { restored with dirty := dirty }
        setMessage (ensureVisible restored) "Redo."

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
        | .ctrl 'z' => undoEdit model
        | .ctrl 'y' => redoEdit model
        | .left => moveLeft model
        | .right => moveRight model
        | .ctrlLeft => moveWordLeft model
        | .ctrlRight => moveWordRight model
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
        pure (.continue (setMessage
          { model with savedRevision := model.revision, dirty := false } "Saved."))
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
            then selectionStyle
            else if documentLine == model.cursor.line && model.richColor then currentLineStyle
            else Style.plain
          renderLineChars model (screen.putChar screenRow screenCol ch style)
            documentLine screenRow (screenCol + 1) fuel

  def renderRows (model : EditorState) (screen : Screen) (screenRow fuel : Nat) : Screen :=
    match fuel with
    | 0 => screen
    | fuel + 1 =>
        if screenRow >= model.screenSize.rows then screen
        else
          let documentLine := model.scrollTop + screenRow - 1
          let screen := renderLineChars model screen documentLine screenRow 0 (model.screenSize.cols + 1)
          renderRows model screen (screenRow + 1) fuel

  /-- Paints a complete screen row with text and trailing cells in the chrome style. -/
  def renderChromeRow (screen : Screen) (row : Nat) (text : String) : Screen :=
    let background := String.ofList (List.replicate screen.size.cols ' ')
    (screen.putString row 0 background chromeStyle).putString row 0 text chromeStyle

  /-- Renders the document, selection, status, and cursor into a screen buffer. -/
  def viewRule (model : EditorState) : Screen :=
    let screen := Screen.blank model.screenSize
    if model.screenSize.rows == 0 then screen
    else
      let help := " Ctrl-S Save  Ctrl-Z/Y Undo/Redo  Shift+Arrows Select  Ctrl-C/X/V Clipboard"
      let screen := renderChromeRow screen 0 help
      let screen := renderRows model screen 1 (contentRows model.screenSize + 1)
      let dirtyMark := if model.dirty then "* " else "  "
      let location := fileName model.path ++ "  Ln " ++ toString (model.cursor.line + 1) ++
        ", Col " ++ toString (model.cursor.col + 1)
      let screen :=
        if model.screenSize.rows > 1 then
          renderChromeRow screen (model.screenSize.rows - 1)
            (dirtyMark ++ model.message ++ "  |  " ++ location)
        else screen
      if model.screenSize.rows <= 2 || model.screenSize.cols == 0 ||
        model.cursor.line < model.scrollTop ||
        model.cursor.line >= model.scrollTop + contentRows model.screenSize then screen
      else
        let cursorRow := 1 + model.cursor.line - model.scrollTop
        let cursorCol := model.cursor.col - model.scrollLeft
        let pos : Pos := { row := cursorRow, col := cursorCol }
        let cell := screen.get pos
        screen.set pos { cell with style := cursorStyle }

  def resizeEditor (size : Size) (model : EditorState) : EditorState :=
    ensureVisible { model with screenSize := size }

  def initModel (path : String) (size : Size) (fileLines : Array String) (richColor : Bool) : EditorState :=
    let lines := if fileLines.isEmpty then #[""] else fileLines
    ensureVisible { path := path, lines := lines, screenSize := size, richColor := richColor }

  def editorApp (size : Size) (path : String) (fileLines : Array String) (richColor : Bool) :
      App EditorState EditorEffect :=
    { init := initModel path size fileLines richColor
      view := viewRule
      update := updateEditor
      resize := resizeEditor }

  /-- Loads a file, creates the clipboard interpreter, and runs the editor. -/
  def main (cmdLineArgs : List String) : IO Unit := do
    let size := (← Terminal.size?).getD { rows := 12, cols := 72 }
    match cmdLineArgs with
    | [path] =>
        let fileLines ← (readFileLines path).run FileSystem.interpret
        let richColor ← Terminal.supportsTruecolor
        let clipboard ← Clipboard.Store.create
        let interpret : Command.Interpreter EditorEffect :=
          Sum.interpret FileSystem.interpret clipboard.interpret
        App.run interpret (editorApp size path fileLines richColor)
    | _ => IO.Process.exit 1

end Editor
