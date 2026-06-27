import LeanTui.Terminal

namespace LeanTui

/-- The result of applying a key event to an application model. -/
inductive Step (Model : Type) where
  /-- Continue running with the next model. -/
  | continue (model : Model)
  /-- Continue running and redraw the next frame from scratch. -/
  | redrawFull (model : Model)
  /-- Stop the app after rendering the final model. -/
  | quit (model : Model)

/-- A terminal UI application. -/
structure App (Model : Type) where
  /-- Initial application model. -/
  init : Model
  /-- Renders a model into a screen buffer. -/
  view : Model -> Screen
  /-- Updates the model in response to one key. -/
  update : Key -> Model -> Step Model
  /-- Updates the model in response to a terminal resize. -/
  resize : Size -> Model -> Model := fun _ model => model
  /-- Optional blocking key reader, useful for tests or custom input. -/
  readKey? : Option (IO Key) := none
  /-- Whether the app wants terminal mouse press/release events. -/
  mouse : Bool := false

namespace App

/-- Internal loop state after applying pending keys. -/
structure FrameState (Model : Type) where
  /-- Current model. -/
  model : Model
  /-- Whether the next frame should be rendered from scratch. -/
  redrawFull : Bool := false
  /-- Whether the app should stop after this frame. -/
  quit : Bool := false

/-- Applies a batch of keys to frame state in order. -/
partial def applyKeys (app : App Model) (state : FrameState Model) : List Key -> FrameState Model
  | [] => state
  | key :: rest =>
      if state.quit then
        state
      else
        match app.update key state.model with
        | Step.quit model =>
            applyKeys app { model := model, redrawFull := state.redrawFull, quit := true } rest
        | Step.redrawFull model =>
            applyKeys app { model := model, redrawFull := true } rest
        | Step.continue model =>
            applyKeys app { model := model, redrawFull := state.redrawFull } rest

/-- Draws the next frame, either as a full render or as a diff. -/
def drawFrame (redrawFull : Bool) (previous next : Screen) : IO Unit := do
  if redrawFull then
    Terminal.drawFull next
  else
    Terminal.drawDiff previous next

/-- Applies an app resize hook when a post-input size poll detects a changed terminal. -/
def applyResize
    (app : App Model)
    (previous : Screen)
    (state : FrameState Model) : IO (FrameState Model) := do
  match (← Terminal.size?) with
  | none => pure state
  | some size =>
      if size == previous.size then
        pure state
      else
        pure { state with model := app.resize size state.model, redrawFull := true }

/-- Runs the app loop by waiting for input, then rendering one resulting frame. -/
partial def loopWithReader
    (app : App Model)
    (reader : Terminal.InputReader)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let keys ← reader.readKeys
  let state := applyKeys app { model := model } keys
  let state ← applyResize app previous state
  let next := app.view state.model
  drawFrame state.redrawFull previous next
  if state.quit then
    pure ()
  else
    loopWithReader app reader next state.model

/-- Runs the app loop using a caller-provided blocking key reader. -/
partial def loopWithReadKey
    (app : App Model)
    (readKey : IO Key)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let key ← readKey
  let state := applyKeys app { model := model } [key]
  let state ← applyResize app previous state
  let next := app.view state.model
  drawFrame state.redrawFull previous next
  if state.quit then
    pure ()
  else
    loopWithReadKey app readKey next state.model

/--
Runs an application in the alternate screen.
Raw terminal mode is restored and the alternate screen is left when the app exits.
-/
def run (app : App Model) : IO Unit := do
  let mode? ← Terminal.enableRaw
  let reader ← Terminal.mkInputReader
  Terminal.enter
  try
    if app.mouse then
      Terminal.write (Ansi.enableMouseTracking ++ Ansi.enableSgrMouse)
      Terminal.flush
    let initial := app.view app.init
    Terminal.drawFull initial
    match app.readKey? with
    | some readKey => loopWithReadKey app readKey initial app.init
    | none => loopWithReader app reader initial app.init
  finally
    if app.mouse then
      Terminal.write (Ansi.disableMouseTracking ++ Ansi.disableSgrMouse)
      Terminal.flush
    Terminal.leave
    Terminal.restore mode?

end App

end LeanTui
