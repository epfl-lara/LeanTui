import LeanTui.Terminal

namespace LeanTui

/-- The result of applying a key event to an application model. -/
inductive Step (Model : Type) where
  /-- Continue running with the next model. -/
  | continue (model : Model)
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
  /-- Optional blocking key reader, useful for tests or custom input. -/
  readKey? : Option (IO Key) := none
  /-- Delay between frames in milliseconds. -/
  frameDelayMs : UInt32 := 16

namespace App

/-- Internal loop state after applying pending keys. -/
structure FrameState (Model : Type) where
  /-- Current model. -/
  model : Model
  /-- Whether the app should stop after this frame. -/
  quit : Bool := false

/-- Sleeps for `app.frameDelayMs`, unless the delay is zero. -/
def waitFrame (app : App Model) : IO Unit := do
  if app.frameDelayMs == 0 then
    pure ()
  else
    IO.sleep app.frameDelayMs

/-- Applies a batch of keys to frame state in order. -/
partial def applyKeys (app : App Model) (state : FrameState Model) : List Key -> FrameState Model
  | [] => state
  | key :: rest =>
      if state.quit then
        state
      else
        match app.update key state.model with
        | Step.quit model => applyKeys app { model := model, quit := true } rest
        | Step.continue model => applyKeys app { model := model } rest

/-- Runs the app loop using nonblocking raw input polling. -/
partial def loopWithReader
    (app : App Model)
    (reader : Terminal.InputReader)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let keys ← reader.pollKeys
  let state := applyKeys app { model := model } keys
  let next := app.view state.model
  Terminal.drawDiff previous next
  if state.quit then
    pure ()
  else
    waitFrame app
    loopWithReader app reader next state.model

/-- Runs the app loop using a caller-provided blocking key reader. -/
partial def loopWithReadKey
    (app : App Model)
    (readKey : IO Key)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let key ← readKey
  let state := applyKeys app { model := model } [key]
  let next := app.view state.model
  Terminal.drawDiff previous next
  if state.quit then
    pure ()
  else
    waitFrame app
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
    let initial := app.view app.init
    Terminal.drawFull initial
    waitFrame app
    match app.readKey? with
    | some readKey => loopWithReadKey app readKey initial app.init
    | none => loopWithReader app reader initial app.init
  finally
    Terminal.leave
    Terminal.restore mode?

end App

end LeanTui
