import LeanTui.Terminal
import LeanEffects.Command

namespace LeanTui

open LeanEffects

/-- The result of applying a key event to an application model, specifying next state, whether to redraw, whether to quit. -/
inductive Step (Model : Type) where
  /-- Continue running with the next model. -/
  | continue (model : Model)
  /-- Continue running and redraw the next frame from scratch. -/
  | redrawFull (model : Model)
  /-- Stop the app after rendering the final model. -/
  | quit (model : Model)

/-- A terminal UI application whose handlers may describe requests from `Effect`. -/
structure App (Model : Type) (Effect : Type -> Type) where
  /-- Initial application model. -/
  init : Model
  /-- Renders a model into a screen buffer. -/
  view : Model -> Screen
  /-- Describes how to update the model in response to one key. -/
  update : Key -> Model -> Command Effect (Step Model)
  /-- Updates the model in response to a terminal resize. -/
  resize : Size -> Model -> Model := fun _ model => model
  /-- Updates the model with the cumulative number of characters written to the terminal. -/
  terminalOutput : Nat -> Model -> Model := fun _ model => model
  /-- Optional blocking key reader, useful for tests or custom input. -/
  readKey? : Option (IO Key) := none
  /-- Whether the app wants terminal mouse press/release events. -/
  mouse : Bool := false
  /-- Describes work to run with the final model just before the app exits. -/
  onExit : Model -> Command Effect Unit := fun _ => pure ()

namespace App

/-- Internal loop state after applying pending keys. -/
structure FrameState (Model : Type) where
  /-- Current model. -/
  model : Model
  /-- Whether the next frame should be rendered from scratch. -/
  redrawFull : Bool := false
  /-- Whether the app should stop after this frame. -/
  quit : Bool := false

/-- Tracks terminal output emitted by the app runner. -/
structure OutputMeter where
  /-- Cumulative number of characters written to the terminal. -/
  count : IO.Ref Nat

namespace OutputMeter

/-- Creates a terminal output meter with a zero count. -/
def create : IO OutputMeter := do
  pure { count := (← IO.mkRef 0) }

/-- Returns the cumulative number of characters written through this meter. -/
def get (meter : OutputMeter) : IO Nat :=
  meter.count.get

/-- Adds a measured write count to this meter. -/
def add (meter : OutputMeter) (amount : Nat) : IO Unit := do
  meter.count.modify (· + amount)

/-- Writes text through the terminal and records the number of characters written. -/
def write (meter : OutputMeter) (text : String) : IO Unit := do
  meter.add (← Terminal.writeMeasured text)

end OutputMeter

/-- Applies a batch of keys and their interpreted effects to frame state in order. -/
def applyKeys
    (interpret : Command.Interpreter Effect)
    (app : App Model Effect)
    (state : FrameState Model) : List Key -> IO (FrameState Model)
  | [] => pure state
  | key :: rest =>
      if state.quit then
        pure state
      else
        do match (← (app.update key state.model).run interpret) with
        | Step.quit model =>
            applyKeys interpret app
              { model := model, redrawFull := state.redrawFull, quit := true } rest
        | Step.redrawFull model =>
            applyKeys interpret app { model := model, redrawFull := true } rest
        | Step.continue model =>
            applyKeys interpret app { model := model, redrawFull := state.redrawFull } rest

/-- Draws the next frame, either as a full render or as a diff, and records its output. -/
def drawFrame (meter : OutputMeter) (redrawFull : Bool) (previous next : Screen) : IO Unit := do
  if redrawFull then
    meter.add (← Terminal.drawFullMeasured next)
  else
    meter.add (← Terminal.drawDiffMeasured previous next)

/-- Copies the latest terminal output count into the application model. -/
def applyOutputCount (app : App Model Effect) (meter : OutputMeter) (model : Model) : IO Model := do
  pure (app.terminalOutput (← meter.get) model)

/-- Applies an app resize hook when a post-input size poll detects a changed terminal. -/
def applyResize
    (app : App Model Effect)
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
    (interpret : Command.Interpreter Effect)
    (app : App Model Effect)
    (meter : OutputMeter)
    (reader : Terminal.InputReader)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let keys ← reader.readKeys
  let state ← applyKeys interpret app { model := model } keys
  let state ← applyResize app previous state
  let model ← applyOutputCount app meter state.model
  let state := { state with model := model }
  let next := app.view state.model
  drawFrame meter state.redrawFull previous next
  if state.quit then
    (app.onExit state.model).run interpret
  else
    loopWithReader interpret app meter reader next state.model

/-- Runs the app loop using a caller-provided blocking key reader. -/
partial def loopWithReadKey
    (interpret : Command.Interpreter Effect)
    (app : App Model Effect)
    (meter : OutputMeter)
    (readKey : IO Key)
    (previous : Screen)
    (model : Model) : IO Unit := do
  let key ← readKey
  let state ← applyKeys interpret app { model := model } [key]
  let state ← applyResize app previous state
  let model ← applyOutputCount app meter state.model
  let state := { state with model := model }
  let next := app.view state.model
  drawFrame meter state.redrawFull previous next
  if state.quit then
    (app.onExit state.model).run interpret
  else
    loopWithReadKey interpret app meter readKey next state.model

/--
Runs an application described by `app`.
(Uses the alternate screen. Raw terminal mode is restored and the alternate screen is left when the app exits.)
-/
def run (interpret : Command.Interpreter Effect) (app : App Model Effect) : IO Unit := do
  let meter ← OutputMeter.create
  let mode? ← Terminal.enableRaw
  try
    let reader ← Terminal.mkInputReader
    meter.write
      (Ansi.enterAlternateScreen ++ Ansi.disableAutoWrap ++ Ansi.hideCursor ++ Ansi.clearScreen)
    Terminal.flush
    if app.mouse then
      meter.write (Ansi.enableMouseTracking ++ Ansi.enableSgrMouse)
      Terminal.flush
    let init ← applyOutputCount app meter app.init
    let initial := app.view init
    meter.add (← Terminal.drawFullMeasured initial)
    match app.readKey? with
    | some readKey => loopWithReadKey interpret app meter readKey initial init
    | none => loopWithReader interpret app meter reader initial init
  finally
    if app.mouse then
      meter.write (Ansi.disableMouseTracking ++ Ansi.disableSgrMouse)
      Terminal.flush
    Terminal.leave
    Terminal.restore mode?

end App

end LeanTui
