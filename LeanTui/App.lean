import LeanTui.Terminal

namespace LeanTui

inductive Step (Model : Type) where
  | continue (model : Model)
  | quit (model : Model)

structure App (Model : Type) where
  init : Model
  view : Model -> Screen
  update : Key -> Model -> Step Model
  readKey? : Option (IO Key) := none
  frameDelayMs : UInt32 := 16

namespace App

structure FrameState (Model : Type) where
  model : Model
  quit : Bool := false

def waitFrame (app : App Model) : IO Unit := do
  if app.frameDelayMs == 0 then
    pure ()
  else
    IO.sleep app.frameDelayMs

partial def applyKeys (app : App Model) (state : FrameState Model) : List Key -> FrameState Model
  | [] => state
  | key :: rest =>
      if state.quit then
        state
      else
        match app.update key state.model with
        | Step.quit model => applyKeys app { model := model, quit := true } rest
        | Step.continue model => applyKeys app { model := model } rest

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
