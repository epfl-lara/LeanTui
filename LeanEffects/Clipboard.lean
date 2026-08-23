import LeanEffects.Command

namespace LeanEffects.Clipboard

/-- Typed operations on a text clipboard. -/
inductive Effect : Type -> Type where
  /-- Gets the current clipboard text. -/
  | getText : Effect String
  /-- Replaces the current clipboard text. -/
  | setText (text : String) : Effect Unit

/-- A mutable clipboard used by an effect interpreter. -/
structure Store where
  contents : IO.Ref String

namespace Store

/-- Creates an initially empty clipboard. -/
def create : IO Store := do
  return { contents := (← IO.mkRef "") }

/-- Interprets clipboard requests using this store. -/
def interpret (store : Store) : Command.Interpreter Effect :=
  fun effect =>
    match effect with
    | .getText => store.contents.get
    | .setText text => store.contents.set text

end Store

/-- Describes reading the clipboard text. -/
def getText : Command Effect String :=
  .request .getText .pure

/-- Describes replacing the clipboard text. -/
def setText (text : String) : Command Effect Unit :=
  .request (.setText text) .pure

end LeanEffects.Clipboard
