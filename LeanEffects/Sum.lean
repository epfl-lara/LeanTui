import LeanEffects.Command

namespace LeanEffects

/-- The combination of two typed effect signatures. -/
inductive Sum (Left Right : Type -> Type) : Type -> Type where
  | left (effect : Left α) : Sum Left Right α
  | right (effect : Right α) : Sum Left Right α

namespace Sum

/-- Combines interpreters for the left and right effect signatures. -/
def interpret
    (interpretLeft : Command.Interpreter Left)
    (interpretRight : Command.Interpreter Right) :
    Command.Interpreter (Sum Left Right) :=
  fun effect =>
    match effect with
    | .left request => interpretLeft request
    | .right request => interpretRight request

end Sum

namespace Command

/-- Translates every request in a command to another effect signature. -/
protected def hoist
    (transform : {α : Type} -> Source α -> Target α) :
    Command Source β -> Command Target β
  | .pure value => .pure value
  | .request effect next =>
      .request (transform effect) fun result => (next result).hoist transform

end Command

end LeanEffects
