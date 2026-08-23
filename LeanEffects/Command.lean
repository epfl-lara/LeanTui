namespace LeanEffects

/-- A typed description of effect requests whose eventual result has type `α`. -/
inductive Command (Effect : Type -> Type) (α : Type) where
  /-- Finishes a command without requesting another effect. -/
  | pure (value : α)
  /-- Requests an effect and continues with its typed result. -/
  | request {β : Type} (effect : Effect β) (next : β -> Command Effect α)

namespace Command

/-- Sequences a command with a continuation. -/
protected def bind (command : Command Effect α) (next : α -> Command Effect β) :
    Command Effect β :=
  match command with
  | .pure value => next value
  | .request effect continueWith =>
      .request effect fun result => (continueWith result).bind next

/-- Transforms the result of a command. -/
protected def map (transform : α -> β) (command : Command Effect α) : Command Effect β :=
  command.bind fun value => .pure (transform value)

instance : Monad (Command Effect) where
  pure := Command.pure
  bind := Command.bind
  map := Command.map

/-- An `IO` implementation of every request in an effect signature. -/
abbrev Interpreter (Effect : Type -> Type) :=
  {α : Type} -> Effect α -> IO α

/-- Executes a command by interpreting each typed request in order. -/
partial def run (interpret : Interpreter Effect) : Command Effect α -> IO α
  | .pure value => do return value
  | .request effect next => do
      let result <- interpret effect
      run interpret (next result)

end Command

/-- The empty effect signature, used by applications with pure handlers. -/
inductive NoEffect : Type -> Type

namespace NoEffect

/-- Interprets the empty effect signature; there are no possible requests. -/
def interpret : Command.Interpreter NoEffect :=
  fun effect => nomatch effect

end NoEffect

end LeanEffects
