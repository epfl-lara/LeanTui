import LeanEffects.Command

namespace LeanEffects.FileSystem

/-- Typed filesystem operations that can be embedded in a `Command`. -/
inductive Effect : Type -> Type where
  /-- Reads an entire UTF-8 text file. -/
  | readFile (path : System.FilePath) : Effect String
  /-- Writes an entire UTF-8 text file. -/
  | writeFile (path : System.FilePath) (contents : String) : Effect Unit

/-- Interprets filesystem requests using Lean's standard `IO.FS` operations. -/
def interpret : Command.Interpreter Effect :=
  fun effect =>
    match effect with
    | .readFile path => IO.FS.readFile path
    | .writeFile path contents => IO.FS.writeFile path contents

/-- Describes reading an entire UTF-8 text file. -/
def readFile (path : System.FilePath) : Command Effect String :=
  .request (.readFile path) .pure

/-- Describes writing an entire UTF-8 text file. -/
def writeFile (path : System.FilePath) (contents : String) : Command Effect Unit :=
  .request (.writeFile path contents) .pure

end LeanEffects.FileSystem
