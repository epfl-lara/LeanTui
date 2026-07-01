import Lean

open Lean Elab Term Macro

/-- Enable flag. When true, allows byy to insert sorry as a proof. Example:

set_option LightLean.allow_by_sorry true
theorem oneMore (x:Int) : x + 1 = x + 1 := easy
#print axioms oneMore -- no axiom, as it was not needed
theorem oneOrTwoMore (x:Int) : x + 1 = x + 2 := easy
#print axioms oneOrTwoMore -- axiom inserted, but you can compile and run Lean code

-/
register_option LightLean.allow_by_sorry : Bool := {
  defValue := false
  descr := "allow byy to fall back to a sorry proof"
}

/--
Like `by`, but when `LightLean.allow_by_sorry` is true, falls back to `sorry`
if the supplied tactic proof does not solve the goal.
-/
syntax (name := byyTerm) "byy" ppDedent(ppLine tacticSeq) : term

@[term_elab byyTerm] def elabByy : TermElab := fun stx expectedType? => do
  match stx with
  | `(byy $seq:tacticSeq) =>
      let proofStx ←
        if LightLean.allow_by_sorry.get (← getOptions) then
          `(by
              solve
              | $seq:tacticSeq
              | sorry)
        else
          `(by $seq:tacticSeq)
      elabTerm proofStx expectedType?
  | _ => throwUnsupportedSyntax

/-- Try to prove theorem (for now) using grind. Insert sorry if this fails. -/
syntax "easy" : term

macro_rules
  | `(easy) => `(byy
      grind)
