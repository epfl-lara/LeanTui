import Lean

open Lean Elab Term Macro

/-- Enable flag. When true, allows byy to insert sorry as a proof. -/
register_option LightLean.allow_by_sorry : Bool := {
  defValue := false
  descr := "allow byy to fall back to a sorry proof"
}

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

syntax "easy" : term

macro_rules
  | `(easy) => `(byy
      grind)
