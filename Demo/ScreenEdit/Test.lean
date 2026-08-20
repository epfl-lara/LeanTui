

-- Show fold n identity gives n (fold n of identity == n)
example : (3 : Nat).fold (fun acc _ _ => acc + 1) 0 = 3 := sorry

-- Or reason with simp:
example : (4 : Nat).fold (fun acc _ i => acc + i) 0 = 6 := sorry
