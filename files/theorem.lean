import Mathlib.Data.Nat.Prime.Basic
import Mathlib.Algebra.BigOperators.Group.List.Lemmas
import Mathlib.Algebra.Order.BigOperators.GroupWithZero.List
import Mathlib.Algebra.Order.Ring.Nat

theorem primes : ¬ ∃ p : ℕ, p.Prime ∧ ∀ n : ℕ, n.Prime → n ≤ p := by

  -- (1) Introduce our assumption
  unfold Not -- desugaring
  intro h

  -- (2) Construct ps
  obtain ⟨p, hp, hmax⟩ := h
  let ps := (List.range (p + 1)).filter (fun n => decide (Nat.Prime n))

  -- (3) Construct the counterexample x
  let x := ps.prod + 1
  -- (3a) Prove a lemma we need for prime factorization
  -- In fact, prime factorization Nat.exists_prime_and_dvd formalized in Lean
  -- does not require us to show x is not prime, only that x != 1
  have hx : x ≠ 1 := by
    -- Prove that ps.prod > 0 by showing all members are non-zero
    have hpos : 0 < ps.prod := List.prod_pos (fun q hq => by
      simp only [ps, List.mem_filter, decide_eq_true_eq] at hq
      exact hq.2.pos)
    -- Prove a linear inequality ps.prod > 0 => ps.prod + 1 > 1
    omega
  -- (3b) Apply prime factorization to x
  obtain ⟨q, hq, hqx⟩ := Nat.exists_prime_and_dvd hx

  -- (4) Derive a contradiction by showing q is and is not a factor of x
  have hcontra : q ∣ x ∧ ¬ q ∣ x := by
    constructor
    · exact hqx -- This is what exists_prime_and_dvd gives us
    · intro hdvd -- This will show that x % q = 1
      have hqps : q ∈ ps := by -- q is in ps, i.e., ps is exhaustive
        have := hmax q hq
        simp [ps, hq]
        omega
      have hprod : q ∣ ps.prod := List.dvd_prod hqps -- a member of ps is a factor of ps
      have hmod : x % q = 1 := by -- finally showing the remainder itself
        obtain ⟨k, hk⟩ := hprod
        show (ps.prod + 1) % q = 1
        rw [hk, Nat.mul_add_mod]
        exact Nat.mod_eq_of_lt hq.one_lt
      -- since x % q = 1, q not| x ... technically we are proving an internal ⊥
      -- so we need something that contradicts hmod
      have hzero : x % q = 0 := Nat.mod_eq_zero_of_dvd hdvd
      omega
  exact hcontra.2 hcontra.1
