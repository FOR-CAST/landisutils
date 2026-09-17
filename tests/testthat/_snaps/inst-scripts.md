# verify_candidate_loss.R refuses a missing or abbreviated candidate_ref

    Code
      source(script, local = new.env(parent = globalenv()))
    Condition
      Error:
      ! Set `candidate_ref` (a landisutils branch, tag or full sha) before sourcing this script.

---

    Code
      source(script, local = env)
    Condition
      Error:
      ! `candidate_ref` looks like an abbreviated sha; give the full 40-character sha.

