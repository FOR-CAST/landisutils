# site-selection flags are refused where the parser would reject them

    Code
      harvestPrescription(name = "CC", StandRanking = "MaxCohortAge", SiteSelection = "Complete",
        AllowOverlap = TRUE, CohortsRemoved = "ClearCut")
    Condition
      Error:
      ! `AllowOverlap` applies only to `SiteSelection = "PatchCutting"`.

---

    Code
      harvestPrescription(name = "CC", StandRanking = "MaxCohortAge", SiteSelection = "PatchCutting",
        PatchPercentage = 50, PatchSize = 4, RepeatExactCells = FALSE,
        CohortsRemoved = "ClearCut")
    Condition
      Error:
      ! `RepeatExactCells` is read only after `MultipleRepeat`.

