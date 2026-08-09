# .scenario_template_digest() warns instead of silently returning NULL

    Code
      x <- .scenario_template_digest("/nonexistent/scenario-template")
    Condition
      Warning:
      scenario template '/nonexistent/scenario-template' is neither a directory nor a file; the evaluation fingerprint cannot see the template, so cached losses will survive a template change.

