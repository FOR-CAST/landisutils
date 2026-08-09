# .stop_calibration_cluster() warns rather than orphaning silently

    Code
      invisible(.stop_calibration_cluster(list(cl = cl)))
    Condition
      Warning:
      2 of 2 calibration worker pool(s) could not be torn down; their containers are still running and must be removed by hand (docker ps --filter name=landis-). Unreachable worker index(es): 1, 2.

