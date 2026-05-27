# LandisScenario$output_files is read-only and returns fixed scenario outputs

    Code
      scen$output_files <- "something.txt"
    Condition
      Error:
      ! `output_files` is read-only

