# growth_structure_cell_curves() requires the batch column

    Code
      growth_structure_cell_curves(dplyr::select(structure_curves(), -"batch"))
    Condition
      Error in `growth_structure_cell_curves()`:
      ! "batch" %in% names(curves) is not TRUE

