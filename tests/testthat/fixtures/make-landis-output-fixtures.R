## Fixtures for the LANDIS-II output row-order tests (test-landis_results.R).
##
## Neither file can be written by terra: terra always writes a north-up
## geotransform, and "stored south-up" is the whole point of these two.
##
## landis-output-no-geotransform.tif
##   A real LANDIS-II v8 Output Biomass Community map (`output-community-0.tif`)
##   from a 15-row x 1-col Biomass Succession calibration landscape, copied
##   verbatim. Landis.RasterIO.Gdal writes no geotransform and no CRS, so GDAL
##   reports the identity transform -- origin (0, 0), pixel height +1 -- and the
##   file is south-up. The map codes run 3, 4, ... 17 from the first row down
##   (the extension numbers communities itself, from 3, in landscape order).
##
## landis-output-south-up.tif
##   The other way a file can be south-up: an explicit geotransform with a
##   positive pixel height. 4 rows x 3 cols, values 1..12 in file order, so a
##   row flip and a column flip are distinguishable. Built with:
##
##     r <- terra::rast(nrows = 4, ncols = 3, xmin = 0, xmax = 3,
##                      ymin = 0, ymax = 4, crs = "")
##     terra::values(r) <- 1:12
##     terra::writeRaster(r, "tmp-north-up.tif", datatype = "INT4S")
##     system2("gdal_translate", c("-q", "-a_ullr", "0 0 3 4",
##                                 "tmp-north-up.tif", "landis-output-south-up.tif"))
##     unlink("tmp-north-up.tif")
##
##   `-a_ullr 0 0 3 4` puts the upper-left corner at y = 0 and the lower-left at
##   y = 4, which is what makes the pixel height positive. Values are untouched.
