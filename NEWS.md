# landisutils 0.0.129

* `plot_growth_candidate(density = TRUE)` maps the hex density on ALPHA rather than on fill. The binned-median series maps `fill` deliberately, so that its discrete scale merges with the colour legend into one key, and a continuous fill for the hexes collided with it: ggplot2 permits one scale per aesthetic and reports the collision as "continuous value supplied to a discrete scale", which names neither layer. Alpha is unmapped elsewhere, reads adequately for a density, and leaves the legend construction untouched. 0.0.128 could not draw a bundle at all.

