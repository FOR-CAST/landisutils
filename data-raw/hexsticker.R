# Generate the package hex sticker (man/figures/logo.png).
#
# Run from the package root with:
#   Rscript data-raw/hexsticker.R
#
# Requires {hexSticker}, {ggplot2}, {sysfonts}, {showtext} (all "Suggests"-level,
# dev-only). The artwork is a stylized forest-landscape *raster*: a green
# succession gradient laid over a grid of cells (the simulation landscape), with
# conifer silhouettes and an orange "disturbance" patch nodding to the
# succession + disturbance processes LANDIS-II models.

library(hexSticker)
library(ggplot2)
library(sysfonts)
library(showtext)

set.seed(42)

## --- palette ---------------------------------------------------------------
col_border <- "#1f3d1f" # deep forest green (hex edge + text)
col_bg <- "#eef4e3" # pale meadow green (hex fill)
col_text <- "#1f3d1f" # package name (dark, for the light fill)
col_tree <- "#1f3d1f" # conifer silhouette
col_grid <- "#cdddbb" # faint raster grid lines

## --- fonts -----------------------------------------------------------------
font <- tryCatch(
  {
    font_add_google("Oswald", "headerfont")
    "headerfont"
  },
  error = function(e) "sans"
)
showtext_auto()

## --- helpers ---------------------------------------------------------------
# A conifer = stacked triangles (foliage) on a short trunk, returned as polygon
# vertices tagged with a unique `grp` so several trees can be drawn at once.
conifer <- function(cx, base, height, width, grp) {
  tiers <- 3
  trunk_w <- width * 0.12
  trunk_h <- height * 0.12
  trunk <- data.frame(
    x = cx + c(-trunk_w, trunk_w, trunk_w, -trunk_w),
    y = base + c(0, 0, trunk_h, trunk_h),
    grp = paste0(grp, "_trunk"),
    fill = "trunk"
  )
  foliage_base <- base + trunk_h
  foliage_h <- height - trunk_h
  tiers_df <- do.call(
    rbind,
    lapply(seq_len(tiers), function(i) {
      y0 <- foliage_base + (i - 1) * foliage_h / tiers * 0.85
      y1 <- y0 + foliage_h / tiers
      w <- width * (1 - (i - 1) / tiers * 0.55)
      data.frame(
        x = cx + c(-w / 2, w / 2, 0),
        y = c(y0, y0, y1),
        grp = paste0(grp, "_tier", i),
        fill = "tree"
      )
    })
  )
  rbind(trunk, tiers_df)
}

## --- landscape raster (background) -----------------------------------------
nx <- 11
ny <- 8
cells <- expand.grid(x = seq_len(nx), y = seq_len(ny))
# smooth "biomass" field -> green succession gradient
cells$z <- with(
  cells,
  sin(x / 2.2) + cos(y / 2.6) + 0.45 * sin((x + y) / 3) + runif(nrow(cells), -0.25, 0.25)
)
# a small "disturbance" (recently burned) patch in the upper area
burn <- with(cells, x %in% 7:9 & y %in% 6:7)
cells$z[burn] <- min(cells$z) - 0.6

## --- conifer foreground ----------------------------------------------------
trees <- rbind(
  conifer(cx = 2.6, base = 1.4, height = 4.2, width = 2.4, grp = "t1"),
  conifer(cx = 5.5, base = 0.8, height = 5.6, width = 3.0, grp = "t2"),
  conifer(cx = 8.4, base = 1.6, height = 3.8, width = 2.2, grp = "t3")
)
tree_cols <- c(tree = col_tree, trunk = "#3b2a17")

## --- subplot ---------------------------------------------------------------
p <- ggplot() +
  geom_tile(data = cells, aes(x = x, y = y, fill = z), color = col_grid, linewidth = 0.25) +
  scale_fill_gradientn(
    colours = c("#3a1f10", "#5a4a1e", "#2f6b2f", "#3f8f3f", "#6fbf5f"),
    guide = "none"
  ) +
  # conifer silhouettes (drawn larger than the grid so they read at small size)
  lapply(split(trees, trees$grp), function(d) {
    geom_polygon(data = d, aes(x = x * (nx / 11), y = y * 0.95), fill = tree_cols[[d$fill[1]]])
  }) +
  coord_fixed(ratio = ny / nx, expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

## --- compose the sticker ---------------------------------------------------
out <- file.path("man", "figures", "logo.png")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

sticker(
  subplot = p,
  package = "landisutils",
  p_family = font,
  p_size = 15,
  p_y = 1.5,
  p_color = col_text,
  s_x = 1.0,
  s_y = 0.82,
  s_width = 1.45,
  s_height = 1.0,
  h_fill = col_bg,
  h_color = col_border,
  h_size = 1.4,
  dpi = 300,
  filename = out
)

# hexSticker/ggsave can open the default graphics device and leave an empty
# "Rplots.pdf" behind when run non-interactively; clean it up.
if (file.exists("Rplots.pdf")) {
  unlink("Rplots.pdf")
}

message("Wrote ", out)
