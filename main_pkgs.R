# rm(list = ls())
library(sf)
library(maps)
library(fields)
library(dplyr)
library(lubridate)
library(magrittr)
library(stringr)
library(sp)
library(lattice)
library(stars)
library(terra)
library(ggplot2)
# library(maptools)
# library(mapproj)
# library(ggsn)
library(ggthemes)
library(scales)
library(ggpubr)
library(data.table)

library(Ipaper)
library(rcolors)
library(gg.layers)
library(matrixStats)

my_theme <- theme_bw(base_size = 12, base_family = "Times") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
theme_set(my_theme)


plot_spatial <- function(pdat, brks, mid.grey = FALSE) {
  # brks <- c(0.05, 0.1, 0.2, 0.3, 0.5, Inf) %>% c(-rev(.), 0, .)
  # brks <- c(0.02, 0.05, 0.1, 0.2, Inf) %>% c(-rev(.), 0, .)
  nbrk <- length(brks) - 1
  cols <- get_color(rcolors$amwg256, nbrk)
  
  if (mid.grey) {
    mids <- nbrk / 2 + c(0, 1)
    cols[mids] <- "grey80"  
  }
  
  g <- make_colorbar(
    at = brks, col = cols, height = 1,
    tck = 0.4,
    space = "right",
    legend.text.location = c(0.5, 0.5),
    # legend.text.just = c(0, 0.5),
    size = 14,
    legend.margin = bb_margin(r = 0.1),
    padding.right = unit(5, "points"),
    # legend.text = list(fontfamily = "Times", cex = 1.4),
    hjust = 0.0
  )
  
  # write_fig(g, "legend.pdf", 10, 5)
  # pdat <- dat[model %in% .models]

  p <- ggplot(pdat) +
    stat_cut(aes(lon, lat, z = value, color = after_stat(level), fill = after_stat(level)),
      breaks = brks,
      shape = 16, size = 0.6, alpha = 0.4
    ) +
    # geom_point(aes(lon, lat, color = level, fill = level),
    #   shape = 16, size = 0.6, alpha = 0.4
    # ) +
    geom_richtext_npc(
      data = d_lab[1:8, ], aes(label = model_level),
      npcx = 0, npcy = 1, hjust = 0, vjust = 1,
      size = 6, fontface = "bold", family = "Times"
    ) +
    layer_barchart(aes(z = value),
      width = unit(0.27, "npc"),
      height = unit(0.40, "npc"),
      brks = brks, cols = cols,
      theme = theme(
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
      )
    ) +
    theme(
      panel.border = element_blank(),
      strip.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2),
      legend.position = "none",
      plot.margin = margin(r = 5),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    geom_sf(data = worldshp, fill = "transparent", color = "black", size = 0.1) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(expand = c(0, 0), limits = c(-180, 180)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-60, 93), breaks = seq(-30, 90, 30)) +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    facet_wrap(~model, ncol = 2, dir = "v")

  p2 <- p + g
  p2
}
