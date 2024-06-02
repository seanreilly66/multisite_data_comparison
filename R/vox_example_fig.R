library(tidyverse)
library(lidR)
library(ggpubr)

las_file <- 'data/las/uas_point_cloud_4_treeID.las'

las <- readLAS(las_file) %>%
  filter_poi(treeID == 146) 

dim = max(las$Z) / (max(las$X) - min(las$X))

vox0.1 = voxel_metrics(las, fun = ~length(Z), res = 0.1)
vox0.25 = voxel_metrics(las, fun = ~length(Z), res = 0.25)
vox0.5 = voxel_metrics(las, fun = ~length(Z), res = 0.5)
vox1 = voxel_metrics(las, fun = ~length(Z), res = 1)
vox3.7 <- voxel_metrics(las, fun = ~length(Z), res = 3.7)


theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(linewidth = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

pnt = ggplot(
  data = las@data,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = .05,
    height = .05,
    color = 'slategray4',
    fill = 'slategray4',
    alpha = 0.25) +
  lims(
    y = c(0,36),
    x = c(527128, 527150)
  ) + 
  labs(
    x = 'Point Cloud',
    y = 'Height (m)'
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(rep(1,4), 'lines')) 

plt0.1 = ggplot(
  data = vox0.1,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = 0.1,
    height = 0.1,
    color = 'darkseagreen4',
    fill = 'darkseagreen2' ,
    alpha = 0.25
  ) + 
  labs(
    x = '0.1 m',
    y = NULL
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(rep(1,4), 'lines')) 

plt0.25 = ggplot(
  data = vox0.25,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = 0.25,
    height = 0.25,
    color = 'darkseagreen4',
    fill = 'darkseagreen2',
    alpha = 0.25
  ) + 
  labs(
    x = '0.25 m',
    y = NULL
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(rep(1,4), 'lines')) 

plt0.5 = ggplot(
  data = vox0.5,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = 0.5,
    height = 0.5,
    color = 'darkseagreen4',
    fill = 'darkseagreen2',
    alpha = 0.25
  ) + 
  labs(
    x = '0.5 m',
    y = NULL
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(rep(1,4), 'lines')) 

plt1 = ggplot(
  data = vox1,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = 1,
    height = 1,
    color = 'darkseagreen4',
    fill = 'darkseagreen2',
    alpha = 0.25
  ) + 
  labs(
    x = '1 m',
    y = NULL
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(rep(1,4), 'lines'))


plt3.7 = ggplot(
  data = vox3.7,
  mapping = aes(
    x = X,
    y = Z
  )
) +
  geom_tile(
    width = 3.7,
    height = 3.7,
    color = 'darkseagreen4',
    fill = 'darkseagreen2',
    alpha = 0.25
  ) +
  theme(aspect.ratio = dim) + 
  labs(
    x = '3.7 m',
    y = NULL
  ) +
  theme(aspect.ratio = dim,
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(rep(1,4), 'lines')) 

ggpubr::ggarrange(
  pnt, plt0.25, plt1, plt3.7,
  nrow = 1,
  align = 'hv'
) %>%
  annotate_figure(bottom = text_grob(
    '             Voxel dimension',
    family = 'serif',
    size = 16
  ))

ggsave(
  filename = 'figures/vox_example.png',
  width = 11,
  height = 4,
  units = 'in',
  dpi = 700,
  bg = 'white'
)


