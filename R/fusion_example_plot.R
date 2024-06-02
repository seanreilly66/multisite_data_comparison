library(lidR)
library(tidyverse)
library(glue)
library(patchwork)

hmls <- 'data/las/hmls/c6_zeb_p6_step4_220221.las' %>%
  readLAS(select = '')

uas <- 'data/las/uas/sdlmtn_c6_p6_uas_hnrm.las' %>%
  readLAS(select = '')

ts_width = 3
vox_dim = 1

# ==============================================================================
# ================================== Prep data ================================= 


p <- matrix(
  c(range(hmls$X), 
    range(hmls$Y)),
  nrow = 2
)


p <- matrix(
  c(rep(median(hmls$X, na.rm = T) + 2.5, 2), 
    range(hmls$Y)),
  nrow = 2
)

# p <- matrix(
#   c(range(hmls$X),
#     rep(median(hmls$Y, na.rm = T), 2)),
#   nrow = 2
# )

# ============================ Complete point cloud ============================

hmls_ts <- clip_transect(
  las = hmls,
  width = ts_width,
  p1 = p[1,],
  p2 = p[2,], 
  xz = TRUE)

hmls_crd <- hmls_ts@data %>%
  select(X, Z) %>%
  add_column(
    las = 'HMLS', 
    type = 'pnt', 
    dec = 'comp', 
    dim = 0.05)

hmls_vox_ts <- voxel_metrics(hmls_ts, fun = ~length(Z), res = vox_dim)

hmls_vox_crd <- hmls_vox_ts %>%
  select(X, Z) %>%
  add_column(
    las = 'HMLS', 
    type = 'vox', 
    dec = 'comp',
    dim = vox_dim)


uas_ts <- clip_transect(
  las = uas,
  width = ts_width,
  p1 = p[1,],
  p2 = p[2,],
  xz = TRUE) 

uas_crd <- uas_ts@data %>%
  select(X, Z) %>%
  add_column(
    las = 'UAS-SfM', 
    type = 'pnt', 
    dec = 'comp',
    dim = 0.05)

uas_vox_ts <- voxel_metrics(uas_ts, fun = ~length(Z), res = vox_dim)

uas_vox_crd <- uas_vox_ts %>%
  select(X, Z) %>%
  add_column(
    las = 'UAS-SfM', 
    type = 'vox', 
    dec = 'comp',
    dim = vox_dim)


# ============================ Decimated point cloud ===========================

dens = min(c(density(hmls), density(uas)))

hmls_dec_ts <- decimate_points(
  las = hmls_ts, 
  algorithm = homogenize(
    density = dens,
    res = 1
  ))

hmls_dec_crd <- hmls_dec_ts@data %>%
  select(X, Z) %>%
  add_column(
    las = 'HMLS', 
    type = 'pnt', 
    dec = 'dec', 
    dim = 0.05)

hmls_dec_vox_ts <- voxel_metrics(hmls_dec_ts, fun = ~length(Z), res = vox_dim)

hmls_dec_vox_crd <- hmls_dec_vox_ts %>%
  select(X, Z) %>%
  add_column(
    las = 'HMLS', 
    type = 'vox', 
    dec = 'dec',
    dim = vox_dim)


uas_dec_ts <- decimate_points(
  las = uas_ts, 
  algorithm = homogenize(
    density = dens,
    res = 1
  ))

uas_dec_crd <- uas_dec_ts@data %>%
  select(X, Z) %>%
  add_column(
    las = 'UAS-SfM', 
    type = 'pnt', 
    dec = 'dec',
    dim = 0.05)

uas_dec_vox_ts <- voxel_metrics(uas_dec_ts, fun = ~length(Z), res = vox_dim)

uas_dec_vox_crd <- uas_dec_vox_ts %>%
  select(X, Z) %>%
  add_column(
    las = 'UAS-SfM', 
    type = 'vox', 
    dec = 'dec',
    dim = vox_dim)


crd <- rbind(
  hmls_crd,
  uas_crd,
  hmls_vox_crd,
  uas_vox_crd,
  hmls_dec_crd,
  uas_dec_crd,
  hmls_dec_vox_crd,
  uas_dec_vox_crd
) %>%
  mutate(X = abs(X)) 



# ############# TESTING #############
# 
# crd_temp <- crd

# crd <- crd_temp %>%
#   group_by(las, type, dec) %>%
#   slice_sample(n = 10000) %>%
#   group_by(dec, type) %>%
#   nest() %>%
#   arrange(dec)

# 
# crd <- crd_temp %>%
#   group_by(las, type, dec) %>%
#   slice_sample(prop = 0.25) 


# 
# plot(hmls_ts) %>%
#   plot(uas_ts, add = .)

# ==============================================================================
# ================================== Prep data ================================= 

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(linewidth = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'transparent', 
                                    fill = 'transparent'),
    plot.background = element_rect(color = 'transparent', 
                                   fill = 'transparent'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = 'bottom',
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0.5)
  )
)




plt_fun <- function(df,
                    xlims = abs(range(crd$X)),
                    ylims = range(crd$Z)) {
  
  col_pal = c(
    # 'TLS' = '#117733',
    'HMLS' = '#BB8B46',
    'UAS-SfM' = '#356D34',
    'TLS' = '#882255'
  )
  
  # Fusion plot
  p1 = ggplot(
    data = df,
    mapping = aes(
      x = X,
      y = Z,
      fill = las,
      color = las,
      width = dim,
      height = dim
    )
  ) +
    geom_tile(alpha = 0.25) +
    labs(
      x = NULL,
      y = 'Height (m)'
    ) +
    scale_fill_manual(
      values = colorspace::lighten(col_pal, amount = 0.25),
      name = NULL
      ) +
    scale_color_manual(
      values = col_pal,
      name = NULL
    ) +
    lims(
      x = xlims,
      y = ylims
    ) +
    theme(
      aspect.ratio = 1) +
    guides(color = guide_legend(override.aes = list(alpha = 0.75,
                                                    linewidth = 0.75)))

  # Density plot
  p2 <- ggplot(
    data = df,
    mapping = aes(
      # x = X,
      y = Z
    )
  ) +
    geom_density(
      color = 'black',
      fill = 'snow4',
      linewidth = 0.25,
      alpha = 0.3
    ) +
    theme_void()
  
  # Combine
  plt = p1 + inset_element(
    p2,
    left = 0.036,
    bottom = 0,
    right = 0.25,
    top = 1,
    align_to = 'panel'
  ) 
  
}

cp <- crd %>% 
  filter(
    dec == 'comp', 
    type == 'pnt'
  ) %>%
  plt_fun() 

cv <- crd %>% 
  filter(
    dec == 'comp', 
    type == 'vox'
  ) %>%
  plt_fun()

dp <- crd %>% 
  filter(
    dec == 'dec', 
    type == 'pnt'
  ) %>%
  plt_fun()

dv <- crd %>% 
  filter(
    dec == 'dec', 
    type == 'vox'
  ) %>%
  plt_fun()


comp_plt = cp + cv + plot_annotation(title = 'Complete datasets', subtitle = parse(
  text = glue(
    'HMLS~density:~{round(density(hmls_ts))}~pts~m^2~~~~~~~~~~UAS~density:~{round(density(uas_ts))}~pts~m^2'
  )
)) +
  plot_layout(
    guides = 'collect',
    axis = 'collect'
  )
# comp_plt

dec_plt = dp + dv + plot_annotation(
  title = 'Decimated datasets', 
  subtitle = parse(
    text = glue(
      'HMLS~density:~{round(density(hmls_dec_ts))}~pts~m^2~~~~~~~~~~UAS~density:~{round(density(uas_dec_ts))}~pts~m^2'
    )),
  caption = 'Transect distance (m)') +
  plot_layout(
    guides = 'collect',
    axis = 'collect'
  ) 
# dec_plt


plt = ggpubr::ggarrange(
  comp_plt, 
  dec_plt,
  nrow = 2,
  common.legend = TRUE)




ggsave(
  plt,
  filename = 'temp/test.png',
  width = 8,
  height = 12,
  units = 'in',
  dpi = 700,
  bg = 'white'
)
  











