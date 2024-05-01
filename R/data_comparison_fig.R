# =================================== Libraries ================================

library(tidyverse)
# library(patchwork)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

ldr_z_file <- 'data/lidar_fig_data.csv'
uas_z_file <- 'data/uas_fig_data.csv'
uas_spec_file <- 'data/spectral/uas_spec_val.csv'
planet_spec_file <- 'data/spectral/planet_spec_val.csv'
plot_list_file <- 'data/plot_list.csv'


spec_output <- 'figures/spec_data_comparison.png'
struct_output <- 'figures/struct_data_comparison.png'

# ==============================================================================
# ================================== Data prep =================================
# ==============================================================================

plot_list <- read_csv(plot_list_file) %>%
  mutate(
    site = case_match(
      site,
      'jcksn' ~ 'Jackson',
      'ppwd' ~ 'Pepperwood',
      'sdlmtn' ~ 'Saddle Mtn',
      'ltr' ~ 'LaTour'
    )
  )

# ================================ Height data ================================= 

ldr_z <- read_csv(ldr_z_file) %>%
  inner_join(
    plot_list,
    join_by(
      c == campaign,
      p == plot
    ))

uas_z <- read_csv(uas_z_file) %>%
  inner_join(
    plot_list,
    join_by(
      c == campaign,
      p == plot
    ))

las_z <- ldr_z %>%
  add_row(uas_z)  %>%
  rename(Z = value) %>%
  mutate(
    las_method = case_match(
      las_method,
      'tls' ~ 'TLS',
      'uas' ~ 'UAS-SfM',
      'zeb' ~ 'HMLS'
    )
  ) %>%
  select(site, las_method, Z) %>%
  filter(Z > 0.25)

rm(ldr_z, uas_z)

# #######################################  TEST DATA SAMPLING
# 
# las_z <- las_z %>%
#   group_by(site, las_method) %>%
#   slice_sample(n = 100) %>%
#   ungroup()
# 
# #######################################

# =============================== Spectral data ================================

uas_spec <- read_csv(uas_spec_file) %>%
  inner_join(plot_list) %>%
  add_column(spec_method = 'UAS')

planet_spec <- read_csv(planet_spec_file) %>%
  inner_join(plot_list) %>%
  add_column(spec_method = 'Planet')

spec <- uas_spec %>%
  add_row(planet_spec) %>%
  rename(NDVI  = ndvi) %>%
  select(site, spec_method, NDVI)

rm(uas_spec, planet_spec)

# #######################################  TEST DATA SAMPLING
# 
# spec <- spec %>%
#   group_by(site, spec_method) %>%
#   slice_sample(n = 100) %>%
#   ungroup()
# 
# #######################################

# ==============================================================================
# ==================================== Theme ===================================
# ==============================================================================


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
    title = element_text(size = 12.8),
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.text = element_text(size = 16,
                              vjust = 1),
    
  )
)

# ==============================================================================
# ==================================== Plot ====================================
# ==============================================================================

letters_lab <- function(x){glue::glue("{letters[factor(x)]}) {x}")}

col_pal = c(
  'TLS' = '#117733',
  'HMLS' = '#CC6677',
  'UAS-SfM' = '#69B3D8'
)


struct_fig = ggplot(
  data = las_z,
  aes(
    y = Z,
    # color = las_method,
    fill = las_method,
    x = las_method
  )
) +
  geom_violin(
    alpha = 0.5,
    draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_manual(values = col_pal) +
  facet_wrap(
    ~ site, 
    ncol = 2, 
    strip.position = 'top', 
    axes = 'all',
    axis.labels = 'margins',
    labeller = labeller(site = letters_lab)) +
  labs(
    x = 'Structural data method',
    y = 'Height above ground (m)'
  ) +
  theme(
    strip.placement = 'outside',
    strip.text = element_text(hjust = 0)) +
  guides(
    fill = guide_legend(position = 'bottom'))

# struct_fig

ggsave(
  plot = struct_fig,
  filename = struct_output,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

spec_fig = ggplot(
  data = spec,
  aes(
    x = NDVI,
    color = spec_method,
    fill = spec_method
)) +
  geom_density(
    alpha = 0.25,
    adjust = 0.75) +
  facet_wrap(
    ~ site, 
    ncol = 2, 
    strip.position = 'top', 
    axes = 'all',
    axis.labels = 'margins',
    labeller = labeller(site = letters_lab)) +
  lims(
    x = c(0,1)
  ) +
  labs(
    x = 'NDVI',
    y = 'Density') +
  scale_color_manual(values = c('#E1BE6A', '#69B3D8'),
                     name = 'Spectral data method') +
  scale_fill_manual(values = c('#E1BE6A', '#69B3D8'),
                     name = 'Spectral data method') +
  guides(
    fill = guide_legend(position = 'bottom'),
    color = guide_legend(position = 'bottom')) +
  theme(
    strip.placement = 'outside',
    strip.text = element_text(hjust = 0))

# spec_fig


ggsave(
  plot = spec_fig,
  filename = spec_output,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 700,
  bg = 'white'
)


# ==============================================================================
# ==================================== Stats ===================================
# ==============================================================================

# struct_kt <- kruskal.test(Z ~ las_method, data = las_z)
# struct_dunn <- FSA::dunnTest(Z ~ las_method, data = las_z)
# 1.2