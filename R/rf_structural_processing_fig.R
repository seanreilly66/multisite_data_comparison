# =================================== Libraries ================================

library(tidyverse)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

results_file <- 'data/ml_output/rf_results_stats.csv'
fig_output <- 'figures/rf_struct_processing_fig.png'

# ==============================================================================
# ================================== Data prep =================================
# ==============================================================================

mdl_df <- results_file %>%
  read_csv() %>%
  mutate(across(ends_with('pred'), ~ replace_na(., 'none'))) %>%
  mutate(
    resp_type = case_match(
      resp_type,
      'lai_mean' ~ 'LAI',
      'cbd_mean' ~ 'CBD',
      'cbh' ~ 'CBH',
      'densiometer_mean' ~ 'Canopy Cover',
      'biomass_sum' ~ 'Biomass',
      'h_mean' ~ 'Mean Height'),
    struct_pred = case_match(
      struct_pred,
      'tls' ~ 'TLS',
      'zeb' ~ 'HMLS',
      'uas' ~ 'UAS-SfM',
      'tls_uas' ~ 'TLS + UAS-SfM',
      'zeb_uas' ~ 'HMLS + UAS-SfM',
      'none' ~ 'None'),
    spec_pred = case_match(
      spec_pred,
      'none' ~ 'None',
      'planet' ~ 'Planet',
      'uas' ~ 'UAS'), 
    struct_type = case_match(
      struct_type,
      c('vox', 'voxel') ~ 'Voxel',
      'pntcld' ~ 'Point cloud'
    ),
    struct_pred = factor(
      struct_pred,
      levels = c(
        'TLS',
        'HMLS',
        'UAS-SfM')
    )) %>%
  filter(
    is.na(h_thresh) | h_thresh == 0.25,
    struct_pred != 'TLS + UAS-SfM' & struct_pred != 'ZEB + UAS-SfM',
    spec_pred == 'None') %>%
  mutate(vox_dim = replace_na(vox_dim, 4.25))


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
                              vjust = 1)
  ))

# ==============================================================================
# ============================= Voxel dimension fig ============================
# ==============================================================================

letters_lab <- function(x) {glue::glue("{letters[factor(x)]}) {x}")}


col_pal = c(
  'HMLS' = '#BB8B46',
  'UAS-SfM' = '#356D34',
  'TLS' = '#882255'
)

shape_pal = c(
  'Point cloud' = 21,
  'Voxel' = 22
)

ggplot(
  data = mdl_df,
  mapping = aes(
    x = vox_dim,
    y = Rsquared,
    color = struct_pred
  )
) +
  # geom_point(data = mdl_df %>% filter(struct_spec_p > 0.05),
  #            shape = 4,
  #            size = 2.5,
  #            stroke = 1,
  #            alpha = 0.5) +
  geom_line(
    data = filter(mdl_df, struct_type == 'Voxel'), 
    stat = 'smooth',
    linewidth = 0.5,
    alpha = 0.35) +
  geom_point(mapping = aes(shape = struct_type),
             fill = 'white',
             size = 2,
             stroke = 0.5) +
  geom_point(data = mdl_df %>% filter(struct_spec_p > 0.05),
             mapping = aes(shape = struct_type,
                           fill = struct_pred),
             size = 2) +
  geom_vline(
    xintercept = 4,
    color = 'grey75',
    linewidth = 0.5,
    linetype = 'dashed'
    ) +
  facet_wrap(
    ~resp_type, 
    ncol = 2, 
    strip.position = 'top', 
    axes = 'all',
    axis.labels = 'margins',
    labeller = labeller(resp_type = letters_lab)) +
  scale_color_manual(
    values = col_pal, 
    name = NULL) +
  scale_fill_manual(
    values = col_pal,
    name = NULL
  ) +
  scale_shape_manual(
    values = shape_pal,
    na.translate = F
  ) +
  scale_x_continuous(
    breaks = c(0.1, 0.5, 1, 3.7, 4.25),
    labels = c('0.1', '0.5', '1', '3.7', 'Pnt')
  ) +
  labs(
    x = 'Voxel dimension (m)',
    y = bquote(italic(R)^2),
    shape = NULL
  ) +
  theme(
    strip.placement = 'outside',
    strip.text = element_text(hjust = 0),
    legend.box = 'vertical') +
  guides(
    color = guide_legend(override.aes = list(size = 6,
                                             shape = 15,
                                             fill = 'white',
                                             linetype = NULL)),
    shape = guide_legend(override.aes = list(size = 4))
  )

ggsave(
  filename = fig_output,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 700
)

# ==============================================================================