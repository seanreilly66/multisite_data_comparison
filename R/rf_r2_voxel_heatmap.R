# =================================== Libraries ================================

library(tidyverse)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

input_file <- 'data/ml_output/rf_nofusion_stats.csv'

# ==============================================================================
# ================================== Data prep =================================
# ==============================================================================

mdl_df <- input_file %>%
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
      'none' ~ 'No Struct'),
    spec_pred = case_match(
      spec_pred,
      'none' ~ 'No Spec',
      'planet' ~ 'Planet',
      'uas' ~ 'UAS'), 
    struct_type = case_match(
      struct_type,
      c('vox', 'voxel') ~ 'Voxel',
      'pntcld' ~ 'Point cloud'
    ),
    struct_pred = factor(
      struct_pred,
      levels = rev(c('TLS', 'HMLS', 'UAS-SfM', 'No Struct'))
    ),
    spec_pred = factor(
      spec_pred,
      levels = c('No Spec', 'UAS', 'Planet')
    )) %>%
  filter(is.na(struct_type) | struct_type == 'Voxel')

method_opt <- mdl_df %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  group_by(resp_type) %>%
  arrange(desc(Rsquared), .by_group = T) %>%
  mutate(resp_p = replace_na(resp_p, 10)) %>%
  rowwise() %>%
  mutate(lab = sprintf("%.2f", Rsquared),
         sig = ifelse(resp_p > 0.05, 
                      'bold',
                      'plain'))


# ==============================================================================
# =================================== Theme ==================================
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
  )
)


# ==============================================================================
# =================================== Heatmap ==================================
# ==============================================================================

letters_lab <- function(x) {glue::glue("{letters[factor(x)]}) {x}")}

ggplot(data = method_opt,
       mapping = aes(
         x = spec_pred,
         y = struct_pred,
         fill = Rsquared,
         label = lab
       )) +
  geom_tile(color = 'white') +
  geom_text(
    aes(
      color = sig,
      fontface = sig
    ),
    family = 'serif',
    size = 5
  ) +
  scale_fill_gradient(
    high = 'firebrick',
    low = 'white',
    name = bquote(italic(R)^2),
    limits = c(0.34,0.85)
  ) +
  scale_color_manual(
    values = 
      c('bold' = 'black',
        'plain' = 'grey30')
  ) +
  facet_wrap(
    ~ resp_type, 
    ncol = 2, 
    strip.position = 'top', 
    labeller = labeller(resp_type = letters_lab)) +
  labs(
    x = 'Spectral predictor',
    y = 'Structural predictor',
    shape = NULL
  ) +
  theme(legend.position = 'right',
        axis.line = element_blank(),
        strip.placement = 'outside',
        strip.text = element_text(hjust = 0)
        ) +
  guides(color = 'none')

ggsave(
  filename = 'figures/r2_voxel_heatmap.png',
  width = 7,
  height = 6.5,
  units = 'in',
  dpi = 700
)

