library(tidyverse)

mdl_df <- 'data/ml_output/rf_results_master.csv' %>%
  read_csv() %>%
  mutate(across(ends_with('pred'), ~ replace_na(., 'none'))) %>%
  mutate(resp_type = recode(resp_type,
                            'lai_mean' = 'LAI',
                            'cbd_mean' = 'CBD',
                            'cbh' = 'CBH',
                            'densiometer_mean' = 'Canopy Cover',
                            'biomass_sum' = 'Biomass',
                            'h_mean' = 'Mean Height'
                            ),
         struct_pred = recode(struct_pred,
                              'tls' = 'TLS',
                              'zeb' = 'ZEB',
                              'uas' = 'UAS-SfM',
                              'tls_uas' = 'TLS + UAS-SfM',
                              'zeb_uas' = 'ZEB + UAS-SfM',
                              'none' = 'None'
                              ),
         spec_pred = recode(spec_pred,
                            'none' = 'None',
                            'planet' = 'Planet',
                            'uas' = 'UAS'), 
         struct_type = case_match(
           struct_type,
           c('vox', 'voxel') ~ 'Voxel',
           'pntcld' ~ 'Point cloud'
         )) %>%
  filter(is.na(h_thresh) | h_thresh == 0.25)



method_opt <- mdl_df %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  group_by(resp_type) %>%
  arrange(desc(Rsquared), .by_group = T) %>%
  mutate(pval = replace_na(pval, 10)) %>%
  rowwise() %>%
  mutate(lab = ifelse(pval > 0.05, 
                      sprintf("  %.2f*", round(Rsquared,2)),
                      sprintf("  %.2f  ", round(Rsquared,2))))




# ==============================================================================
# ============================== Correlation plot ==============================
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


ggplot(data = method_opt,
       mapping = aes(
         x = spec_pred,
         y = struct_pred,
         fill = Rsquared,
         label = lab
       )) +
  geom_tile(color = 'white') +
  geom_text(
    family = 'serif',
    fontface = 'plain',
    size = 5
  ) +
  geom_point(
    mapping = aes(shape = struct_type),
    position = position_nudge(x = -0.38, y = 0.26)) +
  scale_fill_gradient2(
    high = scales::muted('red'),
    mid = 'grey',
    low = 'white',
    midpoint = 0.5,
    name = bquote(italic(R)^2)
  ) +
  scale_y_discrete(limits = rev(c('UAS-SfM', 'TLS', 'ZEB', 
                              'TLS + UAS-SfM', 'ZEB + UAS-SfM', 'None'))) +
  scale_x_discrete(limits = c('UAS', 'Planet', 'None')) +
  facet_wrap(~ resp_type, ncol = 2) +
  labs(
    x = 'Spectral predictor',
    y = 'Structural predictor',
    shape = NULL
  ) +
  scale_shape_manual(
    values = c(16, 0),
    na.translate = F
  ) +
  theme(legend.position = 'right',
        axis.line = element_blank()
        )

ggsave(
  filename = 'figures/rf_spec_struct_heatmap.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700
)
