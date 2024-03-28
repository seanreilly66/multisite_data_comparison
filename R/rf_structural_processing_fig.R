
# -------------------------------- Test figures -------------------------------- 

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
  filter(is.na(h_thresh) | h_thresh == 0.25) %>%
  filter(struct_pred != 'TLS + UAS-SfM' & struct_pred != 'ZEB + UAS-SfM',
         spec_pred == 'None') %>%
  mutate(vox_dim = replace_na(vox_dim, -0.25))


# Voxel dimension fig

ggplot(
  data = mdl_df,
  mapping = aes(
    x = vox_dim,
    y = Rsquared,
    color = struct_pred
  )
) +
  geom_smooth(data = filter(mdl_df, struct_type == 'Voxel'), 
              linewidth = 0.5) +
  geom_point(mapping = aes(shape = struct_type)) +
  facet_wrap(~resp_type, ncol = 2) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  scale_shape_manual(
    values = c(16, 0),
    na.translate = F
  ) +
  scale_x_continuous(
    breaks = c(0.1, 0.5, 1, 3.7),
    labels = c('0.1', '0.5', '1', '3.7')
  ) +
  labs(
    x = 'Voxel dimension (m)',
    y = bquote(italic(R)^2),
    shape = NULL
  )

ggsave(
  filename = 'figures/rf_struct_processing_fig.png',
  width = 8,
  height = 6,
  units = 'in',
  dpi = 700
)
