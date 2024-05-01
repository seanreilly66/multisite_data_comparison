library(tidyverse)
library(ggpubr)


nofusion_opt <- 'data/ml_output/rf_nofusion_stats.csv' %>%
  read_csv() %>%
  mutate(across(ends_with('pred'), ~ replace_na(., 'none'))) %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  mutate(no_fuse_sig = ifelse(resp_p > 0.05, 1, 0)) %>%
  select(resp_type, struct_pred, spec_pred, vox_dim, resp_p, no_fuse_sig) %>%
  rename(nofuse_p = resp_p)

fusion_df <- 'data/ml_output/rf_results_stats.csv' %>%
  read_csv() %>%
  mutate(across(ends_with('pred'), ~ replace_na(., 'none')))  %>%
  left_join(nofusion_opt) %>%
  mutate(
    resp_type = recode(
      resp_type,
      'lai_mean' = 'LAI',
      'cbd_mean' = 'CBD',
      'cbh' = 'CBH',
      'densiometer_mean' = 'CC',
      'biomass_sum' = 'Biomass',
      'h_mean' = 'Mean Height'
    ),
    struct_pred = recode(
      struct_pred,
      'tls' = 'TLS',
      'zeb' = 'ZEB',
      'uas' = 'UAS-SfM',
      'tls_uas' = 'TLS + UAS-SfM',
      'zeb_uas' = 'ZEB + UAS-SfM',
      'none' = 'None'
    ),
    spec_pred = recode(
      spec_pred,
      'none' = 'None',
      'planet' = 'Planet',
      'uas' = 'UAS'
    ),
    spec_pred = factor(spec_pred,
                       levels = c('None', 'UAS', 'Planet')),
    struct_type = case_match(struct_type,
                             c('vox', 'voxel') ~ 'Voxel',
                             'pntcld' ~ 'Point cloud'),
    # struct_pred = factor(
    #   struct_pred,
    #   levels = rev(c('TLS + UAS-SfM', 'ZEB + UAS-SfM',' TLS', 'ZEB', 'UAS-SfM', 'None'))),
    sig = ifelse(resp_p > 0.05, 'yes', 'no'),
    no_fuse_sig = replace_na(no_fuse_sig, 10)
  ) %>%
  # filter(is.na(h_thresh) | h_thresh == 0.25)
  filter(is.na(struct_type) | struct_type == 'Voxel') %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  filter(no_fuse_sig > 0)



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



col_pal = c(
  'None' = 'black',
  'UAS' = 'firebrick',
  'Planet' = '#DDCC77'
)

alpha_pal = c(
  'yes' = 1,
  'no' = 0.2
)


plt_lst = list()

resp_type = c('Biomass', 'Mean Height', 'CBH', 'CC', 'CBD', 'LAI')

for (resp  in resp_type) {
  
  plt_df = fusion_df %>%
    filter(resp_type == resp)
  
  plt = ggplot(data = plt_df,
               aes(y = struct_pred,
                   x = Rsquared,
                   fill = spec_pred,
                   alpha = sig)) +
    geom_bar(
      position = 'dodge',
      stat = 'identity',
      width = 0.8,
      color = 'black',
      linewidth = 0.1
    ) +
    # scale_y_discrete(limits = c('None', 'UAS', 'Planet')) +
    # facet_wrap(~ struct_pred, strip.position = "bottom", scales = "free_x", nrow = 1) +
    scale_fill_manual(
      values = col_pal
    ) +
    scale_alpha_manual(
      values = alpha_pal
    ) +
    # scale_y_continuous(breaks = c(0,0.5,1),
    #                    limits = c(0,1)) +
    # geom_label(
    #   mapping = aes(label = sprintf("%.2f", round(Rsquared,2)),
    #                 x = 0),
    #   family = 'serif',
    #   fontface = 'plain',
    #   fill = 'white',
    #   vjust = 'inward',
    #   label.padding = unit(0.15, "lines"),
    #   label.r = unit(0, "lines"),
  #   label.size = 0.1
  # ) +
  labs(x = bquote(italic(R) ^ 2),
       y = resp,
       fill = '                             Spectral Predictor: ') +
    xlim(0,1) +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.box = 'vertical'
    ) +
    guides(alpha = 'none')
  
  plt
  
  plt_lst[[resp]] = plt
  
  
}

plt_lst = lapply(plt_lst[1:5], function(x) {
  x + theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_blank()
  )
}) %>%
  append(plt_lst[6])


ggarrange(
  plotlist = plt_lst,
  ncol = 1,
  common.legend = TRUE,
  heights = c(0.75, 0.75, 0.75, 0.75, 0.75, 1),
  legend = 'bottom'
)  %>%
  annotate_figure(left = text_grob(
    '             Structural Predictor',
    family = 'serif',
    size = 16,
    rot = 90
  ))


ggsave(
  filename = 'figures/r2_fusion_voxel_vertical_bar.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

