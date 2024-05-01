# =================================== Libraries ================================

library(tidyverse)
library(ggpubr)

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
      'zeb' ~ 'ZEB',
      'uas' ~ 'UAS-SfM',
      'tls_uas' ~ 'TLS + UAS-SfM',
      'zeb_uas' ~ 'ZEB + UAS-SfM',
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
    )) %>%
  filter(is.na(struct_type) | struct_type == 'Voxel')

method_opt <- mdl_df %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  group_by(resp_type) %>%
  arrange(desc(Rsquared), .by_group = T) %>%
  mutate(resp_p = replace_na(resp_p, 10)) %>%
  rowwise() %>%
  mutate(lab = ifelse(resp_p > 0.05, 
                      'underline'~as.character(sprintf("%.2f", Rsquared)),
                      sprintf("%.2f", Rsquared)),
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
      fontface = sig,
      color = sig
    ),
    family = 'serif',
    size = 5,
    parse = TRUE
  ) +
  scale_fill_gradient2(
    high = scales::muted('red'),
    mid = 'grey',
    low = 'white',
    midpoint = 0.5,
    name = bquote(italic(R)^2)
  ) +
  scale_color_manual(
    values = 
      c('bold' = 'black',
        'plain' = 'grey10')
  ) +
  scale_y_discrete(limits = rev(c('UAS-SfM', 'TLS', 'ZEB', 'None'))) +
  scale_x_discrete(limits = c('UAS', 'Planet', 'None')) +
  facet_wrap(~ resp_type, ncol = 2) +
  labs(
    x = 'Spectral predictor',
    y = 'Structural predictor',
    shape = NULL
  ) +
  theme(legend.position = 'right',
        axis.line = element_blank()
        ) +
  guides(color = 'none')

ggsave(
  filename = 'figures/r2_voxel_heatmap.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700
)

# ==============================================================================
# =================================== Point ==================================
# ==============================================================================



col_pal = c(
  'None' = 'black',
  'UAS' = 'firebrick',
  'Planet' = '#DDCC77'
)

# shape_pal = c(
#   'None' = 21,
#   'UAS-SfM' = 24,
#   'ZEB' = 23,
#   'TLS' = 22
# )

shape_pal = c(
  'None' = 16,
  'UAS-SfM' = 17,
  'ZEB' = 18,
  'TLS' = 15
)

plt_lst = list()

resp_type = c('Biomass', 'Mean Height', 'CBH', 'CC', 'CBD', 'LAI')

for (resp  in resp_type) {
  
  plt_df = method_opt_pnt %>%
    filter(resp_type == resp)
  
  plt <- ggplot(
    data = plt_df,
    mapping = aes(x = Rsquared,
                  y = struct_pred,
                  color = spec_pred,
                  shape = struct_pred)) +
    geom_jitter(
      size = 3,
      height = 0.1
      ) +
    scale_color_manual(values = col_pal) +
    scale_shape_manual(values = shape_pal,
                       ) +
    xlim(0, 1) +
    labs(x = bquote(italic(R) ^ 2),
         color = 'Spectral Predictor',
         y = resp) +
    guides(shape = 'none')
  
  # geom_segment(
  #   data = min_max %>%
  #     filter(Site == 'All'),
  #   mapping = aes(
  #     x = min_r2,
  #     y = Field_Metric,
  #     xend = max_r2,
  #     yend = Field_Metric
  #   ),
  #   linetype = 'dashed',
  #   linewidth = 0.6
  # ) +
  
  plt_lst[[resp]] = plt
  
}

plt_lst = lapply(plt_lst[1:5], function(x) {
  x + theme(
    axis.text.x = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_blank()
  )
}) %>%
  append(plt_lst[6])

ggarrange(
  plotlist = plt_lst,
  ncol = 1,
  common.legend = TRUE,
  heights = c(0.75, 0.75, 0.75, 0.75, 0.75, 1),
  legend = 'bottom'
) %>%
  annotate_figure(left = text_grob(
    '             Structural Predictor',
    family = 'serif',
    size = 16,
    rot = 90
  ))

ggsave(
  filename = 'figures/r2_voxel_point.png',
  width = 6,
  height = 8,
  units = 'in',
  dpi = 700,
  bg = 'white'
)


# ==============================================================================
# =================================== Vertical bar ==================================
# ==============================================================================

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
  
  plt_df = method_opt_hbar %>%
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
  filename = 'figures/r2_voxel_vertical_bar.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

