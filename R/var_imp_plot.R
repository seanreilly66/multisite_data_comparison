# =================================== Libraries ================================

library(tidyverse)
library(glue)
library(ggpubr)
library(patchwork)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

mdl_results_file <- 'data/ml_output/rf_results_master.Rdata'
stats_results_file <- 'data/ml_output/rf_nofusion_stats.csv'

struct_pred_file <- 'data/predictor_df/uas_struct_predictors.csv'

uas_spec_file <- 'data/predictor_df/uas_spec_predictors.csv'

planet_spec_file <- 'data/predictor_df/planet_spec_predictors.csv'

# ==============================================================================
# ============================== Results data prep =============================
# ==============================================================================

mdl_varimp <- function(mdl) {
  
  var_imp <- caret::varImp(mdl) %>%
    pluck('importance') %>%
    rownames_to_column() %>%
    rename(var = rowname, imp = Overall)
  
  return(var_imp)
  
}

results_df <- read_rds(mdl_results_file) %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  filter(!is.na(imp_1)) %>%
  filter(is.na(struct_type) | !(struct_type == 'pntcld' & h_thresh != 0.25)) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  filter(struct_pred %in% c('uas', 'tls', 'zeb')) %>%
  select(-data) %>%
  mutate(var_imp = map(.x = rf,
                       .f = ~ mdl_varimp(mdl = .x))) %>%
  unnest(var_imp) %>%
  mutate(
    var_type = case_match(
      var,
      c('z_max', glue('z_p{i}', i = seq(65, 95, 5))) ~ 'Upper',
      c('z_mean', glue('z_p{i}', i = seq(35, 60, 5))) ~ 'Middle',
      glue('z_p{i}', i = seq(0, 30, 5)) ~  'Lower',
      c(
        'z_sd',
        'z_cv',
        'z_kurtosis',
        'z_skewness',
        'z_iqr',
        'canopy_relief_ratio'
      ) ~ 'Variation',
      c(glue('d0{i}', i = 1:9), 'd10') ~ 'Density',
      c('cc_1m', 'cc_mean') ~ 'Cover',
      .default = 'Spectral'
    )
  ) %>%
  group_by(resp_type, struct_pred, spec_pred, var_type, Rsquared) %>%
  summarize(var_imp = sum(imp)) %>%
  mutate(
    resp_type = case_match(
      resp_type,
      'lai_mean' ~ 'LAI',
      'cbd_mean' ~ 'CBD',
      'cbh' ~ 'CBH',
      'densiometer_mean' ~ 'CC',
      'biomass_sum' ~ 'Biomass',
      'h_mean' ~ 'Mean Height'
    ),
    struct_pred = case_match(
      struct_pred,
      'tls' ~ 'TLS Struct',
      'zeb' ~ 'HMLS Struct',
      'uas' ~ 'UAS-SfM Struct'
    ),
    struct_pred = factor(
      struct_pred,
      levels = c('TLS Struct', 'HMLS Struct', 'UAS-SfM Struct')
    ),
    spec_pred = replace_na(spec_pred, 'None'),
    spec_pred = case_match(spec_pred,
                           'None' ~ 'No spec',
                           'planet' ~ 'Planet',
                           'uas' ~ 'UAS Spec'),
    var_type = factor(
      var_type,
      levels = c(
        'Spectral',
        'Cover',
        'Variation',
        'Density',
        'Upper',
        'Middle',
        'Lower'
      )
    ),
    lab = sprintf("%.2f", round(Rsquared, 2))
  )



stats_results <- stats_results_file %>%
  read_csv %>%
  filter(struct_pred %in% c('uas', 'tls', 'zeb')) %>%
  transmute(
    resp_type = case_match(
      resp_type,
      'lai_mean' ~ 'LAI',
      'cbd_mean' ~ 'CBD',
      'cbh' ~ 'CBH',
      'densiometer_mean' ~ 'CC',
      'biomass_sum' ~ 'Biomass',
      'h_mean' ~ 'Mean Height'
    ),
    struct_pred = case_match(
      struct_pred,
      'tls' ~ 'TLS Struct',
      'zeb' ~ 'HMLS Struct',
      'uas' ~ 'UAS-SfM Struct'
    ),
    struct_pred = factor(
      struct_pred,
      levels = c('TLS Struct', 'HMLS Struct', 'UAS-SfM Struct')
    ),
    spec_pred = case_match(spec_pred,
                           'none' ~ 'No spec',
                           'planet' ~ 'Planet',
                           'uas' ~ 'UAS Spec'),
    sig = ifelse(resp_p > 0.05, 
                 'yes',
                 'no'),
    Rsquared = Rsquared,
    resp_p = resp_p
  ) 

results_df <- results_df %>%
  left_join(stats_results, relationship = 'many-to-many')

# ==============================================================================
# ============================= Reference data prep ============================
# ==============================================================================

struct_pred <- read_csv(struct_pred_file) %>%
  select(!campaign:vox_dim, -file) %>%
  colnames() %>%
  as_tibble() %>%
  rename(var = value) %>%
  add_column(method = 'No spec', .before = 1) %>%
  mutate(
    var_type = case_match(
      var,
      c('z_max', glue('z_p{i}', i = seq(65, 95, 5))) ~ 'Upper',
      c('z_mean', glue('z_p{i}', i = seq(35, 60, 5))) ~ 'Middle',
      glue('z_p{i}', i = seq(0, 30, 5)) ~  'Lower',
      c(
        'z_sd',
        'z_cv',
        'z_kurtosis',
        'z_skewness',
        'z_iqr',
        'canopy_relief_ratio'
      ) ~ 'Variation',
      c(glue('d0{i}', i = 1:9), 'd10') ~ 'Density',
      c('cc_1m', 'cc_mean') ~ 'Cover',
      .default = 'Spectral'
    )
  )

uas_spec_pred <- read_csv(uas_spec_file) %>%
  select(!campaign:method) %>%
  colnames() %>%
  as_tibble() %>%
  rename(var = value) %>%
  add_column(var_type = 'Spectral') %>%
  add_row(select(struct_pred, -method)) %>%
  add_column(method = 'UAS Spec')

planet_spec_pred <- read_csv(planet_spec_file) %>%
  select(!campaign:method) %>%
  colnames() %>%
  as_tibble() %>%
  rename(var = value) %>%
  add_column(var_type = 'Spectral') %>%
  add_row(select(struct_pred, -method)) %>%
  add_column(method = 'Planet')

ref_df <- struct_pred %>%
  add_row(uas_spec_pred) %>%
  add_row(planet_spec_pred) %>%
  group_by(var_type, method) %>%
  summarize(n = n()) %>%
  mutate(
    var_type = factor(
      var_type,
      levels = c(
        'Spectral',
        'Cover',
        'Variation',
        'Density',
        'Upper',
        'Middle',
        'Lower'
      )
    )
  )

# ==============================================================================
# =============================== Generate plot ================================ 
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
    plot.title = element_text(face = 'italic', hjust = 0.5, size = 16),
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.text = element_text(size = 16,
                              vjust = 1)
  )
)

col_pal = c(
  'Upper' = '#d55e00',
  'Middle' = '#e69f00',
  'Lower' = '#f0e442',
  'Variation' = '#56b4e9',
  'Density' = '#009e73',
  'Cover' = '#0072b2',
  'Spectral' = '#cc79a7'
)

# ================================ Results plot ================================ 

plt_lst = list()

resp_type = c('Biomass', 'Mean Height', 'CBH', 'CC', 'CBD', 'LAI')

for (resp  in resp_type) {
  
  plt_df = results_df %>%
    filter(resp_type == resp)
  
  plt = ggplot(data = plt_df,
               aes(x = spec_pred,
                   y = var_imp,
                   fill = var_type)) +
    geom_bar(
      position = 'fill',
      stat = 'identity',
      width = 0.95,
      color = 'black',
      linewidth = 0.1
    ) +
    scale_x_discrete(limits = c('No spec', 'UAS Spec', 'Planet')) +
    facet_wrap(~ struct_pred, strip.position = "bottom", scales = "free_x") +
    scale_fill_manual(
      values = col_pal,
      limits = c(
        'Spectral',
        'Cover',
        'Variation',
        'Density',
        'Upper',
        'Middle',
        'Lower'
      )
    ) +
    geom_label(
      mapping = aes(label = lab,
                    y = 0),
      size = 4.5,
      family = 'serif',
      fontface = 'plain',
      fill = 'white',
      color = 'grey40',
      vjust = 'inward',
      label.padding = unit(0.15, "lines"),
      label.r = unit(0, "lines"),
      label.size = 0.1
    ) +
    geom_label(
      data = plt_df %>% filter(sig == 'yes'),
      mapping = aes(label = lab,
                    y = 0),
      size = 4.5,
      family = 'serif',
      fontface = 'bold',
      fill = 'grey75',
      vjust = 'inward',
      label.padding = unit(0.15, "lines"),
      label.r = unit(0, "lines"),
      label.size = 0.1
    ) +
    labs(x = NULL,
         y = resp,
         fill = NULL) +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  # plt
  
  plt_lst[[resp]] = plt
  
  
}

# =============================== Reference plot =============================== 

ref_plt = ggplot(data = ref_df,
             aes(x = method,
                 y = n,
                 fill = var_type)) +
  geom_bar(
    position = 'fill',
    stat = 'identity',
    width = 0.95,
    color = 'black',
    linewidth = 0.1
  ) +
  scale_x_discrete(limits = c('No spec', 'UAS Spec', 'Planet')) +
  scale_fill_manual(
    values = col_pal,
    limits = c(
      'Spectral',
      'Cover',
      'Variation',
      'Density',
      'Upper',
      'Middle',
      'Lower'
    )
  ) +
  labs(x = NULL,
       y = '% Available',
       fill = NULL,
       title = 'Reference') +
  theme(
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

# ================================ Plot layout ================================= 

plt_lst = lapply(plt_lst[1:5], function(x) {
  x + theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_blank()
  )
}) %>%
  append(plt_lst[6])


design <- "
11111
22222
33333
44444
55555
66666
#777#
"

plt = Reduce('+', plt_lst) + ref_plt + 
  plot_layout(design = design, guides='collect') +
  theme(legend.position='bottom') 

plt = wrap_elements(plt) +
  labs(tag = "Relative Variable Importance") +
  theme(
    plot.tag = element_text(size = 18, angle = 90, hjust = 0.65),
    plot.tag.position = 'left'
  ) 

# plt

ggsave(
  plot = plt,
  filename = 'figures/rf_varimp.png',
  width = 9,
  height = 9.5,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

