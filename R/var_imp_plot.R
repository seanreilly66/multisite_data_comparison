library(tidyverse)
library(glue)
library(ggpubr)

mdl_varimp <- function(mdl) {
  
  var_imp <- caret::varImp(mdl) %>%
    pluck('importance') %>%
    rownames_to_column() %>%
    rename(var = rowname, imp = Overall)
  
  return(var_imp)
  
}

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

df1 <- read_rds('data/ml_output/rf_results_master.Rdata') 

df <- df1 %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  filter(!is.na(imp_1)) %>%
  slice_max(order_by = Rsquared, n = 1) %>%
  filter(struct_pred %in% c('uas', 'tls', 'zeb')) %>%
  select(-data) %>%
  mutate(
    var_imp = map(.x = rf,
                  .f = ~ mdl_varimp(mdl = .x))
  ) %>%
  unnest(var_imp) %>%
  mutate(
    var_type = case_match(
      var,
      c('z_max', glue('z_p{i}', i = seq(65, 95, 5))) ~ 'Upper',
      c('z_mean', glue('z_p{i}', i = seq(35, 60, 5))) ~ 'Middle',
      glue('z_p{i}', i = seq(0, 30, 5)) ~  'Lower',
      c('z_sd', 'z_cv', 'z_kurtosis', 'z_skewness', 'z_iqr', 'canopy_relief_ratio') ~ 'Variation',
      c(glue('d0{i}', i = 1:9), 'd10') ~ 'Density',
      c('cc_1m', 'cc_mean') ~ 'Cover',
      .default = 'Spectral'
    )
  ) %>%
  group_by(resp_type, struct_pred, spec_pred, var_type) %>%
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
      'zeb' ~ 'ZEB Struct',
      'uas' ~ 'UAS-SfM Struct',
      'tls_uas' ~ 'TLS + UAS-SfM',
      'zeb_uas' ~ 'ZEB + UAS-SfM',
      'none' ~ 'None'
    ),
    spec_pred = case_match(
      spec_pred,
      'none' ~ 'None',
      'planet' ~ 'Planet',
      'uas' ~ 'UAS'
    ),
    spec_pred = replace_na(spec_pred, 'None'),
    var_type = factor(var_type, 
                       levels = c('Spectral', 'Cover', 'Variation', 'Density', 'Upper', 'Middle', 'Lower'))
  )



cols = c(
  'Upper' = '#d55e00', 
  'Middle' = '#e69f00', 
  'Lower' = '#f0e442', 
  'Variation' = '#56b4e9', 
  'Density' = '#009e73', 
  'Cover' = '#0072b2', 
  'Spectral' = '#cc79a7')

plt_lst = list()

for (resp  in unique(df$resp_type)) {
  
  plt_df = df %>%
    filter(resp_type == resp)
  
  plt = ggplot(data = plt_df,
               aes(x = spec_pred,
                   y = var_imp,
                   fill = var_type)) +
    geom_bar(position = 'fill',
             stat = 'identity',
             width = 1) +
    scale_x_discrete(limits = c('UAS', 'Planet', 'None')) +
    # geom_text(aes(label = paste(Value, "%")), vjust = -0.25) +
    facet_wrap( ~ struct_pred, strip.position = "bottom", scales = "free_x") +
    scale_fill_manual(
      values = cols,
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
         y = resp,
         fill = NULL) +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  
  plt_lst[[resp]] = plt
  
  
}

plt_lst = lapply(plt_lst[1:5], function(x) {
  x + theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_blank())
  }) %>%
  append(plt_lst[6])


ggarrange(plotlist = plt_lst, ncol = 1, common.legend = TRUE,
          heights = c(0.75, 0.75, 0.75, 0.75, 0.75, 1), 
          legend = 'bottom') %>%
  annotate_figure(
    left = text_grob('Relative Variable Importance', family = 'serif', size = 16, rot = 90)
  )

ggsave(
  filename = 'figures/rf_varimp.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700
)

