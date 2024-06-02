# ==============================================================================
#
# Field data plots
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 9 March 2022
# Last commit:
#
# Status: Needs documentation
#
# Created as part of 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Produces a series of plots capturing descriptive statistics of the field
# data measurements and relationships between them.
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# tidyverse
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(tidyverse)
library(ggpubr)
library(ggcorrplot)

# ================================= User inputs ================================

field_data_file <- 'data/field_data/plot_field_measurements.csv'


# ==============================================================================
# =================================== Setup ====================================
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
    title = element_text(size = 12.8)
  )
)

gg_theme = theme(
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
  title = element_text(size = 12.8)
)

# ==============================================================================
# ========================= Data distribution box plot =========================
# ==============================================================================

# ---------------------------- Input data cleaning ----------------------------- 


field_data <- field_data_file %>%
  read_csv() %>%
  mutate(campaign = if_else(campaign == 9, 3, campaign)) %>%
  mutate(
    across(c(site, campaign, plot), as.factor),
    site = fct_recode(
      site,
      Pepperwood = 'ppwd',
      Jackson = 'jcksn',
      LaTour = 'ltr',
      'Saddle Mtn' = 'sdlmtn'
    ),
    site = fct_relevel(
      site,
      c('Pepperwood', 'Jackson', 'LaTour', 'Saddle Mtn')
    )
  ) %>%
  mutate(cc = 100 - densiometer_mean) %>%
  select(-ends_with(c('_n', '_na'))) %>%
  mutate(biomass_sum = biomass_sum/0.04)


field_data <- field_data %>%
  pivot_longer(
    cols = -c(site, campaign, plot),
    names_to = 'metric',
    names_ptypes = factor()
  )

# ----------------------------- Plotting function ------------------------------ 

plot_func <- function(plot_metric = 'Biomass') {
  
  color_values = c('#88CCEE', '#117733', '#DDCC77', '#CC6677')
  
  plot_output <- ggplot(mapping = aes(x = site,
                                      y = value)) +
    geom_violin(
      data = field_data %>%
        filter(metric == plot_metric),
      mapping = aes(fill = site),
      alpha = 0.5,
      draw_quantiles = c(0.25, 0.5, 0.75)) +
    scale_fill_manual(values = color_values) +
    # geom_jitter(
    #   data = field_data %>%
    #     filter(metric == plot_metric,
    #            site == 'All Sites'),
    #   mapping = aes(color = campaign),
    #   width = 0.3,
    #   alpha = 0.6,
    #   shape = 17
    # ) +
    scale_color_manual(values = color_values) +
    # geom_boxplot(data = field_data %>%
    #                filter(metric == plot_metric),
    #              fill = NA) +
    theme(legend.position = 'none') +
    labs(x = NULL)
  
}

# -------------------------------- Metric plots -------------------------------- 

biomass <- 'biomass_sum' %>%
  plot_func() +
  labs(y = expression(atop('Biomass', paste('(Mg h', a^-1, ')')))) +
  ylim(0, NA)

biomass

# ggsave(
#   plot = biomass,
#   filename = 'figures/field_metrics/biomass.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

lai <- 'lai_mean' %>%
  plot_func() +
  labs(y = 'LAI') +
  ylim(0, NA)

lai

# ggsave(
#   plot = lai,
#   filename = 'figures/field_metrics/lai.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

bquote('Canopy Bulk Density\n'('kg m'^3))

cbd <- 'cbd_mean' %>%
  plot_func() +
  labs(y = expression(atop('CBD', paste('(kg ', m^-3, ')')))) +
  ylim(0, NA)

cbd

# ggsave(
#   plot = cbd,
#   filename = 'figures/field_metrics/cbd.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

cbh <- 'cbh' %>%
  plot_func() +
  labs(y = 'CBH\n(m)') +
  ylim(0, NA)

cbh

# ggsave(
#   plot = cbh,
#   filename = 'figures/field_metrics/cbh.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

# dens <- 'densiometer_mean' %>%
#   plot_func() +
#   labs(y = 'Densiometer') +
#   ylim(0, NA)
# 
# dens
# 
# ggsave(
#   plot = dens,
#   filename = 'figures/field_metrics/dens.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

cc <- 'cc' %>%
  plot_func() +
  labs(y = 'Canopy Cover\n(%)') +
  ylim(0, NA)

cc

# ggsave(
#   plot = cc,
#   filename = 'figures/field_metrics/cc.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )

height <- 'h_mean' %>%
  plot_func() +
  labs(y = 'Mean height\n(m)') +
  ylim(0, NA)

height

# ggsave(
#   plot = height,
#   filename = 'figures/field_metrics/height.png',
#   width = 8,
#   height = 5,
#   units = 'in',
#   dpi = 700
# )


metric_fig <- ggarrange(
  biomass +
    scale_x_discrete(labels = c(' ', ' ', ' ', ' ', ' ')),
  height +
    scale_x_discrete(labels = c(' ', ' ', ' ', ' ', ' ')),
  lai +
    scale_x_discrete(labels = c(' ', ' ', ' ', ' ', ' ')),
  cbd +
    scale_x_discrete(labels = c(' ', ' ', ' ', ' ', ' ')),
  cc +
    scale_x_discrete(labels = c(' ', ' ', ' ', ' ', ' ')),
  cbh, 
  nrow = 6,
  ncol = 1,
  align = 'hv'
)

ggsave(
  plot = metric_fig,
  filename = 'figures/combined_metrics.png',
  width = 6,
  height = 7,
  units = 'in',
  dpi = 700
)




## STATISTICS


# field_data <- field_data_file %>%
#   read_csv() %>%
#   add_row(field_data_file %>%
#             read_csv() %>%
#             mutate(site = 'All Sites')) %>%
#   mutate(campaign = if_else(campaign == 9, 3, campaign)) %>%
#   mutate(
#     across(c(site, campaign, plot), as.factor),
#     site = fct_recode(
#       site,
#       Pepperwood = 'ppwd',
#       Jackson = 'jcksn',
#       LaTour = 'ltr',
#       'Saddle Mtn' = 'sdlmtn'
#     ),
#     site = fct_relevel(
#       site,
#       c('All Sites', 'Pepperwood', 'Jackson', 'LaTour', 'Saddle Mtn')
#     )
#   ) %>%
#   mutate(cc = 100 - densiometer_mean) %>%
#   select(-ends_with(c('_n', '_na'))) %>%
#   pivot_longer(
#     cols = -c(site, campaign, plot),
#     names_to = 'metric',
#     names_ptypes = factor()
#   ) %>%
#   filter(site != 'All Sites')
# 
# 
# 
# aov(value ~ site, data = field_data %>%
#       filter(metric == 'cbh')) %>%
  # TukeyHSD()
# field_data <- field_data_file %>%
#   read_csv() %>%
#   add_row(field_data_file %>%
#             read_csv() %>%
#             mutate(site = 'All Sites')) %>%
#   mutate(campaign = if_else(campaign == 9, 3, campaign)) %>%
#   mutate(
#     across(c(site, campaign, plot), as.factor),
#     site = fct_recode(
#       site,
#       Pepperwood = 'ppwd',
#       Jackson = 'jcksn',
#       LaTour = 'ltr',
#       'Saddle Mtn' = 'sdlmtn'
#     ),
#     site = fct_relevel(
#       site,
#       c('All Sites', 'Pepperwood', 'Jackson', 'LaTour', 'Saddle Mtn')
#     )
#   ) %>%
#   mutate(cc = 100 - densiometer_mean) %>%
#   select(-ends_with(c('_n', '_na'))) %>%
#   pivot_longer(
#     cols = -c(site, campaign, plot),
#     names_to = 'metric',
#     names_ptypes = factor()
#   ) %>%
#   filter(site != 'All Sites')
# 
# 
# 
# aov(value ~ site, data = field_data %>%
#       filter(metric == 'cbh')) %>%
#   TukeyHSD()
