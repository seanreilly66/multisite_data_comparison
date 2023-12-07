uas_cld_metrics <- function(z, 
                            r,
                            g,
                            b,
                            re,
                            nir,
                            ndvi,
                            ndre,
                            gndvi) {
  # ----------------------------- Height metrics -------------------------------
  
  z_max = max(z, na.rm = TRUE)
  z_mean = mean(z, na.rm = TRUE)
  
  z_p0 = quantile(z,
                  probs = 0.0,
                  names = FALSE,
                  na.rm = TRUE)
  z_p5 = quantile(z,
                  probs = 0.05,
                  names = FALSE,
                  na.rm = TRUE)
  z_p10 = quantile(z,
                   probs = 0.10,
                   names = FALSE,
                   na.rm = TRUE)
  z_p15 = quantile(z,
                   probs = 0.15,
                   names = FALSE,
                   na.rm = TRUE)
  z_p20 = quantile(z,
                   probs = 0.20,
                   names = FALSE,
                   na.rm = TRUE)
  z_p25 = quantile(z,
                   probs = 0.25,
                   names = FALSE,
                   na.rm = TRUE)
  z_p30 = quantile(z,
                   probs = 0.30,
                   names = FALSE,
                   na.rm = TRUE)
  z_p35 = quantile(z,
                   probs = 0.35,
                   names = FALSE,
                   na.rm = TRUE)
  z_p40 = quantile(z,
                   probs = 0.40,
                   names = FALSE,
                   na.rm = TRUE)
  z_p45 = quantile(z,
                   probs = 0.45,
                   names = FALSE,
                   na.rm = TRUE)
  z_p50 = quantile(z,
                   probs = 0.50,
                   names = FALSE,
                   na.rm = TRUE)
  z_p55 = quantile(z,
                   probs = 0.55,
                   names = FALSE,
                   na.rm = TRUE)
  z_p60 = quantile(z,
                   probs = 0.60,
                   names = FALSE,
                   na.rm = TRUE)
  z_p65 = quantile(z,
                   probs = 0.65,
                   names = FALSE,
                   na.rm = TRUE)
  z_p70 = quantile(z,
                   probs = 0.70,
                   names = FALSE,
                   na.rm = TRUE)
  z_p75 = quantile(z,
                   probs = 0.75,
                   names = FALSE,
                   na.rm = TRUE)
  z_p80 = quantile(z,
                   probs = 0.80,
                   names = FALSE,
                   na.rm = TRUE)
  z_p85 = quantile(z,
                   probs = 0.85,
                   names = FALSE,
                   na.rm = TRUE)
  z_p90 = quantile(z,
                   probs = 0.90,
                   names = FALSE,
                   na.rm = TRUE)
  z_p95 = quantile(z,
                   probs = 0.95,
                   names = FALSE,
                   na.rm = TRUE)
  
  
  z_sd = sd(z, na.rm = TRUE)
  z_cv = z_sd / z_mean
  z_kurtosis = moments::kurtosis(z, na.rm = TRUE)
  z_skewness = moments::skewness(z, na.rm = TRUE)
  z_iqr = IQR(z, na.rm = TRUE)
  
  canopy_relief_ratio = (z_mean - z_p0) / (z_max - z_p0)
  
  height_metrics = list(
    z_max = z_max,
    z_mean = z_mean,
    z_p0 = z_p0,
    z_p5 = z_p5,
    z_p10 = z_p10,
    z_p15 = z_p15,
    z_p20 = z_p20,
    z_p25 = z_p25,
    z_p30 = z_p30,
    z_p35 = z_p35,
    z_p40 = z_p40,
    z_p45 = z_p45,
    z_p50 = z_p50,
    z_p55 = z_p55,
    z_p60 = z_p60,
    z_p65 = z_p65,
    z_p70 = z_p70,
    z_p75 = z_p75,
    z_p80 = z_p80,
    z_p85 = z_p85,
    z_p90 = z_p90,
    z_p95 = z_p95,
    z_sd = z_sd,
    z_cv = z_cv,
    z_kurtosis = z_kurtosis,
    z_skewness = z_skewness,
    z_iqr = z_iqr,
    canopy_relief_ratio = canopy_relief_ratio
  )
  
  # --------------------------- Vertical densities -----------------------------
  
  n_z_gt_1 = sum(z >= 1)
  bins = seq(1, z_max, (z_max - 1) / 10)
  
  d01 = sum(z >= bins[1] & z < bins[2]) / n_z_gt_1
  d02 = sum(z >= bins[2] & z < bins[3]) / n_z_gt_1
  d03 = sum(z >= bins[3] & z < bins[4]) / n_z_gt_1
  d04 = sum(z >= bins[4] & z < bins[5]) / n_z_gt_1
  d05 = sum(z >= bins[5] & z < bins[6]) / n_z_gt_1
  d06 = sum(z >= bins[6] & z < bins[7]) / n_z_gt_1
  d07 = sum(z >= bins[7] & z < bins[8]) / n_z_gt_1
  d08 = sum(z >= bins[8] & z < bins[9]) / n_z_gt_1
  d09 = sum(z >= bins[9] & z < bins[10]) / n_z_gt_1
  d10 = sum(z >= bins[10] & z <= bins[11]) / n_z_gt_1
  
  vertical_densities = list(
    d01 = d01,
    d02 = d02,
    d03 = d03,
    d04 = d04,
    d05 = d05,
    d06 = d06,
    d07 = d07,
    d08 = d08,
    d09 = d09,
    d10 = d10
  )
  
  # ------------------------------ Canopy cover --------------------------------
  
  n_z = length(z)
  
  cc_1m = n_z_gt_1 / n_z
  cc_mean = sum(z >= z_mean) / n_z
  
  canopy_cover = list(cc_1m = cc_1m,
                      cc_mean = cc_mean)
  
  # ---------------------------- Spectral metrics ------------------------------
  
  b_mean = mean(b, na.rm = TRUE)
  b_sd = sd(b, na.rm = TRUE)
  g_mean = mean(g, na.rm = TRUE)
  g_sd = sd(g, na.rm = TRUE)
  r_mean = mean(r, na.rm = TRUE)
  r_sd = sd(r, na.rm = TRUE)
  re_mean = mean(re, na.rm = TRUE)
  re_sd = sd(re, na.rm = TRUE)
  nir_mean = mean(nir, na.rm = TRUE)
  nir_sd = sd(nir, na.rm = TRUE)
  ndvi_mean = mean(ndvi, na.rm = TRUE)
  ndvi_sd = sd(ndvi, na.rm = TRUE)
  ndre_mean = mean(ndre, na.rm = TRUE)
  ndre_sd = sd(ndre, na.rm = TRUE)
  gndvi_mean = mean(gndvi, na.rm = TRUE)
  gndvi_sd = sd(gndvi, na.rm = TRUE)
  
  full_df <- tibble(z, b, g, r, re, nir, ndvi, ndre, gndvi) %>%
    mutate(bin = cut(
      z,
      breaks = bins,
      include.lowest = TRUE,
      right = FALSE,
      labels = 1:10
    ))
  
  bin_df <- full_df %>%
    filter(bin == 1)
  
  b01_mean = mean(bin_df$b, na.rm = TRUE)
  b01_sd = sd(bin_df$b, na.rm = TRUE)
  g01_mean = mean(bin_df$g, na.rm = TRUE)
  g01_sd = sd(bin_df$g, na.rm = TRUE)
  r01_mean = mean(bin_df$r, na.rm = TRUE)
  r01_sd = sd(bin_df$r, na.rm = TRUE)
  re01_mean = mean(bin_df$re, na.rm = TRUE)
  re01_sd = sd(bin_df$re, na.rm = TRUE)
  nir01_mean = mean(bin_df$nir, na.rm = TRUE)
  nir01_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi01_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi01_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre01_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre01_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi01_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi01_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 2)
  
  b02_mean = mean(bin_df$b, na.rm = TRUE)
  b02_sd = sd(bin_df$b, na.rm = TRUE)
  g02_mean = mean(bin_df$g, na.rm = TRUE)
  g02_sd = sd(bin_df$g, na.rm = TRUE)
  r02_mean = mean(bin_df$r, na.rm = TRUE)
  r02_sd = sd(bin_df$r, na.rm = TRUE)
  re02_mean = mean(bin_df$re, na.rm = TRUE)
  re02_sd = sd(bin_df$re, na.rm = TRUE)
  nir02_mean = mean(bin_df$nir, na.rm = TRUE)
  nir02_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi02_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi02_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre02_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre02_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi02_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi02_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 3)
  
  b03_mean = mean(bin_df$b, na.rm = TRUE)
  b03_sd = sd(bin_df$b, na.rm = TRUE)
  g03_mean = mean(bin_df$g, na.rm = TRUE)
  g03_sd = sd(bin_df$g, na.rm = TRUE)
  r03_mean = mean(bin_df$r, na.rm = TRUE)
  r03_sd = sd(bin_df$r, na.rm = TRUE)
  re03_mean = mean(bin_df$re, na.rm = TRUE)
  re03_sd = sd(bin_df$re, na.rm = TRUE)
  nir03_mean = mean(bin_df$nir, na.rm = TRUE)
  nir03_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi03_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi03_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre03_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre03_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi03_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi03_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 4)
  
  b04_mean = mean(bin_df$b, na.rm = TRUE)
  b04_sd = sd(bin_df$b, na.rm = TRUE)
  g04_mean = mean(bin_df$g, na.rm = TRUE)
  g04_sd = sd(bin_df$g, na.rm = TRUE)
  r04_mean = mean(bin_df$r, na.rm = TRUE)
  r04_sd = sd(bin_df$r, na.rm = TRUE)
  re04_mean = mean(bin_df$re, na.rm = TRUE)
  re04_sd = sd(bin_df$re, na.rm = TRUE)
  nir04_mean = mean(bin_df$nir, na.rm = TRUE)
  nir04_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi04_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi04_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre04_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre04_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi04_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi04_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 5)
  
  b05_mean = mean(bin_df$b, na.rm = TRUE)
  b05_sd = sd(bin_df$b, na.rm = TRUE)
  g05_mean = mean(bin_df$g, na.rm = TRUE)
  g05_sd = sd(bin_df$g, na.rm = TRUE)
  r05_mean = mean(bin_df$r, na.rm = TRUE)
  r05_sd = sd(bin_df$r, na.rm = TRUE)
  re05_mean = mean(bin_df$re, na.rm = TRUE)
  re05_sd = sd(bin_df$re, na.rm = TRUE)
  nir05_mean = mean(bin_df$nir, na.rm = TRUE)
  nir05_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi05_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi05_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre05_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre05_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi05_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi05_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 6)
  
  b06_mean = mean(bin_df$b, na.rm = TRUE)
  b06_sd = sd(bin_df$b, na.rm = TRUE)
  g06_mean = mean(bin_df$g, na.rm = TRUE)
  g06_sd = sd(bin_df$g, na.rm = TRUE)
  r06_mean = mean(bin_df$r, na.rm = TRUE)
  r06_sd = sd(bin_df$r, na.rm = TRUE)
  re06_mean = mean(bin_df$re, na.rm = TRUE)
  re06_sd = sd(bin_df$re, na.rm = TRUE)
  nir06_mean = mean(bin_df$nir, na.rm = TRUE)
  nir06_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi06_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi06_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre06_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre06_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi06_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi06_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 7)
  
  b07_mean = mean(bin_df$b, na.rm = TRUE)
  b07_sd = sd(bin_df$b, na.rm = TRUE)
  g07_mean = mean(bin_df$g, na.rm = TRUE)
  g07_sd = sd(bin_df$g, na.rm = TRUE)
  r07_mean = mean(bin_df$r, na.rm = TRUE)
  r07_sd = sd(bin_df$r, na.rm = TRUE)
  re07_mean = mean(bin_df$re, na.rm = TRUE)
  re07_sd = sd(bin_df$re, na.rm = TRUE)
  nir07_mean = mean(bin_df$nir, na.rm = TRUE)
  nir07_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi07_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi07_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre07_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre07_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi07_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi07_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 8)
  
  b08_mean = mean(bin_df$b, na.rm = TRUE)
  b08_sd = sd(bin_df$b, na.rm = TRUE)
  g08_mean = mean(bin_df$g, na.rm = TRUE)
  g08_sd = sd(bin_df$g, na.rm = TRUE)
  r08_mean = mean(bin_df$r, na.rm = TRUE)
  r08_sd = sd(bin_df$r, na.rm = TRUE)
  re08_mean = mean(bin_df$re, na.rm = TRUE)
  re08_sd = sd(bin_df$re, na.rm = TRUE)
  nir08_mean = mean(bin_df$nir, na.rm = TRUE)
  nir08_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi08_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi08_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre08_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre08_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi08_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi08_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 9)
  
  b09_mean = mean(bin_df$b, na.rm = TRUE)
  b09_sd = sd(bin_df$b, na.rm = TRUE)
  g09_mean = mean(bin_df$g, na.rm = TRUE)
  g09_sd = sd(bin_df$g, na.rm = TRUE)
  r09_mean = mean(bin_df$r, na.rm = TRUE)
  r09_sd = sd(bin_df$r, na.rm = TRUE)
  re09_mean = mean(bin_df$re, na.rm = TRUE)
  re09_sd = sd(bin_df$re, na.rm = TRUE)
  nir09_mean = mean(bin_df$nir, na.rm = TRUE)
  nir09_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi09_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi09_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre09_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre09_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi09_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi09_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  bin_df <- full_df %>%
    filter(bin == 10)
  
  b10_mean = mean(bin_df$b, na.rm = TRUE)
  b10_sd = sd(bin_df$b, na.rm = TRUE)
  g10_mean = mean(bin_df$g, na.rm = TRUE)
  g10_sd = sd(bin_df$g, na.rm = TRUE)
  r10_mean = mean(bin_df$r, na.rm = TRUE)
  r10_sd = sd(bin_df$r, na.rm = TRUE)
  re10_mean = mean(bin_df$re, na.rm = TRUE)
  re10_sd = sd(bin_df$re, na.rm = TRUE)
  nir10_mean = mean(bin_df$nir, na.rm = TRUE)
  nir10_sd = sd(bin_df$nir, na.rm = TRUE)
  ndvi10_mean = mean(bin_df$ndvi, na.rm = TRUE)
  ndvi10_sd = sd(bin_df$ndvi, na.rm = TRUE)
  ndre10_mean = mean(bin_df$ndre, na.rm = TRUE)
  ndre10_sd = sd(bin_df$ndre, na.rm = TRUE)
  gndvi10_mean = mean(bin_df$gndvi, na.rm = TRUE)
  gndvi10_sd = sd(bin_df$gndvi, na.rm = TRUE)
  
  spectral_metrics = list(
    b_mean = b_mean,
    b_sd = b_sd,
    g_mean = g_mean,
    g_sd = g_sd,
    r_mean = r_mean,
    r_sd = r_sd,
    re_mean = re_mean,
    re_sd = re_sd,
    nir_mean = nir_mean,
    nir_sd = nir_sd,
    ndvi_mean = ndvi_mean,
    ndvi_sd = ndvi_sd,
    ndre_mean = ndre_mean,
    ndre_sd = ndre_sd,
    gndvi_mean = gndvi_mean,
    gndvi_sd = gndvi_sd,
    b01_mean = b01_mean,
    b01_sd = b01_sd,
    g01_mean = g01_mean,
    g01_sd = g01_sd,
    r01_mean = r01_mean,
    r01_sd = r01_sd,
    re01_mean = re01_mean,
    re01_sd = re01_sd,
    nir01_mean = nir01_mean,
    nir01_sd = nir01_sd,
    ndvi01_mean = ndvi01_mean,
    ndvi01_sd = ndvi01_sd,
    ndre01_mean = ndre01_mean,
    ndre01_sd = ndre01_sd,
    gndvi01_mean = gndvi01_mean,
    gndvi01_sd = gndvi01_sd,
    b02_mean = b02_mean,
    b02_sd = b02_sd,
    g02_mean = g02_mean,
    g02_sd = g02_sd,
    r02_mean = r02_mean,
    r02_sd = r02_sd,
    re02_mean = re02_mean,
    re02_sd = re02_sd,
    nir02_mean = nir02_mean,
    nir02_sd = nir02_sd,
    ndvi02_mean = ndvi02_mean,
    ndvi02_sd = ndvi02_sd,
    ndre02_mean = ndre02_mean,
    ndre02_sd = ndre02_sd,
    gndvi02_mean = gndvi02_mean,
    gndvi02_sd = gndvi02_sd,
    b03_mean = b03_mean,
    b03_sd = b03_sd,
    g03_mean = g03_mean,
    g03_sd = g03_sd,
    r03_mean = r03_mean,
    r03_sd = r03_sd,
    re03_mean = re03_mean,
    re03_sd = re03_sd,
    nir03_mean = nir03_mean,
    nir03_sd = nir03_sd,
    ndvi03_mean = ndvi03_mean,
    ndvi03_sd = ndvi03_sd,
    ndre03_mean = ndre03_mean,
    ndre03_sd = ndre03_sd,
    gndvi03_mean = gndvi03_mean,
    gndvi03_sd = gndvi03_sd,
    b04_mean = b04_mean,
    b04_sd = b04_sd,
    g04_mean = g04_mean,
    g04_sd = g04_sd,
    r04_mean = r04_mean,
    r04_sd = r04_sd,
    re04_mean = re04_mean,
    re04_sd = re04_sd,
    nir04_mean = nir04_mean,
    nir04_sd = nir04_sd,
    ndvi04_mean = ndvi04_mean,
    ndvi04_sd = ndvi04_sd,
    ndre04_mean = ndre04_mean,
    ndre04_sd = ndre04_sd,
    gndvi04_mean = gndvi04_mean,
    gndvi04_sd = gndvi04_sd,
    b05_mean = b05_mean,
    b05_sd = b05_sd,
    g05_mean = g05_mean,
    g05_sd = g05_sd,
    r05_mean = r05_mean,
    r05_sd = r05_sd,
    re05_mean = re05_mean,
    re05_sd = re05_sd,
    nir05_mean = nir05_mean,
    nir05_sd = nir05_sd,
    ndvi05_mean = ndvi05_mean,
    ndvi05_sd = ndvi05_sd,
    ndre05_mean = ndre05_mean,
    ndre05_sd = ndre05_sd,
    gndvi05_mean = gndvi05_mean,
    gndvi05_sd = gndvi05_sd,
    b06_mean = b06_mean,
    b06_sd = b06_sd,
    g06_mean = g06_mean,
    g06_sd = g06_sd,
    r06_mean = r06_mean,
    r06_sd = r06_sd,
    re06_mean = re06_mean,
    re06_sd = re06_sd,
    nir06_mean = nir06_mean,
    nir06_sd = nir06_sd,
    ndvi06_mean = ndvi06_mean,
    ndvi06_sd = ndvi06_sd,
    ndre06_mean = ndre06_mean,
    ndre06_sd = ndre06_sd,
    gndvi06_mean = gndvi06_mean,
    gndvi06_sd = gndvi06_sd,
    b07_mean = b07_mean,
    b07_sd = b07_sd,
    g07_mean = g07_mean,
    g07_sd = g07_sd,
    r07_mean = r07_mean,
    r07_sd = r07_sd,
    re07_mean = re07_mean,
    re07_sd = re07_sd,
    nir07_mean = nir07_mean,
    nir07_sd = nir07_sd,
    ndvi07_mean = ndvi07_mean,
    ndvi07_sd = ndvi07_sd,
    ndre07_mean = ndre07_mean,
    ndre07_sd = ndre07_sd,
    gndvi07_mean = gndvi07_mean,
    gndvi07_sd = gndvi07_sd,
    b08_mean = b08_mean,
    b08_sd = b08_sd,
    g08_mean = g08_mean,
    g08_sd = g08_sd,
    r08_mean = r08_mean,
    r08_sd = r08_sd,
    re08_mean = re08_mean,
    re08_sd = re08_sd,
    nir08_mean = nir08_mean,
    nir08_sd = nir08_sd,
    ndvi08_mean = ndvi08_mean,
    ndvi08_sd = ndvi08_sd,
    ndre08_mean = ndre08_mean,
    ndre08_sd = ndre08_sd,
    gndvi08_mean = gndvi08_mean,
    gndvi08_sd = gndvi08_sd,
    b09_mean = b09_mean,
    b09_sd = b09_sd,
    g09_mean = g09_mean,
    g09_sd = g09_sd,
    r09_mean = r09_mean,
    r09_sd = r09_sd,
    re09_mean = re09_mean,
    re09_sd = re09_sd,
    nir09_mean = nir09_mean,
    nir09_sd = nir09_sd,
    ndvi09_mean = ndvi09_mean,
    ndvi09_sd = ndvi09_sd,
    ndre09_mean = ndre09_mean,
    ndre09_sd = ndre09_sd,
    gndvi09_mean = gndvi09_mean,
    gndvi09_sd = gndvi09_sd,
    b10_mean = b10_mean,
    b10_sd = b10_sd,
    g10_mean = g10_mean,
    g10_sd = g10_sd,
    r10_mean = r10_mean,
    r10_sd = r10_sd,
    re10_mean = re10_mean,
    re10_sd = re10_sd,
    nir10_mean = nir10_mean,
    nir10_sd = nir10_sd,
    ndvi10_mean = ndvi10_mean,
    ndvi10_sd = ndvi10_sd,
    ndre10_mean = ndre10_mean,
    ndre10_sd = ndre10_sd,
    gndvi10_mean = gndvi10_mean,
    gndvi10_sd = gndvi10_sd
  )
  
  
  return(c(
    height_metrics,
    vertical_densities,
    canopy_cover,
    spectral_metrics
  ))
  
}