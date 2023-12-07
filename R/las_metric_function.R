als_cld_metrics <- function(z) {
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
  
  return(c(
    height_metrics,
    vertical_densities,
    canopy_cover
  ))
  
}