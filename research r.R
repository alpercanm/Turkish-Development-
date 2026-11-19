###############################################################################
# Synthetic Control Method 
###############################################################################

library(Synth)
library(readxl)

## ---------------------------------------------------------------- 1. DATA ---
panel_groups <- as.data.frame(read_excel(
  "research.xlsx", sheet = "treat"))


panel_groups$sh_A       <- panel_groups$A    / panel_groups$GDP
panel_groups$sh_BCDE    <- panel_groups$BCDE / panel_groups$GDP
panel_groups$sh_GHI     <- panel_groups$GHI  / panel_groups$GDP
panel_groups$sh_F       <- panel_groups$F    / panel_groups$GDP
panel_groups$sh_MN       <- panel_groups$MN    / panel_groups$GDP

panel_groups$plate <- as.numeric(panel_groups$plate)
# Donor pool: everything except the two blobs, the 7 SUTVA neighbours and 34.
excl   <- c(12, 23, 36, 56, 63, 76, 34, 79)
donors <- sort(setdiff(unique(panel_groups$plate), c(100, excl)))   

PRED  <- c("pop_den", "sh_A", "sh_BCDE", "sh_GHI", "sh_F", "sh_MN")
YEARS <- 2000:2024


###############################################################################
## MODEL 1 - 2015
#################################################
dp1 <- dataprep(
  foo                   = panel_groups,
  predictors            = PRED,
  predictors.op         = "mean",
  time.predictors.prior = 2000:2014,
  special.predictors = list(
    list("OPQ_pc", 2000, "mean"),
    list("OPQ_pc", 2005, "mean"),
    list("median_age", 2007:2014, "mean"),
    list("male_female_ratio", 2007:2014, "mean")),
  dependent             = "OPQ_pc",
  unit.variable         = "plate",
  unit.names.variable   = "province",
  time.variable         = "year",
  treatment.identifier  = 100,
  controls.identifier   = donors,
  time.optimize.ssr     = 2000:2014,
  time.plot             = YEARS)

s1  <- synth(data.prep.obj = dp1)
t1  <- synth.tab(synth.res = s1, dataprep.res = dp1)


print(t1$tab.pred, digits = 10)
t1$tab.pred                                                #  balance table
t1$tab.loss
w1 <- t1$tab.w[order(-t1$tab.w$w.weights), ]               #  donor weights
w1[w1$w.weights >= 0.001, ]

path.plot(synth.res = s1, dataprep.res = dp1,              # path plot
          Ylab = "OPQ per capita (USD)", Xlab = "Year", Main = "Treatment Provinces",
          Legend = c("Treatment", "Synthetic"), Legend.position = "topleft")
abline(v = 2015, lty = 2)

gaps.plot(synth.res = s1, dataprep.res = dp1,
          Ylab = "Gap in OPQ per capita (USD)", Xlab = "Year", Main = "Treatment")
abline(v = 2015, lty = 2)


synthetic_outcome1 <- dp1$Y0plot %*% s1$solution.w

# (Treated - Synthetic)
gaps1 <- dp1$Y1plot - synthetic_outcome1


gap_table1 <- data.frame(
  Year = as.numeric(rownames(gaps1)),
  Treated = as.numeric(dp1$Y1plot),
  Synthetic = as.numeric(synthetic_outcome1),
  Gap = as.numeric(gaps1)
)


print(max(gap_table1$Gap))
print(min(gap_table1$Gap))

print(gap_table1)


###############################################################################
## (4) IN-SPACE PLACEBOS - re-run each model with every donor as the treated
###############################################################################
placebo <- function(dep, tr_id, pre_end) {
  units <- c(tr_id, donors)
  gaps  <- matrix(NA, length(YEARS), length(units),
                  dimnames = list(YEARS, units))
  for (u in units) {
    r <- tryCatch({
      dp <- dataprep(
        foo                   = panel_groups,
        predictors            = PRED,
        predictors.op         = "mean",
        time.predictors.prior = 2000:pre_end,
        special.predictors = list(
          list("OPQ_pc", 2000, "mean"),
          list("OPQ_pc", 2005, "mean"),
          list("median_age", 2007:2014, "mean"),
          list("male_female_ratio", 2007:2014, "mean")),
        dependent             = dep,
        unit.variable         = "plate",
        unit.names.variable   = "province",
        time.variable         = "year",
        treatment.identifier  = u,
        controls.identifier   = setdiff(units, u),
        time.optimize.ssr     = 2000:pre_end,
        time.plot             = YEARS)
      s <- synth(data.prep.obj = dp, verbose = FALSE)
      as.numeric(dp$Y1plot - dp$Y0plot %*% s$solution.w)
    }, error = function(e) rep(NA, length(YEARS)))
    gaps[, as.character(u)] <- r
  }
  gaps
}

# RMSPE ratio (post/pre) and the implied one-sided permutation p-value
rmspe_ratio <- function(gaps, tr_id, pre_end) {
  pre  <- as.character(2000:pre_end)
  post <- as.character((pre_end + 1):max(YEARS))
  rr   <- apply(gaps, 2, function(g)
    sqrt(mean(g[post]^2)) / sqrt(mean(g[pre]^2)))
  rr   <- sort(rr[!is.na(rr)], decreasing = TRUE)
  list(ratios  = rr,
       p_value = mean(rr >= rr[as.character(tr_id)]))
}

placebo_plot <- function(gaps, tr_id, tr_year, ylab) {
  matplot(YEARS, gaps, type = "l", lty = 1, col = "gray70",
          xlab = "Year", ylab = ylab)
  lines(YEARS, gaps[, as.character(tr_id)], lwd = 2, col = "black")
  abline(v = tr_year, lty = 2); abline(h = 0)
}

pl1 <- placebo("OPQ_pc", 100, 2014)



placebo_plot(pl1, 100, 2015, "Gap in OPQ per capita (USD)")
rmspe_ratio(pl1, 100, 2014)




fit <- function(dp, s, pre_end) {
  gap <- as.numeric(dp$Y1plot - dp$Y0plot %*% s$solution.w)
  yrs <- as.numeric(rownames(dp$Y1plot))
  pre <- yrs <= pre_end
  pr  <- sqrt(mean(gap[pre]^2))
  c(rmspe_pre = pr,
    rmse_post = sqrt(mean(gap[!pre]^2)),
    ratio     = sqrt(mean(gap[!pre]^2)) / pr,
    pct_mean  = 100 * pr / mean(dp$Y1plot[pre]))
}

sum(is.na(pl1$loss.v))

round(rbind(M1 = fit(dp1, s1, 2014))
      
      
     
      