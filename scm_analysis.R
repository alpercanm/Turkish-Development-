###############################################################################
# Synthetic Control Method - 4 models
#   M1: Blob_U1, opq_pc, 2015      M3: Blob_U1, gdp, 2012
#   M2: Blob_U2, opq_pc, 2015      M4: Blob_U2, gdp, 2012
###############################################################################

library(Synth)
library(readxl)

## ---------------------------------------------------------------- 1. DATA ---
panel_groups <- as.data.frame(read_excel(
  "Dissertation_dataset_current_prices_USD.xlsx", sheet = "panel_groups"))


panel_groups$opq_pc     <- panel_groups$OPQ * 1000 / panel_groups$population  # USD p.c.
panel_groups$gdp        <- panel_groups$GDP                                   # thousand USD
panel_groups$sh_A       <- panel_groups$A    / panel_groups$GDP
panel_groups$sh_BCDE    <- panel_groups$BCDE / panel_groups$GDP
panel_groups$sh_GHI     <- panel_groups$GHI  / panel_groups$GDP
panel_groups$sh_F       <- panel_groups$F    / panel_groups$GDP
panel_groups$phys_p1000 <- panel_groups$total_physician / panel_groups$population * 1000

U1 <- 101
U2 <- 102

# Donor pool: everything except the two blobs, the 7 SUTVA neighbours and 34.
excl   <- c(12, 23, 36, 56, 63, 76, 34, 79)
donors <- sort(setdiff(unique(panel_groups$plate), c(U1, U2, excl)))   

PRED  <- c("population_density", "sh_A", "sh_BCDE", "sh_GHI", "sh_F")
YEARS <- 2000:2024


###############################################################################
## MODEL 1 - Blob_U1, opq_pc, treatment 2015
#################################################
dp1 <- dataprep(
  foo                   = panel_groups,
  predictors            = PRED,
  predictors.op         = "mean",
  time.predictors.prior = 2000:2014,
  special.predictors    = list(
    list("gdp",        2002,      "mean"),
    list("gdp",        2011,      "mean"),
    list("phys_p1000", 2002:2014, "mean"),
    list("mys_male",   2008:2014, "mean")),
  dependent             = "opq_pc",
  unit.variable         = "plate",
  unit.names.variable   = "province",
  time.variable         = "year",
  treatment.identifier  = U1,
  controls.identifier   = donors,
  time.optimize.ssr     = 2000:2014,
  time.plot             = YEARS)

s1  <- synth(data.prep.obj = dp1)
t1  <- synth.tab(synth.res = s1, dataprep.res = dp1)

t1$tab.pred                                                #  balance table
t1$tab.loss
w1 <- t1$tab.w[order(-t1$tab.w$w.weights), ]               #  donor weights
w1[w1$w.weights >= 0.001, ]

path.plot(synth.res = s1, dataprep.res = dp1,              # path plot
          Ylab = "OPQ per capita (USD)", Xlab = "Year", Main = "Muş, Diyarbakır, Aggrı, Bitlis, Tunceli",
          Legend = c("U1", "Synthetic Blob_U1"), Legend.position = "topleft")
abline(v = 2015, lty = 2)

gaps.plot(synth.res = s1, dataprep.res = dp1,
          Ylab = "Gap in OPQ per capita (USD)", Xlab = "Year", Main = "Muş, Diyarbakır, Aggrı, Bitlis, Tunceli")
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
## MODEL 2 - Blob_U2, opq_pc, treatment 2015
###############################################################################
dp2 <- dataprep(
  foo                   = panel_groups,
  predictors            = PRED,
  predictors.op         = "mean",
  time.predictors.prior = 2000:2014,
  special.predictors    = list(
    list("gdp",        2002,      "mean"),
    list("gdp",        2011,      "mean"),
    list("phys_p1000", 2002:2014, "mean"),
    list("mys_male",   2008:2014, "mean")),
  dependent             = "opq_pc",
  unit.variable         = "plate",
  unit.names.variable   = "province",
  time.variable         = "year",
  treatment.identifier  = U2,
  controls.identifier   = donors,
  time.optimize.ssr     = 2000:2014,
  time.plot             = YEARS)

s2  <- synth(data.prep.obj = dp2)
t2  <- synth.tab(synth.res = s2, dataprep.res = dp2)

t2$tab.pred
t2$tab.loss
w2 <- t2$tab.w[order(-t2$tab.w$w.weights), ]
w2[w2$w.weights >= 0.001, ]

path.plot(synth.res = s2, dataprep.res = dp2,
          Ylab = "OPQ per capita (USD)", Xlab = "Year",
          Legend = c("U2", "Synthetic Blob_U2"), Legend.position = "topleft")
abline(v = 2015, lty = 2)

gaps.plot(synth.res = s2, dataprep.res = dp2,
          Ylab = "Gap in OPQ per capita (USD)", Xlab = "Year")
abline(v = 2015, lty = 2)

synthetic_outcome2 <- dp2$Y0plot %*% s2$solution.w

# the gap (Treated - Synthetic)
gaps2 <- dp2$Y1plot - synthetic_outcome2


gap_table2 <- data.frame(
  Year = as.numeric(rownames(gaps2)),
  Treated = as.numeric(dp2$Y1plot),
  Synthetic = as.numeric(synthetic_outcome2),
  Gap = as.numeric(gaps2)
)


print(max(gap_table2$Gap))
print(min(gap_table2$Gap))

print(gap_table2)



###############################################################################
## MODEL 3 - Blob_U1, gdp, treatment 2012
###############################################################################
dp3 <- dataprep(
  foo                   = panel_groups,
  predictors            = PRED,
  predictors.op         = "mean",
  time.predictors.prior = 2000:2011,  
  special.predictors    = list(
    list("gdp",        2003,      "mean"),   
    list("gdp",        2011,      "mean"),
    list("phys_p1000", 2002:2011, "mean"),   
    list("mys_male",   2008:2011, "mean")),  
  dependent             = "gdp",
  unit.variable         = "plate",
  unit.names.variable   = "province",
  time.variable         = "year",
  treatment.identifier  = U1,
  controls.identifier = donors,
  time.optimize.ssr     = 2000:2011,
  time.plot             = YEARS)
s3  <- synth(data.prep.obj = dp3)
t3  <- synth.tab(synth.res = s3, dataprep.res = dp3)

t3$tab.pred
t3$tab.loss
w3 <- t3$tab.w[order(-t3$tab.w$w.weights), ]
w3[w3$w.weights >= 0.001, ]

path.plot(synth.res = s3, dataprep.res = dp3,
          Ylab = "GDP (thousand USD)", Xlab = "Year", Main = "Muş, Diyarbakır, Aggrı, Bitlis, Tunceli",
          Legend = c("U1", "Synthetic Blob_U1"), Legend.position = "topleft")
abline(v = 2012, lty = 2)

gaps.plot(synth.res = s3, dataprep.res = dp3,
          Ylab = "Gap in GDP (thousand USD)", Xlab = "Year", Main = "Muş, Diyarbakır, Aggrı, Bitlis, Tunceli")
abline(v = 2012, lty = 2)


synthetic_outcome3 <- dp3$Y0plot %*% s3$solution.w


gaps3 <- dp3$Y1plot - synthetic_outcome3


gap_table3 <- data.frame(
  Year = as.numeric(rownames(gaps3)),
  Treated = as.numeric(dp3$Y1plot),
  Synthetic = as.numeric(synthetic_outcome3),
  Gap = as.numeric(gaps3)
)


print(gap_table3)
print(max(gap_table3$Gap))
print(min(gap_table3$Gap))
###############################################################################
## MODEL 4 - Blob_U2, gdp, treatment 2012
###############################################################################
dp4 <- dataprep(
  foo                   = panel_groups,
  predictors            = PRED,
  predictors.op         = "mean",
  time.predictors.prior = 2000:2011,
  special.predictors    = list(
    list("gdp",        2002,      "mean"),
    list("gdp",        2011,      "mean"),
    list("phys_p1000", 2002:2014, "mean"),
    list("mys_male",   2008:2014, "mean")),
  dependent             = "gdp",
  unit.variable         = "plate",
  unit.names.variable   = "province",
  time.variable         = "year",
  treatment.identifier  = U2,
  controls.identifier   = donors,
  time.optimize.ssr     = 2000:2011,
  time.plot             = YEARS)

s4  <- synth(data.prep.obj = dp4)
t4  <- synth.tab(synth.res = s4, dataprep.res = dp4)

t4$tab.pred
t4$tab.loss
w4 <- t4$tab.w[order(-t4$tab.w$w.weights), ]
w4[w4$w.weights >= 0.001, ]

path.plot(synth.res = s4, dataprep.res = dp4,
          Ylab = "GDP (thousand USD)", Xlab = "Year", Main = "Şırnak, Hakkari, Batman, Mardin, Van",
          Legend = c("U2", "Synthetic Blob_U2"), Legend.position = "topleft")
abline(v = 2012, lty = 2)

gaps.plot(synth.res = s4, dataprep.res = dp4,
          Ylab = "Gap in GDP (thousand USD)", Xlab = "Year", Main = "Şırnak, Hakkari, Batman, Mardin, Van")
abline(v = 2012, lty = 2)

synthetic_outcome4 <- dp4$Y0plot %*% s4$solution.w


gaps4 <- dp4$Y1plot - synthetic_outcome4


gap_table4 <- data.frame(
  Year = as.numeric(rownames(gaps4)),
  Treated = as.numeric(dp4$Y1plot),
  Synthetic = as.numeric(synthetic_outcome4),
  Gap = as.numeric(gaps4)
)


print(gap_table4)
print(max(gap_table4$Gap))
print(min(gap_table4$Gap))

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
        special.predictors    = list(
          list("gdp",        2002,           "mean"),
          list("gdp",        2011,           "mean"),
          list("phys_p1000", 2002:pre_end,   "mean"),
          list("mys_male",   2008:pre_end,   "mean")),
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

pl1 <- placebo("opq_pc", U1, 2014)
pl2 <- placebo("opq_pc", U2, 2014)
pl3 <- placebo("gdp",    U1, 2011)
pl4 <- placebo("gdp",    U2, 2011)

placebo_plot(pl1, U1, 2015, "Gap in OPQ per capita (USD)")
rmspe_ratio(pl1, U1, 2014)

placebo_plot(pl2, U2, 2015, "Gap in OPQ per capita (USD)")
rmspe_ratio(pl2, U2, 2014)

placebo_plot(pl3, U1, 2012, "Gap in GDP (thousand USD)")
rmspe_ratio(pl3, U1, 2011)

placebo_plot(pl4, U2, 2012, "Gap in GDP (thousand USD)")
rmspe_ratio(pl4, U2, 2011)

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

round(rbind(M1 = fit(dp1, s1, 2014), M2 = fit(dp2, s2, 2014),
            M3 = fit(dp3, s3, 2011), M4 = fit(dp4, s4, 2011)), 3)
