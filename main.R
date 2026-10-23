# RStudio adaptation of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025), adapted to our study.
# The State-Dependent Effects of Fiscal Policy in a Commodity-Exporting Economy

rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

D <- load_data(file.path("data", "data_peru.csv"))
Nodraws <- 5000
burn <- floor(0.10 * Nodraws)
seed <- 1234
regXP <- c("Low Xport Prices Regime", "High Xport Prices Regime")
summ <- character(0)
fig <- function(name) file.path("figures", name)

## 1. Baseline: x = [PX, G, GDP, T], 1997Q1-2019Q4, p = 1, two regimes
set.seed(seed)
S <- sample_window(D, 1997, 1, 2019, 4)
Y <- cbind(S$PX, S$G, S$GDP, S$T)
lagsel <- lag_selection(Y, 4)
bic <- which.min(lagsel[, 3])
model <- setup_model(Y, 1, S$PX)
est <- estimate_threshold(model, 2, 500)
thr <- est$thr
summ <- c(summ, sprintf("Baseline: BIC lag = %d, threshold = %.4f, low = %.1f%%, high = %.1f%%",
                        bic, thr, 100 * mean(S$PX <= thr), 100 * mean(S$PX > thr)))

dst <- S$date[model$VarLag:(nrow(S) - 1)]
plot_regime_series(fig("fig01_regimes"), S$date, list(S$PX, S$G, S$T, S$GDP),
                   c("Export Prices", "Government Spending", "Taxes Revenue", "GDP"),
                   dst, list(model$state > thr), list(c(0.85, 0.55, 0.10)), 0.25, 2, 2)

linG <- linear_irfs(model, 2, 20, Nodraws)
linT <- linear_irfs(model, 4, 20, Nodraws)
plot_irf_linear(fig("fig02_linear_G"), linG, c(2, 3), c("Government Spending (G)", "GDP"))
plot_irf_linear(fig("fig03_linear_T"), linT, c(4, 3), c("Taxes Revenue (T)", "GDP"))

samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 1, 2, 40, burn)
irfT <- regime_irfs(samples, 1, 4, 40, burn)
plot_irf_regimes(fig("fig04_setvar_G"), irfG, c(2, 3), c("Government Spending", "GDP"), regXP)
plot_irf_regimes(fig("fig05_setvar_T"), irfT, c(4, 3), c("Taxes Revenue", "GDP"), regXP)

mom <- regime_moments(samples, model, 90)
vn <- c("Export Prices", "Government Spending", "GDP", "Taxes Revenue")
lin <- function(a, b) seq(a, b, length.out = 5000)
plot_densities(fig("fig06_means"), mom$lmean, rbind(lin(-10, 40), lin(-10, 20), lin(-3, 12), lin(-3, 30)), vn)
plot_densities(fig("fig07_volatilities"), mom$sd, rbind(lin(0, 40), lin(2, 20), lin(2, 10), lin(2, 20)), vn)
plot_persistence(fig("fig08_persistence"), mom$pred, c(1, 4, 3, 2),
                 c("Export Prices", "Taxes Revenue", "GDP", "Government Spending"), c(1, 4, 8), c("Low", "High"))
cr <- regime_correlations(samples, model)
plot_correlations(fig("fig09_correlations"), cr$corr, cr$pairs, c("PX", "G", "PBI", "T"), c("Low", "High"))
saveRDS(list(thr = thr, post = est$posterior, gam = est$gam, lagsel = lagsel, linG = linG, linT = linT,
             irfG = irfG, irfT = irfT), file.path("results", "baseline.rds"))

## 2. EMBIG: x = [PX, EMBIG, G, GDP, T], 1998Q1-2019Q4
set.seed(seed)
S <- sample_window(D, 1998, 1, 2019, 4)
Y <- cbind(S$PX, S$EMBIG, S$G, S$GDP, S$T)
lagsel <- lag_selection(Y, 4)
bic <- which.min(lagsel[, 3])
model <- setup_model(Y, 1, S$PX)
thr <- estimate_threshold(model, 2, 500)$thr
summ <- c(summ, sprintf("EMBIG: BIC lag = %d, threshold = %.4f", bic, thr))
dst <- S$date[model$VarLag:(nrow(S) - 1)]
plot_regime_series(fig("fig10_regimes_embig"), S$date, list(S$PX, S$EMBIG, S$G, S$T, S$GDP),
                   c("Export Prices", "EMBIG", "Government Spending", "Taxes Revenue", "GDP"),
                   dst, list(model$state > thr), list(c(0.85, 0.55, 0.10)), 0.25, 2, 3)
samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 1, 3, 30, burn)
irfT <- regime_irfs(samples, 1, 5, 30, burn)
plot_irf_regimes(fig("fig11_embig_G"), irfG, c(3, 4), c("Government Spending", "GDP"), regXP)
plot_irf_regimes(fig("fig12_embig_T"), irfT, c(5, 4), c("Taxes Revenue", "GDP"), regXP)
saveRDS(list(thr = thr, lagsel = lagsel, irfG = irfG, irfT = irfT), file.path("results", "embig.rds"))

## 3. Three regimes: estimated on 1997Q1-2019Q4, classification plotted up to 2025Q4
set.seed(seed)
S <- sample_window(D, 1997, 1, 2019, 4)
Y <- cbind(S$PX, S$G, S$GDP, S$T)
model <- setup_model(Y, 1, S$PX)
est3 <- estimate_threshold(model, 3, 100)
thr <- est3$thr
summ <- c(summ, sprintf("Three regimes: thresholds = %.4f, %.4f", thr[1], thr[2]))
FS <- sample_window(D, 1997, 1, 2025, 4)
sF <- FS$PX[1:(nrow(FS) - 1)]
dst <- FS$date[1:(nrow(FS) - 1)]
plot_regime_series(fig("fig13_regimes_three"), FS$date, list(FS$PX, FS$G, FS$T, FS$GDP),
                   c("Export Prices", "Government Spending", "Taxes Revenue", "GDP"), dst,
                   list(sF > thr[1] & sF <= thr[2], sF > thr[2]), list(c(0.65, 0.80, 0.65), c(0.90, 0.70, 0.55)), 0.35, 2, 2)
samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 1, 2, 16, 0)
irfT <- regime_irfs(samples, 1, 4, 16, 0)
reg3 <- c("Low Xport Prices Regime", "Middle Xport Prices Regime", "High Xport Prices Regime")
plot_irf_mosaic(fig("fig14_three_G"), irfG, c(2, 3), c("Government Spending", "GDP"), reg3)
plot_irf_mosaic(fig("fig15_three_T"), irfT, c(4, 3), c("Taxes Revenue", "GDP"), reg3)
saveRDS(list(thr = thr, post3 = est3$posterior, gam3 = est3$gam, irfG = irfG, irfT = irfT), file.path("results", "three_regimes.rds"))

## 4. Terms of trade as state variable: x = [TOT, G, GDP, T]
set.seed(seed)
S <- sample_window(D, 1997, 1, 2019, 4)
Y <- cbind(S$TOT, S$G, S$GDP, S$T)
lagsel <- lag_selection(Y, 4)
bic <- which.min(lagsel[, 3])
model <- setup_model(Y, 1, S$TOT)
thr <- estimate_threshold(model, 2, 500)$thr
summ <- c(summ, sprintf("Terms of trade: BIC lag = %d, threshold = %.4f", bic, thr))
samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 1, 2, 40, burn)
irfT <- regime_irfs(samples, 1, 4, 40, burn)
regTOT <- c("Low ToT Regime", "High ToT Regime")
plot_irf_regimes(fig("figA1_tot_G"), irfG, c(2, 3), c("Government Spending", "GDP"), regTOT)
plot_irf_regimes(fig("figA2_tot_T"), irfT, c(4, 3), c("Taxes Revenue", "GDP"), regTOT)
saveRDS(list(thr = thr, lagsel = lagsel, irfG = irfG, irfT = irfT), file.path("results", "tot.rds"))

## 5. Alternative ordering: x = [PX, T, G, GDP]
set.seed(seed)
S <- sample_window(D, 1997, 1, 2019, 4)
Y <- cbind(S$PX, S$T, S$G, S$GDP)
lagsel <- lag_selection(Y, 4)
bic <- which.min(lagsel[, 3])
model <- setup_model(Y, 1, S$PX)
thr <- estimate_threshold(model, 2, 500)$thr
summ <- c(summ, sprintf("Alternative ordering: BIC lag = %d, threshold = %.4f", bic, thr))
samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 1, 3, 40, burn)
irfT <- regime_irfs(samples, 1, 2, 40, burn)
plot_irf_regimes(fig("rob_order_G"), irfG, c(3, 4), c("Government Spending", "GDP"), regXP)
plot_irf_regimes(fig("rob_order_T"), irfT, c(2, 4), c("Taxes Revenue", "GDP"), regXP)
saveRDS(list(thr = thr, lagsel = lagsel, irfG = irfG, irfT = irfT), file.path("results", "alt_ordering.rds"))

## 6. Two lags: x = [PX, G, GDP, T], p = 2
set.seed(seed)
S <- sample_window(D, 1997, 1, 2019, 4)
Y <- cbind(S$PX, S$G, S$GDP, S$T)
model <- setup_model(Y, 2, S$PX)
thr <- estimate_threshold(model, 2, 500)$thr
summ <- c(summ, sprintf("Two lags: threshold = %.4f", thr))
samples <- setvar_draws(model, thr, Nodraws)
irfG <- regime_irfs(samples, 2, 2, 40, burn)
irfT <- regime_irfs(samples, 2, 4, 40, burn)
plot_irf_regimes(fig("rob_lag2_G"), irfG, c(2, 3), c("Government Spending", "GDP"), regXP)
plot_irf_regimes(fig("rob_lag2_T"), irfT, c(4, 3), c("Taxes Revenue", "GDP"), regXP)
saveRDS(list(thr = thr, irfG = irfG, irfT = irfT), file.path("results", "two_lags.rds"))

writeLines(summ, file.path("results", "summary.txt"))
cat(summ, sep = "\n")
