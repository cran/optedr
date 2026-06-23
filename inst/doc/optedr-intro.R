## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6,
  fig.height = 4,
  out.width  = "90%"
)

## ----setup--------------------------------------------------------------------
library(optedr)

## ----d-optimal----------------------------------------------------------------
result_D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)
result_D

## ----d-plot, fig.cap = "Sensitivity function for the D-optimal design."-------
plot(result_D)

## ----d-summary----------------------------------------------------------------
summary(result_D)

## ----ds-optimal---------------------------------------------------------------
result_Ds <- opt_des(
  criterion    = "Ds-Optimality",
  model        = y ~ th0 * exp(x / th1),
  parameters   = c("th0", "th1"),
  par_values   = c(10.4963, -3.2940),
  design_space = c(0.94, 30),
  par_int      = c(1)
)
result_Ds

## ----a-optimal----------------------------------------------------------------
result_A <- opt_des(
  criterion    = "A-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)
result_A

## ----i-optimal----------------------------------------------------------------
result_I <- opt_des(
  criterion    = "I-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  reg_int      = c(380, 422)
)
result_I

## ----l-optimal----------------------------------------------------------------
result_L <- opt_des(
  criterion    = "L-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  matB         = diag(c(1, 0))
)
result_L

## ----l-compare----------------------------------------------------------------
cat("D-optimal support:\n"); print(result_D$optdes)
cat("L-optimal support:\n"); print(result_L$optdes)

## ----2d-optimal---------------------------------------------------------------
result_2D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10))
)
result_2D

## ----2d-plot, fig.cap = "Sensitivity heatmap for the 2D D-optimal design."----
plot(result_2D)

## ----2d-l---------------------------------------------------------------------
result_2D_L <- opt_des(
  criterion    = "L-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  matB         = diag(c(0, 1, 0))
)
result_2D_L

## ----3d-optimal---------------------------------------------------------------
result_3D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 * x3 / ((K1+x1) * (K2+x2) * (K3+x3)),
  parameters   = c("Vmax", "K1", "K2", "K3"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10))
)
result_3D
plot(result_3D)

## ----compound-----------------------------------------------------------------
result_DI <- opt_des(
  criterion    = "Compound",
  model        = y ~ 10^(a - b / (c + x)),
  parameters   = c("a", "b", "c"),
  par_values   = c(8.07131, 1730.63, 233.426),
  design_space = c(1, 100),
  compound     = list(
    list(criterion = "D-Optimality", weight = 0.7),
    list(criterion = "I-Optimality", weight = 0.3, reg_int = c(60, 100))
  )
)
result_DI

## ----compound-compare---------------------------------------------------------
result_D_ant <- opt_des(
  "D-Optimality",
  y ~ 10^(a - b / (c + x)), c("a", "b", "c"),
  c(8.07131, 1730.63, 233.426), c(1, 100)
)
cat("D-optimal:\n");         print(result_D_ant$optdes)
cat("Compound D+I (70/30):\n"); print(result_DI$optdes)

## ----efficiency---------------------------------------------------------------
design_ad_hoc <- data.frame(
  Point  = c(220, 300, 400),
  Weight = c(1/3, 1/3, 1/3)
)
eff <- design_efficiency(design_ad_hoc, result_D)
cat("Efficiency of ad-hoc design:", round(eff * 100, 2), "%\n")

## ----efficiency-2d------------------------------------------------------------
corners_2d <- data.frame(
  x1     = c(0.1, 10,  0.1, 10),
  x2     = c(0.1, 0.1, 10,  10),
  Weight = rep(0.25, 4)
)
eff_2d <- design_efficiency(corners_2d, result_2D)
cat("Efficiency of corner design vs 2D D-optimal:", round(eff_2d * 100, 2), "%\n")

## ----efficient-round----------------------------------------------------------
exact_design <- efficient_round(result_D$optdes, n = 20)
print(exact_design)
cat("Total observations:", sum(exact_design$Weight), "\n")

## ----combo-round--------------------------------------------------------------
combo_design <- combinatorial_round(result_D, n = 10)
print(combo_design)

## ----round-eff----------------------------------------------------------------
approx <- exact_design
approx$Weight <- approx$Weight / sum(approx$Weight)
cat("Efficiency of rounded design:", round(design_efficiency(approx, result_D) * 100, 2), "%\n")

