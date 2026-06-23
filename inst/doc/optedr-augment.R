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

## ----region-1d----------------------------------------------------------------
init_des <- data.frame(
  Point  = c(30, 60, 90),
  Weight = c(1/3, 1/3, 1/3)
)

region <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b / (c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = FALSE,
  delta_val           = 0.85
)
print(region)

## ----augment-1d---------------------------------------------------------------
new_pt <- mean(region$region[1:2])

augmented <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b / (c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = FALSE,
  delta_val           = 0.85,
  new_points          = data.frame(Point = new_pt, Weight = 1)
)
print(augmented)
cat("Sum of weights:", sum(augmented$Weight), "\n")

## ----efficiency-1d------------------------------------------------------------
result_opt <- opt_des(
  "D-Optimality",
  y ~ 10^(a - b / (c + x)), c("a", "b", "c"),
  c(8.07131, 1730.63, 233.426), c(1, 100)
)

eff_before <- design_efficiency(init_des, result_opt)
eff_after  <- design_efficiency(augmented, result_opt)

cat("Efficiency before augmenting:", round(eff_before * 100, 2), "%\n")
cat("Efficiency after augmenting: ", round(eff_after  * 100, 2), "%\n")
cat("Gain:                        ", round((eff_after - eff_before) * 100, 2),
    "percentage points\n")

## ----augment-with-opt, eval=FALSE---------------------------------------------
# region_opt <- get_augment_region(
#   criterion           = "D-Optimality",
#   init_design         = init_des,
#   alpha               = 0.25,
#   model               = y ~ 10^(a - b / (c + x)),
#   parameters          = c("a", "b", "c"),
#   par_values          = c(8.07131, 1730.63, 233.426),
#   design_space        = c(1, 100),
#   calc_optimal_design = TRUE,
#   delta_val           = 0.85
# )

## ----region-2d----------------------------------------------------------------
init_2d <- data.frame(
  x1     = c(0.8, 10, 5),
  x2     = c(10, 0.8, 5),
  Weight = c(1/3, 1/3, 1/3)
)

result_2D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10))
)

region_2d <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.85
)

## ----augment-2d---------------------------------------------------------------
best_2d <- region_2d$region[which.max(region_2d$region$efficiency), ]

eff_antes <- suppressMessages(design_efficiency(init_2d, result_2D))

aug_2d <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.85,
  new_points          = data.frame(x1 = best_2d$x1, x2 = best_2d$x2, Weight = 1)
)

eff_despues <- suppressMessages(design_efficiency(aug_2d, result_2D))

cat("Efficiency before:", round(eff_antes  * 100, 2), "%\n")
cat("Efficiency after: ", round(eff_despues * 100, 2), "%\n")
print(aug_2d)

## ----region-3d----------------------------------------------------------------
init_3d <- data.frame(
  x1     = c(0.8, 10,  10,  0.8, 10),
  x2     = c(10,  0.8, 10,  10,  0.8),
  x3     = c(10,  10,  0.8, 0.8, 10),
  Weight = rep(0.2, 5)
)

region_3d <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_3d,
  alpha               = 0.45,
  model               = y ~ Vmax * x1 * x2 * x3 / ((K1+x1) * (K2+x2) * (K3+x3)),
  parameters          = c("Vmax", "K1", "K2", "K3"),
  par_values          = c(1, 1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.93
)
cat("Number of candidate points:", nrow(region_3d$region), "\n")
plot(region_3d$plot)

## ----augment-ds---------------------------------------------------------------
region_ds <- get_augment_region(
  criterion           = "Ds-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  par_int             = c(1),
  delta_val           = 0.85,
  n_lhs               = 5000
)

best_ds <- region_ds$region[which.max(region_ds$region$efficiency), ]

aug_ds <- augment_design(
  criterion           = "Ds-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  par_int             = c(1),
  delta_val           = 0.85,
  new_points          = data.frame(x1 = best_ds$x1, x2 = best_ds$x2, Weight = 1),
  n_lhs               = 5000
)
print(aug_ds)

