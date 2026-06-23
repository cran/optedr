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

## ----kl-mm--------------------------------------------------------------------
result_kl_mm <- opt_des(
  "KL-Optimality",
  model        = y ~ Vmax * x / (Km + x),
  parameters   = c("Vmax", "Km"),
  par_values   = c(2, 1),
  design_space = c(0.1, 5),
  rival_model  = y ~ a * x,
  rival_params = c("a"),
  rival_pars   = c(1),
  rival_lower  = c(0.2),
  rival_upper  = c(2.5),
  family       = "Normal",
  phi          = 1
)
result_kl_mm

## ----kl-mm-summary------------------------------------------------------------
summary(result_kl_mm)

## ----kl-mm-plot, fig.cap = "KL sensitivity function: MM vs linear rival."-----
plot(result_kl_mm)

## ----kl-mm-eff----------------------------------------------------------------
design_unif <- data.frame(
  Point  = c(0.1, 1.3, 2.5, 3.8, 5.0),
  Weight = rep(1/5, 5)
)
eff_kl <- design_efficiency(design_unif, result_kl_mm)
cat("Efficiency of uniform design:", round(eff_kl * 100, 2), "%\n")

## ----kl-poisson---------------------------------------------------------------
result_kl_pois <- opt_des(
  "KL-Optimality",
  model        = y ~ exp(a - b * x),
  parameters   = c("a", "b"),
  par_values   = c(2, 0.5),
  design_space = c(0, 4),
  rival_pars   = c(2, 1.0),
  rival_lower  = c(1.5, 0.8),
  rival_upper  = c(2.5, 1.5),
  family       = "Poisson",
  phi          = 1
)
result_kl_pois
summary(result_kl_pois)

## ----kl-poisson-plot, fig.cap = "KL sensitivity for Poisson decay model."-----
plot(result_kl_pois)

## ----kl-poisson-rival---------------------------------------------------------
hv <- attr(result_kl_pois, "hidden_value")
cat("Optimal rival: a =", round(hv$beta2_star[1], 3),
    " b =", round(hv$beta2_star[2], 3), "\n")

## ----kl-variances-------------------------------------------------------------
kl_fn_var <- make_kl_fun(
  "Normal",
  model1      = y ~ a * exp(-b * x),
  params1     = c("a", "b"),
  par_values1 = c(1, 0.5),
  phi1        = 1,
  family2     = "Normal",
  model2      = y ~ c * exp(-d * x),
  params2     = c("c", "d"),
  phi2        = 4
)

result_kl_var <- opt_des(
  "KL-Optimality",
  model        = y ~ a * exp(-b * x),
  parameters   = c("a", "b"),
  par_values   = c(1, 0.5),
  design_space = c(0, 4),
  kl_fun       = kl_fn_var,
  rival_pars   = c(1, 1.0),
  rival_lower  = c(0.5, 0.8),
  rival_upper  = c(2.0, 1.5)
)
result_kl_var
summary(result_kl_var)

## ----kl-var-plot, fig.cap = "KL sensitivity: Normal phi=1 vs Normal phi=4."----
plot(result_kl_var)

## ----kl-2d--------------------------------------------------------------------
kl_fn_2d <- make_kl_fun(
  "Normal",
  model1      = y ~ Vmax * x1 * x2 / ((Km1 + x1) * (Km2 + x2)),
  params1     = c("Vmax", "Km1", "Km2"),
  par_values1 = c(1, 1, 1),
  model2      = y ~ alpha * x1,
  params2     = "alpha"
)

result_kl_2d <- opt_des(
  "KL-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((Km1 + x1) * (Km2 + x2)),
  parameters   = c("Vmax", "Km1", "Km2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 5), x2 = c(0.1, 5)),
  kl_fun       = kl_fn_2d,
  rival_pars   = c(0.5),
  rival_lower  = c(0.05),
  rival_upper  = c(2.0)
)
result_kl_2d

## ----kl-2d-plot, fig.cap = "KL sensitivity heatmap: 2D MM vs linear rival."----
plot(result_kl_2d)

## ----kl-hill------------------------------------------------------------------
kl_fun_hill <- function(x, beta2) {
  sigma_sq <- beta2[1]
  eta <- (1.70 - 0.137) * (x / 111)^(-1.03) / (1 + (x / 111)^(-1.03)) + 0.137
  0.5 * (eta^2 / sigma_sq - 1 + log(sigma_sq / eta^2))
}

result_kl_hill <- opt_des(
  "KL-Optimality",
  model        = y ~ (Econ - b) * (x / IC50)^s / (1 + (x / IC50)^s) + b,
  parameters   = c("Econ", "b", "IC50", "s"),
  par_values   = c(1.70, 0.137, 111, -1.03),
  design_space = c(0.01, 1500),
  kl_fun       = kl_fun_hill,
  rival_pars   = c(1.0),
  rival_lower  = c(1e-4),
  rival_upper  = c(1e6)
)
result_kl_hill
summary(result_kl_hill)

## ----kl-hill-plot, fig.cap = "KL sensitivity for the Hill model (error structure discrimination)."----
plot(result_kl_hill)

## ----kl-hill-rival------------------------------------------------------------
hv_hill <- attr(result_kl_hill, "hidden_value")
cat("Optimal rival sigma_abs^2 =", round(hv_hill$beta2_star, 4), "\n")

