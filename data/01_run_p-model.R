library(tidyverse)
library(rsofun)   # using tag 4.3

#---- whc ----

# optimized parameters from previous
# work
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

whc <- readRDS("data/whc_p-model_driver_data.rds")

# run the model for these parameters
output_whc <- rsofun::runread_pmodel_f(
  whc,
  par = params_modl
)

saveRDS(output_whc, file = "data/whc_p-model_output.rds", compress = "xz")

#---- s0 -----

pars <- readRDS("data/s0_p-model_parameters.rds")  # read from calibration output (script 02)

# optimized parameters from previous
# work
params_modl <- list(
  kphio           = pars$par,
  soilm_par_a     = 0,
  soilm_par_b     = 0,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

s0 <- readRDS("data/s0_p-model_driver_data.rds")

# run the model for these parameters
output_s0 <- rsofun::runread_pmodel_f(
  s0,
  par = params_modl
)

saveRDS(output_s0, file = "data/s0_p-model_output.rds", compress = "xz")

