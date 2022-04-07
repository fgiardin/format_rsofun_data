library(dplyr)
library(ingestr)
library(rsofun)

#---- load data ----

# read in s0 driver data
s0 <- readRDS("data/s0_p-model_driver_data.rds")

# load validation data (fluxnet gpp observations)
load("data-raw/ddf_fluxnet_gpp.Rdata")

# grab New Phyt sites
flue_sites <- readr::read_csv( "data-raw/flue_stocker18nphyt.csv" ) %>%
  dplyr::filter( !is.na(cluster) ) %>%
  distinct(site) %>%
  pull(site)

# select calibrations sites
calibsites <- siteinfo_fluxnet2015 %>%
  dplyr::filter(
    !(sitename %in% c("DE-Akm", "IT-Ro1"))
  ) %>%
  dplyr::filter(
    sitename != "FI-Sod"
  ) %>%
  dplyr::filter(
    c4 %in% c(FALSE, NA) &
      classid != "CRO" &
      classid != "WET"
  ) %>%
  dplyr::filter(
    sitename %in% flue_sites
  ) %>%
  pull(sitename)

#---- calibration ----

# set calibration parameters
settings_calib <- list(
  method      = "bayesiantools",
  targetvars  = c("gpp"),
  timescale   = list(targets_obs = "d"),
  metric      = cost_rmse_fullstack,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 500,
      iterations = 3000
    )
  ),
  par = list(
    kphio = list(lower=0.04, upper=0.1, init = 0.05),
    a = list(lower=0, upper=5, init = 3.5),
    b = list(lower=1, upper=5, init=3.5)
  )
)

# subset original data with calibration sites
s0_calib <- s0 %>%
  dplyr::filter(
    sitename %in% calibsites
    )

# optimize parameters
pars <- calib_sofun(
  drivers  = s0_calib,
  obs      = ddf_fluxnet_gpp,
  settings = settings_calib
)

#----- save stuff ----

print(pars)

# save paramteres
saveRDS(
  parss,
  file = "data/s0_p-model_parameters.rds",
  compress = "xz"
  )
