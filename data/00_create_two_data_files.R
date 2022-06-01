library(Matrix)
library(tidyverse)
library(purrr)
library(lubridate)

#---- read in base data ----

flue_sites <- readr::read_csv( "data-raw/flue_stocker18nphyt.csv" ) %>%
  dplyr::filter( !is.na(cluster) ) %>%
  distinct(site) %>%
  pull(site)

# calib_sites <- ingestr::siteinfo_fluxnet2015 %>%
#   dplyr::filter(!(sitename %in% c("DE-Akm", "IT-Ro1"))) %>%
#   dplyr::filter(sitename != "FI-Sod") %>%
#   dplyr::filter( c4 %in% c(FALSE, NA) & classid != "CRO" & classid != "WET" ) %>%
#   dplyr::filter( sitename %in% flue_sites ) %>%
#   pull(sitename)

# The soil water holding capacity (WHC) information provided in file
# siteinfo_fluxnet2015_sofun+whc.csv was created by David Sandoval Calle
# (Imperial College) based on Soilgrids data
# (see Stocker et al., 2018 Nature Geoscience).

# load data
s0 <- readRDS("data/df_S0.RDS")

load("data-raw/df_drivers_fluxnet2015.Rdata")
df <- df_drivers_fluxnet2015 %>%
  filter(
    sitename %in% flue_sites
  )

rm("df_drivers_fluxnet2015")

#---- process base data ----

# preprocess original data - to now rsofun format
df <- df %>%
  dplyr::select(sitename, forcing) %>%
  unnest(forcing) %>%
  dplyr::filter(!(month(date)==2 & mday(date)==29)) %>%

  # model requires flux per seconds now
   mutate(
     prec = prec / (60*60*24),
     ppfd = ppfd / (60*60*24)
     ) %>%

  ## assuming all precipitation in liquid form
  mutate(
    rain = prec,
    snow = 0
    ) %>%

  ## required for new version, but not used because
  mutate(
    tmin = temp,
    tmax = temp
    ) %>%

  group_by(sitename) %>%
  nest() %>%
  rename(forcing = data) %>%
  right_join(
    df %>%
      dplyr::select(-forcing),
    by = "sitename"
  ) %>%
  ungroup() %>%
  rename(
    site_info = siteinfo,
    params_soil = df_soiltexture
    )

# rename file
df_orig <- df

# save the whc (original data)
saveRDS(df_orig, file = "data/whc_p-model_driver_data.rds", compress = "xz")

swap_value <- function(x){

  # match coordinates to split
  # out S0 value from Beni's project / paper
  ss <- s0 %>%
    filter(
      lon == x$lon,
      lat == x$lat
    )

  # replace value of wch with S0
  x %>%
    mutate(
      whc = ss$S0[1]
    )

  # implicit return of site_info element X
}

# map swap_value over all site info values
# this could  have been an lapply()
df$site_info <- map(
    df$site_info,
    swap_value
  )

# save data to disk
saveRDS(df, file = "data/s0_p-model_driver_data.rds", compress = "xz")

df <- df %>% unnest(cols = c("sitename", "site_info"))
df_orig <- df_orig %>% unnest(cols = c("sitename", "site_info"))

plot(df$whc, df_orig$whc)
