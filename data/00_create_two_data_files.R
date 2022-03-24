library(tidyverse)
library(purrr)

# load data
s0 <- readRDS("data/df_S0.RDS")

load("data-raw/df_drivers_fluxnet2015_allsites.Rdata")
df <- df_drivers_fluxnet2015_allsites
rm("df_drivers_fluxnet2015_allsites")

# rename the columns of the rsofun data
df <- df %>%
  rename(
    'site_info' = 'siteinfo',
    'params_soil' = 'df_soiltexture'
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
