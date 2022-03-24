# eval data
# copy from Rmarkdown Beni

library(tidyverse)
#devtools::install_github("stineb/rbeni")
library(rbeni)
source("R/eval_sofun.R")
source("R/get_stats.R")

# load ugly data
load("data-raw/obs_eval_fluxnet2015.Rdata")

# read model output calculate before
output <- readRDS("data/s0_p-model_output.rds")

# set evaluation sites
evalsites <- output %>%
  mutate(ntsteps = purrr::map_dbl(data, ~nrow(.))) %>%
  dplyr::filter(ntsteps > 0) %>%
  pull(sitename)

# set evaluation settings
settings_eval <- list(
  benchmark = list( gpp = c("fluxnet") ),
  sitenames = evalsites,
  agg = 8
)

# run evaluation (dirty code - don't trust it much)
out_eval <- eval_sofun(
  output,
  settings_eval,
  settings_sims,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

p <- out_eval$gpp$fluxnet$data$meandoydf_byclim %>%
  dplyr::filter(climatezone %in% c("Aw south", "BSk north", "Cfa north", "Cfb north", "Cfb south", "Csa north", "Csb north", "Dfb north", "Dfc north")) %>%
  dplyr::filter(koeppen_code != "-") %>%
  pivot_longer(c(obs_mean, mod_mean), names_to = "source", values_to = "gpp") %>%
  ggplot() +
  geom_ribbon(
    aes(x = doy, ymin = obs_min, ymax = obs_max),
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
  labs(y = expression( paste("Simulated GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "DOY") +
  facet_wrap( ~climatezone ) +    # , labeller = labeller(climatezone = list_rosetta)
  theme_gray() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    name="Setup: ",
    values=c("red", "black")
    # values=c("FULL" = "#DE1A1A", "Observed" = "black")
  )

plot(p)
