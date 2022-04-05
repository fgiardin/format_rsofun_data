# eval data

# devtools::install_github("computationales/rsofun")
# devtools::install_github("stineb/LSD")
library(tidyverse)
library(lubridate)
library(rsofun)
library(LSD)
library(patchwork)
source("R/eval_sofun.R")
source("R/get_stats.R")
source("R/analyse_modobs2.R")

# load ugly data
load("data-raw/obs_eval_fluxnet2015.Rdata")

# read model output calculate before
whc_output <- readRDS("data/whc_p-model_output.rds")

# set evaluation settings
settings_eval <- list(
  benchmark = list( gpp = c("fluxnet") ),
  sitenames = unique(output$sitename),
  agg = 8
)

# run evaluation (dirty code - don't trust it much)
whc_eval <- eval_sofun(
  whc_output,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

p1 <- whc_eval$gpp$fluxnet$plot$gg_modobs_xdaily

# read model output calculate before
s0_output <- readRDS("data/s0_p-model_output.rds")

s0_eval <- eval_sofun(
  s0_output,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

p2 <- s0_eval$gpp$fluxnet$plot$gg_modobs_xdaily

p3 <- whc_eval$gpp$fluxnet$data$meandoydf_byclim %>%
  dplyr::filter(climatezone %in% c("Aw south",
                                   "BSk north",
                                   "Cfa north",
                                   "Cfb north",
                                   "Cfb south",
                                   "Csa north",
                                   "Csb north",
                                   "Dfb north",
                                   "Dfc north")) %>%
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

p4 <- s0_eval$gpp$fluxnet$data$meandoydf_byclim %>%
  dplyr::filter(climatezone %in% c("Aw south",
                                   "BSk north",
                                   "Cfa north",
                                   "Cfb north",
                                   "Cfb south",
                                   "Csa north",
                                   "Csb north",
                                   "Dfb north",
                                   "Dfc north")) %>%
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

p <- (p1 + p2 + p3 + p4 + plot_layout(
  ncol = 2,
  guides = "keep")
  )

plot(p)

# save data for publication
ggsave("manuscript/evaluation_plot.png")
