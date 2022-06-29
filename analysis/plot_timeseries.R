# load libraries
library(Matrix)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(grid)
library(bigleaf)

# AU-Cpr ------------------------------------------------------------------
# load data
df_Cpr_raw <- read.csv("/Users/fgiardina/Desktop/Wang Han task/Materials for Francesco/AU-Cpr_2013.csv")

# extract gpp from s0
output_s0 <- readRDS("data/s0_p-model_output.rds")
df_s0_1 <- output_s0 %>%
  dplyr::filter(sitename == "AU-Cpr")
df_s0_1 <- df_s0_1[[3]][[1]]
df_s0_1 <- df_s0_1%>%
  dplyr::select(date, gpp) %>%
  rename(gpp_s0 = gpp)

# reformat df
df_Cpr <- df_Cpr_raw %>%
  mutate(
    date = lubridate::make_datetime( # adjust date
      year = YEAR,
      month = MONTH,
      day = DAY,
      hour = HOUR,
      min = MINUTE
      ),
    Pmodel_GPP_accli = Pmodel_GPP*beta_LSM, # calculate yellow line plot
    ) %>%
  dplyr::select(date, obs_GPP, Pmodel_GPP_accli, LSM_GPP) %>% # select variables of interest
  mutate(date = lubridate::date(date)) %>% #summarise daily
  group_by(date) %>%
  summarise(obs_GPP = mean(obs_GPP, na.rm = TRUE),
            Pmodel_GPP_accli = mean(Pmodel_GPP_accli, na.rm = TRUE),
            LSM_GPP = mean(LSM_GPP, na.rm = TRUE)
            ) %>%
  left_join(df_s0_1, by = "date") %>% # join with gpp s0 data set (also daily, our current implementation of pmodel does not support half hourly)
  pivot_longer(cols = obs_GPP:gpp_s0, # pivot in long format
               names_to = "names",
               values_to = "value"
               )

# annotation
grob_a <- grobTree(textGrob("AU-Cpr", x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface="bold")))

# plot
a <- ggplot(data = df_Cpr %>% dplyr::filter(names != "LSM_GPP")
            # %>% # from 4th July,2013 to 10th July, 2013
            #   dplyr::filter(date > "2013-07-04 00:00:00") %>%
            #   dplyr::filter(date < "2013-07-11 00:00:00")
              ) +
  geom_path(
    aes(
      date,
      value,
      color = names
      #group = names,
    ),
    size=0.6
  ) +
  labs(
    x = "Time",
    y = "GPP"  #expression(paste("ET (mm ", d^-1, ")"))
  ) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_color_manual(  # set line colors and labels
    values = c(obs_GPP = "gray23",
               Pmodel_GPP_accli = "#ECB021",
               #LSM_GPP = "#7C2E8E",
               gpp_s0 = "blue"
               ),
    labels = c(obs_GPP = "obs", # set labels for legend
               Pmodel_GPP_accli = "Noah-MP accli.",
               #LSM_GPP = "Noah-MP default",
               gpp_s0 = "s0-based GPP"
               )
  ) +
  #scale_x_date(date_breaks="1 month", date_labels = "%b") + # set correct x axis
  theme( # set legend position and orientation, as well as text size
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size = 12)
  ) +
  annotation_custom(grob_a)
plot(a)
ggsave("./AU-Cpr_daily.png", width = 12, height = 4)


 # old version ------------------------------------------------------------------
# load data
df_GWW_raw <- read.csv("/Users/fgiardina/Desktop/Wang Han task/Materials for Francesco/AU-GWW_2013.csv")
colnames(df_Cpr_raw) <- c("date", "obs_GPP",  "Pmodel_GPP", "LSM_GPP", "beta_LSM")

# reformat df
df_Cpr <- df_Cpr_raw %>%
  mutate(
    date = seq(   # convert time
      from=as.POSIXct("2013-1-1 0:00", tz="UTC"),
      to=as.POSIXct("2013-12-31 23:30", tz="UTC"),
      by="30 min"
    ),
    Pmodel_GPP_accli = Pmodel_GPP*beta_LSM, # calculate yellow line plot
  ) %>%
  dplyr::select(date, obs_GPP, Pmodel_GPP_accli, LSM_GPP) %>% # select variables of interest
  pivot_longer(cols = obs_GPP:LSM_GPP,
               names_to = "names",
               values_to = "value"
  )

# extract
output_s0 <- readRDS("data/s0_p-model_output.rds")
df_s0_1 <- output_s0 %>%
  dplyr::filter(sitename == "AU-Cpr")


# annotation
grob_a <- grobTree(textGrob("AU-Cpr", x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface="bold")))

# plot
a <- ggplot(data = df_Cpr %>% slice(1:1000)) +
  geom_path(
    aes(
      date,
      value,
      color = names
      #group = names,
    ),
    size=0.6
  ) +
  labs(
    x = "Time",
    y = "GPP"  #expression(paste("ET (mm ", d^-1, ")"))
  ) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_color_manual(  # set line colors
    values = c(obs_GPP = "black", # green
               Pmodel_GPP_accli = "#ECB021", # blue
               LSM_GPP = "#7C2E8E" # red
    ),
    # labels = c(obs = expression(paste(ET[obs])), # set labels for legend
    #            nn_act = expression(paste(ET[NN])),
    #            nn_pot = expression(paste(PET[NN])),
    #            netrad = "Net Radiation"
    #            )
  ) +
  #scale_x_date(date_breaks="1 month", date_labels = "%b") + # set correct x axis
  theme( # set legend position and orientation, as well as text size
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size = 12)
  ) +
  annotation_custom(grob_a)
plot(a)
# don't correspond to her plot because we don't know the date
