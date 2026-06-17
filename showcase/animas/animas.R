###
###
### Model probability of Animas River discharge at Durango, CO
###
###


###
### Libraries and subroutines
###

library(dataRetrieval)  # get hydrograph data
library(lubridate)
library(tidyverse)
library(pdftools)  # convert pdf to png
library(mgcv)
library(hexbin)  # for geom_hex(...)
library(ggtext)  # for expressions in ggplot


###
### Get data
###

# Animas River at Durango, CO
gage_id <- "USGS-09361500"

# Get daily discharge values
dat <- read_waterdata_daily(
    monitoring_location_id = gage_id,
    parameter_code = "00060",  # discharge (ft³/s)
    statistic_id = "00003",  # mean
    time = c("1991-01-01", "2026-06-14")
  ) %>%
  as_tibble() %>%
  rename(date = time, discharge = value) %>%
  mutate(
    jday = yday(date),  # Julian day
    sdate = as.Date("2024-01-01") + jday - 1,  # standardize date to 2024 for plotting
    year = year(date),  # numeric year
    fyear = as.factor(year)  # factor year
  ) %>%
  dplyr::select(year, fyear, date, sdate, jday, discharge)

# Confirm data are complete (all days represented)
table(dat$date[-1] - dat$date[-nrow(dat)])

dat %>% 
  group_by(year) %>% 
  count() %>% 
  print(n = 100)


###
### Fit GAM to 1991-2025 river flows
###

xdat <- dat %>% filter(year < 2026)  # subset data
n_year <- n_distinct(xdat$year)  # number of years

# Tensor interaction model with main effects...
out <- gam(
  log(discharge) ~
    s(fyear, bs = "re") +  # same as s(year,bs="tp",k=n_year)
    # s(year,bs="tp",k=n_year) +  # k < n_year will smooth across years
    s(jday, bs = "cc", k = 52) +
    ti(year, jday, bs = c("tp", "cc"), k = c(n_year, 52)),
  data = xdat,
  method = "REML",
  select = TRUE
)

summary(out)
gam.check(out)
plot(out)


###
### Sample posterior distribution
###

n <- 1000  # number of posterior samples

new_data <- xdat %>%  # new prediction data
  dplyr::select(fyear, year, sdate, jday)

# Get linear predictor matrix
Xp <- predict(out, newdata = new_data, type = "lpmatrix")

# Extract model estimates
beta <- coef(out)  # posterior mean of coefficients
Sigma_beta <- vcov(out)  # posterior covariance of coefficients

# Simulate from Gaussian approximation to the posterior of the model coefficients
beta_post <- mvrn(n, beta, Sigma_beta)  # n coefficient vectors from posterior

# Posterior distribution of linear predictor
ps <- Xp %*% t(beta_post) %>%  # linear predictor
  as_tibble() %>%
  bind_cols(new_data %>% dplyr::select(sdate), .) %>%  # add data used for prediction
  pivot_longer(cols = starts_with("V"), values_to = "discharge") %>%
  dplyr::select(-name)

# Bin modeled estimates and calculate probability of occurrence
ps <- ps %>%
  mutate(
    discharge = round(discharge, 2),  # create "bins"
    discharge = exp(discharge)  # revert to original scale
  ) %>%
  group_by(sdate, discharge) %>%
  count() %>%  # count per "bin"
  group_by(sdate) %>%
  mutate(prob = n / sum(n))  # probability per "bin"


###
### Create figure
###

ps %>%
  ggplot(aes(x = sdate, y = discharge, z = prob)) +
  stat_summary_hex(
    fun =  ~ mean(.x, na.rm = TRUE),
    bins = 100,
    color = "white",
    linewidth = 0.1
  ) +
  geom_line(
    data = dat %>% filter(year == 2026),
    aes(x = sdate, y = discharge),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_label(
    aes(x = sdate, y = discharge, label = year),
    inherit.aes = FALSE,
    data = data.frame(
      sdate = as.Date("2024-06-27"),
      discharge = 610,
      year = "2026"
    ),
    size = 2.5,
    alpha = 0.8,
    fill = "white",
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2024-02-10"),
    y = 2500,
    label = "March heatwave\nmelts an already\nhistorically-low\nsnowpack",
    size = 2.5
  ) +
  geom_curve(
    aes(
      x = as.Date("2024-02-10"),
      y = 1825,
      xend = as.Date("2024-03-20"),
      yend = 850
    ),
    curvature = 0.2,
    arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
    linewidth = 0.25
  ) +
  annotate(
    geom = "text",
    x = as.Date("2024-09-08"),
    y = 5000,
    label = "Modeled probability\nbased on river flows\nfrom 1991 to 2025",
    size = 2.5
  ) +
  geom_curve(
    aes(
      x = as.Date("2024-08-01"),
      y = 5000,
      xend = as.Date("2024-06-25"),
      yend = 4750
    ),
    curvature = 0.2,
    arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c("#859bd4", "#e8bec9", "#db98aa", "#cd728c", "#c04c6d"),
    name = "Prob.",
    labels = scales::percent,
    breaks = seq(0, 0.0125, by = 0.002)
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = 0.5,
      keywidth = 2,
      title.position = "right",
      label.position = "bottom",
      nrow = 1
    )
  ) +
  labs(
    title = "Animas River at Durango, CO",
    subtitle = "2026 discharge compared to previous 35 years",
    y = expression("Discharge (ft"^3 * "/s)")
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.key.spacing.x = unit(0, "pt")
  )

ggsave("out/animas.pdf", height = 6, width = 6)
pdf_convert(
  "out/animas.pdf",
  filenames = "out/animas.png",
  format = "png",
  dpi = 600
)
