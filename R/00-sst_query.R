# SST code
# edited from code obtained from Steve.Barbeaux@noaa.gov
# original code from Jordan.Watson@noaa.gov

# ben.williams@noaa.gov
# data accessed:
# 2022-04-18
# Note that I'm trimming these data to a specific location
# and allocating months to "summer" or "winter" temps

# load ----
library(tidyverse)
library(tidync)
library(lubridate)
library(httr)
library(vroom)

# data ----

#  Download the data for a fixed spatial and temporal period.
#  (note this is a lot of data and will take a few minutes to download if you do the whole thing)

GET("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst[(1981-09-01T12:00:00Z):1:(2019-12-31T12:00:00Z)][(0.0):1:(0.0)][(52):1:(62)][(200):1:(215)]",
    write_disk("data/sst.nc", overwrite=TRUE))

# process data - note: remove april and october as 'transition' months

tidync::tidync(here::here("data", "sst.nc")) %>%
  tidync::hyper_tibble() %>%
  tidytable::filter.(!(longitude<202 & latitude>56), !(longitude<204 & latitude>57)) %>%
  tidytable::mutate.(date = lubridate::as_datetime(time),
                     month = lubridate::month(date),
                     year = lubridate::year(date),
                     season = tidytable::case_when.(month %in% 5:9 ~ "summer",
                                                    month %in% c(1:3, 11:12) ~ "winter"),
                     year = tidytable::case_when.(month >= 5 ~ year + 1,
                                                  TRUE ~ year)) %>%
  tidytable::summarise.(sst = mean(sst), .by = c(year, season)) %>%
  tidytable::drop_na.() %>% 
  tidytable::pivot_wider.(names_from = season, values_from = sst) -> sst_dat

vroom_write(sst_dat, here::here("data", "sst_dat.csv"), delim = ",")

# eda figs

sst_dat %>%
  ggplot(aes(year, summer)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = winter), color = 2) +
  geom_point(aes(y = winter), color = 2)

sst_dat %>%
  ggplot(aes(scale(winter), scale(summer), color = year)) +
  geom_point() +
  geom_abline(slope = 1, lty = 3) +
  scico::scale_color_scico(palette = "roma")