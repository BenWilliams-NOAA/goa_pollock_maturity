# notes ----
# clean data

# load ----
library(lubridate)
library(tidytable)
library(here)
library(vroom)

# fecundity data ----
vroom::vroom(here('data', 'fecundity.csv')) %>%
  mutate.(Year = factor(year),
          Month = factor(month),
          Age = factor(age),
          date = dmy(date),
          ID = factor(year + month * 0.1 + haul * 0.001),
          length = round(length),
          yc = year - age -1,
          Hist = factor(histocatagory)) %>%
  filter.(long > -157, mfec > 0) -> ss


# maturity data ----
vroom::vroom(here("data", "update_data.csv")) %>%
  mutate.(Year = factor(year),
          Age = factor(age),
          hauls = ifelse(is.na(hauls), cruise, hauls),
          hauls = factor(hauls),
          length = round(length),
          dum = 1,
          mature = case_when(maturity_table==3 & maturity <3 ~ 0,
                             maturity_table==3 & maturity>=3 ~ 1,
                             TRUE ~ mature),
          Mature = factor(mature),
          weight = ifelse(year>2013, weight * 1000, weight)) %>%
  filter.(!is.na(weight), !is.na(length), !is.na(maturity), !is.na(age)) %>%
  select.(-...1) -> poll

# pre-spawning mature female data
poll %>%
  filter.(maturity_table == 11 & maturity %in% c(4, 5) |
            maturity_table == 3  & maturity == 3) %>%
  droplevels() -> poll3

# compare average fishery weight to average survey weights

vroom::vroom(here::here("data", "wt_compare.csv")) %>% 
  pivot_longer.(-c(year, fleet, tot_bio, sp_bio)) %>% 
  mutate.(age = as.numeric(gsub("age-", "", name)),
          Age = factor(age)) %>% 
  mutate.(tot_bio = mean(tot_bio, na.rm=T),
          sp_bio = mean(sp_bio, na.rm=T),
          .by=year) %>% 
  pivot_wider.(names_from=fleet, values_from = value) %>% 
  filter.(!is.na(fishery)) -> dat


# temp data ----
sst_dat <- vroom::vroom(here("data", "sst_dat.csv"))

# length-weight
# all females - examined using a linear model with log transformed variables
lw <- lm(log(weight) ~ log(length), data = poll)
# lwfit <- exp(fitted(lw) * exp((sigma(lw)^2) / 2))
# lwresid <- poll$weight - lwfit
# plot(lwfit, lwresid)
# abline(h=0, lty=4)
# summary(lw)

# pre-spawning females - examined using a linear model with log transformed variables
lw3 <- lm(log(weight) ~ log(length), data = poll3)
# lwfit3 <- exp(fitted(lw3) * exp((sigma(lw3)^2) / 2))
# lwresid3 <- poll3$weight - lwfit3
# plot(lwfit3, lwresid3)
# abline(h=0, lty=4)
# summary(lw3)

# body condition Kr ----
# based upon predicted weight at length.
# Predict Kr for maturity data
# Body condition Kr based upon predicted weight at length.
# Predict Kr for maturity data
poll %>%
  mutate.(w = exp(predict(lw, .)) * exp((sigma(lw)^2) / 2),
          Kr = weight / w) -> poll

poll3 %>%
  mutate.(w = exp(predict(lw3, .)) * exp((sigma(lw)^2) / 2),
          Kr = weight / w) -> poll3

# Predict Kr for fecundity data
ss %>%
  mutate.(Age = factor(age)) %>%
  mutate.(w = exp(predict(lw3, .)) * exp((sigma(lw3)^2) / 2),
          Kr = weight / w) -> ss

# filter data ----
# Subset fecundity data to look at fecundity with complete age/length/weight data
ss %>%
  filter.(length>0, weight>0, age>0) %>%
  mutate.(dum=1) -> ssawl

# Subset fecundity with complete length/weight data (more samples)
ss %>%
  filter.(length>0, weight>0) %>%
  mutate.(dum=1) -> sswl

# weights ----
# compare average fishery weight to average survey weights

dat %>% 
  ggplot(aes(fishery, survey, group = age, color = Age)) + 
  geom_point() +
  stat_smooth(method='lm') +
  geom_abline(slope = 1)

# model as single slope
fit <- lm(survey ~ fishery, data = dat)
# model with multiple slopes
fit1 <- lm(survey ~ fishery * Age, data = dat)
summary(fit)
summary(fit1)
AIC(fit, fit1)

# multiple slopes is a better fit and has better resid properties
dat %>% 
  mutate.(fit = predict(fit, .),
          resid = survey - fit) %>% 
  ggplot(aes(age, resid)) + 
  geom_jitter(width=0.1)

dat %>% 
  mutate.(fit = predict(fit, .),
          resid = survey - fit) %>% 
  ggplot(aes(fishery, fit, group = Age, color=Age)) + 
  geom_line() +
  geom_point(aes(y=survey), alpha = 0.2)

# multiple slopes results
dat %>% 
  mutate.(fit = predict(fit1, .),
          resid = survey - fit) %>% 
  ggplot(aes(age, resid)) + 
  geom_jitter(width=0.1)

dat %>% 
  mutate.(fit = predict(fit1, .),
          resid = survey - fit) %>% 
  ggplot(aes(fishery, fit, group = Age, color=Age)) + 
  geom_line() +
  geom_point(aes(y=survey), alpha = 0.2)


# compare average fishery wt to fish sampled for maturity (female wt)
dat %>% 
  select.(-c(name, survey)) %>% 
  left_join.(poll) %>% 
  filter.(weight>0) %>% 
  mutate.(weight=weight/1000) -> df


fit2 <- lm(weight ~ fishery, data = df)
fit3 <- lm(weight ~ fishery * Age, data = df)
summary(fit2)
summary(fit3)
AIC(fit2, fit3)

# multiple slopes is a better fit
df %>% 
  mutate.(fit = predict(fit3, .),
          resid = weight - fit) %>% 
  ggplot(aes(age, resid)) + 
  geom_jitter(width=0.1)

df %>% 
  mutate.(fit = predict(fit3, .),
          resid = weight - fit) %>% 
  ggplot(aes(fishery, fit, group = Age, color=Age)) + 
  geom_line() +
  geom_point(aes(y=weight), alpha = 0.1) + 
  geom_abline(slope=1, lty=3)

# calculate spawning weight from fishery weight

vroom::vroom(here::here("data", "wt_compare.csv")) %>% 
  select.(-c(tot_bio, sp_bio)) %>% 
  # filter.(fleet == "fishery") %>% 
  pivot_longer.(-c(year, fleet), values_to = "fishery") %>% 
  pivot_wider.(names_from = fleet, values_from = fishery) %>% 
  mutate.(age = as.numeric(gsub("age-", "", name)),
          Age = factor(age)) %>% 
  mutate.(weight = as.numeric(predict.lm(fit3, .))) %>% 
  select.(year, age, ay_abund = abundance, weight) %>% 
  mutate.(ay_bio = ay_abund * weight) %>% 
  arrange.(year, age) %>% 
  mutate.(y_abund = sum(ay_abund),
          y_bio = sum(ay_bio),
          lay_abund = lags.(ay_abund), 
          lay_bio = lags.(ay_bio), 
          .by = year) -> wt


wt %>% 
  filter(!is.na(weight)) %>% 
  summarise.(y_abund = mean(y_abund),
             y_bio = mean(y_bio), 
             .by = year) %>% 
  mutate.(ly_abund = lags.(y_abund),
          ly_bio = lags.(y_bio)) %>% 
  drop_na.() %>% 
  select.(-c(y_abund, y_bio)) %>% 
  left_join.(., wt) %>% 
  drop_na.() %>% 
  pivot_longer.(-c(year, age, weight)) %>% 
  mutate.(year = ifelse(stringr::str_detect(name, "lay"), year + 1, year)) %>% 
  select.(-weight) -> pop


# create full datasets

# mature only females for 
poll3 %>% 
  dplyr::select(latitude, longitude, length, weight, age, maturity, maturity_table, mature, year, Year, Mature, Age, Kr) %>% 
  left_join(pop) %>% 
  left_join(sst_dat) %>% 
  arrange(year) -> dat

poll %>% 
  dplyr::select(latitude, longitude, length, weight, age, maturity, maturity_table, mature, year, Year, Mature, Age, Kr) %>% 
  left_join(pop) %>% 
  left_join(sst_dat) %>% 
  arrange(year) -> dat2

ssawl %>% 
  left_join(pop) %>% 
  left_join(sst_dat) %>% 
  arrange(year) %>% 
  mutate(age = ifelse(age>10, 10, age),
         Age = factor(age)) -> dat3

sswl %>% 
  left_join(pop) %>%
  left_join(sst_dat) %>% 
  arrange(year) %>% 
  mutate(age = ifelse(age>10, 10, age),
         Age = factor(age)) -> dat4
