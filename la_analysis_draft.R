library(dplyr)
library(ggplot2)
library(tidyr)

la <- readRDS("los_angeles.rds")
glimpse(la)

la %>% count(subject_race)

#=====Q5 Trends=============================================

#========Some plots=========================================
la_ts <- la %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(
    total_stops      = n(),
    nonwhite_stops   = sum(subject_race != "white", na.rm=TRUE),
    prop_nonwhite    = nonwhite_stops / total_stops
  ) %>%
  arrange(month)

p_nwstop <- ggplot(la_ts, aes(x=month, y=prop_nonwhite)) +
  geom_line() +
  # locally weighted regression
  #geom_smooth(method="loess", span=0.3) +
  labs(y="Proportion non-white stops")


la_ts_long <- la_ts %>%
  select(month, total_stops, nonwhite_stops) %>%
  pivot_longer(
    cols = c(total_stops, nonwhite_stops),
    names_to = "stop_type",
    values_to = "count")

p_stops <- ggplot(la_ts_long, aes(x = month, y = count, color = stop_type)) +
  geom_line(size = 1) +
  labs(
    x      = "Month",
    y      = "Number of Stops",
    color  = "Stop category",
    title  = "Monthly Total vs. Non-white Stops") +
  scale_color_manual(
    values = c("total_stops" = "steelblue", "nonwhite_stops" = "firebrick"),
    labels = c("All stops", "Non-white stops")) +
  theme_minimal()


#===========Detect Change Point======================================
# 1. install & load
# install.packages("changepoint")
library(changepoint)

ts_vec <- la_ts$total_stops

# detect changes in mean AND variance, using PELT + BIC penalty
cpt <- cpt.meanvar(ts_vec,
                   method    = "PELT",
                   penalty   = "BIC",
                   test.stat = "Normal")  

ind_changepoint <- cpts(cpt)        # the estimated change-point indices
param.est(cpt)   # estimated mean/variance per segment
plot(cpt, cpt.col="blue", main="Change-point in Total Stops")

change_months <- la_ts$month[ind_changepoint]
change_months

ggplot(la_ts, aes(x = month, y = total_stops)) +
  geom_line() +
  geom_vline(xintercept = change_months,
             linetype   = "dashed",
             color      = "red") +
  labs(
    x     = "Month",
    y     = "Total stops",
    title = "Monthly Total Stops with Detected Change-Points") +
  theme_minimal()



#=============trend in proportion of non-white stops?==============
# add a numeric time index (in months since start)
la_ts_1 <- la_ts %>%
  ungroup() %>%
  mutate(time = as.numeric(month - first(month)) / 30)

# fit a binomial GLM: log-odds of nonwhite stop ~ time
m_binom <- glm(
  cbind(nonwhite_stops, total_stops - nonwhite_stops) ~ time,
  data   = la_ts_1,
  family = binomial
)
summary(m_binom)

library(lubridate)
la_ts_2 <- la_ts_1 %>%
  mutate(mon = factor(month(month, label=TRUE)))

m2 <- glm(
  cbind(nonwhite_stops, total_stops - nonwhite_stops) ~ time + mon,
  data   = la_ts_2,
  family = binomial
)
summary(m2)

plot(residuals(m2, type="pearson") ~ fitted(m2)); abline(h=0, lty=2)
acf(residuals(m2, type="pearson"))







install.packages("zyp")
library(zyp)

ts_slope <- zyp.sen(prop_nonwhite ~ time, data=la_ts)
ts_slope$coefficients


