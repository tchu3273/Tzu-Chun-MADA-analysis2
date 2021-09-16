###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(tidyverse) #for plotting
library(here) #for data loading/saving
library(usmap)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################

# Plot of covid 19 deaths over time stratified by health condition and age group
p1 <- mydata |>
  group_by(age.group, health.condition, start.date) |>
  summarize(y = mean(covid.19.deaths, na.rm = TRUE)) |>
  ggplot(aes(x = start.date, y = y, col = age.group)) +
  geom_line() +
  facet_wrap(~health.condition, scales = "free_y") +
  theme_bw() +
  labs(x = "date", y = "mean COVID-19 deaths",
       title = "COVID-19 deaths in Georgia over time stratified by age group and comorbidities")

ggsave(p1, filename = here::here("results", "p1.png"), height = 8.5, width = 11)

# mean deaths in georgia by month
p2 <- mydata %>%
  filter(state == "Georgia", health.condition == "COVID-19") %>%
  group_by(month) %>%
  summarize(mean_se(covid.19.deaths)) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = y, ymin = ymin, ymax = ymax, group = 1)) +
  geom_ribbon(alpha = 0.5, fill = "cadetblue") +
  geom_line(size = 1, lineend = "round", linejoin = "round") +
  scale_x_discrete(labels = month.abb) +
  theme_bw() +
  labs(x = NULL, y = "mean COVID-19 deaths (+/- 1 SE)",
       title = "COVID-19 deaths by month with no comorbidities for Georgia")

ggsave(p2, filename = here::here("results", "p2.png"), height = 8.5, width = 11)

p3 <- mydata %>%
  filter(health.condition == "COVID-19") %>%
  group_by(state, age.group) %>%
  summarize(deaths = max(covid.19.deaths)) %>%
  plot_usmap(regions = "states", values = "deaths", data = .) +
  facet_wrap(~age.group, nrow = 2) +
  scale_fill_viridis_c(name = "deaths") +
  theme(legend.position = "bottom", legend.justification = "center") +
  ggtitle("Overall maximum reported COVID-19 deaths for each state by age group")

ggsave(p3, filename = here::here("results", "p3.png"), height = 8.5, width = 11)
