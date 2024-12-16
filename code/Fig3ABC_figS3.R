# Evaluate acute changes in admissions via 3-day rolling rates of change 

library(tidyverse)
library(lubridate)
library(zoo)
library(timeDate)
library(recipes)
library(ggrepel)
library(paletteer)

# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# Annotated top most common CCs (at least 500 admissions)
CCanot <- read_csv('./manuscript/source_data/ED_TopCCs.csv') |>
  mutate(AREA = gsub('Injury & Musculoskeletal', 'Injury', AREA))

# One row per admit
admit <- infile |>
  inner_join(CCanot, by = 'CC') |> 
  distinct(ENC_ID, .keep_all = TRUE) |> 
  select(ENC_ID, ADMISSION_LOCATION, CC, AREA,
         ED_VISIT_DATE, ED_ADMISSION_DAY)

# Counts by datetime
counts <- admit |> 
  arrange(ED_VISIT_DATE) |> 
  count(ADMISSION_LOCATION, ED_VISIT_DATE)

# Generate a sequence of all dates within the study range
start_date <- min(infile$ED_VISIT_DATE)
end_date <- max(infile$ED_VISIT_DATE)

# Replace missing dates with 1-count (not zero b/c of rate change compute)
all_dates <- data.frame(
  ED_VISIT_DATE = rep(seq.Date(from = start_date, to = end_date, by = "day"), 2)) %>% 
  mutate(ADMISSION_LOCATION = rep(c('ED', 'URGENT'), nrow(.) / 2)) %>%
  left_join(counts, by = c('ADMISSION_LOCATION', 'ED_VISIT_DATE')) |> 
  mutate(n = ifelse(is.na(n), 1, n))

#Label major US holidays
sample_times <- all_dates |>  select(ED_VISIT_DATE)
holiday_rec <- recipe(~ ED_VISIT_DATE, sample_times) %>%
  step_holiday(
    all_predictors(), holidays = c("USLaborDay", "USNewYearsDay", "USMemorialDay",
                                   "USPresidentsDay", "USThanksgivingDay",
                                   "USIndependenceDay", "USGoodFriday", "USMLKingsBirthday",
                                   "USColumbusDay", "USChristmasDay", "EasterSunday",
                                   "GoodFriday", "AllSaints"))
holiday_rec <- prep(holiday_rec, training = sample_times)
holiday_values <- bake(holiday_rec, new_data = sample_times)

holidays <- holiday_values |> 
  pivot_longer(names_to = "ED_VISIT_DATE_HOLIDAY", values_to = "HOLIDAY", -ED_VISIT_DATE) %>%
  filter(HOLIDAY == 1) |>  select(-HOLIDAY) |> 
  mutate(ED_VISIT_DATE_HOLIDAY = gsub("ED_VISIT_DATE_", "", ED_VISIT_DATE_HOLIDAY)) %>%
  distinct(ED_VISIT_DATE, .keep_all = TRUE) |> 
  right_join(all_dates, by = "ED_VISIT_DATE") |> 
  select(HOLIDAY = ED_VISIT_DATE_HOLIDAY, everything())

# Add time changes
dst = tibble(ED_VISIT_DATE = as.Date(c("2010-03-14", "2011-03-13", "2012-03-11",
                             "2013-03-10", "2014-03-09", "2015-03-08",
                             "2016-03-13", "2017-03-12", "2018-03-11",
                             "2019-03-10", "2020-03-08", "2021-03-14"))) |> 
  mutate(TIME_SHIFT = 'to_DST')

all_dates_anot = tibble(ED_VISIT_DATE = as.Date(c("2010-11-07", "2011-11-06", "2012-11-04",
                           "2013-11-03", "2014-11-02", "2015-11-01",
                           "2016-11-06", "2017-11-05", "2018-11-04",
                           "2019-11-03", "2020-11-01", "2021-11-01"))) |> 
  mutate(TIME_SHIFT = 'from_DST') |> 
  bind_rows(dst) |> 
  right_join(holidays, by = 'ED_VISIT_DATE')
  
# Compute rolling rates of change over entire study period
# The rollapply() function takes the data vector, the window width,
# a function to compute the rate of change (ROC), which is computed as the
# difference between the last and first values in the window,
# divided by the first value. Align = "right" ensures that the rate of change
# corresponds to the last value in each window.
window_size <- 2 # Set window size
rollingROC <- all_dates_anot |> 
  arrange(ADMISSION_LOCATION, ED_VISIT_DATE) |> 
  group_by(ADMISSION_LOCATION) |> 
  mutate(YEAR = year(ED_VISIT_DATE),
         roll_roc = rollapply(n, width = window_size, FUN = function(x)
           (x[length(x)] - x[1]) / x[1], align = "right", fill = NA))
         
# All data facet by year
pA <- rollingROC |> 
  filter(YEAR >= 2010, YEAR <= 2014) |> 
  ggplot(aes(ED_VISIT_DATE, roll_roc)) + 
  geom_point(size = 0.25) +
  geom_line(size = 0.25) +
  geom_label_repel(aes(label = ifelse(ADMISSION_LOCATION == 'URGENT',
                                     HOLIDAY, "")), size = 1.5,color = 'black',
                  max.overlaps = 1000, min.segment.length = 0.0001) +
  geom_label_repel(aes(label = ifelse(ADMISSION_LOCATION == 'URGENT',
                                     TIME_SHIFT, "")), size = 1.5, color = 'red',
                  max.overlaps = 1000, min.segment.length = 0.00001) +
  facet_wrap(vars(YEAR), scales = 'free', ncol = 1) +
  labs(y = '3-day rolling rate of change in admissions') +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 9),
        strip.text = element_blank(),
        strip.background = element_rect(fill = NA))
ggsave('./manuscript/figs/figS3.png', pA, width = 6.5, height = 7.25)

# Plot an example year, facet by ED v UC
facets <- c(
  `ED` = "Emergency Department (2019)",
  `URGENT` = "Urgent Care (2019)")

pB <- rollingROC |> 
  filter(YEAR == 2019) |> 
  ggplot(aes(ED_VISIT_DATE, roll_roc)) + 
  geom_point(size = 0.25) +
  geom_line(size = 0.25) +
  geom_label_repel(aes(label = HOLIDAY), size = 1.5, color = 'red',
                   max.overlaps = 1000, min.segment.length = 0.0001,
                   label.padding = 0.1) +
  geom_label_repel(aes(label = TIME_SHIFT), size = 1.5, color = 'red',
                   max.overlaps = 1000, min.segment.length = 0.00001,
                   label.padding = 0.1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~ADMISSION_LOCATION, scales = 'free_y', ncol = 1,
             labeller = as_labeller(facets)) +
  labs(y = '3-day rolling rate of change in admissions') +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 9),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = 'grey90'))
#ggsave('./figs/Fig3A.pdf', pB, width = 6.5, height = 3.5)


# Admissions by day of week, per year
wk_yr <- admit |> 
  mutate(ED_ADMISSION_DAY = wday(ED_VISIT_DATE, label = TRUE),
         YEAR = year(ED_VISIT_DATE)) |> 
  group_by(ADMISSION_LOCATION, YEAR, ED_ADMISSION_DAY) |> 
  summarise(n = n()) |> 
  mutate(pct_total = round(n/sum(n)*100, digits = 1),
         n_scale = scale(n))

pC <- ggplot(wk_yr, aes(ED_ADMISSION_DAY, pct_total,
               group = YEAR, color = YEAR)) + 
  geom_point(size = 0.25) +
  geom_line(size = 0.25) +
  scale_color_gradient(low = "blue", high = "red", limits = c(2010, 2021), name = "",
                       breaks = seq(2010, 2021, 1), labels = seq(2010, 2021, 1),
                       guide = guide_colorbar(
                         barheight = unit(0.25, "cm"),
                         barwidth = unit(3.75, "cm"))) +
  facet_wrap(~ADMISSION_LOCATION, scales = 'free_y', ncol = 1,
             labeller = as_labeller(facets)) +
  labs(y = 'percent of total admissions') +
  theme_bw() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 8,angle = 90,
                                   hjust = 0.25, vjust = 0.5),
        legend.margin = margin(b = -10),
        legend.box.margin = margin(l = -5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, size = 9),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = 'grey90'))
#ggsave('./figs/Fig3B.pdf', pC, width = 2.1, height = 3)
  
# Admissions by day of week, per CC area
wk_area <- admit |> 
  mutate(ED_ADMISSION_DAY = wday(ED_VISIT_DATE, label = TRUE)) |> 
  group_by(ADMISSION_LOCATION, AREA, ED_ADMISSION_DAY) |> 
  summarise(n = n()) |> 
  mutate(pct_total = round(n/sum(n)*100, digits = 1),
         n_scale = scale(n))
pD <- ggplot(wk_area, aes(ED_ADMISSION_DAY, pct_total, group = AREA,
                          color = AREA)) + 
  geom_point(size = 0.5) +
  geom_line(size = 0.5) +
  facet_wrap(~ADMISSION_LOCATION, scales = 'free_y', ncol = 1,
             labeller = as_labeller(facets)) +
  scale_color_paletteer_d(
    palette = 'ggthemes::calc', guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 3),
      ncol = 4,
      title.hjust = 0.5, 
      keywidth=0.1,
      keyheight=0.1,
      order = 1)) +
  labs(y = 'percent of total admissions') +
  theme_bw() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.1, "cm"),
        legend.margin = margin(b = -7, l = -20),
        legend.box.margin = margin(l = -5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, size = 9),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = 'grey90'))
#ggsave('./figs/Fig3C.pdf', pD, width = 2.1, height = 3)


 
