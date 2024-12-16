# Evaluate admissions just before vs after time shifts (dst & st)

library(tidyverse)
library(lubridate)
library(ggrepel)

# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# Annotated top most common CCs (at least 500 admissions)
CCanot <- read_csv('./manuscript/source_data/ED_TopCCs.csv') |>
  mutate(AREA = gsub('Injury & Musculoskeletal', 'Injury', AREA))

# One row per admit ED-only
ed <- infile |>
  filter(ADMISSION_LOCATION == "ED") |> 
  inner_join(CCanot, by = 'CC') |> 
  distinct(ENC_ID, .keep_all = TRUE) |> 
  select(ENC_ID, CC, AREA, ED_VISIT_DATE)

# Df with DST & ST shift dates & before/after date ranges 
dst = tibble(dst = as.Date(c("2010-03-14", "2011-03-13", "2012-03-11",
                             "2013-03-10", "2014-03-09", "2015-03-08",
                             "2016-03-13", "2017-03-12", "2018-03-11",
                             "2019-03-10", "2020-03-08", "2021-03-14"))) |> 
  mutate(before = dst - days(7),
         after = dst + days(6))

st = tibble(st = as.Date(c("2010-11-07", "2011-11-06", "2012-11-04",
                             "2013-11-03", "2014-11-02", "2015-11-01",
                             "2016-11-06", "2017-11-05", "2018-11-04",
                             "2019-11-03", "2020-11-01", "2021-11-01"))) |> 
  mutate(before = st - days(7),
         after = st + days(6))

# Filter the data frame for each date range and combine into a single data frame
dst_before <- map2_dfr(dst$before, dst$dst, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE < .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y),
           type = 'dst_before')
  })

dst_after <- map2_dfr(dst$dst, dst$after, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE <= .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y),
           type = 'dst_after')
  })

st_before <- map2_dfr(st$before, st$st, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE < .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y),
           type = 'st_before')
  })

st_after <- map2_dfr(st$st, st$after, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE <= .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y),
           type = 'st_after')
  })

tc_area <- bind_rows(dst_before, dst_after, st_before, st_after) |> 
  count(type, AREA) |> 
  # Percent changes before vs after
  pivot_wider(names_from = type, values_from = n) |> 
  mutate(dstDif = (dst_after - dst_before) / ((dst_after + dst_before) / 2) * 100,
         stDif = (st_after - st_before) / ((st_after + st_before) / 2) * 100,
         total = dst_before + dst_after + st_before + st_after) 

# Percent changes before vs after all admissions
tc_all <- bind_rows(dst_before, dst_after, st_before, st_after) |> 
  count(type) |> 
  # Percent changes before vs after
  pivot_wider(names_from = type, values_from = n) |> 
  mutate(dstDif = (dst_after - dst_before) / ((dst_after + dst_before) / 2) * 100,
         stDif = (st_after - st_before) / ((st_after + st_before) / 2) * 100,
         total = dst_before + dst_after + st_before + st_after,
         AREA = 'all')

pA <- tc_area |> 
  ggplot(aes(dstDif, stDif)) +
  geom_vline(xintercept = 0, color = 'black', linetype = 2) +
  geom_hline(yintercept = 0, color = 'black', linetype = 2) +
  geom_point(aes(size = total), shape = 0) +
  scale_size_continuous(name  = 'admissions', breaks = c(2500, 7500, 12500),
                        guide = guide_legend(
                          ncol = 1,
                          title.hjust = 0.5, 
                          keywidth=0.1,
                          keyheight=0.1)) +
  geom_point(aes(dstDif, stDif), data = tc_all, shape = 9, size = 4, color = 'red') +
  geom_text_repel(aes(label = AREA), size = 3) +
  labs(x = 'shift to DST  (%)',
       y = 'shift away from DST  (%)',
       title = 'Percent change in admissions after:') +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.box.margin = margin(l = -7))
#ggsave('./figs/Fig4A.pdf', pA, width = 3.25, height = 2.25)

# Build an empirical distribution by computing rolling sums over every day
# of the year on 10 years of data. Look at distributions of % changes for each
# CC Area. Where do the time shift % changes sit in that background distrib?

# Generate sequence of dates covering full range of admissions data
start_date <- as.Date("2010-01-15") # Trim b/c data starts 2010-01-01
end_date <- as.Date("2021-12-15") 
date_seq <- seq(from = start_date, to = end_date, by = "day")
date_list <- data.frame(date = date_seq) |>
  mutate(before = date - days(7),
         after = date + days(6))

# Filter the data frame for each date range and combine into a single data frame
before <- map2_dfr(date_list$before, date_list$date, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE < .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y), date = .y, type = 'before')
  })

after <- map2_dfr(date_list$date, date_list$after, ~ {
  filter(ed, ED_VISIT_DATE >= .x & ED_VISIT_DATE <= .y) %>% 
    mutate(date_range = paste0(.x, " to ", .y), date = .x, type = 'after')
  })

# Combine into single df, aggregate counts
bg_area <- bind_rows(before, after) |>
  mutate(month = month(date), day = day(date)) |> 
  count(AREA, type, month, day) |> 
  # Percent changes before vs after for each day of the year by CC area
  pivot_wider(names_from = type, values_from = n) |> 
  mutate(Dif = (after - before) / ((after + before) / 2) * 100,
         total = before + after)

# Percent changes before vs after for all data, bind with above df by CC area
bg_area_all <- bind_rows(before, after) |> 
  mutate(month = month(date), day = day(date)) |>
  count(type, month, day) |> 
  mutate(AREA = 'all') |> 
  pivot_wider(names_from = type, values_from = n) |> 
  mutate(Dif = (after - before) / ((after + before) / 2) * 100,
         total = before + after) |> 
  bind_rows(bg_area)

# Compute 15th & 985th percentiles for background data by CC area
bg_pctiles <- bg_area_all %>%
  group_by(AREA) %>%
  summarise(pct_10 = quantile(Dif, probs = 0.15),
            pct_90 = quantile(Dif, probs = 0.85))

pB <- ggplot(bg_area_all, aes(Dif)) +
  geom_density() +
  geom_vline(aes(xintercept = dstDif), data = tc_area |> bind_rows(tc_all), color = 'green') +
  geom_vline(aes(xintercept = stDif), data = tc_area |> bind_rows(tc_all), color = 'blue') +
  geom_vline(aes(xintercept = pct_10), data = bg_pctiles, linetype = 3) +
  geom_vline(aes(xintercept = pct_90), data = bg_pctiles, linetype = 3) +
  labs(x = 'percent change in admissions', y = 'background distribution') +
  facet_wrap(~AREA, scales = 'free_y', ncol = 2) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = 'grey90'))
#ggsave('./figs/Fig4B.pdf', pB, width = 3.2, height = 5)

# Not used in manuscript -- CC breakdown by share of areas
focus <- c('Injury')

# Admits in dst & st analysis (from above)
dat_focus <- bind_rows(dst_before, dst_after, st_before, st_after) |> 
  filter(AREA %in% focus) |> 
  count(AREA, CC) |> 
  group_by(AREA) |> 
  arrange(desc(n), .by_group = TRUE) |> 
  mutate(CC_share = n / sum(n)) |> ungroup()
  
# Plot top 5 CCs in each category
pC <- dat_focus |> 
  group_by(AREA) |> 
  top_n(n, n = 10) |> ungroup() |> 
  mutate(CC = fct_reorder(CC, CC_share)) |> 
  ggplot(aes(CC_share, CC)) +
  geom_col() +
  labs(x = '% of all injury-related admissions', y = 'Top 10 injury-related CCs') +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 13),
        strip.background = element_blank())
ggsave('./figs/FigXx.pdf', pC, width = 4, height = 3.5)


