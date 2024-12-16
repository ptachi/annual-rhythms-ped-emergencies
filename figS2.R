# Chief Complaint incidence by month

library(tidyverse)
library(broom)
library(mgcv)
library(ggrepel)
library(paletteer)

# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# One row per admit
admit <- infile |>
  distinct(ENC_ID, .keep_all = TRUE) |> 
  select(ENC_ID, PAT_ID, GENDER, RSN_ED_VISIT, ED_ADMISSION_MONTH,
         ED_ADMISSION_YEAR, ADMISSION_LOCATION)

# CCs w/at least N admits
top <- admit |> 
  distinct(ENC_ID, .keep_all = TRUE) |> 
  count(RSN_ED_VISIT) |> 
  slice_max(n >= 500) |> select(-n) |> # Require 500 observations
  inner_join(admit, by = 'RSN_ED_VISIT') # Rejoin data

freqs <- top |> 
  mutate(across(.cols = c('RSN_ED_VISIT', 'GENDER', 'ED_ADMISSION_YEAR', 'ED_ADMISSION_MONTH'), as.factor)) |> 
  group_by(RSN_ED_VISIT, ED_ADMISSION_YEAR, GENDER, ED_ADMISSION_MONTH, .drop = FALSE) |>
  #group_by(RSN_ED_VISIT, GENDER, ED_ADMISSION_MONTH, .drop = FALSE) |>
  summarise(n = n())|>
  ungroup() |> 
  mutate(across(.cols = c('ED_ADMISSION_YEAR', 'ED_ADMISSION_MONTH'), as.numeric)) |>  
  mutate(ED_ADMISSION_MONTH = as.numeric(ED_ADMISSION_MONTH)) |> 
  left_join(days <- tibble(
    ED_ADMISSION_MONTH = seq(1, 12, 1),
    DAYS = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    ADJUST = max(DAYS)/DAYS), by = 'ED_ADMISSION_MONTH') |> 
  mutate(MONTH_COUNT_ADM = round(n*ADJUST, digits = 0)) # Total per month

# Fit four GAM models to each CC
mod <- formula(MONTH_COUNT_ADM ~ s(ED_ADMISSION_MONTH) + s(ED_ADMISSION_YEAR))
mod2 <- formula(MONTH_COUNT_ADM ~ s(ED_ADMISSION_MONTH, ED_ADMISSION_YEAR))
mod3 <- formula(MONTH_COUNT_ADM ~ s(ED_ADMISSION_MONTH) + GENDER)
mod4 <- formula(MONTH_COUNT_ADM ~ s(ED_ADMISSION_MONTH, by = GENDER) + GENDER)

gam_models <- freqs |> 
  filter(GENDER != 'Unknown') |> 
  group_by(RSN_ED_VISIT) |> 
  nest() %>%
  mutate(two_year = map(data, ~ gam(mod, family = 'poisson', method = "REML", data = .)),
         int_year = map(data, ~ gam(mod2, family = 'poisson', method = "REML", data = .)),
         two_sex = map(data, ~ gam(mod3, family = 'poisson', method = "REML", data = .)),
         int_sex = map(data, ~ gam(mod4, family = 'poisson', method = "REML", data = .)),
         AICtwo_year = map_dbl(two_year, AIC),
         AICint_year = map_dbl(int_year, AIC),
         AICtwo_sex = map_dbl(two_sex, AIC),
         AICint_sex = map_dbl(int_sex, AIC),
         AICdiff_year = AICtwo_year - AICint_year,
         AICdiff_sex = AICtwo_sex - AICint_sex,
         AICpctdiff_year = (AICdiff_year / AICtwo_year) * 100,
         AICpctdiff_sex = (AICdiff_sex / AICtwo_sex) * 100) %>%
  select(-c('data', 'two_year', 'int_year', 'two_sex', 'int_sex'))

fig <- freqs |> 
  group_by(RSN_ED_VISIT)|> 
  summarise(admits = sum(n)) |> 
  inner_join(gam_models, by = 'RSN_ED_VISIT') |>
  # Normalized delta AICdiff
  mutate(AICndiff_year = (AICdiff_year / admits), 
         AICndiff_sex = (AICdiff_sex / admits),
         RSN_ED_VISIT = as.character(RSN_ED_VISIT))
         
pA <- ggplot(fig, aes(AICpctdiff_sex, AICpctdiff_year)) +
  geom_point(shape = 1) +
  geom_text_repel(aes(label = ifelse(
    AICpctdiff_year > 12 | AICpctdiff_sex > 2.1, RSN_ED_VISIT, "")),
    min.segment.length = 0.01, size = 2.5, max.overlaps = 1000) +
  geom_hline(yintercept = 0, color = 'red', alpha = 0.5) +
  geom_vline(xintercept = 0, color = 'red', alpha = 0.5) +
  labs(x = 'sex', y = 'year', title = 'Impact on CC annual rhythms') +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 2)),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))
ggsave('./figs/figS2A.pdf', pA, width = 2.35, height = 3.25)

# Plot identified interactions
year <- c('COVID-19', 'Chin Laceration', 'Fever,Cough')
sex <- c('Knee Injury', 'Arm Injury', 'Shoulder Injury')
p_year <- freqs |> 
  filter(RSN_ED_VISIT %in% year & GENDER != 'Unknown') |> 
  ggplot(aes(ED_ADMISSION_MONTH, MONTH_COUNT_ADM,
                   group = factor(ED_ADMISSION_YEAR),
                   color = factor(ED_ADMISSION_YEAR))) +
  geom_smooth(method = 'loess', size = 0.35) +
  scale_color_paletteer_d(
    labels = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017',
              '2018', '2019', '2020', '2021'),
    palette = 'ggthemes::calc', guide = guide_legend(
      override.aes = list(linewidth = 1.5),
      title = element_blank(),
      ncol = 1,
      title.hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1,12, by = 1),
                     labels = c("jan", "feb", "mar", "apr", "may", "jun", "jul",
                                "aug", "sep", "oct", "nov", "dec"),
                     guide = guide_axis(n.dodge = 1)) +
  labs(y = 'total admissions') +
  facet_wrap(~RSN_ED_VISIT, scale = 'free_y', ncol = 1) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 8),
        legend.box.margin = margin(l = -10),
        legend.key.size = unit(0.25, "cm"),
        strip.text = element_text(size = 9))
#ggsave('./figs/figS2B.pdf', p_year, width = 2.15, height = 3.25)
  
p_sex <- freqs |> 
  filter(RSN_ED_VISIT %in% sex & GENDER != 'Unknown') |> 
  ggplot(aes(ED_ADMISSION_MONTH, MONTH_COUNT_ADM,
             group = GENDER, color = GENDER)) +
  geom_smooth(formula = y ~ s(x, bs = "cc"), method = "gam", size = 0.35) +
  scale_color_paletteer_d(
    palette = 'ggthemes::calc', guide = guide_legend(
      override.aes = list(linewidth = 1.5),
      title = element_blank(),
      ncol = 1,
      title.hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1,12, by = 1),
                     labels = c("jan", "feb", "mar", "apr", "may", "jun", "jul",
                                "aug", "sep", "oct", "nov", "dec"),
                     guide = guide_axis(n.dodge = 1)) +
  #labs(y = 'total admissions') +
  facet_wrap(~RSN_ED_VISIT, scale = 'free_y', ncol = 1) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 8),
        legend.box.margin = margin(l = -10),
        legend.key.size = unit(0.25, "cm"),
        strip.text = element_text(size = 9))
#ggsave('./figs/figS2C.pdf', p_sex, width = 2.1, height = 3.25)


