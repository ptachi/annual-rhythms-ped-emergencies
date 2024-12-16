# GAM fit monthly admissions by chief complaint (CC) LCA class  

library(tidyverse)
library(mgcv)
library(broom)
library(ggeffects)
library(ggpmisc)

infile <- read_csv('./manuscript/supp_files/FileS3.csv') # LCA admit classes
infile2 <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')  # ED & UC admissions

  # Select CCs
sel <- c('Poison Ivy/Poison Oak/Poison Sumac Exposure',
         'Fever,Rash', 'Psych Evaluation')

# Add LCA classes to full admit data
dat <- infile2 |> 
  filter(CC %in% sel) |> 
  distinct(ENC_ID, ED_ADMISSION_MONTH) |> 
  inner_join(infile, by = 'ENC_ID')

# Monthly counts by CC LCA class
month <- dat |>
  mutate(LCA_CLASS = 0) |> # Build background set
  bind_rows(dat) |>
  mutate(across(.cols = c('LCA_CLASS', 'ED_ADMISSION_MONTH'), as.factor)) |> 
  count(CC, LCA_CLASS, ED_ADMISSION_MONTH, .drop = FALSE) |> 
  mutate(ED_ADMISSION_MONTH = as.numeric(ED_ADMISSION_MONTH)) |> 
  # Adjust for differences in days per month
  left_join(tibble(ED_ADMISSION_MONTH = seq(1, 12, 1),
                   DAYS = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                   ADJUST = max(DAYS)/DAYS), by = 'ED_ADMISSION_MONTH') |>
  mutate(DAY_AVG_ADM = round(n/DAYS/12, digits = 3), # Avg per day
         MONTH_AVG_ADM = round(n*ADJUST/12, digits = 2), # Avg per month
         MONTH_COUNT_ADM = round(n*ADJUST, digits = 0)) |>  # Total per month
  group_by(CC, LCA_CLASS) |> 
  # Remove LCA classes that don't exist, ie. n = 0 for all months
  filter(!all(n == 0)) |> 
  mutate(MONTH_COUNT_ADM_CNTRD = scale(MONTH_COUNT_ADM, scale = F), # Mean center at zero
         MONTH_COUNT_ADM_SCALED = scale(MONTH_COUNT_ADM, scale = T)) |>  # Scale
  ungroup()

# GAM admissions per day by month
mod <- month |> 
  dplyr::select(CC, LCA_CLASS, ED_ADMISSION_MONTH, MONTH_COUNT_ADM) |> 
  nest(data = -c('CC', 'LCA_CLASS')) %>% 
  mutate(fit = purrr::map(data, ~ gam(
    MONTH_COUNT_ADM ~ s(ED_ADMISSION_MONTH, bs = "cc"), family = 'poisson',
    method = "REML", data = .x)),
    results = purrr::map(fit, glance),
    rsq = purrr::map_dbl(fit, ~ summary(.)$r.sq),
    reml = purrr::map_dbl(fit, ~ summary(.)$sp.criterion),
    month.p = purrr::map_dbl(fit, ~ summary(.)$s.table['s(ED_ADMISSION_MONTH)', 'p-value']),
    month.Chisq = purrr::map_dbl(fit, ~ summary(.)$s.table['s(ED_ADMISSION_MONTH)', 'Chi.sq']),
    mem = purrr::map(fit, ~ ggpredict(., terms = 'ED_ADMISSION_MONTH'))) %>%  # Marginal effect of month
  select(-fit) |> 
  unnest(results) |> 
  # FDR adjust pvals
  mutate(BHq = p.adjust(month.p, method = "BH",
                        n = nrow(unique(month[, c('CC', 'LCA_CLASS')])))) |> 
  mutate(sig = ifelse(BHq <= 0.01, "yes", "no")) %>%
  unnest(c(mem, data)) %>% # Marginal effects
  group_by(CC, LCA_CLASS) %>%
  mutate(pred_cntr = scale(predicted, scale = F), 
         pred_scale = scale(predicted, scale = T),
         pred_norm = predicted/mean(predicted)) |> 
  mutate(across(c('predicted', 'std.error', 'conf.low', 'conf.high', 'pred_norm',
                  'pred_cntr', 'pred_scale'),
                as.numeric)) |> 
  ungroup () |> 
  select(CC, LCA_CLASS, everything(), -c(group, x)) 
#write_csv(mod, './ChiefComplaint/data/CC_LCA_GAMfits.csv')

# Share of admits in each CC class
class_shares <- infile |>
  filter(CC %in% sel) |> 
  group_by(CC, LCA_CLASS) |> 
  summarise(n = n()) |> 
  mutate(class_share = n / sum(n)) |> 
  ungroup() |> 
  nest(-CC)

# Plot bar
plot_class_shares <- function(x) {
  ggplot(x, aes(class_share, y = paste(sum(n)), fill = factor(LCA_CLASS))) +
  geom_col() +
  scale_fill_manual(values = c('#00798C', '#D1495B', '#EDAE49', '#009E73')) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) 
  }
data.tb <- class_shares |> 
  mutate(plot = map(data, plot_class_shares), x = 1, y = Inf)

# Plot
labels <- mod %>% # Helper for labels
  distinct(CC, LCA_CLASS, .keep_all = TRUE) %>%
  count(sig, .drop = FALSE) |> 
  mutate(sig= factor(sig, levels = c('no', 'yes')))

facets <- c(
    `Fever,Rash` = 'Fever,Rash',
    `Poison Ivy/Poison Oak/Poison Sumac Exposure` = 'Poison Ivy',
    `Psych Evaluation` = "Psych Evaluation")

pA<- ggplot(mod, aes(ED_ADMISSION_MONTH, pred_norm)) + 
  geom_plot(data = data.tb, aes(x, y, label = plot, 
                                vp.width = 0.65, vp.height = 0.20)) +
  geom_line(aes(group = LCA_CLASS, color = LCA_CLASS,
                       linetype = factor(sig, levels = c('yes', 'no')))) +
  scale_color_manual(values = c('#BABABA', '#00798C', '#D1495B', '#EDAE49', '#009E73'),
                     labels = c('all', '1', '2', '3', '4'),
                     guide = guide_legend(
                       override.aes = list(size=1.5),
                       title = 'class',
                       keywidth = 1,
                       keyheight = 0.85,
                       nrow = 1,
                       order = 2,
                       show.legend = FALSE)) +
  scale_linetype_manual(values = c('solid', 'dashed'),
                        guide = guide_legend(
                          override.aes = list(size=1.5),
                          title = 'fdr < 0.01',
                          keywidth = 1,
                          keyheight = 0.85,
                          nrow = 1,
                          order = 1)) +
  scale_x_continuous(breaks = seq(1,12, by = 1),
                     labels = c("jan", "feb", "mar", "apr", "may", "jun", "jul",
                                "aug", "sep", "oct", "nov", "dec"),
                     guide = guide_axis(n.dodge = 1)) +
  labs(y = "admissions (normalized)") +
  theme_minimal() +
  facet_wrap(~CC, scales = 'free_y', labeller = as_labeller(facets), ncol = 3) +
  theme(legend.position = 'top',
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.margin = margin(l = -10, b = -5),
        legend.spacing = unit(1.5, "cm"),
        strip.background = element_rect(colour = 'grey75'))
ggsave('./manuscript/figs//Fig3C.pdf', pA, width = 6.4, height = 2.3)

