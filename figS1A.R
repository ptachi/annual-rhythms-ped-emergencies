# ED and UC cohort overview in ED vs UC 

library(tidyverse)

# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# Admissions per year
yearly <- infile |> 
  distinct(ENC_ID, .keep_all = TRUE) |> 
  count(ADMISSION_LOCATION, ED_ADMISSION_YEAR)

pA <- ggplot(yearly, aes(ED_ADMISSION_YEAR, n/1000, fill = ADMISSION_LOCATION)) +
  geom_col() +
  scale_fill_manual(values = c('black', 'grey'),
                    labels = c('ED', 'Urgent care'),
                    guide = guide_legend(
                      title = element_blank())) +
  scale_x_continuous(breaks = seq(2010, 2021, by = 1)) +
  labs(y = 'admissions (x1000)') +
  theme_bw() +
  theme(
    legend.position = 'top',
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    legend.text = element_text(size = 10),
    legend.margin = margin(b = -7),
    legend.key.size = unit(0.5, "cm"))
ggsave('./figs/figS1A.pdf', pA, width = 2.75, height = 2.5)


## Not used for manuscript ##
## ICD10 prevalence ED vs UC
dx_rank <- infile |> 
  group_by(ADMISSION_LOCATION, DISCHARGE_ICD10) |>
  summarise(n = n()) |> 
  arrange(desc(n), .by_group = TRUE) |> 
  mutate(rank = seq(1, n())) |> 
  select(-n) |> 
  pivot_wider(names_from = ADMISSION_LOCATION, values_from = rank) |> 
  # Difference in rank between ED & UC
  mutate(rank_diff = URGENT-ED, 
         ED_UC = case_when(
           rank_diff > 500 ~ 'ED_ONLY',
           rank_diff < -500 ~ 'UC_ONLY',
           rank_diff < 25 & ED < 50 & URGENT < 50 ~ 'ED_UC',
           TRUE ~ 'NA')) |> 
  left_join(infile |> distinct(DISCHARGE_ICD10, .keep_all = T) |> 
              select(DISCHARGE_ICD10, DX_NAME), by = 'DISCHARGE_ICD10')

# Add text to extreme ED- & UC-only DXnames
pB <- ggplot(dx_rank, aes(ED, URGENT, color = rank_diff)) +
  geom_point(shape = 17, size = 0.7) +
  geom_abline(slope = 1, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(0, 800, by = 100), labels = c('1', seq(100, 800, by = 100))) +
  scale_y_continuous(breaks = seq(0, 800, by = 100), labels = c('1', seq(100, 800, by = 100))) +
  scale_colour_continuous_divergingx(
    palette = 'Zissou 1', guide = guide_colorbar(
      title = 'Difference\n in rank', barwidth = unit(0.25, 'cm'), barheight = unit(2.5, 'cm'))) +
  
  geom_encircle(data = subset(dx_rank, ED_UC != 'NA'),
                aes(group = ED_UC), color = c('black'),
                expand = 0, alpha = 0.5, size = 2) +
  labs(x = 'ED', y = 'Urgent care', title = 'Ranking ICD10 prevalence') +
  theme_bw() +
  theme(
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, hjust = 1, vjust = 0.5, angle = 90),
    legend.text = element_text(size = 13),
    plot.title = element_text(size = 13, margin = margin(l = -10, b = 1)),
    legend.margin = margin(l = -7))
#ggsave('./fig1B.pdf', pB, width = 3.6, height = 3)


