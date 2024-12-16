# LCA identified classes of each CC based on ICD-10 profiles. For each CC, compute
# the correlation in annual patterns of admission between all possible pairs of classes

library(tidyverse)
library(ggrepel)

# LCA admit classes
infile <- read_csv('./manuscript/supp_files/FileS3.csv')
# ED & UC admissions
infile2 <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds') 

# Add LCA classes to full admit data
dat <- infile2 |> 
  distinct(ENC_ID, ED_ADMISSION_MONTH) |> 
  inner_join(infile, by = 'ENC_ID')

month <- dat |>
  mutate(across(.cols = c('LCA_CLASS', 'ED_ADMISSION_MONTH'), as.factor)) |> 
  count(CC, LCA_CLASS, ED_ADMISSION_MONTH, .drop = FALSE) |> 
  mutate(across(c(ED_ADMISSION_MONTH, LCA_CLASS), as.numeric)) |> 
# Adjust for differences in days per month
  left_join(tibble(ED_ADMISSION_MONTH = seq(1, 12, 1),
                   DAYS = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                   ADJUST = max(DAYS)/DAYS), by = 'ED_ADMISSION_MONTH') |>
  mutate(MONTH_COUNT_ADM = round(n*ADJUST, digits = 0)) |>  # Total per month
  group_by(CC, LCA_CLASS) |> 
  # Remove LCA classes that don't exist, ie. n = 0 for all months
  filter(!all(n == 0)) |> 
  # Mean center at zero, then scale
  mutate(MONTH_COUNT_ADM_SCALED = as.numeric(scale(MONTH_COUNT_ADM, scale = T))) |> 
  select(CC, LCA_CLASS, ED_ADMISSION_MONTH, 
                MONTH_COUNT_ADM, MONTH_COUNT_ADM_SCALED) |> 
  ungroup()

# CCs with >1 class
multi_class <- month |> 
  group_by(CC) |> 
  filter(n() > 12)

# Function to compute Spearman rank correlation (rho)
correlation <- function(vec1, vec2) {
  cor(vec1, vec2, method = "spearman")
  }

# Compute Spearman rho b/t all pairs of LCA classes for each CC
cor_results <- multi_class |> 
  group_by(CC) |> 
  summarise(
    total_admits = sum(MONTH_COUNT_ADM),
    subgroup1 = combn(unique(LCA_CLASS), 2)[1, ],
    subgroup2 = combn(unique(LCA_CLASS), 2)[2, ],
    correlation = combn(unique(LCA_CLASS), 2, function(subgroup_pair) {
      correlation(MONTH_COUNT_ADM_SCALED[LCA_CLASS == subgroup_pair[1]], 
                  MONTH_COUNT_ADM_SCALED[LCA_CLASS == subgroup_pair[2]])
      }, simplify = TRUE))

# Calculating average correlation for each group
group_avg_cor <- cor_results %>%
  group_by(CC) |> 
  summarise(avg_corr = mean(correlation),
            total_admits = total_admits[1])
# Label 3 examples of high, low, and mid corr values
sel <- c('Poison Ivy/Poison Oak/Poison Sumac Exposure',
         'Fever,Rash', 'REF,Psych Evaluation')
pA <- group_avg_cor |>
  mutate(CC_ABBREV = case_when(
    CC == 'Poison Ivy/Poison Oak/Poison Sumac Exposure' ~ 'Poison Ivy',
    CC == 'REF,Psych Evaluation' ~ 'Psych Evaluation',
    TRUE ~ CC),
    color = ifelse(CC %in% sel, 'yes', 'no')) |> 
  ggplot(aes(avg_corr, total_admits/1000)) +
  geom_point(shape = 0, size = 2, aes(color = color)) +
  scale_color_manual(values = c('grey60', 'black')) +
  geom_text_repel(aes(label = ifelse(CC %in% sel, CC_ABBREV, "")),
                  max.overlaps = 1000, min.segment.length = 0.000001, size = 2.5, fontface = 'bold',
                  segment.size = 0.5, nudge_x = 0.17, nudge_y = 0.17) +
  labs(x = 'inter-class correlation in annual rhythms (rho)', y = 'total admits (x1,000)') +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 10, margin = margin(r = -5)),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10, margin = margin(l = -10)))
#ggsave('./figs/Fig2B.pdf', pA, width = 3.25, height = 2.75)


## Not used in manuscript ##
# Function to compute peak:trough ratio for a vector of values
compute_ratio <- function(MONTH_COUNT_ADM) {
  sorted_values <- sort(MONTH_COUNT_ADM)
  min_value <- mean(MONTH_COUNT_ADM[1:2])
  max_value <- mean(tail(sorted_values, 2))
  ratio <- max_value / min_value
  return(ratio)
  }

# Compute peak:trough ratio for each LCA class
ptr_results <- multi_class |> 
  group_by(CC, LCA_CLASS) |> 
  summarise(ptr = compute_ratio(MONTH_COUNT_ADM)) |> 
  ungroup()

# Calculating average correlation for each group
group_avg_ptr <- ptr_results |> 
  group_by(CC) |> 
  summarise(classes = n(),
            avg_ptr = mean(ptr),
            sd_ptr = sd(ptr)) |> 
  inner_join(group_avg_cor, by = 'CC')


    

