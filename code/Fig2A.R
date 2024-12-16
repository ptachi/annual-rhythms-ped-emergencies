# Evaluate ICD-10 similarity of admits for each Chief Complaint

library(tidyverse)
library(data.table)

# ED & UC admissions
dat <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# Most frequent CCs (at least 500 admits)
topCC <- read_csv('./manuscript/source_data/ED_TopCCs.csv')
dat_topCC <- inner_join(topCC, dat, by = 'CC') |> 
  select(ENC_ID, CC, DISCHARGE_ICD10)

# Sample N percent of admits from each CC
set.seed(0918)
dat_topCC_samp <- dat_topCC |> 
  distinct(ENC_ID, CC) |> 
  group_by(CC) |> 
  slice_sample(prop = 0.05) |>
  ungroup() 

# Function to find all pairwise combinations of admits for each CC
get_pairs <- function(x) {
  data.frame(t(combn(x$ENC_ID, 2))) 
  }

# Function to compute Jaccard index: the size of the intersection of the two sets
# divided by the size of the union of the two sets. For binary vectors, the
# size of the intersection is the no. of positions where both vectors == 1.
# The size of the union is the no. of positions where at least one of the vectors == 1.
jaccard_index <- function(x, y) {
  # Initialize count of common ones
  common_ones <- 0
  # Loop through each element in the vectors
  for (i in 1:length(x)) {
    # If the elements are both 1, increment the count
    if (x[i] == 1 && y[i] == 1) {
      common_ones <- common_ones + 1}
  }
  # Compute the Jaccard index
  total_ones <- sum(x == 1 | y == 1)
  jaccard <- signif(common_ones / total_ones, digits = 2)
  return(jaccard)
  }

# Run function over all CCs. Warning: takes ~2-3 hrs to run
out <- data.frame()
for (i in 1:length(topCC$CC)) {
  tp <- dat_topCC_samp |> 
    filter(CC %in% topCC$CC[i])
  vectors <- inner_join(dat_topCC, tp, by = 'PAT_ENC_CSN_ID') |> 
    select(ENC_ID, DISCHARGE_ICD10) %>%
    mutate(presence = 1) %>%
    pivot_wider(names_from = DISCHARGE_ICD10, values_from = presence, values_fill = 0) %>%
    mutate(ENC_ID = as.character(ENC_ID)) %>%
    as.data.frame(.)
  vectors_mtrx <- as.matrix(vectors[, -1])
  rownames(vectors_mtrx) <- vectors[, 1]
  colnames(vectors_mtrx) <- NULL
  
tp2 <- tp |> 
  mutate(ENC_ID = as.character(ENC_ID)) |> 
  nest(data = -CC)
  df <- tp2 %>% 
    mutate(pairs = map(data, get_pairs)) |> 
    select(-data) |> 
    unnest(pairs) %>%
    rename(x = X1, y = X2)
  # Convert data frame to data table
  setDT(df)
  
# Add new columns to df with binary vectors corresponding to row names
system.time(df[, x1 := lapply(df$x, function(i) vectors_mtrx[i,])])
system.time(df[, y1 := lapply(df$y, function(i) vectors_mtrx[i,])])
 
# Apply function to df
system.time(tp3 <- df |> mutate(jsim = map2(x1, y1, jaccard_index)))

tp4 <- data.frame(
  CC = tp3$CC[1],
  mean_jsim = signif(mean(unlist(tp3$jsim)), digits = 3),
  med_jsim = signif(median(unlist(tp3$jsim)), digits = 3))
  out <- bind_rows(out, tp4)
  }

# Admit complexity, ie. number of ICD10s, per cluster
admits_per_CC <- dat_topCC |> 
  distinct(PAT_ENC_CSN_ID, .keep_all = TRUE) |> 
  group_by(CC) |>
  summarise(admits = n())

icd10s_per_CC <- dat_topCC |> 
  group_by(CC, PAT_ENC_CSN_ID) |> 
  summarise(n = n()) |> 
  summarise(med_ICD10s = median(n),
            mean_ICD10s = signif(mean(n), digits = 2)) |> 
  inner_join(admits_per_CC, by = 'CC') |> 
  inner_join(out, by = 'CC')
write_csv(icd10s_per_CC |> 
            select(CC, admits, everything()),
          './manuscript/source_data/ED_CC_Jaccard.csv')

# Plot admit complexity for each CC; using files created above
icd10s_per_CC <- read_csv('./manuscript/source_data/ED_CC_Jaccard.csv')
topCC <- read_csv('./manuscript/source_data/ED_TopCCs.csv')

library(paletteer)
library(ggrepel)
sel <- c('Psych Evaluation', 'Fever,Rash', 'Foreign Body in Nose', 'Migraine')
pA <- icd10s_per_CC |> 
  inner_join(topCC, by = 'CC') |> 
  rename('admissions' = admits) |> 
  mutate(AREA = gsub('Injury & Musculoskeletal', 'Injury & MS', AREA),
         mean_ICD10s_cap = ifelse(mean_ICD10s > 7.5, 7.5, mean_ICD10s),
         LABEL = ifelse(CC %in% sel, CC, ""),
         LABEL = ifelse(LABEL == 'REF,Psych Evaluation', 'Psych Evaluation', LABEL)) |> 
  ggplot(aes(mean_jsim, mean_ICD10s_cap)) +
  geom_point(aes(color = AREA), shape = 0, size = 2) +
  geom_text_repel(aes(color = AREA, label = ifelse(
    CC %in% c('Psych Evaluation', 'Fever,Rash',
                        'Foreign Body in Nose', 'Migraine'), CC, "")),
    min.segment.length = 0.01, max.overlaps = 1000,
    force_pull = 0, size = 2.5, show.legend = FALSE, nudge_x = 0.20,
    nudge_y = 0.20, segment.color = 'grey50', fontface = 'bold') +
  scale_color_paletteer_d(
    palette = 'ggthemes::calc', guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 3.5),
      ncol = 1,
      title.hjust = 0.5, 
      keywidth=0.1,
      keyheight=0.1,
      order = 1)) +
  labs(x = 'patient similarity (jaccard index)',
       y = 'median ICD10s per patient') +
  theme_bw() +
  theme(legend.position = c(0.75, 0.65),
        legend.box = "vertical",
        legend.margin = margin(t = -2),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))
ggsave('./figs/Fig2A.pdf', pA, width = 2.95, height = 2.75)


  

