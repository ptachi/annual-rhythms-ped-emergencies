# Latent Class Analysis (LCA) per CC

library(tidyverse)
library(poLCA)

# Top 318 CCs, w/Jaccard similarity scores
CCjsim <- read_csv('./manuscript/source_data/ED_CC_Jaccard.csv')
# Require at least 1K admissions
CCjsimF <- CCjsim |> 
  filter(admits > 1000)
# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds') |> 
  filter(CC %in% CCjsimF$CC)

# Calculate ICD10 counts and percents by CC, remove very low frequency ICD-10s by CC
CCadm <- infile |> # Distinct admissions per CC
  group_by(CC) |>
  summarise(patients = n_distinct(ENC_ID))
ICD10filtr <- infile |> 
  count(CC, DX_NAME) |> 
  left_join(CCadm, by = 'CC') |> 
  mutate(percentage = n / patients * 100) |> 
  filter(percentage >= 1) |> # Require at least 0.5% of admits w/CC
  ungroup()

# Clean ICD10 nomenclature for LCA input
formatICD10 <- ICD10filtr |> 
  distinct(DX_NAME) |> 
  mutate(DX_NAME_CLEAN = str_remove_all(DX_NAME, "\\(.*?\\)"),
         DX_NAME_CLEAN = str_replace(DX_NAME_CLEAN, "\\s+$", ""),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, ",\\s*", "_"),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, "\\s+", "_"),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, "-", ""),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, "'", ""),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, "/", ""),
         DX_NAME_CLEAN = str_replace_all(DX_NAME_CLEAN, "%", "pct"),
         DX_NAME_CLEAN = str_remove_all(DX_NAME_CLEAN, "\\s+")) |> 
  inner_join(ICD10filtr, by = 'DX_NAME')

# Join admit data
dat <- formatICD10 |> 
  dplyr::select(CC, DX_NAME, DX_NAME_CLEAN) |> 
  inner_join(infile, by = c('CC', 'DX_NAME'))

# Confirm admit counts per CC
obs <- dat |> 
  group_by(CC) |> 
  summarise(observations = n_distinct(ENC_ID))

# Function to run LCA on all CCs. Warning: ~30mins to run
# ICD-10 presence/absence matrix 
cc_list <- CCjsimF$CC[1:194]
#cc_list <- CCjsimF$CC[1:5]
sum <- tibble()
adm <- tibble()
prob <- tibble()
for (i in 1:length(cc_list)) {
  of <- dat %>%
    filter(CC %in% cc_list[i]) |>
    dplyr::select(ENC_ID, DX_NAME_CLEAN) |> 
    mutate(presence = 1) %>%
    pivot_wider(names_from = DX_NAME_CLEAN, values_from = presence, values_fill = 0) |> 
    column_to_rownames(var = 'ENC_ID') |> 
    # Re-code for LCA; absence = 1, presence = 2
    mutate(across(.cols = everything(), ~ifelse(. == 0, 1, 2)))
  
  # LCA model
  all_columns <- paste(names(of), collapse = ",")
  lcFormula <- as.formula(paste("cbind(", all_columns, ") ~ 1"))
  nclass = 4 # Starting point, downclass if < 250 admits in any class
  set.seed(0918)
  lcModel <- poLCA(lcFormula, of, nclass = nclass, maxiter = 10000)

  # Admit class shares
  tmp <- tibble(LCA_CLASS = lcModel$predclass) |> 
    group_by(LCA_CLASS) %>%
    summarise(n = n(), pct_total = n / nrow(.)) |> 
    mutate(CC = cc_list[i]) 
  
  while (min(tmp$n) < 250) { # Rerun LCA if <250 admits per class
    nclass = nclass-1
    lcModel <- poLCA(lcFormula, of, nclass = nclass, maxiter = 10000)
    tmp <- tibble(LCA_CLASS = as.numeric(lcModel$predclass)) |> 
      group_by(LCA_CLASS) %>%
      summarise(n = n(), pct_total = n / nrow(.)) |> 
      mutate(CC = cc_list[i])
    }
  sum <- bind_rows(sum, tmp)
  # Admit IDs
  tmp2 <- tibble(
    ENC_ID = rownames(of), 
    LCA_CLASS = as.numeric(lcModel$predclass)) |> 
    mutate(CC = cc_list[i]) 
  adm <- bind_rows(adm, tmp2)
  
  # ICD10 shares per cluster
  tmp3 <- data.frame(lcModel$probs) |>
    dplyr::select(ends_with(".Pr.2."))
  colnames(tmp3) <- gsub(".Pr.2.", "", colnames(tmp3))
  rownames(tmp3) <- seq(1, nrow(tmp3), 1)
  tmp3 <- tmp3 %>%
    rownames_to_column(var = 'LCA_CLASS') %>% 
    pivot_longer(cols = 2:ncol(.), names_to = 'ICD10', values_to = 'CLSTR_SHARE') |> 
    mutate(CC = cc_list[i],
           CLSTR_SHARE = signif(CLSTR_SHARE, digits = 3)) 
  prob <- bind_rows(prob, tmp3)
  }

# Save as supplemental files for manuscript
write_csv(prob |> # ICD-10 shares per CC class
            dplyr::select(CC = CC, LCA_CLASS, ICD10,
                          CLASS_SHARE = CLSTR_SHARE), './manuscript/supp_files/FileS2.csv')
write_csv(adm |>
            dplyr::select(ENC_ID, CC = CC, LCA_CLASS),
          './manuscript/supp_files/FileS3.csv')


