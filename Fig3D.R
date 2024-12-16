# Evaluate CC area admissions by month and day of week

library(tidyverse)
library(ggrepel)

# ED & UC admissions
infile <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')

# Annotated top most common CCs (at least 500 admissions)
CCanot <- read_csv('./manuscript/source_data/ED_TopCCs.csv') |>
  mutate(AREA = gsub('Injury & Musculoskeletal', 'Injury', AREA))

# All psych-related CCs
select_area <- 'Psych'

# One row per admit, top CCs (at least 500 admissions)
admit <- infile |>
  inner_join(CCanot, by = 'CC') |> 
  filter(AREA %in% select_area & ADMISSION_LOCATION == 'ED') |> 
  distinct(ENC_ID, .keep_all = TRUE) |> 
  select(ENC_ID, CC, AREA, ED_ADMISSION_MONTH, ED_ADMISSION_DAY)

# Admissions by month and day of week
month_day <- admit |> 
  group_by(ED_ADMISSION_MONTH, ED_ADMISSION_DAY) |> 
  summarise(n = n()) |> 
  
  mutate(pct_total = round(n/sum(n)*100, digits = 1),
         n_scale = scale(n))
pD <- ggplot(month_day, aes(ED_ADMISSION_DAY, ED_ADMISSION_MONTH,
                          fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(
    guide = guide_colorbar(
      title = 'Psych admissions',
      title.position = 'top',
      title.hjust = 0.5,
      barheight = unit(0.25, "cm"),
      barwidth = unit(3.75, "cm"),
      label.nudge.x = 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(1, 12, 1),
                     labels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                                'jul', 'aug', 'sep', 'oct', 'nov', 'dec')) +
  scale_x_discrete(labels = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')) +
  theme(panel.background = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.box.margin = margin(b = -10),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, size = 9))
ggsave('./figs/Fig3D.pdf', pD, width = 1.9, height = 3.1)


 
