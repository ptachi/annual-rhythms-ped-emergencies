# Visualize LCA results

library(tidyverse)
library(ggrepel)
library(ggpmisc)

infile <- read_csv('./manuscript/supp_files/FileS3.csv') # LCA admit classes
infile2 <- read_csv('./manuscript/supp_files/FileS2.csv') # LCA ICD10 class shares 
infile3 <- readRDS('./manuscript/source_data/ED_Admissions_ICD10s.rds')  # ED & UC admissions

sel <- c('Poison Ivy/Poison Oak/Poison Sumac Exposure',
         'Fever,Rash', 'Psych Evaluation')

dat <- infile3 |> # Add LCA classes to full admit data
  filter(CC%in% sel) |> 
  distinct(ENC_ID, ED_ADMISSION_MONTH, GENDER, AGE_ED_ADMISSION) |> 
  inner_join(infile , by = 'ENC_ID')

# Plot age
age <- dat |> 
  nest(-CC)
plot_ages <- function(x) {
  ggplot(x, aes(AGE_ED_ADMISSION)) +
    geom_histogram(aes(fill = factor(LCA_CLASS))) +
    scale_fill_manual(values = c('#00798C', '#D1495B', '#EDAE49', '#009E73')) +
    labs(x = 'age', y = 'admissions') +
    theme_bw() +
    theme(axis.title.x = element_text(size = 8, vjust = 2),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = 'none',
          plot.margin = margin(t = 0, b = 0, l = 0, r = 0)) 
  }
data.tb <- age |> 
  mutate(plot = map(data, plot_ages),
         #x = Inf, y = 0.68,
         x = 2, y = 0.97)

# Plot sex
sex <- dat |> 
  group_by(CC, LCA_CLASS) |> 
  summarise(admits = n(), pct_female = round(
      length(ENC_ID[GENDER == 'Female'])/n(), digits = 2)) |> 
  nest(-CC)

plot_sex <- function(x) {
  ggplot(x, aes(pct_female, y = '1', group = factor(LCA_CLASS))) +
    geom_point(aes(color = factor(LCA_CLASS)), shape = 5) +
    scale_color_manual(values = c('#00798C', '#D1495B', '#EDAE49', '#009E73')) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    geom_vline(xintercept = 0.5, size = 0.2) +
    labs(title = '% female') +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 8, hjust = 0.5, vjust = -2),
          legend.position = 'none',
          plot.margin = margin(t = 0, b = 0, l = 0, r = 0)) 
}
data.tb2 <- sex |> 
  mutate(plot = map(data, plot_sex), x = 40, y = 0.97)

# Plot ICD10 class shares
probs <- infile2 |> 
  filter(CC%in% sel) |> 
  group_by(CC, ICD10) |> 
  mutate(LABEL = ifelse(CLASS_SHARE == max(CLASS_SHARE) & 
                        max(CLASS_SHARE) > 0.40, ICD10, "")) |> 
  ungroup() |> 
  # Abbreviate names of top share ICD10s for plotting
  mutate(LABEL_ABREV = case_when(
    LABEL == 'Suicidal_ideations' ~ 'suicide ideations',
    LABEL == 'Streptococcal_pharyngitis' ~ 'strep throat',
    LABEL == 'Scarlet_fever' ~ 'scarlet fever',
    LABEL == 'Rash_and_other_nonspecific_skin_eruption' ~ 'rash, nonspecific',
    LABEL == 'Fever_unspecified' ~ 'fever',
    LABEL == 'Family_history_of_other_mental_and_behavioral_disorders' ~ 
      'fam h/o mental disorder',
    LABEL == 'Enteroviral_vesicular_stomatitis_with_exanthem' ~ 'VSV',
    LABEL == 'Contact_dermatitis_and_other_eczema_due_to_unspecified_cause' ~ 
      'contact dermatitis unspecified',
    LABEL == 'Contact_dermatitis_and_other_eczema_due_to_plants' ~ 
      'contact dermatitis plants',
    LABEL == 'Attention_deficit_disorder_with_hyperactivity' ~ 'ADHD',
    LABEL == 'Allergic_contact_dermatitis_due_to_plants_except_food' ~ 
      'allergic contact dermatitis plants',
    LABEL == 'Unspecified_viral_infection_characterized_by_skin_and_mucous_membrane_lesions' ~ 
      'Unspecified virus skin lesions',
    LABEL == 'Intermittent_explosive_disorder' ~ 'IED',
    TRUE ~ LABEL))

#Aggregate plot
pA <- ggplot(probs, aes(ICD10, CLASS_SHARE,group = factor(LCA_CLASS),
                        color = factor(LCA_CLASS))) +
  geom_line() +
  scale_color_manual(values = c('#00798C', '#D1495B', '#EDAE49', '#009E73'),
                     labels = c('1', '2', '3', '4'),
                     guide = guide_legend(
                       override.aes = list(size=1.5),
                       title = 'cluster',
                       keywidth = 1,
                       keyheight = 0.85,
                       ncol = 3,
                       order = 1)) +
  geom_text_repel(aes(label = LABEL_ABREV), size = 2.5, show.legend = F,
                  min.segment.length = 0.05, segment.color = 'grey',
                  force = 50) +
  #scale_y_discrete(expand = c(0, 0)) +
  #geom_plot(data = data.tb, aes(x, y, label = plot, vp.width = 0.28, vp.height = 0.3)) +
  #geom_plot(data = data.tb2, aes(x, y, label = plot, vp.width = 0.2, vp.height = 0.16)) +
  facet_wrap(~CC, scales = 'free_x', ncol = 3) +
  labs(x = 'discharge ICD-10', y = 'class share') +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10), 
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.x = element_line(color = 'grey'))
ggsave('./manuscript/figs/Fig3D.pdf', pA, width = 6.35, height = 2)

