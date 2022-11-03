# Load packages -----------------------------------------------------------
#https://stackoverflow.com/questions/61977439/table-below-x-axis-in-ggplot
require(pacman)
pacman::p_load(tidyverse, janitor, here, patchwork)

# Load data ---------------------------------------------------------------
raw_data <-
  read.csv(list.files(
    here('data'),
    recursive = T,
    pattern = '.csv',
    full.names = T
  ))

# Wrangle data ------------------------------------------------------------

patients <-
  raw_data %>% select(1:4) %>% mutate(across(!category, as.character)) %>%
  pivot_longer(-c(1:2)) %>%
  mutate(value = as.numeric(str_replace_all(value, ',', ''))) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(disorder = factor(
    disorder,
    levels = c('pd', 'other_mh'),
    labels = c('Personality disorder', 'Other mental health disorder')
  ))


# Make column graph -------------------------------------------------------


(
  p1 <- ggplot(data = patients, aes(x = (category), y = patient_pc)) +
    geom_col() +
    theme_bw() + labs(x = '', y = '') +
    scale_x_continuous(#breaks = c(0:12),
      #limits = c(0, 12),
      expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = c(0, 0), limits=c(0,70)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),
      text = element_text(size = 20), 
      panel.spacing = unit(2, "lines")
    ) +
    facet_wrap(~disorder)
)


# Make data frame for table -----------------------------------------------
(
  df <- patients %>% select(category, disorder,patients, patient_pc) %>%
    mutate(numbers = category) %>%
    pivot_longer(
      c(numbers, patients, patient_pc),
      names_to = "layer",
      values_to = "label"
    )
)

# Make table --------------------------------------------------------------


p2 <- df %>%
  mutate(label_format=prettyNum(label, big.mark = ",", scientific = FALSE)) %>% 
  ggplot(aes(x = category, y = factor(
    layer,
    c("patient_pc", "patients", "numbers"),
    label = c('Patients %', 'Patients', "Attendances")
  ))) +
  #ggplot(aes(x=category, y=0)) +
  geom_tile(fill = NA,
            alpha = .4,
            color = NA) +
  geom_text(aes(label = label_format), size=6) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  labs(y = "", x = NULL) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20)
  ) +
  facet_wrap(~ disorder)


# Combine plots -----------------------------------------------------------

p1 / p2 +  plot_layout(heights = c(8, 1))

