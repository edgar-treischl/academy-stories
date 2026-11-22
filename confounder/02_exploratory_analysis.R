
# ============================================================
# 02_EXPLORATORY_ANALYSIS.R
# ============================================================

library(tidyverse)
library(patchwork)

# Datensatz betrachten
glimpse(schools)

# Deskriptive Statistiken
schools %>%
  select(
    digital_devices,
    math_performance,
    social_index,
    school_size,
    teacher_experience
  ) %>%
  summary()

# Verteilung der digitalen Ausstattung
p1 <- ggplot(
  schools,
  aes(digital_devices)
) +
  geom_histogram(
    bins = 20,
    fill = "#3678A8",
    color = "white"
  ) +
  labs(
    title = "Digitale Ausstattung der Schulen",
    x = "Endgeräte pro 100 Schüler",
    y = "Anzahl Schulen"
  ) +
  theme_minimal()

# Verteilung der Mathematikleistung
p2 <- ggplot(
  schools,
  aes(math_performance)
) +
  geom_histogram(
    bins = 20,
    fill = "#D58B42",
    color = "white"
  ) +
  labs(
    title = "Mathematikleistungen",
    x = "Mathematik-Score",
    y = "Anzahl Schulen"
  ) +
  theme_minimal()

# Verteilungen gegenüberstellen
p1 / p2
