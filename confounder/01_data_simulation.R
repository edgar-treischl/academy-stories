
# ============================================================
# LEARNING STORY
# 01_data_simulation.R
# ============================================================
#
# Erzeugung synthetischer Schuldaten
#
# Benötigt:
# 00_story.R
#
# Ergebnis:
# schools
#
# ============================================================


library(tidyverse)


# ------------------------------------------------------------
# 1. SIMULATIONSPARAMETER
# ------------------------------------------------------------

# Reproduzierbarkeit
set.seed(2026)

# Anzahl Schulen
n <- data_description$n_observations


# ------------------------------------------------------------
# 2. EFFEKTPARAMETER
# ------------------------------------------------------------

# Diese Werte können verändert werden, um die Stärke
# der Zusammenhänge in der Story zu beeinflussen.

simulation_parameters <- list(
  
  # Sozialindex
  social_index_mean = 50,
  social_index_sd = 15,
  social_index_min = 10,
  social_index_max = 90,
  
  # Digitale Ausstattung
  social_to_devices = 0.55,
  size_to_devices = -0.20,
  devices_noise = 0.85,
  
  # Mathematikleistung
  social_to_math = 0.65,
  experience_to_math = 0.18,
  schooltype_to_math = 0.45,
  math_noise = 0.90,
  
  # Schulgröße
  school_size_mean = 500,
  school_size_sd = 180,
  school_size_min = 100,
  
  # Lehrerfahrung
  teacher_experience_mean = 15,
  teacher_experience_sd = 6,
  teacher_experience_min = 1
  
)


# ------------------------------------------------------------
# 3. GRUNDVARIABLEN GENERIEREN
# ------------------------------------------------------------

# Sozialindex
social_index <- rnorm(
  n,
  mean = simulation_parameters$social_index_mean,
  sd = simulation_parameters$social_index_sd
) %>%
  pmax(simulation_parameters$social_index_min) %>%
  pmin(simulation_parameters$social_index_max)


# Schulgröße
school_size <- rnorm(
  n,
  mean = simulation_parameters$school_size_mean,
  sd = simulation_parameters$school_size_sd
) %>%
  pmax(simulation_parameters$school_size_min)


# Lehrerfahrung
teacher_experience <- rnorm(
  n,
  mean = simulation_parameters$teacher_experience_mean,
  sd = simulation_parameters$teacher_experience_sd
) %>%
  pmax(simulation_parameters$teacher_experience_min)


# Schulart
school_type <- sample(
  c(
    "Gymnasium",
    "Realschule",
    "Mittelschule"
  ),
  n,
  replace = TRUE,
  prob = c(0.30, 0.35, 0.35)
)


# ------------------------------------------------------------
# 4. STANDARDISIERUNG
# ------------------------------------------------------------

social_z <- as.numeric(
  scale(social_index)
)

size_z <- as.numeric(
  scale(school_size)
)

experience_z <- as.numeric(
  scale(teacher_experience)
)


# Effekt der Schulart
school_type_effect <- case_when(
  
  school_type == "Gymnasium" ~ 0.8,
  
  school_type == "Realschule" ~ 0,
  
  school_type == "Mittelschule" ~ -0.8
  
)



# ------------------------------------------------------------
# 5. DIGITALE AUSSTATTUNG GENERIEREN (X)
# ------------------------------------------------------------

# Die digitale Ausstattung wird beeinflusst durch:
# - Sozialindex
# - Schulgröße
# - Zufallsvariation

devices_z <-
  
  simulation_parameters$social_to_devices * social_z +
  
  simulation_parameters$size_to_devices * size_z +
  
  rnorm(
    n,
    mean = 0,
    sd = simulation_parameters$devices_noise
  )


# ------------------------------------------------------------
# 6. MATHEMATIKLEISTUNG GENERIEREN (Y)
# ------------------------------------------------------------

# Die Mathematikleistung wird beeinflusst durch:
# - Sozialindex
# - Lehrerfahrung
# - Schulart
# - Zufallsvariation
#
# WICHTIG:
# Es gibt KEINEN direkten Effekt der digitalen
# Ausstattung auf die Mathematikleistung.

math_z <-
  
  simulation_parameters$social_to_math * social_z +
  
  simulation_parameters$experience_to_math * experience_z +
  
  simulation_parameters$schooltype_to_math * school_type_effect +
  
  rnorm(
    n,
    mean = 0,
    sd = simulation_parameters$math_noise
  )


# ------------------------------------------------------------
# 7. DATENSATZ ERSTELLEN
# ------------------------------------------------------------

schools <- tibble(
  
  school_id = paste0(
    "S",
    sprintf("%03d", 1:n)
  ),
  
  # X: Digitale Ausstattung
  digital_devices = round(
    40 + 12 * devices_z,
    1
  ),
  
  # Y: Mathematikleistung
  math_performance = round(
    500 + 35 * math_z,
    1
  ),
  
  # Potenzielle Drittvariablen
  social_index = round(
    social_index,
    1
  ),
  
  school_size = round(
    school_size
  ),
  
  teacher_experience = round(
    teacher_experience,
    1
  ),
  
  school_type = factor(
    school_type,
    levels = c(
      "Gymnasium",
      "Realschule",
      "Mittelschule"
    )
  )
  
) %>%
  
  mutate(
    
    social_group = cut(
      social_index,
      breaks = c(-Inf, 40, 60, Inf),
      labels = c(
        "Niedrig",
        "Mittel",
        "Hoch"
      )
    )
    
  )


# ------------------------------------------------------------
# 8. DATENQUALITÄT PRÜFEN
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("DATENSIMULATION ABGESCHLOSSEN\n")
cat("========================================\n\n")

cat("Anzahl Schulen:", nrow(schools), "\n")

cat(
  "Fehlende Werte:",
  sum(is.na(schools)),
  "\n\n"
)

print(glimpse(schools))


# ------------------------------------------------------------
# 9. ERSTE ÜBERSICHT
# ------------------------------------------------------------

summary(schools)


# ------------------------------------------------------------
# 10. SIMULATIONSPARAMETER DOKUMENTIEREN
# ------------------------------------------------------------

print(simulation_parameters)
