
# ============================================================
# 07_PLOT_CATALOG.R
# TEIL 1: GRUNDLAGEN & PLOT-KATALOG
# ============================================================

library(tidyverse)
library(patchwork)
library(viridis)
library(jsonlite)


# ------------------------------------------------------------
# 1. VORAUSSETZUNGEN
# ------------------------------------------------------------

stopifnot(
  exists("schools"),
  all(
    c(
      "digital_devices",
      "math_performance",
      "social_index",
      "school_size",
      "teacher_experience",
      "school_type"
    ) %in% names(schools)
  )
)


# Sozialindex-Gruppen

if (!"social_group" %in% names(schools)) {
  
  schools <- schools %>%
    mutate(
      social_group = cut(
        social_index,
        breaks = c(-Inf, 40, 60, Inf),
        labels = c("Niedrig", "Mittel", "Hoch"),
        ordered_result = TRUE
      )
    )
  
}


# ------------------------------------------------------------
# 2. DESIGN-SYSTEM
# ------------------------------------------------------------

COLORS <- list(
  primary = "#2878A0",
  secondary = "#D55E00",
  tertiary = "#7A68A6",
  neutral = "#718096",
  background = "#FFFFFF",
  grid = "#E8EDF1",
  highlight = "#E6A23C"
)


plot_theme <- theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      color = "#263746"
    ),
    
    plot.subtitle = element_text(
      size = 10,
      color = "#718096"
    ),
    
    axis.title = element_text(
      size = 10,
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = COLORS$grid,
      linewidth = 0.4
    ),
    
    legend.title = element_text(
      face = "bold",
      size = 9
    ),
    
    legend.text = element_text(size = 9)
  )


# ------------------------------------------------------------
# 3. VARIABLEN-KATALOG
# ------------------------------------------------------------

variables <- tribble(
  
  ~id, ~label, ~description, ~type, ~unit,
  
  "digital_devices",
  "Digitale Ausstattung",
  "Anzahl digitaler Endgeräte pro 100 Schüler",
  "numeric",
  "Geräte pro 100 Schüler",
  
  "math_performance",
  "Mathematikleistung",
  "Durchschnittlicher Mathematik-Score einer Schule",
  "numeric",
  "Punkte",
  
  "social_index",
  "Sozialindex",
  "Soziale Ausgangslage der Schülerschaft",
  "numeric",
  "Indexpunkte",
  
  "school_size",
  "Schulgröße",
  "Anzahl der Schüler einer Schule",
  "numeric",
  "Schüler",
  
  "teacher_experience",
  "Lehrerfahrung",
  "Durchschnittliche Berufserfahrung der Lehrkräfte",
  "numeric",
  "Jahre",
  
  "school_type",
  "Schulart",
  "Schulform der jeweiligen Schule",
  "categorical",
  "Kategorie"
  
)


# ------------------------------------------------------------
# 4. PLOT-KATALOG
# ------------------------------------------------------------

plot_catalog <- list(
  
  # A: DESKRIPTIVE EXPLORATION
  
  list(
    id = "plot_01",
    title = "Verteilung der Mathematikleistung",
    category = "descriptive",
    type = "histogram",
    x = "math_performance",
    y = NULL,
    color = NULL,
    facet = NULL,
    purpose = "distribution",
    research_question =
      "Wie verteilen sich die Mathematikleistungen?",
    interpretation =
      "Zeigt Verteilung und Streuung der Mathematikleistungen.",
    limitation =
      "Ein Histogramm zeigt keine Zusammenhänge zwischen Variablen.",
    difficulty = 1
  ),
  
  list(
    id = "plot_02",
    title = "Mathematikleistung nach Schulart",
    category = "descriptive",
    type = "boxplot",
    x = "school_type",
    y = "math_performance",
    color = "school_type",
    facet = NULL,
    purpose = "group_comparison",
    research_question =
      "Unterscheiden sich die Mathematikleistungen zwischen Schularten?",
    interpretation =
      "Zeigt Unterschiede der Verteilungen zwischen den Schularten.",
    limitation =
      "Unterschiede zwischen Schularten belegen keine kausale Wirkung.",
    difficulty = 1
  ),
  
  list(
    id = "plot_03",
    title = "Verteilung der digitalen Ausstattung",
    category = "descriptive",
    type = "histogram",
    x = "digital_devices",
    y = NULL,
    color = NULL,
    facet = NULL,
    purpose = "distribution",
    research_question =
      "Wie unterschiedlich sind Schulen digital ausgestattet?",
    interpretation =
      "Zeigt die Verteilung der digitalen Ausstattung.",
    limitation =
      "Die Verteilung erklärt noch keine Zusammenhänge.",
    difficulty = 1
  ),
  
  # B: ZUSAMMENHÄNGE EXPLORIEREN
  
  list(
    id = "plot_04",
    title = "Digitale Ausstattung und Mathematikleistung",
    category = "relationship",
    type = "scatter",
    x = "digital_devices",
    y = "math_performance",
    color = NULL,
    facet = NULL,
    purpose = "initial_relationship",
    research_question =
      "Gibt es einen Zusammenhang zwischen digitaler Ausstattung und Mathematikleistung?",
    interpretation =
      "Zeigt die positive aggregierte Korrelation.",
    limitation =
      "Eine Korrelation allein erlaubt keine Aussage über Ursache und Wirkung.",
    difficulty = 1
  ),
  
  list(
    id = "plot_05",
    title = "Sozialindex und digitale Ausstattung",
    category = "relationship",
    type = "scatter",
    x = "social_index",
    y = "digital_devices",
    color = NULL,
    facet = NULL,
    purpose = "third_variable",
    research_question =
      "Hängt die soziale Ausgangslage mit der digitalen Ausstattung zusammen?",
    interpretation =
      "Zeigt den Zusammenhang zwischen Sozialindex und Ausstattung.",
    limitation =
      "Der Plot erklärt noch nicht den Zusammenhang mit der Mathematikleistung.",
    difficulty = 2
  ),
  
  list(
    id = "plot_06",
    title = "Sozialindex und Mathematikleistung",
    category = "relationship",
    type = "scatter",
    x = "social_index",
    y = "math_performance",
    color = NULL,
    facet = NULL,
    purpose = "third_variable",
    research_question =
      "Hängt die soziale Ausgangslage mit der Mathematikleistung zusammen?",
    interpretation =
      "Zeigt den Zusammenhang zwischen Sozialindex und Mathematikleistung.",
    limitation =
      "Ein Zusammenhang beweist keine kausale Wirkung.",
    difficulty = 2
  ),
  
  list(
    id = "plot_07",
    title = "Digitale Ausstattung und Mathematikleistung nach Sozialindex",
    category = "relationship",
    type = "scatter",
    x = "digital_devices",
    y = "math_performance",
    color = "social_index",
    facet = NULL,
    purpose = "confounder_exploration",
    research_question =
      "Verändert sich die Interpretation, wenn der Sozialindex sichtbar wird?",
    interpretation =
      "Macht die soziale Ausgangslage als zusätzliche Dimension sichtbar.",
    limitation =
      "Eine Farbverteilung allein belegt noch keine Konfundierung.",
    difficulty = 3
  ),
  
  list(
    id = "plot_08",
    title = "Vergleich nach Sozialindex-Gruppen",
    category = "relationship",
    type = "scatter_facet",
    x = "digital_devices",
    y = "math_performance",
    color = NULL,
    facet = "social_group",
    purpose = "within_group_comparison",
    research_question =
      "Bleibt der Zusammenhang innerhalb ähnlicher sozialer Ausgangslagen bestehen?",
    interpretation =
      "Ermöglicht den Vergleich innerhalb der drei Sozialindex-Gruppen.",
    limitation =
      "Die Kategorisierung eines kontinuierlichen Indexes reduziert Information.",
    difficulty = 3
  ),
  
  list(
    id = "plot_09",
    title = "Digitale Ausstattung nach Schulgröße",
    category = "relationship",
    type = "scatter",
    x = "school_size",
    y = "digital_devices",
    color = NULL,
    facet = NULL,
    purpose = "alternative_explanation",
    research_question =
      "Hängt die digitale Ausstattung mit der Schulgröße zusammen?",
    interpretation =
      "Zeigt einen möglichen alternativen Zusammenhang.",
    limitation =
      "Ein Zusammenhang allein erklärt nicht die Mathematikleistung.",
    difficulty = 2
  ),
  
  list(
    id = "plot_10",
    title = "Mathematikleistung nach Lehrerfahrung",
    category = "relationship",
    type = "scatter",
    x = "teacher_experience",
    y = "math_performance",
    color = NULL,
    facet = NULL,
    purpose = "alternative_explanation",
    research_question =
      "Gibt es einen Zusammenhang zwischen Lehrerfahrung und Mathematikleistung?",
    interpretation =
      "Ermöglicht die Untersuchung einer weiteren plausiblen Erklärungsvariable.",
    limitation =
      "Eine fehlende lineare Korrelation schließt andere Zusammenhänge nicht aus.",
    difficulty = 2
  ),
  
  # C: STATISTISCHE ÜBERPRÜFUNG
  
  list(
    id = "plot_11",
    title = "Regression ohne zusätzliche Variablen",
    category = "statistical",
    type = "regression",
    x = "digital_devices",
    y = "math_performance",
    color = NULL,
    facet = NULL,
    purpose = "baseline_model",
    research_question =
      "Wie stark ist der geschätzte lineare Zusammenhang?",
    interpretation =
      "Zeigt die unbereinigte Regressionsgerade und deren Unsicherheit.",
    limitation =
      "Das Modell berücksichtigt keine weiteren erklärenden Variablen.",
    difficulty = 2
  ),
  
  list(
    id = "plot_12",
    title = "Zusammenhang nach Kontrolle des Sozialindex",
    category = "statistical",
    type = "residual",
    x = "digital_devices",
    y = "math_performance",
    color = NULL,
    facet = NULL,
    purpose = "partial_relationship",
    research_question =
      "Bleibt ein Zusammenhang bestehen, wenn der Sozialindex berücksichtigt wird?",
    interpretation =
      "Zeigt den Zusammenhang zwischen den um den Sozialindex bereinigten Variablen.",
    limitation =
      "Die Kontrolle berücksichtigt nur spezifizierte lineare Zusammenhänge.",
    difficulty = 4
  )
  
)


# ------------------------------------------------------------
# 5. KATALOGÜBERSICHT
# ------------------------------------------------------------

catalog_overview <- tibble(
  
  id = map_chr(plot_catalog, "id"),
  
  title = map_chr(plot_catalog, "title"),
  
  category = map_chr(plot_catalog, "category"),
  
  type = map_chr(plot_catalog, "type"),
  
  purpose = map_chr(plot_catalog, "purpose"),
  
  difficulty = map_int(plot_catalog, "difficulty")
  
)

print(catalog_overview)


# ============================================================
# ENDE TEIL 1
# Weiter mit Teil 2: Referenzplots
# ============================================================



# ============================================================
# 07_PLOT_CATALOG.R
# TEIL 2: REFERENZPLOTS
# ============================================================


# Plot 01: Mathematikleistung

p_distribution_math <- ggplot(
  schools,
  aes(x = math_performance)
) +
  geom_histogram(
    bins = 25,
    fill = COLORS$primary,
    color = "white"
  ) +
  labs(
    title = "Verteilung der Mathematikleistung",
    x = "Mathematik-Score",
    y = "Anzahl Schulen"
  ) +
  plot_theme


# Plot 02: Schulart

p_school_type <- ggplot(
  schools,
  aes(
    x = school_type,
    y = math_performance,
    fill = school_type
  )
) +
  geom_boxplot(
    alpha = 0.75,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.4,
    size = 1.8
  ) +
  labs(
    title = "Mathematikleistung nach Schulart",
    x = "Schulart",
    y = "Mathematik-Score"
  ) +
  guides(fill = "none") +
  plot_theme


# Plot 03: Digitale Ausstattung

p_distribution_devices <- ggplot(
  schools,
  aes(x = digital_devices)
) +
  geom_histogram(
    bins = 25,
    fill = COLORS$primary,
    color = "white"
  ) +
  labs(
    title = "Verteilung der digitalen Ausstattung",
    x = "Geräte pro 100 Schüler",
    y = "Anzahl Schulen"
  ) +
  plot_theme


# Plot 04: Ausgangskorrelation

p_devices_math <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Digitale Ausstattung und Mathematikleistung",
    x = "Geräte pro 100 Schüler",
    y = "Mathematik-Score"
  ) +
  plot_theme


# Plot 05: Sozialindex und Ausstattung

p_social_devices <- ggplot(
  schools,
  aes(
    x = social_index,
    y = digital_devices
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Sozialindex und digitale Ausstattung",
    x = "Sozialindex",
    y = "Geräte pro 100 Schüler"
  ) +
  plot_theme


# Plot 06: Sozialindex und Mathematikleistung

p_social_math <- ggplot(
  schools,
  aes(
    x = social_index,
    y = math_performance
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Sozialindex und Mathematikleistung",
    x = "Sozialindex",
    y = "Mathematik-Score"
  ) +
  plot_theme


# Plot 07: Sozialindex als Farbvariable

p_social_color <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance,
    color = social_index
  )
) +
  geom_point(
    size = 2.7,
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "C",
    name = "Sozialindex"
  ) +
  labs(
    title = "Digitale Ausstattung und Mathematikleistung",
    subtitle = "Zusätzliche Information: Sozialindex",
    x = "Geräte pro 100 Schüler",
    y = "Mathematik-Score"
  ) +
  plot_theme


# Plot 08: Sozialindex-Gruppen

p_social_facet <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.65,
    size = 2.2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  facet_wrap(
    ~ social_group,
    nrow = 1
  ) +
  labs(
    title = "Zusammenhang innerhalb der Sozialindex-Gruppen",
    x = "Geräte pro 100 Schüler",
    y = "Mathematik-Score"
  ) +
  plot_theme


# Plot 09: Schulgröße

p_school_size <- ggplot(
  schools,
  aes(
    x = school_size,
    y = digital_devices
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Schulgröße und digitale Ausstattung",
    x = "Anzahl Schüler",
    y = "Geräte pro 100 Schüler"
  ) +
  plot_theme


# Plot 10: Lehrerfahrung

p_teacher_experience <- ggplot(
  schools,
  aes(
    x = teacher_experience,
    y = math_performance
  )
) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Lehrerfahrung und Mathematikleistung",
    x = "Durchschnittliche Lehrerfahrung (Jahre)",
    y = "Mathematik-Score"
  ) +
  plot_theme


# Plot 11: Unbereinigte Regression

p_regression <- p_devices_math


# Plot 12: Residuenanalyse

model_x <- lm(
  digital_devices ~ social_index,
  data = schools
)

model_y <- lm(
  math_performance ~ social_index,
  data = schools
)

schools_analysis <- schools %>%
  mutate(
    devices_residual = resid(model_x),
    math_residual = resid(model_y)
  )


p_residual <- ggplot(
  schools_analysis,
  aes(
    x = devices_residual,
    y = math_residual
  )
) +
  geom_hline(
    yintercept = 0,
    color = "#C7D0D9",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    color = "#C7D0D9",
    linetype = "dashed"
  ) +
  geom_point(
    color = COLORS$primary,
    alpha = 0.7,
    size = 2.4
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = COLORS$secondary,
    fill = COLORS$secondary,
    alpha = 0.12
  ) +
  labs(
    title = "Zusammenhang nach Kontrolle des Sozialindex",
    x = "Bereinigte digitale Ausstattung",
    y = "Bereinigte Mathematikleistung"
  ) +
  plot_theme


# ------------------------------------------------------------
# 13. REFERENZPLOTS REGISTRIEREN
# ------------------------------------------------------------

reference_plots <- list(
  
  plot_01 = p_distribution_math,
  plot_02 = p_school_type,
  plot_03 = p_distribution_devices,
  plot_04 = p_devices_math,
  plot_05 = p_social_devices,
  plot_06 = p_social_math,
  plot_07 = p_social_color,
  plot_08 = p_social_facet,
  plot_09 = p_school_size,
  plot_10 = p_teacher_experience,
  plot_11 = p_regression,
  plot_12 = p_residual
  
)


# ============================================================
# ENDE TEIL 2
# Weiter mit Teil 3: Ergebnisse & Export
# ============================================================


# ============================================================
# 07_PLOT_CATALOG.R
# TEIL 3: ERGEBNISSE & EXPORT
# ============================================================


# ------------------------------------------------------------
# 14. STATISTISCHE REFERENZERGEBNISSE
# ------------------------------------------------------------

model_baseline <- lm(
  math_performance ~ digital_devices,
  data = schools
)

model_adjusted <- lm(
  math_performance ~ digital_devices + social_index,
  data = schools
)


reference_results <- list(
  
  correlations = list(
    
    devices_math = cor(
      schools$digital_devices,
      schools$math_performance
    ),
    
    social_devices = cor(
      schools$social_index,
      schools$digital_devices
    ),
    
    social_math = cor(
      schools$social_index,
      schools$math_performance
    )
    
  ),
  
  models = list(
    
    baseline = model_baseline,
    
    adjusted = model_adjusted
    
  ),
  
  partial_correlation = cor(
    resid(lm(
      digital_devices ~ social_index,
      data = schools
    )),
    resid(lm(
      math_performance ~ social_index,
      data = schools
    ))
  )
  
)


# ------------------------------------------------------------
# 15. REFERENZ-GALERIE
# ------------------------------------------------------------

plot_gallery <-
  
  (p_distribution_math | p_school_type) /
  
  (p_devices_math | p_social_color) /
  
  (p_social_facet | p_residual) +
  
  plot_annotation(
    
    title = "Explorative Analyse: Digitale Ausstattung und Mathematikleistung",
    
    subtitle = "Ausgewählte Visualisierungen der simulierten Schuldaten",
    
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold"
      ),
      
      plot.subtitle = element_text(
        size = 11,
        color = "#718096"
      )
    )
    
  )


print(plot_gallery)


# ------------------------------------------------------------
# 16. STATISTISCHE KENNZAHLEN
# ------------------------------------------------------------

reference_summary <- tibble(
  
  metric = c(
    "cor_devices_math",
    "cor_social_devices",
    "cor_social_math",
    "beta_devices_baseline",
    "beta_devices_adjusted",
    "partial_correlation"
  ),
  
  value = c(
    
    reference_results$correlations$devices_math,
    
    reference_results$correlations$social_devices,
    
    reference_results$correlations$social_math,
    
    coef(model_baseline)["digital_devices"],
    
    coef(model_adjusted)["digital_devices"],
    
    reference_results$partial_correlation
    
  )
  
)


print(reference_summary)


# ------------------------------------------------------------
# 17. EXPORTVERZEICHNIS
# ------------------------------------------------------------

export_dir <- "export"

dir.create(
  export_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# ------------------------------------------------------------
# 18. SCHULDATEN EXPORTIEREN
# ------------------------------------------------------------

write_csv(
  schools,
  file.path(
    export_dir,
    "schools.csv"
  )
)


# ------------------------------------------------------------
# 19. VARIABLEN-KATALOG EXPORTIEREN
# ------------------------------------------------------------

write_json(
  variables,
  file.path(
    export_dir,
    "variables.json"
  ),
  pretty = TRUE,
  auto_unbox = TRUE
)


# ------------------------------------------------------------
# 20. PLOT-KATALOG EXPORTIEREN
# ------------------------------------------------------------

write_json(
  plot_catalog,
  file.path(
    export_dir,
    "plots.json"
  ),
  pretty = TRUE,
  auto_unbox = TRUE
)


# ------------------------------------------------------------
# 21. REFERENZERGEBNISSE EXPORTIEREN
# ------------------------------------------------------------

write_csv(
  reference_summary,
  file.path(
    export_dir,
    "reference_results.csv"
  )
)


# ------------------------------------------------------------
# 22. ABSCHLUSSPRÜFUNG
# ------------------------------------------------------------

stopifnot(
  
  length(plot_catalog) == 12,
  
  length(reference_plots) == 12,
  
  nrow(schools) > 0,
  
  file.exists(
    file.path(export_dir, "schools.csv")
  ),
  
  file.exists(
    file.path(export_dir, "variables.json")
  ),
  
  file.exists(
    file.path(export_dir, "plots.json")
  )
  
)


# ------------------------------------------------------------
# 23. STATUS
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("PLOT-KATALOG ERFOLGREICH ERSTELLT\n")
cat("========================================\n")

cat(
  "Anzahl Schulen:",
  nrow(schools),
  "\n"
)

cat(
  "Anzahl definierter Plots:",
  length(plot_catalog),
  "\n"
)

cat(
  "Anzahl Referenzplots:",
  length(reference_plots),
  "\n"
)

cat(
  "Ursprüngliche Korrelation:",
  round(reference_results$correlations$devices_math, 3),
  "\n"
)

cat(
  "Partielle Korrelation:",
  round(reference_results$partial_correlation, 3),
  "\n"
)

cat(
  "Exportverzeichnis:",
  export_dir,
  "\n"
)

cat("========================================\n")
