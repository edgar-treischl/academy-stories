
# ============================================================
# 08_EXPORT_FOR_REACT.R
#
# Export der simulierten Schuldaten und didaktischen Story
# für eine spätere React-Anwendung.
#
# Voraussetzung:
# - schools aus 01_DATA_SIMULATION.R
# - plot_catalog aus 07_PLOT_CATALOG.R
# - reference_plots / statistische Modelle aus 07_PLOT_CATALOG.R
#
# Keine erneute Datensimulation!
# ============================================================


library(tidyverse)
library(jsonlite)
library(broom)


# ============================================================
# 1. KONFIGURATION
# ============================================================

EXPORT_DIR <- "export"

DATASET_VERSION <- "1.0.0"

STORY_ID <- "confounding_school_education"

dir.create(
  EXPORT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. VORAUSSETZUNGEN PRÜFEN
# ============================================================

required_objects <- c(
  "schools",
  "variables",
  "plot_catalog",
  "reference_summary",
  "reference_results"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1)
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Fehlende R-Objekte: ",
    paste(missing_objects, collapse = ", "),
    "\nBitte vorher 01_DATA_SIMULATION.R und 07_PLOT_CATALOG.R ausführen."
  )
}


# ============================================================
# 3. HILFSFUNKTIONEN
# ============================================================

export_json <- function(object, filename) {
  
  write_json(
    object,
    path = file.path(EXPORT_DIR, filename),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    dataframe = "rows",
    digits = 6
  )
  
  message("JSON exportiert: ", filename)
  
}


export_csv <- function(object, filename) {
  
  write_csv(
    object,
    file.path(EXPORT_DIR, filename),
    na = ""
  )
  
  message("CSV exportiert: ", filename)
  
}


# ============================================================
# 4. SCHULDATEN
# ============================================================

# Nur die eigentlichen Simulationsvariablen exportieren.
# Interne Analysevariablen wie Residuen bleiben separat.

schools_export <- schools %>%
  select(
    school_id,
    digital_devices,
    math_performance,
    social_index,
    school_size,
    teacher_experience,
    school_type,
    any_of("social_group")
  )

export_csv(
  schools_export,
  "schools.csv"
)


# ============================================================
# 5. VARIABLEN-METADATEN
# ============================================================

variables_export <- variables %>%
  mutate(
    dataset_version = DATASET_VERSION
  )

export_json(
  variables_export,
  "variables.json"
)


# ============================================================
# 6. PLOT-KATALOG
# ============================================================

# Enthält die Konfigurationen der möglichen Visualisierungen.
# React kann daraus dynamisch Plotly-Charts erzeugen.

export_json(
  plot_catalog,
  "plots.json"
)


# ============================================================
# 7. STATISTISCHE REFERENZERGEBNISSE
# ============================================================

model_baseline <- reference_results$models$baseline

model_adjusted <- reference_results$models$adjusted


# Koeffizienten der beiden Modelle

baseline_coefficients <- tidy(
  model_baseline,
  conf.int = TRUE
)

adjusted_coefficients <- tidy(
  model_adjusted,
  conf.int = TRUE
)


# Modellübersicht

model_statistics <- bind_rows(
  
  glance(model_baseline) %>%
    mutate(model_id = "baseline"),
  
  glance(model_adjusted) %>%
    mutate(model_id = "adjusted")
  
) %>%
  select(
    model_id,
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value,
    df.residual
  )


# Korrelationen

correlation_matrix <- schools_export %>%
  select(
    where(is.numeric)
  ) %>%
  cor(
    use = "complete.obs"
  )


# Alle statistischen Referenzinformationen bündeln

reference_export <- list(
  
  metadata = list(
    story_id = STORY_ID,
    dataset_version = DATASET_VERSION,
    description = "Statistische Ergebnisse der simulierten Schuldaten"
  ),
  
  correlations = list(
    
    devices_math =
      reference_results$correlations$devices_math,
    
    social_devices =
      reference_results$correlations$social_devices,
    
    social_math =
      reference_results$correlations$social_math,
    
    partial_correlation =
      reference_results$partial_correlation,
    
    matrix = correlation_matrix
    
  ),
  
  models = list(
    
    baseline = list(
      description = "Unbereinigter Zusammenhang",
      coefficients = baseline_coefficients,
      statistics = model_statistics %>%
        filter(model_id == "baseline")
    ),
    
    adjusted = list(
      description = "Zusammenhang unter Kontrolle des Sozialindex",
      coefficients = adjusted_coefficients,
      statistics = model_statistics %>%
        filter(model_id == "adjusted")
    )
    
  ),
  
  interpretation = list(
    
    initial_observation =
      "Zwischen digitaler Ausstattung und Mathematikleistung besteht ein positiver Zusammenhang.",
    
    confounder_candidate =
      "Der Sozialindex hängt sowohl mit der digitalen Ausstattung als auch mit der Mathematikleistung zusammen.",
    
    adjusted_observation =
      "Nach statistischer Kontrolle des Sozialindex ist der eigenständige lineare Zusammenhang zwischen digitaler Ausstattung und Mathematikleistung deutlich schwächer.",
    
    conclusion =
      "Die Ergebnisse sind mit einer Scheinkorrelation aufgrund einer gemeinsamen Drittvariable vereinbar."
    
  )
  
)


export_json(
  reference_export,
  "reference_results.json"
)


# ============================================================
# 8. STORY UND LEARNING PATH
# ============================================================

story_export <- list(
  
  metadata = list(
    
    story_id = STORY_ID,
    
    title = "Digitale Ausstattung und Mathematikleistung",
    
    subtitle = "Entdecke eine mögliche Scheinkorrelation im schulischen Kontext",
    
    version = DATASET_VERSION,
    
    language = "de",
    
    dataset = "schools.csv"
    
  ),
  
  learning_objectives = list(
    
    "Korrelationen erkennen und interpretieren",
    
    "Zusammenhänge explorativ untersuchen",
    
    "Mögliche Drittvariablen identifizieren",
    
    "Konfundierung grafisch untersuchen",
    
    "Unbereinigte und bereinigte Zusammenhänge vergleichen"
    
  ),
  
  learning_path = list(
    
    list(
      id = "step_01",
      title = "Eine erste Beobachtung",
      type = "exploration",
      description = "Untersuche den Zusammenhang zwischen digitaler Ausstattung und Mathematikleistung.",
      task = "Erstelle einen Scatterplot und beschreibe den sichtbaren Zusammenhang.",
      available_variables = c(
        "digital_devices",
        "math_performance"
      ),
      expected_plot = "plot_04"
    ),
    
    list(
      id = "step_02",
      title = "Weitere Variablen entdecken",
      type = "exploration",
      description = "Untersuche, welche weiteren Merkmale mit der digitalen Ausstattung und der Mathematikleistung zusammenhängen.",
      task = "Finde eine Variable, die mit beiden Variablen zusammenhängen könnte.",
      available_variables = c(
        "digital_devices",
        "math_performance",
        "social_index",
        "school_size",
        "teacher_experience",
        "school_type"
      )
    ),
    
    list(
      id = "step_03",
      title = "Die dritte Variable",
      type = "investigation",
      description = "Untersuche den Sozialindex und seine Beziehung zu den beiden ursprünglichen Variablen.",
      task = "Erstelle zwei Scatterplots, um die Beziehungen zu untersuchen.",
      expected_plots = c(
        "plot_05",
        "plot_06"
      )
    ),
    
    list(
      id = "step_04",
      title = "Den Zusammenhang neu betrachten",
      type = "investigation",
      description = "Untersuche, ob der Zusammenhang zwischen Ausstattung und Leistung innerhalb vergleichbarer Sozialindex-Gruppen bestehen bleibt.",
      task = "Nutze Farbe oder Facetten, um den Sozialindex sichtbar zu machen.",
      expected_plots = c(
        "plot_07",
        "plot_08"
      )
    ),
    
    list(
      id = "step_05",
      title = "Die Konfundierung überprüfen",
      type = "analysis",
      description = "Vergleiche den ursprünglichen Zusammenhang mit dem Zusammenhang nach Kontrolle des Sozialindex.",
      task = "Erstelle eine Visualisierung, die deine Vermutung zur Scheinkorrelation verdeutlicht.",
      expected_plot = "plot_12"
    ),
    
    list(
      id = "step_06",
      title = "Erkenntnisse reflektieren",
      type = "reflection",
      description = "Bewerte, was die Analyse über die Beziehung zwischen digitaler Ausstattung und Mathematikleistung aussagt.",
      task = "Erkläre, warum eine Korrelation nicht automatisch einen kausalen Zusammenhang bedeutet."
    )
    
  )
  
)


export_json(
  story_export,
  "story.json"
)


# ============================================================
# 9. OPTIONALE PLOT-DATEN
# ============================================================

# Residuen sind für die React-Anwendung hilfreich,
# wenn die User eine entsprechende Analyse durchführen.

plot_data_export <- list(
  
  residual_analysis = schools_analysis %>%
    select(
      school_id,
      devices_residual,
      math_residual,
      social_index,
      social_group
    ),
  
  group_summary = schools %>%
    group_by(social_group) %>%
    summarise(
      n = n(),
      mean_devices = mean(digital_devices),
      mean_math = mean(math_performance),
      mean_social_index = mean(social_index),
      .groups = "drop"
    )
  
)


export_json(
  plot_data_export,
  "plot_data.json"
)


# ============================================================
# 10. DATENSET-METADATEN
# ============================================================

dataset_metadata <- list(
  
  id = STORY_ID,
  
  version = DATASET_VERSION,
  
  created_by = "R",
  
  description =
    "Simulierter Schuldaten-Datensatz zur explorativen Untersuchung möglicher Konfundierung.",
  
  observations = nrow(schools_export),
  
  variables = names(schools_export),
  
  files = c(
    "schools.csv",
    "variables.json",
    "plots.json",
    "story.json",
    "reference_results.json",
    "plot_data.json"
  )
  
)


export_json(
  dataset_metadata,
  "dataset_metadata.json"
)


# ============================================================
# 11. EXPORT VALIDIEREN
# ============================================================

expected_files <- c(
  "schools.csv",
  "variables.json",
  "plots.json",
  "story.json",
  "reference_results.json",
  "plot_data.json",
  "dataset_metadata.json"
)


file_status <- tibble(
  
  filename = expected_files,
  
  exists = file.exists(
    file.path(EXPORT_DIR, expected_files)
  ),
  
  size_bytes = file.info(
    file.path(EXPORT_DIR, expected_files)
  )$size
  
)


print(file_status)


stopifnot(
  all(file_status$exists),
  all(file_status$size_bytes > 0)
)


# CSV erneut einlesen

schools_check <- read_csv(
  file.path(EXPORT_DIR, "schools.csv"),
  show_col_types = FALSE
)


stopifnot(
  nrow(schools_check) == nrow(schools_export),
  identical(
    names(schools_check),
    names(schools_export)
  )
)


# JSON erneut einlesen

json_files <- expected_files[
  str_detect(expected_files, "\\.json$")
]

for (filename in json_files) {
  
  fromJSON(
    file.path(EXPORT_DIR, filename),
    simplifyVector = FALSE
  )
  
}


# ============================================================
# 12. ABSCHLUSS
# ============================================================

cat("\n")
cat("============================================\n")
cat("REACT EXPORT ERFOLGREICH ABGESCHLOSSEN\n")
cat("============================================\n")

cat(
  "Schulen:",
  nrow(schools_export),
  "\n"
)

cat(
  "Variablen:",
  ncol(schools_export),
  "\n"
)

cat(
  "Plots:",
  length(plot_catalog),
  "\n"
)

cat(
  "Exportierte Dateien:",
  length(expected_files),
  "\n"
)

cat(
  "Verzeichnis:",
  normalizePath(EXPORT_DIR),
  "\n"
)

cat("============================================\n")
