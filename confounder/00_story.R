
# ============================================================
# LEARNING STORY
# 00_story.R
# ============================================================
#
# Titel: Der digitale Bildungsvorsprung
# Thema: Data Literacy – Scheinkorrelation / Confounding
#
# Ziel:
# Eine datenbasierte Untersuchung des scheinbaren
# Zusammenhangs zwischen digitaler Ausstattung und
# Mathematikleistung an Schulen.
#
# ============================================================


# ------------------------------------------------------------
# 1. STORY-KONTEXT
# ------------------------------------------------------------

story_title <- "Der digitale Bildungsvorsprung"

story_subtitle <- "Führt bessere digitale Ausstattung zu besseren Schulleistungen?"

story_context <- paste(
  "Eine Bildungsredaktion untersucht den Zusammenhang",
  "zwischen der digitalen Ausstattung von Schulen und",
  "den Mathematikleistungen ihrer Schülerinnen und Schüler.",
  "",
  "Eine erste Auswertung zeigt:",
  "Schulen mit mehr digitalen Endgeräten erzielen",
  "tendenziell bessere Mathematikleistungen.",
  "",
  "Die Redaktion möchte wissen, ob daraus geschlossen",
  "werden kann, dass eine bessere digitale Ausstattung",
  "zu besseren schulischen Leistungen führt."
)

research_question <- paste(
  "Gibt es einen tatsächlichen Zusammenhang zwischen",
  "der digitalen Ausstattung und der Mathematikleistung?",
  "Oder lässt sich die beobachtete Korrelation durch",
  "andere Faktoren erklären?"
)


# ------------------------------------------------------------
# 2. ANALYTISCHER AUSGANGSPUNKT
# ------------------------------------------------------------

# Diese Informationen dienen der Story-Autorin /
# dem Story-Autor und werden nicht an Lernende ausgegeben.

analysis_context <- list(
  
  research_domain = "Schulische Bildung",
  
  observation = paste(
    "Schulen mit mehr digitalen Endgeräten weisen",
    "im Durchschnitt höhere Mathematikleistungen auf."
  ),
  
  initial_hypothesis = paste(
    "Die digitale Ausstattung könnte positiv mit",
    "der Mathematikleistung zusammenhängen."
  ),
  
  investigation_goal = paste(
    "Untersuchen, ob weitere Variablen den beobachteten",
    "Zusammenhang erklären können."
  )
  
)


# ------------------------------------------------------------
# 3. VARIABLENDEFINITION
# ------------------------------------------------------------

# X: Unabhängige Variable
# Y: Abhängige Variable
# Z: Potenzielle Drittvariablen

variables <- list(
  
  digital_devices = list(
    name = "Digitale Ausstattung",
    description = paste(
      "Anzahl digitaler Endgeräte pro 100 Schülerinnen",
      "und Schüler."
    ),
    type = "numeric",
    role = "independent_variable",
    unit = "Geräte pro 100 Schüler"
  ),
  
  math_performance = list(
    name = "Mathematikleistung",
    description = paste(
      "Durchschnittlicher Mathematik-Score einer Schule."
    ),
    type = "numeric",
    role = "dependent_variable",
    unit = "Punkte"
  ),
  
  social_index = list(
    name = "Sozialindex",
    description = paste(
      "Sozioökonomische Ausgangslage der Schülerschaft.",
      "Höhere Werte stehen für günstigere soziale",
      "Ausgangsbedingungen."
    ),
    type = "numeric",
    role = "candidate_variable",
    unit = "Indexpunkte"
  ),
  
  school_size = list(
    name = "Schulgröße",
    description = paste(
      "Anzahl der Schülerinnen und Schüler einer Schule."
    ),
    type = "numeric",
    role = "candidate_variable",
    unit = "Schüler"
  ),
  
  teacher_experience = list(
    name = "Lehrerfahrung",
    description = paste(
      "Durchschnittliche Berufserfahrung der Lehrkräfte."
    ),
    type = "numeric",
    role = "candidate_variable",
    unit = "Jahre"
  ),
  
  school_type = list(
    name = "Schulart",
    description = paste(
      "Schulart als kategoriale Variable."
    ),
    type = "categorical",
    role = "candidate_variable",
    categories = c(
      "Gymnasium",
      "Realschule",
      "Mittelschule"
    )
  )
  
)


# ------------------------------------------------------------
# 4. DATENSTRUKTUR
# ------------------------------------------------------------

data_description <- list(
  
  observation_unit = "Schule",
  
  n_observations = 180,
  
  observation_period = "Simulierte Querschnittsdaten",
  
  data_source = "Synthetisch generierte Daten",
  
  geographic_scope = "Fiktive Schulen in Deutschland"
  
)


# ------------------------------------------------------------
# 5. STORY-ABLAUF
# ------------------------------------------------------------

story_chapters <- list(
  
  chapter_01 = list(
    title = "Die Ausgangssituation",
    question = "Was wird untersucht?",
    available_variables = c(
      "digital_devices",
      "math_performance"
    )
  ),
  
  chapter_02 = list(
    title = "Erste Datenexploration",
    question = "Wie sehen die Daten aus?",
    available_variables = c(
      "digital_devices",
      "math_performance"
    )
  ),
  
  chapter_03 = list(
    title = "Die auffällige Korrelation",
    question = "Gibt es einen Zusammenhang?",
    available_variables = c(
      "digital_devices",
      "math_performance"
    )
  ),
  
  chapter_04 = list(
    title = "Alternative Erklärungen",
    question = "Welche weiteren Faktoren könnten relevant sein?",
    available_variables = c(
      "digital_devices",
      "math_performance",
      "school_size",
      "teacher_experience",
      "school_type"
    )
  ),
  
  chapter_05 = list(
    title = "Eine weitere Perspektive",
    question = "Welche Rolle spielt die soziale Ausgangslage?",
    available_variables = c(
      "digital_devices",
      "math_performance",
      "social_index"
    )
  ),
  
  chapter_06 = list(
    title = "Die Zusammenhänge neu betrachten",
    question = "Was passiert, wenn wir den Sozialindex berücksichtigen?",
    available_variables = names(variables)
  ),
  
  chapter_07 = list(
    title = "Die Auflösung",
    question = "Welche Schlussfolgerungen lassen sich ziehen?",
    available_variables = names(variables)
  )
  
)


# ------------------------------------------------------------
# 6. GROUND TRUTH
# NUR FÜR DIE STORY-AUTORIN / DEN STORY-AUTOR
# ------------------------------------------------------------

causal_structure <- list(
  
  actual_confounder = "social_index",
  
  direct_effect = list(
    digital_devices_to_math = 0
  ),
  
  relationships = list(
    social_index_to_devices = TRUE,
    social_index_to_math = TRUE,
    school_size_to_devices = TRUE,
    teacher_experience_to_math = TRUE,
    school_type_to_math = TRUE
  ),
  
  interpretation = paste(
    "Der Sozialindex ist die gemeinsame Ursache der",
    "beobachteten Korrelation zwischen digitaler",
    "Ausstattung und Mathematikleistung.",
    "Im Simulationsmodell existiert kein direkter Effekt",
    "der digitalen Ausstattung auf die Mathematikleistung."
  )
  
)


# ------------------------------------------------------------
# 7. LERNZIELE
# ------------------------------------------------------------

learning_objectives <- c(
  
  "Korrelationen erkennen und beschreiben",
  
  "Unterschied zwischen Korrelation und Kausalität verstehen",
  
  "Potenzielle Drittvariablen identifizieren",
  
  "Zusammenhänge zwischen mehreren Variablen untersuchen",
  
  "Konfundierung anhand von Visualisierungen erkennen",
  
  "Datenbasierte Schlussfolgerungen kritisch formulieren"
  
)


# ------------------------------------------------------------
# 8. STORY-INFORMATIONEN AUSGEBEN
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(story_title, "\n")
cat("========================================\n\n")

cat(story_context, "\n\n")

cat("Forschungsfrage:\n")
cat(research_question, "\n\n")

cat("Anzahl Schulen:", data_description$n_observations, "\n")
cat("Forschungsbereich:", data_description$geographic_scope, "\n")
