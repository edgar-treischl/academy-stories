
# ============================================================
# LEARNING STORY
# 05_CONFOUNDER_ANALYSIS.R
# ============================================================
#
# Untersuchung der Rolle des Sozialindex
#
# Vier Perspektiven auf denselben Zusammenhang:
#
# A: Ursprüngliche Korrelation
# B: Sozialindex als zusätzliche Information
# C: Vergleich innerhalb der Sozialindex-Gruppen
# D: Zusammenhang nach statistischer Kontrolle
#
# ============================================================


library(tidyverse)
library(patchwork)
library(viridis)


# ------------------------------------------------------------
# 1. DATEN VORBEREITEN
# ------------------------------------------------------------

# Sozialindex in drei Gruppen einteilen
# Die Grenzen orientieren sich an den Indexwerten.

schools <- schools %>%
  mutate(
    
    social_group = cut(
      social_index,
      breaks = c(-Inf, 40, 60, Inf),
      labels = c(
        "Niedrig",
        "Mittel",
        "Hoch"
      ),
      ordered_result = TRUE
    )
    
  )


# ------------------------------------------------------------
# 2. DESIGN-EINSTELLUNGEN
# ------------------------------------------------------------

color_points <- "#2878A0"
color_line <- "#D55E00"
color_text <- "#263746"
color_grid <- "#E8EDF1"

plot_theme <- theme_minimal(base_size = 12) +
  
  theme(
    
    plot.title = element_text(
      size = 14,
      face = "bold",
      color = color_text,
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      size = 10,
      color = "#718096",
      margin = margin(b = 12)
    ),
    
    axis.title = element_text(
      size = 10,
      face = "bold",
      color = color_text
    ),
    
    axis.text = element_text(
      size = 9,
      color = "#536575"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = color_grid,
      linewidth = 0.4
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(
      12, 15, 12, 15
    )
    
  )


# ------------------------------------------------------------
# 3. STATISTISCHE KENNZAHLEN
# ------------------------------------------------------------

# Ursprüngliche Korrelation
cor_original <- cor(
  schools$digital_devices,
  schools$math_performance,
  use = "complete.obs"
)

# Sozialindex und Ausstattung
cor_social_x <- cor(
  schools$social_index,
  schools$digital_devices,
  use = "complete.obs"
)

# Sozialindex und Mathematikleistung
cor_social_y <- cor(
  schools$social_index,
  schools$math_performance,
  use = "complete.obs"
)


# ------------------------------------------------------------
# 4. PLOT A: URSPRÜNGLICHE KORRELATION
# ------------------------------------------------------------

p1 <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance
  )
) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = color_line,
    fill = color_line,
    alpha = 0.12,
    linewidth = 1
  ) +
  
  geom_point(
    color = color_points,
    alpha = 0.65,
    size = 2.4
  ) +
  
  annotate(
    "label",
    x = Inf,
    y = -Inf,
    label = paste0(
      "r = ",
      round(cor_original, 2)
    ),
    hjust = 1.1,
    vjust = -0.7,
    size = 3.5,
    fontface = "bold",
    fill = "white",
    color = color_text,
    label.size = 0
  ) +
  
  labs(
    title = "A | Der ursprüngliche Zusammenhang",
    subtitle = "Alle Schulen gemeinsam betrachtet",
    x = "Digitale Endgeräte pro 100 Schüler",
    y = "Mathematikleistung"
  ) +
  
  plot_theme


# ------------------------------------------------------------
# 5. PLOT B: SOZIALINDEX ALS ZUSÄTZLICHE INFORMATION
# ------------------------------------------------------------

p2 <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance,
    color = social_index
  )
) +
  
  geom_point(
    size = 2.6,
    alpha = 0.85
  ) +
  
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = color_line,
    linewidth = 1
  ) +
  
  scale_color_viridis_c(
    option = "C",
    begin = 0.1,
    end = 0.9,
    name = "Sozialindex"
  ) +
  
  labs(
    title = "B | Eine zusätzliche Information",
    subtitle = "Die Schulen nach Sozialindex eingefärbt",
    x = "Digitale Endgeräte pro 100 Schüler",
    y = "Mathematikleistung"
  ) +
  
  plot_theme


# ------------------------------------------------------------
# 6. PLOT C: VERGLEICH INNERHALB DER GRUPPEN
# ------------------------------------------------------------

p3 <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance
  )
) +
  
  geom_point(
    color = color_points,
    alpha = 0.65,
    size = 2.2
  ) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = color_line,
    fill = color_line,
    alpha = 0.12,
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ social_group,
    nrow = 1
  ) +
  
  labs(
    title = "C | Vergleich innerhalb der Sozialindex-Gruppen",
    subtitle = "Wie sieht der Zusammenhang innerhalb ähnlicher sozialer Ausgangslagen aus?",
    x = "Digitale Endgeräte pro 100 Schüler",
    y = "Mathematikleistung"
  ) +
  
  plot_theme +
  
  theme(
    strip.text = element_text(
      size = 10,
      face = "bold",
      color = color_text
    ),
    
    strip.background = element_rect(
      fill = "#F0F3F6",
      color = NA
    )
  )


# ------------------------------------------------------------
# 7. PLOT D: ZUSAMMENHANG NACH KONTROLLE
# ------------------------------------------------------------

# Residuen der digitalen Ausstattung
modell_x <- lm(
  digital_devices ~ social_index,
  data = schools
)

# Residuen der Mathematikleistung
modell_y <- lm(
  math_performance ~ social_index,
  data = schools
)

# Residuen zum Datensatz hinzufügen
schools <- schools %>%
  mutate(
    
    devices_residual = resid(modell_x),
    
    math_residual = resid(modell_y)
    
  )


# Partielle Korrelation
cor_adjusted <- cor(
  schools$devices_residual,
  schools$math_residual,
  use = "complete.obs"
)


p4 <- ggplot(
  schools,
  aes(
    x = devices_residual,
    y = math_residual
  )
) +
  
  geom_hline(
    yintercept = 0,
    color = "#C7D0D9",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = 0,
    color = "#C7D0D9",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = color_line,
    fill = color_line,
    alpha = 0.12,
    linewidth = 1
  ) +
  
  geom_point(
    color = color_points,
    alpha = 0.65,
    size = 2.4
  ) +
  
  annotate(
    "label",
    x = Inf,
    y = -Inf,
    label = paste0(
      "Partielle r = ",
      round(cor_adjusted, 2)
    ),
    hjust = 1.1,
    vjust = -0.7,
    size = 3.5,
    fontface = "bold",
    fill = "white",
    color = color_text,
    label.size = 0
  ) +
  
  labs(
    title = "D | Der Zusammenhang nach Kontrolle",
    subtitle = "Beide Variablen um den Sozialindex bereinigt",
    x = "Bereinigte digitale Ausstattung",
    y = "Bereinigte Mathematikleistung"
  ) +
  
  plot_theme


# ------------------------------------------------------------
# 8. VISUALISIERUNGEN ZUSAMMENFÜHREN
# ------------------------------------------------------------

plot_confounder_analysis <-
  
  (p1 | p2) /
  
  (p3 | p4) +
  
  plot_annotation(
    
    title = "Was erklärt den Zusammenhang?",
    
    subtitle = paste(
      "Untersuchung der Beziehung zwischen digitaler Ausstattung",
      "und Mathematikleistung unter Berücksichtigung des Sozialindex"
    ),
    
    caption = paste(
      "Datengrundlage: Synthetische Schuldaten |",
      "Jeder Punkt repräsentiert eine Schule."
    ),
    
    theme = theme(
      
      plot.title = element_text(
        size = 19,
        face = "bold",
        color = color_text
      ),
      
      plot.subtitle = element_text(
        size = 11,
        color = "#718096",
        margin = margin(b = 15)
      ),
      
      plot.caption = element_text(
        size = 9,
        color = "#8996A3"
      )
      
    )
    
  )


print(plot_confounder_analysis)


# ------------------------------------------------------------
# 9. KORRELATIONEN VERGLEICHEN
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("KORRELATIONSVERGLEICH\n")
cat("========================================\n\n")

cat(
  "Ursprüngliche Korrelation:",
  round(cor_original, 3),
  "\n"
)

cat(
  "Sozialindex und digitale Ausstattung:",
  round(cor_social_x, 3),
  "\n"
)

cat(
  "Sozialindex und Mathematikleistung:",
  round(cor_social_y, 3),
  "\n"
)

cat(
  "Partielle Korrelation nach Kontrolle:",
  round(cor_adjusted, 3),
  "\n\n"
)


# ------------------------------------------------------------
# 10. REGRESSIONSMODELLE VERGLEICHEN
# ------------------------------------------------------------

model_1 <- lm(
  math_performance ~ digital_devices,
  data = schools
)

model_2 <- lm(
  math_performance ~ digital_devices + social_index,
  data = schools
)

cat("\n")
cat("MODELL 1: OHNE SOZIALINDEX\n")
print(summary(model_1))

cat("\n")
cat("MODELL 2: MIT SOZIALINDEX\n")
print(summary(model_2))
