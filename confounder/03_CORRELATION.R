
# ============================================================
# 03_CORRELATION.R
# Story Entry Point
# ============================================================

library(ggplot2)

# Korrelationskoeffizient berechnen
cor_value <- cor(
  schools$digital_devices,
  schools$math_performance,
  use = "complete.obs"
)

# Plot erstellen
plot_correlation <- ggplot(
  schools,
  aes(
    x = digital_devices,
    y = math_performance
  )
) +
  
  # Konfidenzband
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#D55E00",
    fill = "#D55E00",
    alpha = 0.13,
    linewidth = 1.2
  ) +
  
  # Datenpunkte
  geom_point(
    color = "#2878A0",
    alpha = 0.68,
    size = 2.8
  ) +
  
  # Korrelationskoeffizient
  annotate(
    "label",
    x = Inf,
    y = -Inf,
    label = paste0(
      "Pearson r = ",
      round(cor_value, 2)
    ),
    hjust = 1.12,
    vjust = -0.8,
    size = 4,
    fontface = "bold",
    color = "#263746",
    fill = "white",
    label.size = 0,
    label.padding = unit(0.25, "lines")
  ) +
  
  # Beschriftungen
  labs(
    title = "Digitale Ausstattung und Mathematikleistung",
    subtitle = "Vorläufige des Zusammenhangs",
    x = "Digitale Endgeräte pro 100 Schüler",
    y = "Durchschnittl. Mathematik-Score",
    caption = "Quelle: Eigene Simulation | Jede Beobachtung repräsentiert eine Schule"
  ) +
  
  # Design
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold",
      color = "#263746",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#667788",
      margin = margin(b = 20)
    ),
    
    axis.title = element_text(
      size = 11,
      face = "bold",
      color = "#34495E"
    ),
    
    axis.text = element_text(
      size = 10,
      color = "#536575"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = "#E8EDF1",
      linewidth = 0.45
    ),
    
    plot.background = element_rect(
      fill = "#FFFFFF",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "#FFFFFF",
      color = NA
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#8996A3",
      hjust = 0,
      margin = margin(t = 15)
    ),
    
    plot.margin = margin(
      20, 25, 15, 15
    )
  )

print(plot_correlation)


# Lineare Regression
model_1 <- lm(
  math_performance ~ digital_devices,
  data = schools
)

summary(model_1)


#ggsave("plot.svg", device = "svg", width = 8, height = 6)

