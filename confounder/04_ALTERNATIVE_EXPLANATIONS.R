
# ============================================================
# 04_ALTERNATIVE_EXPLANATIONS.R
# User can inspect alternative explanations
# ============================================================

# Schulgröße und digitale Ausstattung
p_size <- ggplot(
  schools,
  aes(school_size, digital_devices)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Schulgröße und digitale Ausstattung"
  ) +
  theme_minimal()

# Lehrerfahrung und Mathematikleistung
p_experience <- ggplot(
  schools,
  aes(teacher_experience, math_performance)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Lehrerfahrung und Mathematikleistung"
  ) +
  theme_minimal()

# Schulart und Mathematikleistung
p_type <- ggplot(
  schools,
  aes(school_type, math_performance)
) +
  geom_boxplot() +
  labs(
    title = "Mathematikleistung nach Schulart"
  ) +
  theme_minimal()

p_size
p_experience
p_type
p_si


color_points <- "#2878A0"
color_line <- "#D55E00"
color_text <- "#263746"
color_grid <- "#E8EDF1"

p_si <- ggplot(
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
    title = "Sozialindex",
    subtitle = "Die Schulen nach Sozialindex eingefärbt",
    x = "Digitale Endgeräte pro 100 Schüler",
    y = "Mathematikleistung"
  ) +
  plot_theme


p_size
p_experience
p_type

p_si
