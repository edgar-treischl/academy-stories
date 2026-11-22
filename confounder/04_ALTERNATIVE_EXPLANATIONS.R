
# ============================================================
# 04_ALTERNATIVE_EXPLANATIONS.R
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
