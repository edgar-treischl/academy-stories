
# ============================================================
# 06_VALIDATION.R
# Prüfung der Simulationsqualität
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. DATENQUALITÄT
# ------------------------------------------------------------

stopifnot(
  nrow(schools) == 180,
  !anyNA(schools)
)

summary(schools)


# ------------------------------------------------------------
# 2. PLAUSIBILITÄT DER VARIABLEN
# ------------------------------------------------------------

stopifnot(
  all(schools$digital_devices >= 0),
  all(schools$math_performance > 0),
  all(schools$school_size > 0),
  all(schools$teacher_experience > 0)
)


# ------------------------------------------------------------
# 3. GRUPPENGRÖSSEN
# ------------------------------------------------------------

schools %>%
  count(school_type)

schools %>%
  count(social_group)


# ------------------------------------------------------------
# 4. ZENTRALE KORRELATIONEN
# ------------------------------------------------------------

cor_original <- cor(
  schools$digital_devices,
  schools$math_performance
)

cor_social_x <- cor(
  schools$social_index,
  schools$digital_devices
)

cor_social_y <- cor(
  schools$social_index,
  schools$math_performance
)

cat("Ursprüngliche Korrelation:", cor_original, "\n")
cat("Sozialindex und Ausstattung:", cor_social_x, "\n")
cat("Sozialindex und Leistung:", cor_social_y, "\n")


# ------------------------------------------------------------
# 5. REGRESSIONEN
# ------------------------------------------------------------

model_1 <- lm(
  math_performance ~ digital_devices,
  data = schools
)

model_2 <- lm(
  math_performance ~ digital_devices + social_index,
  data = schools
)

summary(model_1)
summary(model_2)


# ------------------------------------------------------------
# 6. SCHULART-EFFEKT
# ------------------------------------------------------------

model_school_type <- lm(
  math_performance ~ school_type,
  data = schools
)

summary(model_school_type)

# Deskriptive Unterschiede
schools %>%
  group_by(school_type) %>%
  summarise(
    n = n(),
    mean_math = mean(math_performance),
    sd_math = sd(math_performance),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 7. KONFIDENZINTERVALLE
# ------------------------------------------------------------

confint(model_1)
confint(model_2)


# ------------------------------------------------------------
# 8. VALIDIERUNG DER KERNAUSSAGE
# ------------------------------------------------------------

beta_original <- coef(model_1)["digital_devices"]

beta_adjusted <- coef(model_2)["digital_devices"]

cat("\n")
cat("Koeffizient ohne Kontrolle:", beta_original, "\n")
cat("Koeffizient mit Kontrolle:", beta_adjusted, "\n")

cat(
  "Reduktion des Koeffizienten:",
  round(
    100 * (1 - abs(beta_adjusted / beta_original)),
    1
  ),
  "%\n"
)


# ------------------------------------------------------------
# 9. KORRELATIONSMATRIX ALLER VARIABLEN
# ------------------------------------------------------------

correlation_matrix <- schools %>%
  select(
    digital_devices,
    math_performance,
    social_index,
    school_size,
    teacher_experience
  ) %>%
  cor() %>%
  round(2)

print(correlation_matrix)


# ------------------------------------------------------------
# 10. ZUSAMMENHÄNGE NACH SCHULART
# ------------------------------------------------------------

schools %>%
  group_by(school_type) %>%
  summarise(
    n = n(),
    devices = mean(digital_devices),
    social_index = mean(social_index),
    math = mean(math_performance),
    .groups = "drop"
  )