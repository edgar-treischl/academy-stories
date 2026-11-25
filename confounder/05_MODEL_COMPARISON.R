
library(ggplot2)
library(dplyr)
library(broom)
library(forcats)


model_1 <- lm(
  math_performance ~ digital_devices,
  data = schools
)

model_2 <- lm(
  math_performance ~ digital_devices + social_index,
  data = schools
)


# --------------------------------------------------
# 2. Extract coefficients and confidence intervals
# --------------------------------------------------

results <- bind_rows(
  tidy(model_1, conf.int = TRUE) %>%
    mutate(model = "Model 1: Without social index"),
  tidy(model_2, conf.int = TRUE) %>%
    mutate(model = "Model 2: With social index")
  
) %>%
  filter(term == "digital_devices") %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "Model 1: Without social index",
        "Model 2: With social index"
      )
    )
  )



# 3. Define visual style
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
    plot.margin = margin(12, 15, 12, 15)
  )


p <- ggplot(
  results,
  aes(
    x = estimate,
    y = model
  )
) +
  geom_vline(
    xintercept = 0,
    color = color_line,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  # Confidence intervals
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.15,
    linewidth = 1,
    color = color_points
  ) +
  # Coefficient estimates
  geom_point(
    size = 3.5,
    color = color_points
  ) +
  # Labels
  labs(
    title = "Effect of digital devices on math performance",
    subtitle = "Comparing regression estimates before and after adjusting for social index",
    x = "Regression coefficient (95% confidence interval)",
    y = NULL
  ) +
  scale_y_discrete(
    limits = rev(levels(results$model))
  ) +
  plot_theme

p


# Calculate percentage change
coef_1 <- tidy(model_1) %>%
  filter(term == "digital_devices") %>%
  pull(estimate)

coef_2 <- tidy(model_2) %>%
  filter(term == "digital_devices") %>%
  pull(estimate)

percentage_change <- 
  (coef_2 - coef_1) / abs(coef_1) * 100

# Annotation text
annotation_text <- paste0(
  "Coefficient change: ",
  ifelse(percentage_change > 0, "+", ""),
  round(percentage_change, 1),
  "%"
)


p <- p +
  annotate(
    "label",
    x = Inf,
    y = 1.5,
    label = annotation_text,
    hjust = 1.1,
    vjust = -0.5,
    size = 3.2,
    color = color_text,
    fill = "#F3F6F8",
    label.size = 0,
    fontface = "bold"
  )

p