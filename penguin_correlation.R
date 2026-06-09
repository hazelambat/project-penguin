# Final Project: Correlation Analysis & Statistical Trends
# Elective 1

library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Load data
data("penguins")

# Clean data - remove NA values
penguins_clean <- penguins %>%
  filter(
    !is.na(flipper_length_mm),
    !is.na(bill_length_mm),
    !is.na(species),
    !is.na(island)
  )

cat("Original rows:", nrow(penguins), "\n")
cat("Clean rows:", nrow(penguins_clean), "\n")

# Create scatterplot with trendline
penguin_correlation <- ggplot(penguins_clean, 
                              aes(x = bill_length_mm, 
                                  y = flipper_length_mm,
                                  color = species,
                                  shape = island)) +
  
  geom_point(size = 3, alpha = 0.8) +
  
  geom_smooth(method = "lm",
              se = TRUE,
              alpha = 0.2,
              color = "black",
              linetype = "dashed") +
  
  scale_color_manual(values = c(
    "Adelie" = "#2E8B57",
    "Chinstrap" = "#9370DB",
    "Gentoo" = "#FF69B4"
  )) +
  
  scale_shape_manual(values = c(
    "Biscoe" = 16,
    "Dream" = 17,
    "Torgersen" = 15
  )) +
  
  labs(
    title = "Biometric Correlation Among Antarctic Penguin Species",
    subtitle = "Flipper Length vs Bill Length with Linear Regression Trendline",
    x = "Bill Length (mm)",
    y = "Flipper Length (mm)",
    color = "Species",
    shape = "Island",
    caption = "Data source: palmerpenguins package | Method: lm (linear model)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14,
                              margin = margin(t = 10, r = 0, b = 5, l = 20)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40",
                                 margin = margin(t = 0, r = 0, b = 10, l = 20)),
    plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 30, unit = "pt"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  
  annotate("text", 
           x = 35, y = 245, 
           label = "Positive Correlation Detected", 
           hjust = 0, size = 3.5, 
           color = "gray30", fontface = "italic")

# Display the plot
print(penguin_correlation)

# Save as PNG
ggsave("penguin_correlation.png", 
       plot = penguin_correlation, 
       width = 12, 
       height = 7, 
       units = "in",
       dpi = 300,
       bg = "white")

cat("\nPlot saved as 'penguin_correlation.png'\n")

# Summary statistics
cat("\n")
cat("========================================\n")
cat("SUMMARY STATISTICS\n")
cat("========================================\n")

correlation_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Count = n(),
    Correlation = round(cor(bill_length_mm, flipper_length_mm, use = "complete.obs"), 3),
    Mean_Bill_Length = round(mean(bill_length_mm), 1),
    Mean_Flipper_Length = round(mean(flipper_length_mm), 1)
  )

print(correlation_stats)

overall_cor <- cor(penguins_clean$bill_length_mm, 
                   penguins_clean$flipper_length_mm, 
                   use = "complete.obs")
cat("\nOverall Correlation (all species):", round(overall_cor, 3), "\n")

# ============================================
# INTERPRETATION
# ============================================

cat("\n")
cat("========================================\n")
cat("TREND ANALYSIS & INTERPRETATION\n")
cat("========================================\n")
cat("\n")
cat("1. DIRECTIONAL RELATIONSHIP:\n")
cat("   - There is a STRONG POSITIVE CORRELATION between\n")
cat("     bill length and flipper length (r =", round(overall_cor, 3), ")\n")
cat("   - As bill length increases, flipper length also increases\n")
cat("\n")
cat("2. SPECIES PATTERNS:\n")
cat("   - Gentoo penguins show the strongest correlation\n")
cat("   - Adelie and Chinstrap show moderate positive correlations\n")
cat("   - Each species occupies a distinct cluster in the scatterplot\n")
cat("\n")
cat("3. ISLAND DISTRIBUTION (visualized by shapes):\n")
cat("   - Different islands show different point distributions\n")
cat("   - Biscoe island (circle) primarily hosts Gentoo penguins\n")
cat("   - Dream (triangle) and Torgersen (square) islands mainly host Adelie & Chinstrap\n")
cat("\n")
cat("4. GEOMETRIC GROWTH TRAJECTORIES:\n")
cat("   - The linear trendline indicates positive allometric scaling\n")
cat("   - Longer bills are associated with longer flippers\n")
cat("\n")
cat("5. CONCLUSION:\n")
cat("   The scatterplot successfully demonstrates the positive\n")
cat("   biometric correlation between bill length and flipper length.\n")
cat("========================================\n")