# Midterm Project: Penguin Distribution Dashboard
# Elective 1

library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Load data
data("penguins")

# Clean data - remove NA values 
penguins_clean <- penguins %>%
  filter(!is.na(species), !is.na(body_mass_g))

# Create faceted multi-panel figure with gray borders
penguin_plot <- ggplot(penguins_clean, 
                       aes(x = species, 
                           y = body_mass_g, 
                           fill = species)) +
  
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.2, 
               alpha = 0.5, 
               fill = "white",
               outlier.color = "red",
               outlier.size = 2) +
  
  facet_wrap(~species, scales = "free_x") +
  
  # NEW COLORS: Green, Purple, Pink
  scale_fill_manual(values = c("Adelie" = "#2E8B57",  
                               "Chinstrap" = "#9370DB",  
                               "Gentoo" = "#FF69B4")) + 
  
  labs(
    title = "Physiological Variations Among Antarctic Penguin Species",
    subtitle = "Distribution of Body Mass by Species | Faceted Multi-Panel Display",
    x = "Penguin Species",
    y = "Body Mass (g)",
    fill = "Species",
    caption = "Data source: palmerpenguins package"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14,
                              margin = margin(t = 10, r = 0, b = 5, l = 25)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40",
                                 margin = margin(t = 0, r = 0, b = 10, l = 25)),
    plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 35, unit = "pt"),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "gray50", linewidth = 1),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Display the plot
print(penguin_plot)

# Save as PNG
ggsave("penguin_distribution.png", 
       plot = penguin_plot, 
       width = 13, 
       height = 7, 
       units = "in",
       dpi = 300,
       bg = "white")

cat("\nPlot saved as 'penguin_distribution.png'\n")

# Summary statistics
summary_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Count = n(),
    Mean_Body_Mass_g = round(mean(body_mass_g), 0),
    Median_Body_Mass_g = round(median(body_mass_g), 0),
    Min_Body_Mass_g = min(body_mass_g),
    Max_Body_Mass_g = max(body_mass_g)
  )

print(summary_stats)

# ================================================================
# INTERPRETATION
# ================================================================

cat("\n")
cat("================================================================\n")
cat("TREND ANALYSIS & INTERPRETATION\n")
cat("================================================================\n")
cat("\n")
cat("1. ECOLOGICAL NICHE SEPARATION:\n")
cat("   - Gentoo penguins have significantly higher body mass\n")
cat("     (mean ~5076g) compared to Adelie (~3701g) and\n")
cat("     Chinstrap (~3733g), confirming distinct ecological niches.\n")
cat("\n")
cat("2. DISTRIBUTION PATTERNS:\n")
cat("   - Violin plots show Adelie has the widest distribution,\n")
cat("     indicating more variability in body mass.\n")
cat("   - Chinstrap shows a compact, symmetric distribution.\n")
cat("   - Gentoo shows a right-skewed distribution with some\n")
cat("     outliers on the lower end.\n")
cat("\n")
cat("3. STATISTICAL SPREAD (from boxplots):\n")
cat("   - Gentoo quartiles are entirely separate from the other\n")
cat("     two species, with no overlap in interquartile ranges.\n")
cat("   - Adelie and Chinstrap have overlapping ranges but\n")
cat("     different distribution shapes.\n")
cat("\n")
cat("4. CONCLUSION:\n")
cat("   The faceted multi-panel visualization successfully\n")
cat("   demonstrates that the three penguin species occupy\n")
cat("   completely different morphological spaces based on\n")
cat("   body mass, supporting the concept of ecological\n")
cat("   niche differentiation.\n")
cat("================================================================\n")