# ------------------------------------------------------------------
# Script GLM + Means + Post-hoc for Fuzzy Index Variables
# (Version with user-specified translations)
# ------------------------------------------------------------------

# Required packages
# Ensure all packages are installed by running the line below if necessary:
# install.packages(c("readxl", "dplyr", "ggplot2", "car", "emmeans", "multcomp", "multcompView", "openxlsx", "ggpattern", "RColorBrewer"))

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(openxlsx)
library(ggpattern)
library(RColorBrewer)


## 1) CLEAR ENVIRONMENT ----
rm(list = ls())
graphics.off()


# ------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------
# Replace "DADOS_GLM.xlsx" with the correct path to your file
# Remember the file needs to be in your working directory or you must provide the full path.
data <- read_excel("DADOS_GLM.xlsx")

# Good practice: ensure categorical variables are factors and rename them to English
data <- data %>%
  dplyr::rename(
    Management = Manejo,
    Crop = Cultura,
    Yield = Produtividade,
    `Crop Morphology` = MrP, # Using the requested name
    `Soil Conservation` = COS,     # Using the requested name
    `Plant Stand` = STAND          # Using the requested name
  ) %>%
  mutate(
    Management = as.factor(Management),
    Crop = as.factor(Crop)
  )

# Translate management codes to English (Figure 3)
mgmt_map <- c(
  CC = "CT",
  CM = "MT",
  PD = "NT"
)
data <- data %>%
  mutate(
    Management = dplyr::recode(as.character(Management), !!!mgmt_map, .default = as.character(Management)),
    Management = factor(Management, levels = c("CT", "MT", "NT"))
  )

# Translate crop codes to English common names (Figure 3)
crop_map <- c(
  CAUPI = "Cowpea",
  CROTA = "Sunn hemp",
  GUANDU = "Pigeon pea",
  MILHETO = "Pearl millet"
)
data <- data %>%
  mutate(
    Crop = dplyr::recode(as.character(Crop), !!!crop_map, .default = as.character(Crop)),
    Crop = factor(Crop, levels = unname(crop_map))
  )

# ------------------------------------------------------------------
# Response variables
# ------------------------------------------------------------------
# Use the new English variable names as specified
response_variables <- c("Yield", "Crop Morphology", "Soil Conservation", "Plant Stand")

# ------------------------------------------------------------------
# Figure 3 output (manuscript panels a–d)
# ------------------------------------------------------------------
fig_dir <- file.path("..", "FIGURAS_USUAIS")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

fig3_map <- c(
  "Yield" = "fig_3a",
  "Crop Morphology" = "fig_3b",
  "Soil Conservation" = "fig_3c",
  "Plant Stand" = "fig_3d"
)

fig3_tag <- c(
  "Yield" = "(a)",
  "Crop Morphology" = "(b)",
  "Soil Conservation" = "(c)",
  "Plant Stand" = "(d)"
)

# Lists to store the results
results <- list()
final_tables <- list()

# --- Style Definitions for Plots (outside the loop) ---
# This ensures color and pattern consistency across all plots.

# Count the number of crops to make the script robust
n_crops <- length(unique(data$Crop))
unique_crops <- unique(data$Crop)

# 1. Pastel color palette
if (n_crops < 3) {
  base_colors <- c("#FBB4AE", "#B3CDE3")
  pastel_colors <- base_colors[1:n_crops]
} else {
  pastel_colors <- brewer.pal(n = n_crops, name = "Pastel1")
}
names(pastel_colors) <- unique_crops

# 2. Hatching patterns (one for each crop)
pattern_list <- c("stripe", "crosshatch", "circle", "wave", "grid", "hexagon", "right45", "left45")
if (n_crops > length(pattern_list)) {
  warning(paste("Warning: The number of crops (", n_crops, ") exceeds the number of unique hatching patterns (", length(pattern_list), "). Patterns will be recycled.", sep=""))
  hatch_patterns <- rep(pattern_list, length.out = n_crops)
} else {
  hatch_patterns <- pattern_list[1:n_crops]
}
names(hatch_patterns) <- unique_crops


# ------------------------------------------------------------------
# Loop for analysis and plot generation
# ------------------------------------------------------------------
for (var in response_variables) {
  
  cat("\n\n#############################\n")
  cat("Analyzing:", var, "\n")
  cat("#############################\n")
  
  # GLM Model - Using backticks (`) for names with spaces
  model_formula <- as.formula(paste0("`", var, "` ~ Management * Crop"))
  model <- lm(model_formula, data = data)
  print(Anova(model, type = "II"))
  
  # Means and post-hoc
  emm <- emmeans(model, ~ Management * Crop)
  contrasts <- contrast(emm, method = "pairwise", adjust = "tukey")
  print(contrasts)
  
  # Grouping letters ('a' = highest mean)
  cld_res <- multcomp::cld(emm, Letters = letters, adjust = "tukey", rev = TRUE)
  print(cld_res)
  
  results[[var]] <- list(model = model, emm = emm, cld = cld_res)
  
  # Organized table for the plot
  tab <- as.data.frame(cld_res) %>%
    dplyr::rename(
      Mean = emmean,
      Error = SE,
      Group = .group
    ) %>%
    mutate(
      Variable = var,
      # Remove whitespace from the letter group for better alignment
      Group = trimws(Group)
    )
  
  final_tables[[var]] <- tab
  
  # --- Plot Generation ---
  # Add a check to ensure there is data to plot
  if (nrow(tab) > 0) {
    g <- ggplot(tab, aes(x = Management, y = Mean, fill = Crop, pattern = Crop)) +
      geom_bar_pattern(
        stat = "identity",
        position = position_dodge(width = 0.9),
        colour = "black",         # Bar border color (black)
        pattern_fill = "gray20",    # Hatching color (dark gray for a softer look)
        pattern_density = 0.1,      # Hatching density (0 to 1)
        pattern_spacing = 0.02,     # Spacing between hatching lines
        pattern_key_scale_factor = 0.6 # Pattern size in the legend
      ) +
      geom_errorbar(
        aes(ymin = Mean - Error, ymax = Mean + Error),
        width = 0.25,
        position = position_dodge(width = 0.9),
        linewidth = 0.5
      ) +
      geom_text(
        # Position the text above the error bar to avoid overlap
        aes(y = Mean + Error, label = Group),
        position = position_dodge(width = 0.9),
        vjust = -0.5, # Vertical adjustment to move the text up
        size = 6.5,
        color = "black"
      ) +
      labs(
        title = paste("Adjusted means for", var),
        y = var,
        x = "Soil Management"
      ) +
      labs(tag = fig3_tag[[var]]) +
      scale_x_discrete(labels = c(
        "CT" = "CT\n(conventional)",
        "MT" = "MT\n(minimum)",
        "NT" = "NT\n(no-tillage)"
      )) +
      scale_fill_manual(values = pastel_colors) +
      scale_pattern_manual(values = hatch_patterns) +
      theme_classic(base_size = 22) + # Enlarged for editor revision (R2 proof)
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.tag = element_text(face = "bold", size = 24),
        plot.tag.position = c(0.02, 0.98),
        axis.title = element_text(size = 22),
        axis.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 16)
      )
    
    # Print the plot to the RStudio plot panel
    print(g)

    # Save manuscript-ready panels for Figure 3 (a–d)
    out_stub <- fig3_map[[var]]
    if (!is.null(out_stub) && !is.na(out_stub)) {
      ggsave(
        filename = file.path(fig_dir, paste0(out_stub, ".png")),
        plot = g,
        width = 8,
        height = 6,
        dpi = 600
      )
    }
  } else {
    cat(paste("\nWARNING: Could not generate plot for '", var, "' because there is no data to plot.\n", sep=""))
  }
  
  # Optional: Save each plot to a high-quality PNG file
  # ggsave(paste0("Plot_", gsub(" ", "_", var), ".png"), plot = g, width = 8, height = 6, dpi = 300)
}

# ------------------------------------------------------------------
# Export combined results to Excel
# ------------------------------------------------------------------
final_table <- bind_rows(final_tables)
write.xlsx(final_table, "Results_GLM_PostHoc.xlsx", row.names = FALSE)

cat("\nFile 'Results_GLM_PostHoc.xlsx' saved successfully!\n")