# Figure 4: IV (2SLS) and multivariable adjusted (OLS) estimates
# Exposure: exposure_var (number of mental health consultation days in general practice)
# Instrument: instrument_var (GP's proxied preference for mental health consultation days per patient)
#
# Horizontal CIs
# IV and OLS estimates on the same line
# Facet columns by outcome, rows by year
# Upright y-axis labels
# Mental vs. somatic noted with A. and B. ONLY
# Year denoted with faceting ONLY
#
# Input:  iv_2SLS_OLS_estimates.csv  (raw esttab output from code_analysis_03_2SLS_OLS.do,
#         run on the SYNTHETIC dataset from code_analysis_00_simulate_data.do)
# Output: Fig4.eps, plus a cleaned estimates file (iv_2SLS_OLS_estimates_cleaned.csv)
#
# NOTE: The data are synthetic, so the estimates differ from those in the manuscript.
#########################################################

library(tidyverse)
library(ggplot2)
library(dplyr)
library(grid)
library(patchwork)
library(ggtext)
library(paletteer)   # only needed to look up the colour codes below (optional)
#########################################################

# USER INPUT (replace with relevant paths) ---------------------------------

projdir <- "path/to/projectdirectory"
resdir  <- file.path(projdir, "results")
figdir  <- file.path(projdir, "figures")

raw_file     <- file.path(resdir, "iv_2SLS_OLS_estimates.csv")           # Stata output (code_analysis_03)
cleaned_file <- file.path(resdir, "iv_2SLS_OLS_estimates_cleaned.csv")   # written by this script
fig4_file    <- file.path(figdir, "Fig4.eps")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

# x-axis limits of the somatic panel (B). c(-0.25, 0.40) was used for the real data;
# the synthetic IV estimates are far larger, so automatic limits (NULL) are used here.
somatic_xlim <- NULL   # or c(-0.25, 0.40)

# Colours used (colorBlindness::paletteMartin)
# paletteer::paletteer_d("colorBlindness::paletteMartin")
# "#000000FF" "#004949FF" "#009292FF" "#FF6DB6FF" "#FFB6DBFF" "#490092FF" "#006DDBFF" "#B66DFFFF" "#6DB6FFFF" "#B6DBFFFF" "#920000FF" "#924900FF" "#DB6D00FF" "#24FF24FF" "#FFFF6DFF"
#########################################################

#########################################################
# 1. ADAPT THE RAW STATA OUTPUT TO THE COLUMNS/HEADERS USED BELOW
#########################################################
# code_analysis_03_2SLS_OLS.do appends one esttab block per model (4 rows each):
#   ,"iv_<outcome>_<j>",...                         <- model name (iv_ or ols_, j = rel_p 0/1/2)
#   ,"b","t","se","p","ci95","min95","max95"        <- column headers
#   "exposure_var", <b>, <t>, <se>, <p>, <ci95>, <min95>, <max95>
#   "N", <n>,...
# The coefficient row is linked to its model name, and the model name is split into
# method (iv/ols), outcome and rel_p. Columns created: Equation, Exposure, Outcome,
# Time, Instrument, b, t, se, p, min95 (= ci_l), max95 (= ci_u).

outcomes <- c("ac_psych_in", "ac_soma_in", "nac_psych_in", "nac_soma_in",
              "nac_psych_out", "nac_soma_out", "ooh_psych", "ooh_soma")
times    <- c("p0", "p+1", "p+2")                                    # rel_p = 0, 1, 2

raw <- read.csv(raw_file, header = FALSE, colClasses = "character", strip.white = TRUE)

# Check that the file has the expected layout (columns: label, b, t, se, p, ci95, min95, max95)
hdr <- raw[raw$V2 == "b", ]
stopifnot(ncol(raw) == 8,
          nrow(hdr) > 0,
          all(hdr$V3 == "t"), all(hdr$V4 == "se"), all(hdr$V5 == "p"),
          all(hdr$V7 == "min95"), all(hdr$V8 == "max95"))

estimates <- raw %>%
  mutate(model = if_else(V1 == "" & grepl("^(iv|ols)_", V2), V2, NA_character_)) %>%
  fill(model) %>%                                   # carry each model name down to its rows
  filter(V1 == "exposure_var") %>%                  # keep the exposure coefficient row of each model
  extract(model, into = c("method", "Outcome", "j"),
          regex = "^(iv|ols)_(.+)_([0-2])$", remove = FALSE) %>%
  transmute(
    model, method, Outcome, j = as.integer(j),
    Exposure   = V1,
    b = as.numeric(V2), t = as.numeric(V3), se = as.numeric(V4), p = as.numeric(V5),
    min95 = as.numeric(V7),                         # ci_l in the Stata code
    max95 = as.numeric(V8)                          # ci_u in the Stata code
  )

if (nrow(estimates) != 2 * length(outcomes) * length(times) || anyDuplicated(estimates$model) > 0 ||
    !all(estimates$Outcome %in% outcomes)) {
  stop("Unexpected models in ", raw_file, ": expected 48 unique models (iv_/ols_ x 8 outcomes x rel_p 0-2). ",
       "Check the file, and that the old output was erased before appending.")
}

cleaned <- estimates %>%
  mutate(
    Time       = times[j + 1],
    Equation   = if_else(method == "iv", "adjusted_IV", "adjusted_regular"),
    Instrument = if_else(method == "iv", "instrument_var", "OLS")
  ) %>%
  select(Equation, Exposure, Outcome, Time, Instrument, b, t, se, p, min95, max95)

write.csv(cleaned, cleaned_file, row.names = FALSE)
#########################################################

#########################################################
# 2. READ CLEANED ESTIMATES AND PREPARE LABELS
#########################################################

main_2sls_ols <- read.csv(cleaned_file, header = TRUE, sep = ",")
main_2sls_ols$Equation <- factor(main_2sls_ols$Equation)
main_2sls_ols$Exposure <- factor(main_2sls_ols$Exposure)
main_2sls_ols$Outcome <- factor(main_2sls_ols$Outcome)
main_2sls_ols$Time <- factor(main_2sls_ols$Time)

main_2sls_ols <- main_2sls_ols %>%
  mutate(care_type = case_when(
    Outcome %in% c("ooh_soma", "nac_soma_out", "nac_soma_in", "ac_soma_in") ~ "Somatic healthcare",
    Outcome %in% c("ooh_psych", "nac_psych_out", "nac_psych_in", "ac_psych_in") ~ "Mental healthcare"
  )) %>%
  mutate(new_outcome_label = case_when(
    Outcome %in% c("ooh_soma", "ooh_psych") ~ "Out-of-hours",
    Outcome %in% c("nac_soma_out", "nac_psych_out") ~ "Non-acute outpatient",
    Outcome %in% c("nac_soma_in", "nac_psych_in") ~ "Non-acute inpatient",
    Outcome %in% c("ac_soma_in", "ac_psych_in") ~ "Acute inpatient"
  )) %>%
  mutate(new_time_label = case_when(
    Time %in% c("p0") ~ "1 year after mental health diagnosis",
    Time %in% c("p+1") ~ "2 years after mental health diagnosis",
    Time %in% c("p+2") ~  "3 years after mental health diagnosis"
  )) %>%
  mutate(new_fill_label = case_when(
    care_type == "Mental healthcare" & new_time_label == "1 year after mental health diagnosis" ~ "Mental healthcare P0",
    care_type == "Mental healthcare" & new_time_label == "2 years after mental health diagnosis" ~ "Mental healthcare P+1",
    care_type == "Mental healthcare" & new_time_label == "3 years after mental health diagnosis" ~ "Mental healthcare P+2",
    care_type == "Somatic healthcare" ~ "Somatic healthcare"
  )) %>%
  mutate(new_equation_label = case_when(
    Equation %in% c("adjusted_IV") ~ "Adjusted IV",
    Equation %in% c("non-adjusted_IV") ~ "Non-adjusted IV",
    Equation %in% c("adjusted_regular") ~  "Adjusted OLS",
    Equation %in% c("non-adjusted_regular") ~ "Non-adjusted OLS"
  )) %>%
  mutate(facet_IV_vs_OLS = case_when(
    Equation %in% c("adjusted_IV", "non-adjusted_IV") ~ "IV",
    Equation %in% c("adjusted_regular", "non-adjusted_regular") ~ "OLS"
  )) %>%
  mutate(facet_adj_vs_nonadj = case_when(
    Equation %in% c("adjusted_IV", "adjusted_regular") ~ "Adjusted",
    Equation %in% c("non-adjusted_IV", "non-adjusted_regular") ~ "Non-adjusted"
  )) %>% 
  mutate(new_exposure_label = case_when(
    Exposure %in% c("exposure_var") ~ "Long mental health consultations in general practice in P+1 & P+2"
  )) %>% 
  mutate(new_instrument_label = case_when(
    Instrument %in% c("OLS") ~ "Multivariable adjusted",
    Instrument %in% c("instrument_var") ~ "Instrumental variable-- GP's practice style for mental health consultations"
  ))



# Ensure the new label variables are factor variables with the desired order

main_2sls_ols$new_outcome_label <- factor(main_2sls_ols$new_outcome_label, 
                                          levels = c("Acute inpatient", "Non-acute inpatient", # Levels order is opposite to what will appear on graph
                                                     "Non-acute outpatient", "Out-of-hours"))
main_2sls_ols$care_type <- factor(main_2sls_ols$care_type, levels = c("Mental healthcare", "Somatic healthcare"))
main_2sls_ols$Time <- factor(main_2sls_ols$Time, levels = c("p0", "p+1", "p+2"))  # Custom order for time variable
main_2sls_ols$new_time_label <- factor(main_2sls_ols$new_time_label, levels = c("1 year after mental health diagnosis",
                                                                                "2 years after mental health diagnosis",
                                                                                "3 years after mental health diagnosis"))
main_2sls_ols$Equation <- factor(main_2sls_ols$Equation, levels = c("non-adjusted_regular", "adjusted_regular", "non-adjusted_IV", "adjusted_IV"))
main_2sls_ols$new_equation_label <- factor(main_2sls_ols$new_equation_label, levels=c("Adjusted IV", "Non-adjusted IV", "Adjusted OLS", "Non-adjusted OLS"))
main_2sls_ols$facet_IV_vs_OLS<- factor(main_2sls_ols$facet_IV_vs_OLS, levels=c("OLS", "IV"))
main_2sls_ols$facet_adj_vs_nonadj <- factor(main_2sls_ols$facet_adj_vs_nonadj, levels=c("Adjusted", "Non-adjusted"))
main_2sls_ols$new_exposure_label <- factor(main_2sls_ols$new_exposure_label, levels = c("Long mental health consultations in general practice in P+1 & P+2"))
main_2sls_ols$new_instrument_label <- factor(main_2sls_ols$new_instrument_label, levels = c("Instrumental variable-- GP's practice style for mental health consultations",
                                                                                            "Multivariable adjusted"
))

#########################################################
#Filter
main_2sls_ols_mental <- main_2sls_ols %>% 
  filter(!care_type %in% c("Somatic healthcare"))

main_2sls_ols_somatic <- main_2sls_ols %>% 
  filter(!care_type %in% c("Mental healthcare"))

#########################################################
#Plots
#########################################################


(plot_main_2sls_ols_mental <- ggplot(main_2sls_ols_mental, aes(x = b, y = new_time_label, color = facet_IV_vs_OLS, shape = new_instrument_label)) +
   scale_shape_manual(values = c(16,17)) +
   geom_vline(xintercept=0, linetype = "dashed", color = "red") + # Add vertical lines using the new data frame (needs to go before geom_point so that the lines don't overlay the points)
   geom_errorbar(aes(xmin = min95, xmax = max95), 
                 width = 0.3,
                 linewidth = 1, 
                 orientation = "y") +  # Error bars with dodge
   geom_point(size = 3.5, stroke = 2) +  # Plot points with dodge
   theme_minimal() +  # Minimal theme
   labs(
     x = "Mean difference\n(95% confidence interval)", 
     y = "Year(s) after mental health diagnosis",
     color = "Year relative to first MH cons.",
     shape = "Care type",
     title = "A. Mental healthcare"
   ) +
   facet_grid(
     cols = vars(new_outcome_label),
     rows = vars(new_time_label),
     switch = "y",
     scale = "free_y",
     space = "free_y"
   ) +
   scale_color_manual(values = c(
     "IV" = "#000000FF", 
     "OLS" = "#6DB6FFFF"
   )) +  # Custom colors for patient groups
   theme(
     legend.position = "bottom",  # Move legend to bottom of plot
     legend.title = element_blank(),  # Remove titles for legend
     legend.background = element_rect(fill = "gray90", linewidth = 0.5, linetype = "blank"),  # Add filled box behind legend text/keys
     legend.text = element_markdown(size = 8),
     legend.box = "vertical", # Ensures that legends are stacked on top of each other
     legend.box.just = "center", # Centers the legends to plot
     plot.title = element_text(size = 12, face = "bold"),
     plot.title.position = "plot", # Can be "panel" (aligned relative to plot only (yes it's confusing)) or "plot" (aligned relative to whole plot)
     strip.text.x = element_textbox(
       size = 10,
       face = "bold",
       color = "black", fill = "white", box.color = "black",
       halign = 0.5, 
       linetype = 1, r = unit(0, "pt"), 
       width = unit(1, "npc"),
       padding = margin(3, 0, 3, 0), margin = margin(3, 0, 1, 0)
     ),  # Markdown for facet titles
     strip.text.y.left = element_textbox(
       size = 8,
       face = "bold",
       color = "black", fill = "white", box.color = "white",
       orientation = "upright", #can be "upright", "left-rotated", "right-rotated", or "inverted"
       halign = 0.5, 
       linetype = 1, r = unit(0, "pt"),
       width = unit(40, "pt"), #NULL means take up exactly the space necessary to render all content. Use a value of unit(1, "npc") to have the box take up all available space.
       padding = margin(3, 0, 3, 0), margin = margin(0, 0, 0, 0)
     ),
     strip.placement = "outside",
     axis.title.y = element_blank(),
     axis.text.y = element_blank(),  # Change font elements for y axis labels
     axis.title.x = element_text(size = 10, face = "bold"),  # Italicize x axis title
     axis.text.x = element_text(size = 10),  # Change font elements for x axis labels
     panel.grid.minor = element_blank(),  # Remove minor grid lines
     panel.grid.major.x = element_blank(), # Remove major grid lines on y axis
     panel.spacing.x = unit(0.3, "lines"),  # Adjust spacing between facets
     panel.spacing.y = unit(0.0, "lines"),
     panel.border = element_rect(color = "#666666", fill = NA, linewidth = 0.5, linetype = "solid"),
     axis.ticks.x = element_line(color = "#666666", linewidth = 0.5),
     plot.margin = margin(0, 0, 0, 0)
   ) +
   guides(
     color = "none",  # Color legend appears first
     shape = guide_legend(order = 1, ncol = 2, override.aes = list(shape = c(16, 17),
                                                                   color= c("black", "#6DB6FFFF"))), # Change the shape legend keys
     fill = "none" # Fill legend removed
   )
)


###################
(plot_main_2sls_ols_somatic <- ggplot(main_2sls_ols_somatic, aes(x = b, y = new_time_label, color = facet_IV_vs_OLS, shape = new_instrument_label)) +
   scale_shape_manual(values = c(16,17)) +
   coord_cartesian(xlim = somatic_xlim) +
   geom_vline(xintercept=0, linetype = "dashed", color = "red") + # Add vertical lines using the new data frame (needs to go before geom_point so that the lines don't overlay the points)
   geom_errorbar(aes(xmin = min95, xmax = max95), 
                 width = 0.3,
                 linewidth = 1, 
                 orientation = "y") +  # Error bars with dodge
   geom_point(size = 3.5, stroke = 2) +  # Plot points with dodge
   theme_minimal() +  # Minimal theme
   labs(
     x = "Mean difference\n(95% confidence interval)",
     y = "Care Types",
     color = "Year relative to first MH cons.",
     shape = "Care type",
     title = "B. Somatic healthcare"
   ) +
   facet_grid(
     cols = vars(new_outcome_label),
     rows = vars(new_time_label),
     switch = "y",
     scale = "free_y",
     space = "free_y"
   ) +
   scale_color_manual(values = c(
     "IV" = "#000000FF", 
     "OLS" = "#6DB6FFFF"
   )) +  # Custom colors for patient groups
   theme(
     legend.position = "bottom",  # Move legend to bottom of plot
     legend.title = element_blank(),  # Remove titles for legend
     legend.background = element_rect(fill = "gray90", linewidth = 0.5, linetype = "blank"),  # Add filled box behind legend text/keys
     legend.text = element_markdown(size = 8),
     legend.box = "vertical", # Ensures that legends are stacked on top of each other
     legend.box.just = "center", # Centers the legends to plot
     plot.title = element_text(size = 12, face = "bold"),
     plot.title.position = "plot", # Can be "panel" (aligned relative to plot only (yes it's confusing)) or "plot" (aligned relative to whole plot)
     strip.text.x = element_textbox(
       size = 10,
       face = "bold",
       color = "black", fill = "white", box.color = "black",
       halign = 0.5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
       padding = margin(3, 0, 3, 0), margin = margin(3, 0, 1, 0)
     ),  # Markdown for facet titles
     strip.text.y.left = element_textbox(
       size = 8,
       face = "bold",
       color = "black", fill = "white", box.color = "white",
       orientation = "upright", #can be "upright", "left-rotated", "right-rotated", or "inverted"
       halign = 0.5,
       linetype = 1, r = unit(0, "pt"),
       width = unit(40, "pt"), #NULL means take up exactly the space necessary to render all content. Use a value of unit(1, "npc") to have the box take up all available space.
       padding = margin(3, 0, 3, 0), margin = margin(0, 0, 0, 0)
     ),
     strip.placement = "outside",
     axis.title.y = element_blank(),  # Remove y-axis title for simplicity
     axis.text.y = element_blank(),  # Change font elements for y axis labels
     axis.title.x = element_text(size = 10, face = "bold"),  # Italicize x axis title
     axis.text.x = element_text(size = 10),  # Change font elements for x axis labels
     panel.grid.minor = element_blank(),  # Remove minor grid lines
     panel.grid.major.x = element_blank(), # Remove major grid lines on y axis
     panel.spacing.x = unit(0.3, "lines"),  # Adjust spacing between facets
     panel.spacing.y = unit(0.0, "lines"),
     panel.border = element_rect(color = "#666666", fill = NA, linewidth = 0.5, linetype = "solid"),
     axis.ticks.x = element_line(color = "#666666", linewidth = 0.5),
     plot.margin = margin(0, 0, 0, 0)
   ) +
   guides(
     color = "none",  # Color legend appears first
     shape = guide_legend(order = 1, ncol = 2, override.aes = list(shape = c(16, 17),
                                                                   color= c("black", "#6DB6FFFF"))), # Change the shape legend keys
     fill = "none" # Fill legend removed
   )
)


###################

#To save the plots in one group (1 column)

(plot_main_2sls_ols_mental_somatic <- plot_main_2sls_ols_mental / plot_main_2sls_ols_somatic / guide_area() +
   plot_layout(guides = "collect", ncol = 1, heights = c(1, 1, 0.11))
 + plot_annotation(theme = theme(legend.position = "bottom", legend.box = "vertical", legend.justification = "center", plot.margin = margin(0, 0, 0, 0))))

###################
ggsave(fig4_file, device = cairo_ps, plot = plot_main_2sls_ols_mental_somatic, width = 17.0, height = 20, units = c("cm"), dpi = 600, bg = "white")

#ggsave(file.path(figdir, "Figure_4.png"), device = png, type = "cairo", plot = plot_main_2sls_ols_mental_somatic, width = 170, height = 225, units = c("mm"), dpi = 600, bg = "white")
