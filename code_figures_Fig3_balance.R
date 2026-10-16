################################################################################
# Reproducible code for Figure 3: Balance test (Bennett et al. 2026)
#
# Input:
#   results/balance_test.csv
#   Expected plotting-table columns: Exposure, Outcome, b, min95, max95
#
# Outcome and exposure names follow code_analysis_02_balance.do.
# The balance_test.csv must contain one row per estimated coefficient, 
# with the estimate and 95% confidence limits in separate columns.
# The code below converts the spreadsheet to a version usable for generating
# the figure, and also generates and saves the figure.
################################################################################

library(tidyverse)
library(ggplot2)
library(grid)
library(ggtext)
################################################################################
# USER INPUT
  # Set this to the location of the balance_test.csv produced/prepared for plotting.
balance_file <- "results/balance_test.csv"

################################################################################

# Read and reshape the balance results. The source spreadsheet may contain
# repeated blocks: the estimate name (for example, b_pat_age) is in column B
# on the row immediately above that block's column headers. The coefficient
# row contains the instrument_var estimate and confidence interval.
#
# The script converts those blocks into the plotting table expected below:
# Exposure, Outcome, b, min95, max95.
raw_balance <- read.csv(
  balance_file,
  header = FALSE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fill = TRUE,
  colClasses = "character",
  strip.white = TRUE
)
raw_balance[] <- lapply(raw_balance, trimws)

# Find each block's header row by the presence of b, min95, and max95.
cell_is <- function(x, value) !is.na(x) & tolower(trimws(x)) == tolower(value)
header_rows <- which(vapply(seq_len(nrow(raw_balance)), function(i) {
  cells <- unlist(raw_balance[i, ], use.names = FALSE)
  any(cell_is(cells, "b")) && any(cell_is(cells, "min95")) &&
    any(cell_is(cells, "max95"))
}, logical(1)))

# Also accept an already-prepared plotting CSV with the required column names.
if (length(header_rows) == 0 && nrow(raw_balance) > 0 &&
    all(c("Exposure", "Outcome", "b", "min95", "max95") %in% raw_balance[1, ])) {
  balance <- read.csv(
    balance_file, header = TRUE, sep = ",", stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  if (length(header_rows) == 0) {
    stop(
      "Could not find the balance-result header rows. Expected repeated blocks " ,
      "with columns including b, min95, and max95, or a prepared CSV with " ,
      "Exposure, Outcome, b, min95, and max95."
    )
  }

  extracted_rows <- lapply(header_rows, function(header_row) {
    if (header_row <= 1 || header_row >= nrow(raw_balance)) return(NULL)

    # Estimate name is in column B, one row above the block's headers.
    estimate_name <- raw_balance[header_row - 1, 2]
    if (is.na(estimate_name) || !grepl("^b_", estimate_name)) return(NULL)

    header <- tolower(trimws(unlist(raw_balance[header_row, ], use.names = FALSE)))
    value_row <- raw_balance[header_row + 1, , drop = TRUE]
    value_row <- unlist(value_row, use.names = FALSE)

    b_col <- which(header == "b")[1]
    cil_col <- which(header == "min95")[1]
    ciu_col <- which(header == "max95")[1]
    if (any(is.na(c(b_col, cil_col, ciu_col)))) return(NULL)

    # Use the instrument coefficient row. If the coefficient label is present,
    # require it to be instrument_var; otherwise use the first result row.
    coefficient_label <- if (length(value_row) > 0) value_row[1] else NA_character_
    if (!is.na(coefficient_label) && nzchar(coefficient_label) &&
        !grepl("instrument_var", coefficient_label, fixed = TRUE)) return(NULL)

    data.frame(
      Exposure = "instrument_var",
      Outcome = sub("^b_", "", estimate_name),
      b = value_row[b_col],
      min95 = value_row[cil_col],
      max95 = value_row[ciu_col],
      stringsAsFactors = FALSE
    )
  })
  extracted_rows <- Filter(Negate(is.null), extracted_rows)
  if (length(extracted_rows) == 0) {
    stop(
      "Found header rows but could not extract any estimates. Check that each " ,
      "estimate name (for example, b_pat_age) is in column B immediately above " ,
      "its header row, and that the instrument_var coefficient row follows it."
    )
  }
  balance <- bind_rows(extracted_rows)
}

# Validate the normalized plotting table.
required_columns <- c("Exposure", "Outcome", "b", "min95", "max95")
missing_columns <- setdiff(required_columns, names(balance))
if (length(missing_columns) > 0) {
  stop(
    "The normalized balance table is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Set Exposure to instrument_var for every observation. 
# Outcome is the part of the stored estimate name after
# the leading "b_" (for example, b_pat_age becomes pat_age).
# Select only the columns needed for plotting.
balance <- balance %>%
  mutate(
    Exposure = "instrument_var",
    Outcome = sub("^b_", "", as.character(Outcome))
  ) %>%
  select(Exposure, Outcome, b, min95, max95)

# Ensure that b, min95, and max95 are encoded as numeric values
balance <- balance %>%
  mutate(
    across(
      c(b, min95, max95),
      ~ as.numeric(as.character(.x))
    )
  )


# Define labels for outcome and facet variables
  # The trailing '2' identifies healthcare-use outcomes measured at T-2.
  # The different "Somatic" labels are differentiated using a different number
  # of spaces after the word.
outcome_labels <- c(
  pat_age       = "**Age**<br>*continuous, in years*",
  edu           = "**Education**<br>*any higher education vs. not*",
  ooh_soma2     = "*Somatic* ", #one space
  ooh_psych2    = "**Out-of-hours**<br>*Mental*",
  nac_soma_out2 = "*Somatic*  ", #two spaces
  nac_psych_out2 = "**Non-acute outpatient**<br>*Mental*",
  nac_soma_in2  = "*Somatic*   ", #three spaces
  nac_psych_in2 = "**Non-acute inpatient**<br>*Mental*",
  ac_soma_in2   = "*Somatic*    ", #four spaces
  ac_psych_in2  = "**Acute inpatient**<br>*Mental*"
)

facet_labels <- c(
  pat_age = "Patient characteristics",
  edu = "Patient characteristics",
  ac_psych_in2 = "Healthcare use two years before assignment",
  nac_psych_in2 = "Healthcare use two years before assignment",
  nac_psych_out2 = "Healthcare use two years before assignment",
  ooh_psych2 = "Healthcare use two years before assignment",
  ac_soma_in2 = "Healthcare use two years before assignment",
  nac_soma_in2 = "Healthcare use two years before assignment",
  nac_soma_out2 = "Healthcare use two years before assignment",
  ooh_soma2 = "Healthcare use two years before assignment"
)

# Stop rather than silently dropping unexpected outcome names.
unrecognised_outcomes <- setdiff(unique(balance$Outcome), names(outcome_labels))
if (length(unrecognised_outcomes) > 0) {
  stop(
    "Unrecognised Outcome value(s) in balance_test.csv: ",
    paste(unrecognised_outcomes, collapse = ", "),
    ". Check the names against code_analysis_02_balance."
  )
}

# Create additional columns with labels for all variables
balance <- balance %>%
  mutate(
    new_outcome_label = unname(outcome_labels[Outcome]),
    new_exposure_label = case_when(
      Exposure == "instrument_var" ~ "Instrument",
      TRUE ~ Exposure
    ),
    new_facet_var = unname(facet_labels[Outcome])
  )

# Set the ordering for outcome variables, with patient characteristics first.

balance$new_outcome_label <- factor(
  balance$new_outcome_label,
  levels = outcome_order
)

balance$new_facet_var <- factor(
  balance$new_facet_var,
  levels = c(
    "Patient characteristics",
    "Healthcare use two years before assignment"
  )
)

################################################################################
# Generate the plot
(plot_balance <- ggplot(balance, aes(x = b, y = new_outcome_label, shape = new_exposure_label)) +
   scale_shape_manual(values = c(16)) +  # Specify shape based on instrument
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
    geom_errorbar(aes(xmin = min95, xmax = max95), 
                  width = 0.3,
                  linewidth = 1,
                  color = "#000000FF") +  # Error bars with dodge
    geom_point(size = 5, color = "#000000FF") +  # Plot points with dodge
    theme_minimal() +  # Minimal theme
    labs(x = "Effect size\n(95% confidence interval)",
         y = "Patient characteristics") +
    facet_wrap(
      vars(new_facet_var),
      ncol = 1,
      scales = "free_y",
      space = "free_y"
    ) +
    theme(
      plot.title = element_blank(),  # Change font elements for title
      axis.text.y.left = element_markdown(size = 12, color = "#000000"),
      strip.placement = "outside",
      strip.text.x = element_textbox_simple(vjust = 1,
                                            orientation = "upright", #can be "upright", "left-rotated", "right-rotated", or "inverted"
                                            width = unit(1, "npc"), #NULL means take up exactly the space necessary to render all content. Use a value of unit(1, "npc") to have the box take up all available space.
                                            halign = 0.5,
                                            padding = margin(3, 0, 3, 0), # in the order top, right, bottom, left
                                            margin = margin(0, 0, 0, 0), # in the order top, right, bottom, left
                                            fill = "white",
                                            color = "black",
                                            box.color = "black",
                                            linetype = 1,
                                            r = unit(0, "pt"),
                                            size = 14,
                                            face = "bold"),
      plot.caption.position = "plot",
      axis.title.y = element_blank(),  # Change font elements for y axis labels
      axis.title.x = element_text(size = 12, face = "bold"),  # Italicize x axis title
      axis.text.x = element_text(size = 12),  # Change font elements for x axis labels
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.grid.major.x = element_blank(), # Remove major grid lines on x axis
      panel.spacing = unit(0.0, "lines"),  # Adjust spacing between facets
      panel.border = element_rect(color = "#666666", fill = NA, linewidth = 0.5, linetype = "solid"),
      axis.ticks.x = element_line(color = "#666666", linewidth = 0.5)
    ) +
    guides(
      shape = "none"  # Shape legend ordered first
    )
)


# PNG output:
ggsave(
  "Figure_3.png", device = "png", type = "cairo",
  plot = plot_balance, width = 17.0, height = 20,
  units = "cm", dpi = 600, bg = "white"
)

# Optional EPS output:
# ggsave(
#   output_file,
#   device = cairo_ps,
#   plot = plot_balance,
#   width = 17.0,
#   height = 20,
#   units = "cm",
#   dpi = 600,
#   bg = "white"
# )


