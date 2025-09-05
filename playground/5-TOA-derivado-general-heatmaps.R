# ============================================================
# Categorical "heatmaps" (really large categorical grids) from
# TOA_derivado_general: 1047 x 11 (t = 1..11)
# - Figure 1: all 21 states, NA shown in black
# - Figure 2: 4 buckets (Loop, Pill, Condom, Other modern) in color;
#             all non-modern + NA shown in black
# ============================================================

# If needed, set your working directory to where the RDS lives:
# setwd("...")


library(RColorBrewer)

# ------------------------------------------------------------------
# 0) Load general object
# ------------------------------------------------------------------
TOA_derivado_general <- readRDS("TOA_derivado_general.rds")

# Matrices (n x P), we will keep t = 1..11 as requested
status_code <- TOA_derivado_general$status_code
stopifnot(ncol(status_code) >= 11)
mat <- status_code[, 1:11, drop = FALSE]   # n x 11 (1047 x 11)

n  <- nrow(mat)
P  <- ncol(mat)
states <- TOA_derivado_general$states
label_to_code <- TOA_derivado_general$codes$label_to_code
code_to_label <- TOA_derivado_general$codes$code_to_label

# Helper: map codes -> labels (character matrix, same shape)
lab_mat <- matrix(NA_character_, n, P)
if (length(code_to_label)) {
  lab_mat[] <- code_to_label[as.character(mat)]
}

# ------------------------------------------------------------------
# 1) FIGURE A: all 21 states (each with its own categorical color)
#    - NA (no report) will be black
# ------------------------------------------------------------------

# Choose 21 distinct categorical colors (non-gradient).
# (You can tweak these for your preferred palette/contrast.)
all_state_colors <- c(
  "firebrick", "forestgreen", "royalblue", "goldenrod", "purple4",
  "tomato", "darkorange3", "dodgerblue3", "seagreen4", "orchid4",
  "brown4", "steelblue4", "darkgoldenrod4", "sienna3", "olivedrab4",
  "mediumpurple4", "indianred4", "darkslateblue", "chocolate4",
  "darkcyan", "slategray4"
)

# states are alphabetical in TOA_derivado_general$states, so keep a stable map
stopifnot(length(states) == 21)
state_to_color <- setNames(all_state_colors, states)

# Build a color matrix: NA -> black, otherwise state color
col_mat_all <- matrix("black", n, P)
ok <- !is.na(lab_mat)
col_mat_all[ok] <- state_to_color[ lab_mat[ok] ]

# ------------------------------------------------------------------
# 2) FIGURE B: 4 buckets like your histogram Strategy 2
#    - Loop, Pill, Condom, Other modern (the rest = black)
# ------------------------------------------------------------------

modern_names <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "TL",
                  "Injection", "Rhythm", "Withdrawal")
modern_codes <- as.integer(label_to_code[modern_names])
modern_codes <- modern_codes[!is.na(modern_codes)]

# The 3 singled-out methods:
code_loop   <- as.integer(label_to_code[["Loop"]])
code_pill   <- as.integer(label_to_code[["Oral Pill"]])
code_condom <- as.integer(label_to_code[["Condom"]])

# Other moderns (apart from Loop/Pill/Condom)
other_modern_codes <- setdiff(modern_codes, c(code_loop, code_pill, code_condom))

# Use the SAME 4 colors you used in the histogram (Set2, n=4):
set2_4 <- brewer.pal(4, "Set2")
names(set2_4) <- c("Loop", "Pill", "Condom", "OtherModern")

# Build a color matrix for the 4 buckets
col_mat_4 <- matrix("black", n, P)  # default: black (non-modern or NA)

is_loop      <- mat == code_loop
is_pill      <- mat == code_pill
is_condom    <- mat == code_condom
is_other_mod <- (!is.na(mat)) & (mat %in% other_modern_codes)

col_mat_4[is_loop]      <- set2_4["Loop"]
col_mat_4[is_pill]      <- set2_4["Pill"]
col_mat_4[is_condom]    <- set2_4["Condom"]
col_mat_4[is_other_mod] <- set2_4["OtherModern"]

# ------------------------------------------------------------------
# 3) Plot helpers: draw a big categorical grid as a PDF
# ------------------------------------------------------------------

draw_categorical_grid <- function(color_matrix,
                                  filename,
                                  title,
                                  legend_items,
                                  legend_colors,
                                  flip_rows = TRUE) {
  # image() draws with row 1 at bottom; flip if you want row 1 at top:
  cm <- color_matrix
  if (flip_rows) cm <- cm[seq(nrow(cm), 1), , drop = FALSE]
  
  # Create a numeric index matrix for image(), plus a palette vector
  # Trick: we encode each unique color as an integer.
  ucols <- unique(as.vector(cm))
  pal   <- ucols
  idx   <- matrix(match(cm, ucols), nrow(cm), ncol(cm))
  
  # Margins generous for legend/title; size wide enough
  pdf(filename, width = 8, height = 20)
  op <- par(mar = c(5, 5, 4, 10) + 0.1, xpd = NA)
  
  # Draw
  image(
    x = 1:ncol(idx),
    y = 1:nrow(idx),
    z = t(idx),            # transpose so x is time, y is row
    col = pal,
    useRaster = TRUE,
    axes = FALSE,
    main = title,
    xlab = "Time step (t = 1..11)",
    ylab = "Individuals (1..1047)"
  )
  
  # Axes ticks
  axis(1, at = 1:ncol(idx), labels = 1:ncol(idx), cex.axis = 0.8)
  # We’ll avoid cluttering the y-axis (1047 rows); just show a few ticks:
  y_ticks <- pretty(1:nrow(idx), n = 10)
  y_ticks <- y_ticks[y_ticks >= 1 & y_ticks <= nrow(idx)]
  axis(2, at = y_ticks, labels = y_ticks, las = 2, cex.axis = 0.7)
  
  # Legend (on right)
  legend(
    "right",
    inset = c(-0.3, 0),
    legend = legend_items,
    fill = legend_colors,
    bty = "n",
    cex = 0.8,
    title = "States / Buckets"
  )
  
  par(op)
  dev.off()
}

# ------------------------------------------------------------------
# 4) Figure A (all 21 states + NA=black)
# ------------------------------------------------------------------

legend_items_A  <- c(states, "No report / NA")
legend_colors_A <- c(state_to_color, "black")

draw_categorical_grid(
  color_matrix = col_mat_all,
  filename     = "heatmap_all_states.pdf",
  title        = "States by individual over time (all 21 states; NA in black)",
  legend_items = legend_items_A,
  legend_colors= legend_colors_A
)

message("Saved: heatmap_all_states.pdf")

# ------------------------------------------------------------------
# 5) Figure B (4 buckets like histogram Strategy 2; others black)
# ------------------------------------------------------------------

legend_items_B  <- c("Loop", "Pill", "Condom", "Other modern", "Non-modern / NA")
legend_colors_B <- c(set2_4["Loop"], set2_4["Pill"], set2_4["Condom"],
                     set2_4["OtherModern"], "black")

draw_categorical_grid(
  color_matrix = col_mat_4,
  filename     = "heatmap_modern_4buckets.pdf",
  title        = "Modern-method buckets by individual over time (non-modern/NA in black)",
  legend_items = legend_items_B,
  legend_colors= legend_colors_B
)

message("Saved: heatmap_modern_4buckets.pdf")

# ------------------------------------------------------------------
# Notes:
# - Rows are in the order provided by TOA_derivado_general (no resorting).
# - If you prefer row 1 at bottom (image default), set flip_rows = FALSE.
# - If you want t = 1..10 instead of 1..11, subset mat[, 1:10] above.
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# 6) Console inspection: sample 20 random individuals and print
#    their trajectories across t = 1..11
#    - Version A: full state labels (21 states + NA)
#    - Version B: 4-bucket mapping used in the histogram/modern heatmap
# ------------------------------------------------------------------

# For reproducibility of the sample:
set.seed(12345)

# Helper: convert a vector of status codes (length P) to full labels
codes_to_full_labels <- function(code_vec, code_to_label_map) {
  out <- character(length(code_vec))
  for (j in seq_along(code_vec)) {
    v <- code_vec[j]
    if (is.na(v)) out[j] <- "NA"
    else {
      lab <- code_to_label_map[[as.character(v)]]
      out[j] <- if (is.null(lab)) "Unknown" else lab
    }
  }
  out
}

# Helper: 4-bucket mapping per entry (Loop, Pill, Condom, OtherModern, Non-modern/NA)
codes_to_four_buckets <- function(code_vec,
                                  code_loop,
                                  code_pill,
                                  code_condom,
                                  other_mod_codes) {
  out <- character(length(code_vec))
  for (j in seq_along(code_vec)) {
    v <- code_vec[j]
    if (is.na(v)) out[j] <- "Non-modern/NA"
    else if (v == code_loop) out[j] <- "Loop"
    else if (v == code_pill) out[j] <- "Pill"
    else if (v == code_condom) out[j] <- "Condom"
    else if (v %in% other_mod_codes) out[j] <- "OtherModern"
    else out[j] <- "Non-modern/NA"
  }
  out
}

# Decide which identifier to show: try meta$specific_id, else row index
id_vec <- if (!is.null(TOA_derivado_general$meta$specific_id)) {
  TOA_derivado_general$meta$specific_id
} else {
  as.character(seq_len(n))
}

# Sample 20 distinct individuals from the n rows
sample_size <- min(20, n)
samp_idx <- sort(sample(seq_len(n), size = sample_size, replace = FALSE))

cat("\n================ 20 RANDOM TRAJECTORIES: FULL STATES ================\n")
for (i in samp_idx) {
  seq_full <- codes_to_full_labels(mat[i, ], code_to_label)
  cat(sprintf("ID %s | %s\n", id_vec[i], paste(seq_full, collapse = " -> ")))
}

cat("\n================ 20 RANDOM TRAJECTORIES: 4 BUCKETS ==================\n")
for (i in samp_idx) {
  seq_4b <- codes_to_four_buckets(
    code_vec        = mat[i, ],
    code_loop       = code_loop,
    code_pill       = code_pill,
    code_condom     = code_condom,
    other_mod_codes = other_modern_codes
  )
  cat(sprintf("ID %s | %s\n", id_vec[i], paste(seq_4b, collapse = " -> ")))
}

# Notes:
# - "Non-modern/NA" aggregates all non-modern states and true missing reports.
# - To inspect specific IDs, replace 'samp_idx' by a fixed vector of row indices.
# - To print more or fewer, change 'sample_size'.