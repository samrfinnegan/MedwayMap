# =============================================================================
# Geographic Area Matching: Finding Statistical Nearest Neighbours
# =============================================================================
# This script demonstrates how to find geographies from a larger pool that
# closely match a set of "seed" geographies of interest, using:
#   - Variable standardisation
#   - Optional PCA for dimensionality reduction
#   - Mahalanobis distance nearest-neighbour matching
#   - Match quality assessment and visualisation
#
# Methodology follows ONS approaches described at:
# https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/methodologies/
#   clusteringsimilarlocalauthoritiesandstatisticalnearestneighboursintheukmethodology
# =============================================================================

# --- 0. PACKAGES --------------------------------------------------------------
# install.packages(c("tidyverse", "ggplot2", "ggrepel", "patchwork", "factoextra"))
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)   # for combining plots
library(factoextra)  # for PCA visualisation


# --- 1. SIMULATE DATA ---------------------------------------------------------
# In practice, replace this section with your real data.
# Structure needed: one row per geography, columns = demographic variables.

set.seed(42)
n_total    <- 200   # total pool of geographies
n_seeds    <- 10    # your geographies of interest

# Age-sex population shares (18 bands × 2 sexes = 36 variables)
# In reality these would come from census mid-year estimates
age_bands  <- c("0_4","5_9","10_14","15_19","20_24","25_34",
                "35_44","45_54","55_64","65_74","75_84","85plus")
age_sex_cols <- c(paste0("m_", age_bands), paste0("f_", age_bands))

# Simulate a plausible age-sex structure using Dirichlet-like random draws
sim_age_sex <- function(n) {
  raw <- matrix(rgamma(n * length(age_sex_cols), shape = 2), 
                nrow = n, ncol = length(age_sex_cols))
  sweep(raw, 1, rowSums(raw), "/")  # normalise to proportions
}

age_sex_data <- sim_age_sex(n_total) |> as.data.frame()
names(age_sex_data) <- age_sex_cols

# IMD score (1–80 range, lower = less deprived) and rank
imd_score <- runif(n_total, 1, 80)
imd_rank  <- rank(imd_score)

# Additional variables
unemployment_rate   <- pmax(0, rnorm(n_total, mean = 5,  sd = 3))   # %
pct_no_qualifications <- pmax(0, rnorm(n_total, mean = 12, sd = 5)) # %
pct_long_term_sick  <- pmax(0, rnorm(n_total, mean = 8,  sd = 3))   # %
population_density  <- exp(rnorm(n_total, mean = 6, sd = 1.2))      # per sq km

# Assemble full dataset
all_areas <- data.frame(
  area_id = paste0("AREA_", sprintf("%03d", seq_len(n_total))),
  age_sex_data,
  imd_score,
  imd_rank,
  unemployment_rate,
  pct_no_qualifications,
  pct_long_term_sick,
  population_density
)

# Designate the first 10 areas as your seeds (in practice, you'd filter by name/code)
seed_ids      <- all_areas$area_id[1:n_seeds]
candidate_ids <- all_areas$area_id[(n_seeds + 1):n_total]

cat("Seed areas:     ", n_seeds, "\n")
cat("Candidate pool: ", length(candidate_ids), "\n")


# --- 2. PREPARE FEATURE MATRIX -----------------------------------------------
# Select the variables to use for matching.
# You might choose to include or exclude certain variables based on your
# research question — document this choice explicitly.

# Here we use all age-sex shares plus the non-redundant additional variables.
# We exclude imd_rank (redundant with imd_score) to avoid double-counting.
match_vars <- c(age_sex_cols,
                "imd_score",
                "unemployment_rate",
                "pct_no_qualifications",
                "pct_long_term_sick",
                "population_density")

feature_matrix <- all_areas |> select(area_id, all_of(match_vars))


# --- 3. STANDARDISE VARIABLES -------------------------------------------------
# Z-score standardisation: subtract mean, divide by SD.
# This ensures no single variable dominates due to scale differences.
#
# ONS uses inter-decile range standardisation for robustness to outliers:
#   (x - median) / (p90 - p10)
# We implement both and let you choose.

# Option A: Z-score (simpler, standard)
zscore_standardise <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Option B: Inter-decile range (more robust to outliers — ONS preferred)
idr_standardise <- function(x) {
  p10 <- quantile(x, 0.10, na.rm = TRUE)
  p90 <- quantile(x, 0.90, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  if ((p90 - p10) == 0) return(rep(0, length(x)))  # guard against zero range
  (x - med) / (p90 - p10)
}

# Apply standardisation across all numeric feature columns
# Change `idr_standardise` to `zscore_standardise` if preferred
std_matrix <- feature_matrix |>
  mutate(across(all_of(match_vars), idr_standardise))


# --- 4. OPTIONAL: PCA FOR DIMENSIONALITY REDUCTION ---------------------------
# With 36+ age-sex variables, the feature space is high-dimensional and
# variables are correlated (age shares sum to 1; deprivation correlates with
# unemployment etc.). PCA projects into uncorrelated components.
#
# Rule of thumb: retain components explaining ~90% of variance.

pca_result <- prcomp(std_matrix[, match_vars], center = FALSE, scale. = FALSE)
                                              # already standardised above

# How many components to retain?
var_explained <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
n_components  <- which(var_explained >= 0.90)[1]
cat("\nPCA: retaining", n_components, "components (explain",
    round(var_explained[n_components] * 100, 1), "% of variance)\n")

# Scree plot
fviz_eig(pca_result, ncp = 20, addlabels = TRUE,
         main = "Scree Plot — Variance Explained by PCA Components") |> print()

# Extract scores for the retained components
pca_scores <- as.data.frame(pca_result$x[, 1:n_components])
pca_scores$area_id <- feature_matrix$area_id

# You can choose to do matching in either PCA space or raw standardised space.
# PCA space is better when n_variables is large; raw space when n_variables is small.
# Set use_pca = FALSE to skip PCA and match in standardised variable space.
use_pca <- TRUE

if (use_pca) {
  match_data <- pca_scores
  match_cols <- names(pca_scores)[names(pca_scores) != "area_id"]
} else {
  match_data <- std_matrix
  match_cols <- match_vars
}


# --- 5. MAHALANOBIS DISTANCE MATCHING ----------------------------------------
# For each seed area, compute Mahalanobis distance to every candidate area.
# Mahalanobis distance accounts for correlations between variables.
# (If using PCA scores, which are orthogonal, this reduces to scaled Euclidean.)

# Covariance matrix from ALL areas (seeds + candidates)
cov_mat <- cov(match_data[, match_cols])

# Split into seeds and candidates
seeds_data      <- match_data |> filter(area_id %in% seed_ids)
candidates_data <- match_data |> filter(area_id %in% candidate_ids)

# For each seed, compute distance to all candidates
compute_distances <- function(seed_row, candidates_mat, cov_matrix) {
  # mahalanobis() returns distances from a single point to many points
  diffs <- sweep(candidates_mat, 2, unlist(seed_row), "-")
  # D^2 = (x - mu)' * S^(-1) * (x - mu)
  tryCatch(
    mahalanobis(candidates_mat, center = unlist(seed_row), cov = cov_matrix),
    error = function(e) {
      # Fallback to Euclidean if covariance matrix is singular
      message("Covariance matrix singular; falling back to Euclidean distance.")
      rowSums(diffs^2)
    }
  )
}

# Build a results dataframe: for each seed, top k matches
k_matches <- 5  # number of nearest neighbours to return per seed

results_list <- map(seed_ids, function(sid) {
  seed_vec   <- seeds_data |> filter(area_id == sid) |> select(all_of(match_cols))
  cand_mat   <- candidates_data |> select(all_of(match_cols)) |> as.matrix()
  
  distances  <- compute_distances(seed_vec, cand_mat, cov_mat)
  
  tibble(
    seed_area      = sid,
    matched_area   = candidates_data$area_id,
    mahal_distance = distances
  ) |>
    arrange(mahal_distance) |>
    slice_head(n = k_matches) |>
    mutate(match_rank = row_number())
})

match_results <- bind_rows(results_list)


# --- 6. MATCH QUALITY THRESHOLDS ---------------------------------------------
# Adapt the ONS SED threshold approach to Mahalanobis distance.
# We define quality bands relative to the observed distance range.

dist_range <- range(match_results$mahal_distance)
dist_span  <- diff(dist_range)

match_results <- match_results |>
  mutate(
    pct_of_range = (mahal_distance - dist_range[1]) / dist_span,
    match_quality = case_when(
      pct_of_range < 0.01  ~ "Extremely similar",
      pct_of_range < 0.025 ~ "Very similar",
      pct_of_range < 0.05  ~ "Similar",
      pct_of_range < 0.10  ~ "Somewhat similar",
      TRUE                  ~ "Poor match"
    )
  )

cat("\n--- Match Quality Summary ---\n")
print(count(match_results, match_quality))

cat("\n--- Top 3 Matches per Seed Area ---\n")
match_results |>
  filter(match_rank <= 3) |>
  select(seed_area, match_rank, matched_area, mahal_distance, match_quality) |>
  print(n = n_seeds * 3)


# --- 7. VISUALISATIONS --------------------------------------------------------

## 7a. Dot plot of distances by seed
p1 <- ggplot(match_results, aes(x = mahal_distance, y = reorder(seed_area, mahal_distance),
                                 colour = match_quality)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = matched_area), size = 2.5, max.overlaps = 5) +
  scale_colour_manual(values = c(
    "Extremely similar" = "#1a7a4a",
    "Very similar"      = "#4caf7d",
    "Similar"           = "#f0b429",
    "Somewhat similar"  = "#e07b39",
    "Poor match"        = "#c0392b"
  )) +
  labs(title    = "Mahalanobis Distance to Nearest Neighbour Matches",
       subtitle = paste0("Top ", k_matches, " matches per seed area"),
       x        = "Mahalanobis Distance",
       y        = "Seed Area",
       colour   = "Match Quality") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p1)

## 7b. PCA biplot — shows seeds vs candidates in 2D demographic space
pca_plot_data <- pca_scores |>
  mutate(type = case_when(
    area_id %in% seed_ids      ~ "Seed",
    area_id %in% unlist(match_results$matched_area) ~ "Match",
    TRUE                        ~ "Other"
  ))

p2 <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, colour = type, size = type)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(data = filter(pca_plot_data, type != "Other"),
                  aes(label = area_id), size = 2.5, max.overlaps = 20) +
  scale_colour_manual(values = c("Seed" = "#c0392b", "Match" = "#2471a3", "Other" = "grey70")) +
  scale_size_manual(values  = c("Seed" = 4, "Match" = 3, "Other" = 1.5)) +
  labs(title    = "PCA Biplot: Seed Areas, Matches, and Full Pool",
       subtitle = "PC1 and PC2 from standardised demographic variables",
       x        = paste0("PC1 (", round(var_explained[1] * 100, 1), "% variance)"),
       y        = paste0("PC2 (", round((var_explained[2] - var_explained[1]) * 100, 1), "% variance)"),
       colour   = NULL, size = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p2)

## 7c. Side-by-side comparison of one seed and its top match on raw variables
# (Useful for a sense-check / stakeholder presentation)
compare_seed   <- seed_ids[1]
compare_match  <- match_results |> filter(seed_area == compare_seed, match_rank == 1) |> pull(matched_area)

compare_data <- all_areas |>
  filter(area_id %in% c(compare_seed, compare_match)) |>
  select(area_id, all_of(c("imd_score", "unemployment_rate",
                            "pct_no_qualifications", "pct_long_term_sick",
                            "population_density"))) |>
  pivot_longer(-area_id, names_to = "variable", values_to = "value") |>
  mutate(area_type = if_else(area_id == compare_seed, "Seed", "Top Match"))

p3 <- ggplot(compare_data, aes(x = variable, y = value, fill = area_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Seed" = "#c0392b", "Top Match" = "#2471a3")) +
  labs(title    = paste0("Variable Comparison: ", compare_seed, " vs Top Match (", compare_match, ")"),
       subtitle = "Non-age-sex variables shown (standardised units differ — use for shape, not magnitude)",
       x = NULL, y = "Raw Value", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom")

print(p3)

## 7d. Age pyramid comparison for seed vs top match
pyramid_data <- all_areas |>
  filter(area_id %in% c(compare_seed, compare_match)) |>
  select(area_id, starts_with("m_"), starts_with("f_")) |>
  pivot_longer(-area_id, names_to = "col", values_to = "proportion") |>
  mutate(
    sex      = if_else(str_starts(col, "m_"), "Male", "Female"),
    age_band = str_replace(col, "^[mf]_", ""),
    proportion = if_else(sex == "Male", -proportion, proportion),
    area_type  = if_else(area_id == compare_seed, "Seed", "Top Match")
  )

p4 <- ggplot(pyramid_data, aes(x = proportion, y = age_band,
                                fill = interaction(sex, area_type),
                                alpha = area_type)) +
  geom_col(position = "identity") +
  scale_fill_manual(values = c(
    "Male.Seed"       = "#1a5276", "Female.Seed"      = "#922b21",
    "Male.Top Match"  = "#5dade2", "Female.Top Match"  = "#f1948a"
  )) +
  scale_alpha_manual(values = c("Seed" = 1, "Top Match" = 0.6)) +
  scale_x_continuous(labels = function(x) scales::percent(abs(x))) +
  labs(title    = "Age-Sex Pyramid Comparison",
       subtitle = paste0(compare_seed, " (solid) vs ", compare_match, " (translucent)"),
       x = "Population Share", y = "Age Band",
       fill = NULL, alpha = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p4)


# --- 8. EXPORT RESULTS --------------------------------------------------------
write_csv(match_results, "matched_areas_results.csv")
cat("\nResults saved to matched_areas_results.csv\n")

# Summary output
cat("\n=== DONE ===\n")
cat("For each of your", n_seeds, "seed areas, the top", k_matches,
    "nearest neighbours are stored in `match_results`.\n")
cat("Review `match_quality` column — aim for 'Similar' or better.\n")
cat("If many seeds return 'Poor match', they may be demographically unusual;\n")
cat("consider reporting this explicitly rather than forcing matches.\n")
