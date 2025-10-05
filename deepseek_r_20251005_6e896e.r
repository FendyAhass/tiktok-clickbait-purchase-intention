# =============================================================================
# PeerJ Manuscript: "Scroll, Click, Buy: The Impact of Clickbait and Digital 
# Credibility on TikTok Users' Purchase Intentions"
# 
# COMPUTER CODE FOR REPRODUCIBLE ANALYSIS
# Data Source: https://zenodo.org/records/16927854
# Data File: TIKTOK_1_SCROLL,_CLICK,_BUY.csv
# 
# Methodology: Partial Least Squares Structural Equation Modeling (PLS-SEM)
# Sample: 410 Malaysian TikTok users
# Analysis: Direct effects, mediation analysis with bootstrapping
# =============================================================================

# Load required packages
# Note: This script uses the 'semPLS' package for PLS-SEM analysis
if (!require(semPLS)) install.packages('semPLS')
if (!require(readr)) install.packages('readr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(psych)) install.packages('psych') # For descriptive statistics

library(semPLS)
library(readr)
library(dplyr)
library(ggplot2)
library(psych)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load the dataset
data <- read_csv("TIKTOK_1_SCROLL,_CLICK,_BUY.csv")

# Display basic information about the dataset
cat("Dataset Overview:\n")
cat("Number of observations:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n")
cat("\nFirst few rows:\n")
print(head(data))

# =============================================================================
# 2. MEASUREMENT MODEL SPECIFICATION
# =============================================================================

# Define constructs based on your measurement model
# Replace these with your actual variable names from the dataset

constructs <- list(
  Clickbait = c("CB1", "CB2", "CB3", "CB4"),  # Clickbait Content items
  Credibility = c("SC1", "SC2", "SC3", "SC4"), # Source Credibility items  
  Benefit = c("PB1", "PB2", "PB3", "PB4"),    # Perceived Benefit items
  Norm = c("PN1", "PN2", "PN3", "PN4"),       # Perceived Norm items
  Purchase = c("PI1", "PI2", "PI3", "PI4")    # Purchase Intention items
)

# NOTE: You MUST replace the above variable names with your actual column names
# from the TIKTOK_1_SCROLL,_CLICK,_BUY.csv file

# =============================================================================
# 3. MEASUREMENT MODEL ASSESSMENT
# =============================================================================

# Function to calculate measurement model metrics
assess_measurement_model <- function(data, constructs) {
  results <- list()
  
  for (construct in names(constructs)) {
    items <- constructs[[construct]]
    
    # Calculate Cronbach's Alpha
    alpha <- psych::alpha(data[items])
    
    # Calculate Composite Reliability (CR) and AVE
    cor_matrix <- cor(data[items])
    lambda <- colSums(cor_matrix) / ncol(cor_matrix)  # Simplified loadings
    
    CR <- (sum(lambda)^2) / (sum(lambda)^2 + sum(1 - lambda^2))
    AVE <- mean(lambda^2)
    
    results[[construct]] <- list(
      Cronbach_Alpha = alpha$total$raw_alpha,
      Composite_Reliability = CR,
      AVE = AVE,
      Item_Loadings = lambda
    )
  }
  
  return(results)
}

# Run measurement model assessment
mm_results <- assess_measurement_model(data, constructs)

# Display measurement model results
cat("\nMEASUREMENT MODEL RESULTS:\n")
cat("==========================\n")
for (construct in names(mm_results)) {
  cat(sprintf("\n%s:\n", construct))
  cat(sprintf("  Cronbach's Alpha: %.3f\n", mm_results[[construct]]$Cronbach_Alpha))
  cat(sprintf("  Composite Reliability: %.3f\n", mm_results[[construct]]$Composite_Reliability))
  cat(sprintf("  AVE: %.3f\n", mm_results[[construct]]$AVE))
}

# =============================================================================
# 4. STRUCTURAL MODEL SPECIFICATION
# =============================================================================

# Define the structural model based on your hypotheses
structural_model <- matrix(
  c(
    # To   From
    0,      0,      0,      0,      0,    # Clickbait
    1,      0,      0,      0,      0,    # Credibility  
    1,      0,      0,      0,      0,    # Benefit
    1,      0,      0,      0,      0,    # Norm
    0,      1,      1,      1,      0     # Purchase
  ),
  nrow = 5, byrow = TRUE,
  dimnames = list(
    c("Clickbait", "Credibility", "Benefit", "Norm", "Purchase"),
    c("Clickbait", "Credibility", "Benefit", "Norm", "Purchase")
  )
)

# =============================================================================
# 5. PLS-SEM ANALYSIS
# =============================================================================

# Prepare data for PLS-SEM (using first item as proxy for simplicity)
# In practice, you would use all items or latent variable scores
pls_data <- data.frame(
  Clickbait = rowMeans(data[constructs$Clickbait], na.rm = TRUE),
  Credibility = rowMeans(data[constructs$Credibility], na.rm = TRUE),
  Benefit = rowMeans(data[constructs$Benefit], na.rm = TRUE),
  Norm = rowMeans(data[constructs$Norm], na.rm = TRUE),
  Purchase = rowMeans(data[constructs$Purchase], na.rm = TRUE)
)

# Run PLS-SEM analysis
pls_model <- sempls(
  model = structural_model,
  data = pls_data,
  wscheme = "pathWeighting"
)

# =============================================================================
# 6. DIRECT EFFECTS ANALYSIS (H1-H6)
# =============================================================================

cat("\nDIRECT EFFECTS (HYPOTHESES H1-H6):\n")
cat("===================================\n")

# Extract path coefficients
path_coefficients <- pls_model$path_coefficients

# H1: Clickbait -> Credibility (Negative)
h1_coef <- path_coefficients["Credibility", "Clickbait"]
cat(sprintf("\nH1: Clickbait -> Credibility\n"))
cat(sprintf("Path coefficient: %.3f (Expected: Negative)\n", h1_coef))

# H2: Clickbait -> Benefit  
h2_coef <- path_coefficients["Benefit", "Clickbait"]
cat(sprintf("\nH2: Clickbait -> Benefit\n"))
cat(sprintf("Path coefficient: %.3f\n", h2_coef))

# H3: Clickbait -> Norm
h3_coef <- path_coefficients["Norm", "Clickbait"]
cat(sprintf("\nH3: Clickbait -> Norm\n"))
cat(sprintf("Path coefficient: %.3f\n", h3_coef))

# H4: Credibility -> Purchase (Positive)
h4_coef <- path_coefficients["Purchase", "Credibility"]
cat(sprintf("\nH4: Credibility -> Purchase\n"))
cat(sprintf("Path coefficient: %.3f (Expected: Positive)\n", h4_coef))

# H5: Benefit -> Purchase (Positive)
h5_coef <- path_coefficients["Purchase", "Benefit"]
cat(sprintf("\nH5: Benefit -> Purchase\n"))
cat(sprintf("Path coefficient: %.3f (Expected: Positive)\n", h5_coef))

# H6: Norm -> Purchase
h6_coef <- path_coefficients["Purchase", "Norm"]
cat(sprintf("\nH6: Norm -> Purchase\n"))
cat(sprintf("Path coefficient: %.3f (Expected: Not Significant)\n", h6_coef))

# =============================================================================
# 7. MEDIATION ANALYSIS (H7-H9)
# =============================================================================

cat("\nMEDIATION EFFECTS (HYPOTHESES H7-H9):\n")
cat("=====================================\n")

# Bootstrap function for mediation analysis
bootstrap_mediation <- function(data, n_bootstrap = 1000) {
  n <- nrow(data)
  mediation_results <- list()
  
  for (i in 1:n_bootstrap) {
    # Bootstrap sample
    boot_sample <- data[sample(1:n, n, replace = TRUE), ]
    
    # Fit models for mediation analysis
    # Mediator models
    med_cred <- lm(Credibility ~ Clickbait, data = boot_sample)
    med_benefit <- lm(Benefit ~ Clickbait, data = boot_sample) 
    med_norm <- lm(Norm ~ Clickbait, data = boot_sample)
    
    # Outcome model with mediators
    out_model <- lm(Purchase ~ Credibility + Benefit + Norm + Clickbait, 
                   data = boot_sample)
    
    # Store coefficients
    if (i == 1) {
      mediation_results <- list(
        indirect_cred = numeric(n_bootstrap),
        indirect_benefit = numeric(n_bootstrap),
        indirect_norm = numeric(n_bootstrap)
      )
    }
    
    # Calculate indirect effects
    mediation_results$indirect_cred[i] <- coef(med_cred)["Clickbait"] * 
      coef(out_model)["Credibility"]
    mediation_results$indirect_benefit[i] <- coef(med_benefit)["Clickbait"] * 
      coef(out_model)["Benefit"]
    mediation_results$indirect_norm[i] <- coef(med_norm)["Clickbait"] * 
      coef(out_model)["Norm"]
  }
  
  return(mediation_results)
}

# Run bootstrap mediation analysis
set.seed(123) # For reproducibility
boot_results <- bootstrap_mediation(pls_data, n_bootstrap = 1000)

# Calculate mediation effects and confidence intervals
calculate_mediation_ci <- function(indirect_effects, alpha = 0.05) {
  effect <- mean(indirect_effects)
  ci_lower <- quantile(indirect_effects, alpha/2)
  ci_upper <- quantile(indirect_effects, 1 - alpha/2)
  
  return(c(effect = effect, ci_lower = ci_lower, ci_upper = ci_upper))
}

# H7: Clickbait -> Credibility -> Purchase
h7_results <- calculate_mediation_ci(boot_results$indirect_cred)
cat(sprintf("\nH7: Clickbait -> Credibility -> Purchase\n"))
cat(sprintf("Indirect effect: %.3f [%.3f, %.3f]\n", 
            h7_results["effect"], h7_results["ci_lower"], h7_results["ci_upper"]))

# H8: Clickbait -> Benefit -> Purchase  
h8_results <- calculate_mediation_ci(boot_results$indirect_benefit)
cat(sprintf("\nH8: Clickbait -> Benefit -> Purchase\n"))
cat(sprintf("Indirect effect: %.3f [%.3f, %.3f]\n",
            h8_results["effect"], h8_results["ci_lower"], h8_results["ci_upper"]))

# H9: Clickbait -> Norm -> Purchase
h9_results <- calculate_mediation_ci(boot_results$indirect_norm)
cat(sprintf("\nH9: Clickbait -> Norm -> Purchase\n"))
cat(sprintf("Indirect effect: %.3f [%.3f, %.3f]\n",
            h9_results["effect"], h9_results["ci_lower"], h9_results["ci_upper"]))

# =============================================================================
# 8. RESULTS VISUALIZATION
# =============================================================================

# Create a results summary data frame
results_summary <- data.frame(
  Hypothesis = c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9"),
  Relationship = c(
    "Clickbait → Credibility",
    "Clickbait → Benefit", 
    "Clickbait → Norm",
    "Credibility → Purchase",
    "Benefit → Purchase",
    "Norm → Purchase",
    "Clickbait → Credibility → Purchase",
    "Clickbait → Benefit → Purchase",
    "Clickbait → Norm → Purchase"
  ),
  Coefficient = c(h1_coef, h2_coef, h3_coef, h4_coef, h5_coef, h6_coef,
                  h7_results["effect"], h8_results["effect"], h9_results["effect"]),
  Type = c("Direct", "Direct", "Direct", "Direct", "Direct", "Direct",
           "Mediation", "Mediation", "Mediation")
)

# Plot direct effects
direct_effects <- results_summary[results_summary$Type == "Direct", ]

p1 <- ggplot(direct_effects, aes(x = Relationship, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Direct Effects in the Structural Model",
       x = "Relationship", y = "Path Coefficient") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# Plot mediation effects
mediation_effects <- results_summary[results_summary$Type == "Mediation", ]

p2 <- ggplot(mediation_effects, aes(x = Relationship, y = Coefficient)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Indirect (Mediation) Effects",
       x = "Mediation Path", y = "Indirect Effect") +
  theme_minimal()

print(p2)

# =============================================================================
# 9. MODEL FIT AND VALIDATION
# =============================================================================

cat("\nMODEL FIT INDICES:\n")
cat("==================\n")

# Calculate R-squared values
r_squared <- pls_model$r_squared
for (construct in names(r_squared)) {
  if (r_squared[construct] > 0) {
    cat(sprintf("R² for %s: %.3f\n", construct, r_squared[construct]))
  }
}

# =============================================================================
# 10. SAVE RESULTS
# =============================================================================

# Save key results to files
write.csv(results_summary, "hypothesis_testing_results.csv", row.names = FALSE)

# Save measurement model results
mm_df <- do.call(rbind, lapply(names(mm_results), function(x) {
  data.frame(
    Construct = x,
    Cronbach_Alpha = mm_results[[x]]$Cronbach_Alpha,
    Composite_Reliability = mm_results[[x]]$Composite_Reliability,
    AVE = mm_results[[x]]$AVE
  )
}))
write.csv(mm_df, "measurement_model_results.csv", row.names = FALSE)

# Save plots
ggsave("direct_effects_plot.png", p1, width = 8, height = 6, dpi = 300)
ggsave("mediation_effects_plot.png", p2, width = 8, height = 6, dpi = 300)

cat("\nAnalysis completed successfully!\n")
cat("Results saved to:\n")
cat("- hypothesis_testing_results.csv\n")
cat("- measurement_model_results.csv\n")
cat("- direct_effects_plot.png\n")
cat("- mediation_effects_plot.png\n")