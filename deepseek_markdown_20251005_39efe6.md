# Computer Code for PeerJ Manuscript: "Scroll, Click, Buy: The Impact of Clickbait and Digital Credibility on TikTok Users' Purchase Intentions"

This repository contains the complete R code to reproduce the statistical analyses from the manuscript.

## Data
The dataset `TIKTOK_1_SCROLL,_CLICK,_BUY.csv` is available on Zenodo: [https://zenodo.org/records/16927854](https://zenodo.org/records/16927854)

## Analysis Overview
This code performs:
1. **Measurement Model Assessment** - Reliability and validity testing (Cronbach's Alpha, Composite Reliability, AVE)
2. **PLS-SEM Analysis** - Structural equation modeling using Partial Least Squares
3. **Direct Effects Testing** - Hypotheses H1 through H6
4. **Mediation Analysis** - Bootstrapped testing of H7 through H9
5. **Results Visualization** - Bar plots of direct and indirect effects

## Key Variables (UPDATE REQUIRED)
The script uses placeholder variable names. You MUST update the `constructs` list with your actual variable names:

```r
constructs <- list(
  Clickbait = c("ACTUAL_CB1", "ACTUAL_CB2", ...),  # Your clickbait items
  Credibility = c("ACTUAL_SC1", "ACTUAL_SC2", ...), # Your credibility items  
  Benefit = c("ACTUAL_PB1", "ACTUAL_PB2", ...),    # Your benefit items
  Norm = c("ACTUAL_PN1", "ACTUAL_PN2", ...),       # Your norm items
  Purchase = c("ACTUAL_PI1", "ACTUAL_PI2", ...)    # Your purchase intention items
)