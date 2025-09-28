# Quick Start Guide: Two-Stage Logistic Growth Model

## 1. Requirements
- R (≥ 4.0 recommended)
- RStudio (optional but recommended)
- Install packages:
```r
install.packages(c("tidyverse","knitr"))
```

## 2. Run the R Script
1. Open `Two_Stage_Logistic_Model.R` in RStudio (if included separately).
2. Ensure the dataset is in the `data/` folder.
3. Edit `inputs$death_rate` in the code to test different scenarios.
4. Run the script → results will print and save to CSV.

## 3. Knit to PDF
1. Open `Two_Stage_Logistic_Model_PDF.Rmd`.
2. Click **Knit → Knit to PDF**.
3. PDF report includes parameter estimates, plots, and tables.

## 4. Knit to GitHub Markdown
1. Open `Two_Stage_Logistic_Model_GitHub.Rmd`.
2. Click **Knit → Knit to GitHub Document**.
3. Produces `.md` file with inline plots and tables (for GitHub display).

## 5. Experiment
- Modify `death_rate` in the **inputs** list.
- Compare different mortality scenarios.
