# Taipei Housing Price Analysis

This repository contains the final project for **Business Analytics (113-1)**, exploring the determinants of housing prices in Taipei City. The study uses government real estate transaction data to build regression models and identify key drivers of housing prices.

---

## Project Structure
- `clean_taipei_housing.csv` : Cleaned dataset after preprocessing (removing nulls/outliers, feature engineering).
- `taipei_housing_analysis.R` : Main R script for data cleaning, EDA, and regression modeling.
- `taipei_housing_report.pdf` : Final report (in Chinese) summarizing methodology, models, and results.

---

## Research Overview

### Objectives
1. Explore determinants of housing prices in Taipei.
2. Build regression models for:
   - **Unit price per square meter**
   - **Total transaction price**
3. Address multicollinearity and optimize models.
4. Provide policy and market insights for stakeholders.

### Methodology
- **Data Cleaning**
  - Remove null values, outliers (e.g., family/friend transactions).
  - Feature engineering: luxury variable, floor level categorization, public facility ratio.
- **Exploratory Data Analysis (EDA)**
  - Histograms, scatterplots, boxplots for numerical & categorical variables.
- **Modeling**
  - Multiple regression models with transformations (log, squared terms).
  - Addressed multicollinearity (elevator/building type interaction).
- **Evaluation**
  - R² up to **0.89** (Adjusted R²: 0.8926) after optimization.
  - Residual & QQ plots checked model assumptions.

### Key Findings
- **Significant factors:** building area, age (nonlinear effect), luxury status, building type, elevators, public facility ratio, district differences.
- **Regional premium:** Daan, Zhongzheng, Songshan have the highest prices.
- **Luxury effect:** high-end properties drive up averages in elite districts.
- **Model strength:** total price models achieve higher explanatory power than unit price models.

---

## Environment & Dependencies
- R 4.3+
- Libraries: `tidyverse`, `dplyr`, `ggplot2`, `car`, `MASS`

---

## Authors
Group 19 — Business Analytics (113-1) Final Report  
Members: 吕欣融, 杜奕寧, 林姝延, 黃茂殷, 蔡宜芳
