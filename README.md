# AFE Coursework: Time Series Analysis of S&P 500, CPI, and 3-Month Treasury Bill Rate

## Overview

This R script conducts a time series analysis of S&P 500 closing prices, CPI index, and 3-month U.S. Treasury Bill rates from 2009 to 2019. It processes and merges monthly data, applies logarithmic transformation, and tests for:

1. ***Stationarity***: Uses ADF, Phillips-Perron, and KPSS tests to confirm variables are I(1) (first-difference stationary), with S&P 500 potentially trend-stationary after detrending.
2. ***Structural Breaks***: Identifies a break in T-Bill rates around 2016 using the Zivot-Andrews test, subsetting data to 2009–2015 for stability.
3. ***Cointegration***: Applies Johansen tests (trace and eigenvalue) to detect one cointegrating relationship among variables.
4. ***VECM Modeling***: Fits VECM models (2 and 12 lags) to analyze long-run and short-run dynamics, converting to VAR for forecasting and impulse response functions.
5. ***Diagnostics***: Checks for serial correlation (none detected), non-normality (present), parameter stability (confirmed), and ARCH effects (present), suggesting robust standard errors or GARCH modeling for refinement.

---

## Prerequisites

- **R Version:** 3.6 or higher  
- **Required R Packages:**
  - `vars`
  - `xts`
  - `zoo`
  - `urca`
  - `ggplot2`
  - `forecast`
  - `tseries`
  - `tsDyn`

- **Data Files:**
  - `S&P500.csv`: S&P 500 daily closing prices
  - `International_Financial_Statistics_MonthlyEconomicData_Edited.csv`: CPI index data
  - `US_3monthTBill_Edited.csv`: 3-month Treasury Bill rates

> Ensure the working directory is set to the location of these CSV files.

---

## Workflow

### 1. Data Processing

- Loads and cleans the datasets, converting dates using `as.Date`.
- Selects relevant columns:
  - `Close` for S&P 500 and T-Bill
  - `CPI_Index_Value` for CPI
- Converts daily data to monthly:
  - **S&P 500**: last value
  - **T-Bill**: monthly mean
- Merges datasets, applies **logarithmic transformation**, and subsets data to **2009–2019**.
- Saves the processed dataset as `CW_Final.rds`.

---

### 2. Time Series Properties

- Visualizes data to identify trends and potential **structural breaks**.
- Performs **Zivot-Andrews test** to detect a structural break in T-Bill rates around 2016.
- Conducts **ADF**, **Phillips-Perron**, and **KPSS** tests to assess stationarity.
- Detrends S&P 500 data and confirms stationarity of the detrended series.
- Determines all variables (S&P 500, CPI, T-Bill) are **I(1)** (first-difference stationary).

---

### 3. Cointegration Analysis

- Uses `VARselect` to determine optimal lag order (**K = 2**, based on AIC).
- Conducts **Johansen cointegration tests** (trace and max eigenvalue):
  - Includes different deterministic terms (trend, constant, none).
- Assumes **one cointegrating relationship** (**r = 1**) based on results.

---

### 4. VECM Estimation

- Fits VECM models with **2 and 12 lags** using `cajorls` under different deterministic assumptions.
- Re-estimates VECM using `tsDyn::VECM` for **maximum likelihood estimation** and **standard errors**.
- Converts VECM to VAR format for **forecasting** and **Impulse Response Functions (IRFs)**.

---

### 5. Diagnostics and Results

- Generates **12-month forecasts** and visualizes with `fanchart`.
- Computes **IRFs** to analyze the impact of shocks across variables.
- Performs **variance decomposition** to assess variable contributions.
- Conducts diagnostic tests:
  - **Portmanteau** and **Breusch-Godfrey** for serial correlation  
    → No significant autocorrelation
  - **Jarque-Bera**, **skewness**, **kurtosis** tests  
    → Residuals are non-normal
  - **OLS-CUSUM** test  
    → No structural breaks
  - **ARCH** test  
    → Volatility clustering detected

> Suggests use of:
> - Robust standard errors (`vcovHAC`)
> - GARCH modeling
> - Outlier checks for further model refinement

---

## Usage

1. Set the working directory to the folder containing the CSV files.
2. Install required packages (if not already installed):

```r
install.packages(c("vars", "xts", "zoo", "urca", "ggplot2", "forecast", "tseries", "tsDyn"))
