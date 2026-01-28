
REPLICATION CODE – MASTER'S DISSERTATION
======================================

This text file contains:
(1) A README-style documentation of the empirical strategy
(2) The full R replication code with standardized English headers and comments

--------------------------------------------------
EMPIRICAL WORKFLOW
--------------------------------------------------

1. Data Import and Preprocessing
   - Import Bloomberg-based commodity futures indices
   - Handle missing observations
   - Construct log-returns

2. Descriptive Statistics
   - Summary statistics
   - Distributional analysis
   - Normality tests

3. Structural Break Analysis
   - Bai & Perron multiple structural break tests

4. Volatility Modeling
   - Univariate GARCH / eGARCH models
   - Dynamic Conditional Correlation (DCC-GARCH)

5. Systemic Risk Measures
   - Value at Risk (VaR)
   - Conditional Value at Risk (CoVaR)
   - Delta CoVaR (ΔCoVaR)

--------------------------------------------------
SOFTWARE REQUIREMENTS
--------------------------------------------------

R version >= 4.2.0

Main packages:
tidyverse, rugarch, rmgarch, zoo, tseries,
strucchange, PerformanceAnalytics

--------------------------------------------------
REPRODUCIBILITY
--------------------------------------------------

All scripts are designed to be run sequentially.
Random seeds are fixed when applicable.
