# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the
Capital Asset Pricing Model (CAPM) using historical data for AMD and the
S&P 500 index. This exercise is designed to provide a hands-on approach
to understanding how these models are used in financial analysis to
assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between
systematic risk and expected return, especially for stocks. This model
is critical for determining the theoretically appropriate required rate
of return of an asset, assisting in decisions about adding assets to a
diversified portfolio.

## Objectives

1.  **Load and Prepare Data:** Import and prepare historical price data
    for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2.  **CAPM Implementation:** Focus will be placed on applying the CAPM
    to examine the relationship between AMD’s stock performance and the
    overall market as represented by the S&P 500.
3.  **Beta Estimation and Analysis:** Calculate the beta of AMD, which
    measures its volatility relative to the market, providing insights
    into its systematic risk.
4.  **Results Interpretation:** Analyze the outcomes of the CAPM
    application, discussing the implications of AMD’s beta in terms of
    investment risk and potential returns.

## Instructions

### Step 1: Data Loading

-   We are using the `quantmod` package to directly load financial data
    from Yahoo Finance without the need to manually download and read
    from a CSV file.
-   `quantmod` stands for “Quantitative Financial Modelling Framework”.
    It was developed to aid the quantitative trader in the development,
    testing, and deployment of statistically based trading models.
-   Make sure to install the `quantmod` package by running
    `install.packages("quantmod")` in the R console before proceeding.

``` r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing

``` r
colSums(is.na(df))
```

    ## Date  AMD GSPC   RF 
    ##    0    0    0    9

``` r
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that
describes the relationship between systematic risk and expected return
for assets, particularly stocks. It is widely used to determine a
theoretically appropriate required rate of return of an asset, to make
decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula

The formula for CAPM is given by:

*E*(*R*<sub>*i*</sub>) = *R*<sub>*f*</sub> + *β*<sub>*i*</sub>(*E*(*R*<sub>*m*</sub>)−*R*<sub>*f*</sub>)

Where:

-   *E*(*R*<sub>*i*</sub>) is the expected return on the capital asset,
-   *R*<sub>*f*</sub> is the risk-free rate,
-   *β*<sub>*i*</sub> is the beta of the security, which represents the
    systematic risk of the security,
-   *E*(*R*<sub>*m*</sub>) is the expected return of the market.

#### CAPM Model Daily Estimation

-   **Calculate Returns**: First, we calculate the daily returns for AMD
    and the S&P 500 from their adjusted closing prices. This should be
    done by dividing the difference in prices between two consecutive
    days by the price at the beginning of the period.
    $$
    \text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
    $$

``` r
df$AMD_dailyreturn <- 0
df$GSPC_dailyreturn <- 0

for (i in 2:nrow(df)) {
  df$AMD_dailyreturn[i] = (df$AMD[i] - df$AMD[i - 1])/df$AMD[i - 1]
  df$GSPC_dailyreturn[i] = (df$GSPC[i] - df$GSPC[i - 1])/df$GSPC[i - 1]
}

# Remove the first day where AMD_dailyreturn and GSPC_dailyreturn is 0
df <- subset.data.frame(df, AMD_dailyreturn != 0)
```

-   **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by
    conversion of annual risk-free Rate. This conversion accounts for
    the compounding effect over the days of the year and is calculated
    using the formula:
    $$
    \text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
    $$

``` r
df$riskfree_rate <- 0

for (i in 1:nrow(df)) {
  df$riskfree_rate = (1 + df$RF / 100)^(1/360) - 1
}
```

-   **Calculate Excess Returns**: Compute the excess returns for AMD and
    the S&P 500 by subtracting the daily risk-free rate from their
    respective returns.

``` r
df$AMD_excessreturn <- 0
df$GSPC_excessreturn <- 0

for (i in 2:nrow(df)) {
  df$AMD_excessreturn = df$AMD_dailyreturn - df$riskfree_rate
  df$GSPC_excessreturn = df$GSPC_dailyreturn - df$riskfree_rate
}
```

-   **Perform Regression Analysis**: Using linear regression, we
    estimate the beta (*β*) of AMD relative to the S&P 500. Here, the
    dependent variable is the excess return of AMD, and the independent
    variable is the excess return of the S&P 500. Beta measures the
    sensitivity of the stock’s returns to fluctuations in the market.

``` r
model1 <- lm(AMD_excessreturn ~ GSPC_excessreturn, df)
summaryModel1 <- summary(model1)
summaryModel1
```

    ## 
    ## Call:
    ## lm(formula = AMD_excessreturn ~ GSPC_excessreturn, data = df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.095775 -0.014769 -0.001165  0.012297  0.173636 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.001101   0.000726   1.516     0.13    
    ## GSPC_excessreturn 1.570102   0.054132  29.005   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0257 on 1253 degrees of freedom
    ## Multiple R-squared:  0.4017, Adjusted R-squared:  0.4012 
    ## F-statistic: 841.3 on 1 and 1253 DF,  p-value: < 2.2e-16

#### Interpretation

What is your *β*? Is AMD more volatile or less volatile than the market?

**Answer:** 1.5700012. AMD is more volatile than the market because the
gradient is greater than 1. An increase of 1 unit for GSPC excess
returns indicates an estimated increase of 1.57 for AMD excess returns.

#### Plotting the CAPM Line

Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM
regression line.

``` r
plot <- ggplot(df, aes(x=GSPC_excessreturn, y=AMD_excessreturn)) +
  geom_point() +
  geom_smooth(method = "lm", col = 'blue') +
  labs(title = "AMD vs. S&P 500 excess returns")
plot
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](capm_starter_files/figure-markdown_github/plot-1.png)

### Step 3: Predictions Interval

Suppose the current risk-free rate is 5.0%, and the annual expected
return for the S&P 500 is 13.3%. Determine a 90% prediction interval for
AMD’s annual expected return.

*Hint: Calculate the daily standard error of the forecast
(*s*<sub>*f*</sub>), and assume that the annual standard error for
prediction is $s_f \times \sqrt{252}$. Use the simple return average
method to convert daily stock returns to annual returns if needed.*

**Answer:**

``` r
# Calculating the daily expected GSPC excess return
daily_GSPC_return = 0.133 / 252
daily_riskfree_rate = (1 + 0.05)^(1/360) - 1

daily_expected_GSPC_excessreturn = daily_GSPC_return - daily_riskfree_rate

# Calculating the standard error of forecast (sf)
n <- length(df$GSPC_excessreturn)

mean_GSPC_excessreturn = mean(df$GSPC_excessreturn)

sum_squares_error = sum(residuals(model1)^2)
mean_squares_error = sum_squares_error / (n - 1 - 1)
se_daily = sqrt(mean_squares_error)

sum_square_total = sum((df$GSPC_excessreturn - mean_GSPC_excessreturn)^2)

sf_daily = se_daily * sqrt(1 + 1/n + ((daily_expected_GSPC_excessreturn - mean_GSPC_excessreturn)^2 / sum_square_total))
sf_annual = sf_daily * sqrt(252)

# Calculating the prediction internal
alpha <- 0.10

t_value <- qt(1 - alpha/2, df = n - 2)

# Calculating estimate for AMD annual expected return
beta_0 <- summaryModel1$coefficients[1,1]
beta_1 <- summaryModel1$coefficients[2,1]

estimated_AMD_excessreturn = beta_0 + beta_1 * (0.133 - 0.05)
estimated_AMD_expectedreturn = estimated_AMD_excessreturn + 0.05

# Calculating upper and lower bound
lower_bound <- estimated_AMD_expectedreturn - t_value * sf_annual
upper_bound <- estimated_AMD_expectedreturn + t_value * sf_annual

cat("The 90% prediction interval is [",lower_bound, ",", upper_bound,"]\n")
```

    ## The 90% prediction interval is [ -0.4904073 , 0.8532461 ]
