## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by
incorporating financial metrics to evaluate its profitability. This
exercise simulates a real-world scenario where you, as part of a
financial technology team, need to present an improved version of a
trading algorithm that not only executes trades but also calculates and
reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have
been tasked by the Trading Strategies Team to modify your trading
algorithm. This modification should include tracking the costs and
proceeds of trades to facilitate a deeper evaluation of the algorithm’s
profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to
include costs, proceeds, and return on investments metrics to assess the
profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a
    DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm
    based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total
    profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and
    analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the “Data Loading” section
to generate a DataFrame containing AMD stock closing data. This will
serve as the basis for your trading decisions. First, create a data
frame named `amd_df` with the given closing prices and corresponding
dates.

``` r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data Plot the closing prices over time to visualize the
price movement.

``` r
plot(amd_df$date, amd_df$close,'l')
```

![](trading_ricky_chau_files/figure-markdown_github/plot-1.png)

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should
initialize necessary variables, and loop through the dataframe to
execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns
    ‘trade_type’, ‘costs_proceeds’ and ‘accumulated_shares’.
-   Change the algorithm by modifying the loop to include the cost and
    proceeds metrics for buys of 100 shares. Make sure that the
    algorithm checks the following conditions and executes the strategy
    for each one:
    -   If the previous price = 0, set ‘trade_type’ to ‘buy’, and set
        the ‘costs_proceeds’ column to the current share price
        multiplied by a `share_size` value of 100. Make sure to take the
        negative value of the expression so that the cost reflects money
        leaving an account. Finally, make sure to add the bought shares
        to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of
        the previous day, set the ‘trade_type’ to ‘buy’. Set the
        ‘costs_proceeds’ to the current share price multiplied by a
        `share_size` value of 100.
    -   You will not modify the algorithm for instances where the
        current day’s price is greater than the previous day’s price or
        when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the ‘trade_type’ to
        ‘sell’. In this case, also set the ‘costs_proceeds’ column to
        the total number in the `accumulated_shares` variable multiplied
        by the price of the last day.

``` r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  # Sell on last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] = "sell"
    amd_df$costs_proceeds[i] = accumulated_shares * amd_df$close[i]
    accumulated_shares = 0
    
  } else if (previous_price == 0 || amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] = "buy"
    amd_df$costs_proceeds[i] = -amd_df$close[i] * share_size
    accumulated_shares = accumulated_shares + share_size
    
  }
  # Updates the previous price and accumulated shares at the end of the day
  amd_df$accumulated_shares[i] = accumulated_shares
  previous_price = amd_df$close[i]
}
```

## Step 3: Customize Trading Period

-   Define a trading period you wanted in the past five years

``` r
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Choosing first and last trading day
first_day_trading <- as.Date("19/07/2021", "%d/%m/%Y")
last_day_trading <- as.Date("18/07/2023", "%d/%m/%Y")

# Creating a new dataframe with appropriate dates
trading_period_df <- subset.data.frame(amd_df, date >= first_day_trading & date <= last_day_trading)
trading_period_df$trade_type <- NA
trading_period_df$costs_proceeds <- NA
trading_period_df$accumulated_shares <- 0

# Same trading algorithm as Step 2
for (i in 1:nrow(trading_period_df)) {
  if (i == nrow(trading_period_df)) {
    trading_period_df$trade_type[i] = "sell"
    trading_period_df$costs_proceeds[i] = accumulated_shares * trading_period_df$close[i]
    accumulated_shares = 0
    
  } else if (previous_price == 0 || trading_period_df$close[i] < previous_price) {
    trading_period_df$trade_type[i] = "buy"
    trading_period_df$costs_proceeds[i] = -trading_period_df$close[i] * share_size
    accumulated_shares = accumulated_shares + share_size
    
  }
  trading_period_df$accumulated_shares[i] = accumulated_shares
  previous_price = trading_period_df$close[i]
}
```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as
expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss
    from your trades. This should be the sum of all entries in the
    ‘costs_proceeds’ column of your dataframe. This column records the
    financial impact of each trade, reflecting money spent on buys as
    negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is
    equal to the sum of the ‘costs_proceeds’ values for all ‘buy’
    transactions. Since these entries are negative (representing money
    spent), you should take the negative sum of these values to reflect
    the total amount invested.
-   ROI Formula:
    $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

``` r
# Initialize variables
total_profit_loss <- 0
invested_capital <- 0

for (i in 1:nrow(trading_period_df)) {
  # Calculating the total profit/loss
  if (is.na(trading_period_df$costs_proceeds[i]) == FALSE) {
    total_profit_loss = total_profit_loss + trading_period_df$costs_proceeds[i]
  }
  
  # Calculating the total invest capital
  if ((is.na(trading_period_df$trade_type[i]) == FALSE) && 
      ((trading_period_df$trade_type[i] == "buy") == TRUE)) {
    invested_capital = invested_capital + trading_period_df$costs_proceeds[i]
  }
}

roi = (total_profit_loss / -invested_capital) * 100
print(paste("The P/L from the trades performed is", total_profit_loss))
```

    ## [1] "The P/L from the trades performed is 518633.005199999"

``` r
print(paste("The ROI from the trades performed is", roi))
```

    ## [1] "The ROI from the trades performed is 20.9397331096377"

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of
    your holdings if the price has increased by a certain percentage
    (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy
    that you sell half of your holdings if the stock falls by a certain
    percentage (e.g., 20%) from the average purchase price. You don’t
    need to buy 100 stocks on the days that the stop-loss mechanism is
    triggered.

``` r
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_purchase_price <- 0
total_purchase_price <- 0
percentage_cutoff <- 0.20

# Choosing first and last trading day
first_day_trading <- as.Date("19/07/2021", "%d/%m/%Y")
last_day_trading <- as.Date("18/07/2023", "%d/%m/%Y")

# Creating a new dataframe with appropriate dates
trading_period_df <- subset.data.frame(amd_df, date >= first_day_trading & date <= last_day_trading)
trading_period_df$trade_type <- NA
trading_period_df$costs_proceeds <- NA
trading_period_df$accumulated_shares <- 0
trading_period_df$accumulated_purchase <- 0
trading_period_df$average_purchase <- 0


for (i in 1:nrow(trading_period_df)) {
  if (i == nrow(trading_period_df)) {
    trading_period_df$trade_type[i] = "sell"
    trading_period_df$costs_proceeds[i] = accumulated_shares * trading_period_df$close[i]
    accumulated_shares = 0
    
  } else if (trading_period_df$close[i] > average_purchase_price * (1 + percentage_cutoff) 
             && total_purchase_price != 0) {
    # Implemented new logic which sells when price is below a percentage of the average
      trading_period_df$trade_type[i] = "sell"
      trading_period_df$costs_proceeds[i] = accumulated_shares / 2 * trading_period_df$close[i]
      accumulated_shares = accumulated_shares / 2
      total_purchase_price = total_purchase_price / 2
      
  } else if (previous_price == 0 || trading_period_df$close[i] < previous_price) {
    trading_period_df$trade_type[i] = "buy"
    trading_period_df$costs_proceeds[i] = -trading_period_df$close[i] * share_size
    accumulated_shares = accumulated_shares + share_size
  }
  
  # Updates the total purchase price only when the trade is "buy"
  if (is.na(trading_period_df$trade_type[i]) == FALSE &&
      trading_period_df$trade_type[i] == "buy") {
    total_purchase_price = total_purchase_price + trading_period_df$close[i] * share_size
    
  }
  
  # Updates all the variables and columns at the end of the trading day
  average_purchase_price = total_purchase_price / accumulated_shares
  trading_period_df$accumulated_purchase[i] = total_purchase_price
  trading_period_df$average_purchase[i] = average_purchase_price
  trading_period_df$accumulated_shares[i] = accumulated_shares
  previous_price = trading_period_df$close[i]
}
```

## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these
    outcomes may have occurred.

``` r
total_profit_loss <- 0
invested_capital <- 0

for (i in 1:nrow(trading_period_df)) {
  if (is.na(trading_period_df$costs_proceeds[i]) == FALSE) {
    total_profit_loss = total_profit_loss + trading_period_df$costs_proceeds[i]
  }
  
  if ((is.na(trading_period_df$trade_type[i]) == FALSE) && 
      ((trading_period_df$trade_type[i] == "buy") == TRUE)) {
    invested_capital = invested_capital + trading_period_df$costs_proceeds[i]
  }
}

roi = (total_profit_loss / -invested_capital) * 100
print(paste("The P/L from the trades performed is", total_profit_loss))
```

    ## [1] "The P/L from the trades performed is 456077.124694614"

``` r
print(paste("The ROI from the trades performed is", roi))
```

    ## [1] "The ROI from the trades performed is 24.5152538601099"

``` r
plot(trading_period_df$date, trading_period_df$close,'l')
```

![](trading_ricky_chau_files/figure-markdown_github/unnamed-chunk-2-1.png)

For the period 19/07/2021 to 18/07/2023, the AMD share price rose to an
all time high during the period but then proceeded to drop significantly
due to the changes within the market.

The Covid-19 pandemic from 2020-2022 majorly impacted the world’s
economy as consumers had to adapt to quarantine and thus their spending
habits shifted. AMD greatly benefited from the shift towards working
from home and staying indoors. The demand for CPUs and GPUs majorly
increased during the pandemic as consumers needed electronic devices to
work, learn and to provide entertainment. Eventually, this lead to a
global shortage of electronic chips because the demand was too high that
supply couldn’t keep up. AMD increased production and inventory despite
the economic conditions of the world being in recession because of this
increase in demand. This lead to an all time high of the AMD share price
at 154.36 on 27/12/2021. This suggests that investors saw the potential
in AMD shares during the pandemic because of this greater market that
opened up due to the Covid-19 restrictions.

However, as the restrictions eased and people went back to work and
school, the demand for CPUs and GPUs decreased. Furthermore, due to the
high interest from investors during Covid-19, the price of AMD shares
were greatly inflated. Additionally, the U.S Federal Reserve stated that
its benchmark rate would be raised from 0% to try and combat the high
inflation caused by Covid-19. The higher benchmark rate will ultimately
lead to a lower share price for a majority of companies. This may have
caused investors to be weary of holding onto shares due to the
instability of the price, causing them to sell and the share price to
drop significantly.

The first strategy had a profit/loss of $518,633.01 meanwhile my second
strategy had a profit/loss of $456,077.12. Thus, there was a loss in
profit with the change in profit/loss being -$62,555.89. However, the
first strategy had a ROI of 20.94% meanwhile my second strategy had a
ROI of 24.52%. Thus, there was an increase in the return on investment
with the change in ROI being 3.58%.

This indicates that the second strategy purchased less shares compared
to the first strategy and in turn made a higher return on investment
despite gaining less overall profit. I believe this is because the
second strategy could sell the shares at the higher price during the
Covid pandemic and benefit from the rising prices. Meanwhile, the first
strategy only sold on the last day.
