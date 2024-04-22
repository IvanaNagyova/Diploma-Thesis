## Overview
This repository contains the code used for my diploma thesis titled **"APPLICATION AND COMPARISON OF TRADING INDICATORS IN FINANCIAL PRACTICE WITH R"**. The thesis focuses on the analysis of technical analysis indicators, their application in trading financial instruments, and the comparison of the achieved results in terms of risk-return indicators using the statistical system R. The code is written in R and consists of a main script named **Main code.R** and supporting source files stored in the **functions** folder.
## Instructions
To use the code, follow these steps:

1. Ensure you have R and an IDE that supports R installed. If you don't have an appropriate IDE, I recommend you download RStudio from the [RStudio website](https://posit.co/download/rstudio-desktop/).
2. Create a new folder as your preferred directory for storing the code, or if you already have a folder you prefer for coding, you can use it as your working directory.
3. Download the **Main code.R** file and the entire **functions** folder into your working directory.

## Usage
Once you have downloaded and saved the necessary files, you can execute the **Main code.R** script to perform the analyses.

## Details
### Source Files
The **Main code.R** file serves as the entry point for running the analyses and calling the main functions. It references functions located in the **functions** folder, including:

- **initialisation.R**: Contains code that handles the initial setup for the analysis. It installs the necessary packages for data manipulation and analysis. Additionally, it downloads stock price data for a specified ticker and time period. Users can easily modify the ticker symbol and time period parameters in the `prepared_data` function to analyze data for different stocks and time periods according to their analysis requirements.
- **technical_analysis.R**: This file contains functions for conducting technical analysis on stock price data. It includes functions to add various technical indicators such as simple moving averages, exponential moving averages, Commodity Channel Index (CCI), Relative Strength Index (RSI), Average True Range (ATR), Stochastic oscillator, Moving Average Convergence Divergence (MACD), and Bollinger Bands. Additionally, it provides functions to detect buy and sell signals based on these indicators. Moreover, there are functions available to plot these indicators along with buy and sell signals, enabling users to visualize the technical analysis results alongside the stock price data.
- **backtesting.R**: This script contains functions for backtesting trading strategies. It facilitates the simulation of trading strategies by generating and executing trading orders based on specified indicators and parameters. The script handles the creation of orders, including entry and exit points, stop-loss, and take-profit levels. Additionally, it simulates the execution of orders on historical price data, managing opening and closing trades based on predefined strategies and conditions.
- **analysis_of_results.R**: This script contains functions for analyzing the results of trading strategies. It focuses on simulating portfolio performance based on closed trades, computing portfolio returns, and generating visualizations to compare strategy performance against a buy-and-hold benchmark. Additionally, the script provides functions for calculating performance metrics such as the information ratio and identifying better-performing strategies based on cumulative profit and buy-and-hold returns.

## Conclusion
Thank you for exploring my repository! If you have any questions or suggestions, please feel free to reach out to me. You can send me a message through GitHub discussions or find me on other platforms. Your feedback is always welcome, and I'm here to help you in any way I can.

*Bc. Ivana Nagyová*
