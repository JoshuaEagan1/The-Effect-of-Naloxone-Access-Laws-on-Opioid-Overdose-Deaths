# The Effect of Naloxone Access Laws on Opioid Overdose Deaths
### Joshua Eagan
### 12/07/2021

## Summary

I wrote this paper as my final project for Dr. Alyssa Carlson's Economics 8473- Applied Econometrics course at the University of Missouri. For this paper, I construct monthly and yearly panel data sets containing variables tracking the roll-out of different state level opioid policies over time and mortality from a variety of different categories of drug overdoses. In my paper, I explore the correlation between different policy indicator variables, implement a Staggered Difference in Differences model using a Two Way Fixed Effects (TWFE) Estimator to estimate the effects of Naloxone Access Laws, and propose a methodological change for future research to mitigate the bias of the TWFE estimator in this setting.

## How to Reproduce

All the code and data for this analysis are available in this repository, and the paper can be rewritten using the `\paper\The Effects of NAL on Opioid Overdose Mortality_20211201.Rmd` file. To clean the data for this analysis, run the R scripts in the `\code\cleaning` directory.