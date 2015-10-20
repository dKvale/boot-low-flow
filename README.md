---
title: "Bootstrapping confidence intervals"
output: 
  html_document: 
    highlight: tango
    keep_md: yes
    theme: readable
    toc: yes
    toc_depth: 3
---
  
<style type="text/css">
  body, td {font-size: 16px;}
  code.r{font-size: 16px;}
  pre {font-size: 16px} 
</style>
  
  This R file finds the age distributions of the MNDOT registered vehicles
after they have been sorted by vehicle class.


## Packages
```{r message=F}
library(dplyr)
library(readr)
library(stringr)
library(EnvStats)
```

## Data Table

Our data is organized by monitoring site and pollutant. Here's a sample.

```{r kable, message=F, echo=F}
library(knitr)

df <- read_csv(
'"AQS_ID","Date","Pollutant","Concentration"
270535501,"2009-07-30","Lead",0.00148
270535501,"2009-07-30","Selenium",0.00064
270535501,"2009-07-30","Iron",0.34256
270535501,"2009-07-30","Chromium",0.00064
270535502,"2009-07-30","Aluminum",0.26219
270535502,"2009-07-30","Manganese",0.01113
270535502,"2009-07-30","Nickel",0.00044
270535502,"2009-07-30","Antimony",0.00127
270535502,"2009-07-30","Arsenic",0.00113')

kable(df)
```

## Bootstrapping

We currently use the `EnvStats` package to generate our summary values because it has built in functions to accunt for non-detect data. `EnvStats` doesn't seem to provide percentile results, and if you're not dealing with non-detects it's probably simple enough to boot it yourself.


Before you start you'll want to set the random number generator, otherwise you'll never be able to reproduce the same result. I'll use `27` below.
```{r message=F}
set.seed(27)
```


The general idea is to take a random sample from the data set, generate the statistic your interested in, record it, then rinse and repeat. Below is the code for a single site.
```{r message=F}

# Filter data to a single site
df_site1 <- filter(df, AQS_ID == AQS_ID[1])

# Pull random sample
# `replace=T` allows for the same value to be pulled multiple times
# `size=nrow(df)` sets the number of observations in the new table to match the original 
random_df <- sample_n(df, replace=T, size=nrow(df))
  
# Generate summary statistic
quantile(random_df$Concentration, 0.1)
 
```


To repeat this 3,000 times we wrap these steps into a function.
```{r message=F}

# Create  resample function
resample_10pct_flow <- function(data=df){
  
  random_df <- sample_n(df, replace=T, size=nrow(df))
  
  quantile(random_df$Concentration, 0.1)[[1]]
  
}

# Repeat using `sapply`
repeats <- 3000

booted_10pct <- sapply(1:repeats, FUN=function(x) resample_10pct_flow(df))

# The 50th percentile or median low flow
median(booted_10pct)

# Return the 95th percentile of the booted low flows
quantile(booted_10pct, 0.95)[[1]]

# Force the 95th percentile to be a recorded value
sort(booted_10pct)[repeats*.95 +1]

# Upper and lower confidence interval around the median
quantile(booted_10pct, c(0.025, 0.5, 0.975))

```


## Automate

To finish up you can throw these steps all into one function, and run it for all your sites.
```{r message=F}

# Create  resample function
resample_flow <- function(data=df, flow_pct=0.10){
  
  random_df <- sample_n(df, replace=T, size=nrow(df))
  
  quantile(random_df$Concentration, flow_pct)[[1]]
  
}

# Create boot function
boot_low_flow <- function(data=df, flow_pct=0.10, conf_int=0.95, repeats=3000){

  alpha <- (1 - conf_int)/2
  
  booted_10pct <- sapply(1:repeats, FUN=function(x) resample_flow(df, flow_pct))

  # Upper and lower confidence interval around the median
  list(quantile(booted_10pct, c(alpha, 0.5, 1-alpha)))
  
}

# Use `group_by` to send data for each site to your boot function
low_flows <- group_by(df, AQS_ID) %>% 
             mutate(boot_results = boot_low_flow())  %>%
             summarize(LCL95_low_flow = unlist(boot_results)[[1]], 
                       low_flow       = unlist(boot_results)[[2]], 
                       UCL95_low_flow = unlist(boot_results)[[3]])         
                 
```
