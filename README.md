---
title: "Bootstrapping confidence intervals"
output: 
  html_document: 
    highlight: tango
    theme: readable
    toc: yes
    toc_depth: 3
---
  
<style type="text/css">
  body, td {font-size: 18px;}
  code.r{font-size: 18px;}
  pre {font-size: 18px} 
</style>


## Packages
```{r message=F}
library(dplyr)
library(readr)
```

## Data Table

Our data is organized by monitoring site and date. Here's a sample.

```{r kable, message=F, echo=F}
library(knitr)

df <- read_csv(
'"AQS_ID","Date","Flow"
270535501,"2009-07-30",0.00148
270535501,"2009-09-30",0.00064
270535501,"2009-11-30",0.34256
270535501,"2009-12-30",0.00064
270535502,"2009-03-30",0.26219
270535502,"2009-07-30",0.01113
270535502,"2009-09-30",0.00044
270535502,"2009-11-30",0.00127
270535502,"2009-12-30",0.00113')

kable(df)
```

## Bootstrapping

We currently use the `EnvStats` package to generate our summary values. It has built in functions to accunt for non-detect data and allows for different distributions. However, if you're not dealing with non-detects it's probably simple enough to boot things yourself.


Before you start you'll want to set the random number generator to ensure you'll be able to reproduce your results. I'll use `#27` below.
```{r message=F}
set.seed(27)
```

</br>  
The general idea is to take a random sample from the data set, generate the statistic that you're interested in, record it, then rinse and repeat. Below is the code for how to resample a single site.
```{r message=F}

# Filter data to a single site
df_site1 <- filter(df, AQS_ID == AQS_ID[1])

# Pull random sample
# `replace=T` allows for the same value to be pulled multiple times
# `size=nrow(df)` ensures the number of observations in the new table to match the original 
random_df <- sample(df_site1$Flow, replace=T)
  
# Generate summary statistic
quantile(random_df, 0.1)
 
```


To repeat this 3,000 times we can wrap these steps into a `resample` function, and then use `sapply` to collect the results.
```{r message=F}

# Create  resample function
resample_flow <- function(data= df_site1$Flow, flow_pct= 0.10){
  
  random_df <- sample(data, replace=T)
  
  quantile(random_df, flow_pct)[[1]]
  
}

# Repeat using `sapply`
repeats <- 3000

booted_10pct <- sapply(1:repeats, FUN=function(x) resample_flow(df_site1$Flow))

# The 50th percentile or median low flow
median(booted_10pct, na.rm=T)

# Return the 95th percentile of the booted low flows
quantile(booted_10pct, 0.95, na.rm=T)[[1]]

# Force the 95th percentile to be a recorded value
sort(booted_10pct)[repeats*.95 +1]

# Upper and lower confidence interval around the median
quantile(booted_10pct, c(0.025, 0.5, 0.975), na.rm=T)

```


## Automate

To finish up, throw these steps into your personal `boot` function, and then run it on each site using `group_by`.
```{r message=F}

# Create boot function
boot_low_flow <- function(data=df$Flow, flow_pct=0.10, conf_int=0.95, repeats=3000){

  alpha <- (1 - conf_int)/2
  
  booted_10pct <- sapply(1:repeats, FUN=function(x) resample_flow(data, flow_pct))

  # Upper and lower confidence interval around the median
  list(quantile(booted_10pct, c(alpha, 0.5, 1-alpha), na.rm=T))

}

# Use `group_by` to send data for each site to your boot function
low_flows <- group_by(df, AQS_ID) %>% 
             mutate(boot_results = boot_low_flow(Flow, flow_pct=0.10, conf_int=0.95)) %>%
             summarize(LCL95_low_flow  = unlist(boot_results[1])[[1]], 
                       Low_flow        = unlist(boot_results[1])[[2]], 
                       UCL95_low_flow  = unlist(boot_results[1])[[3]])  
```

## Results

The booted confidence limits
`r kable(low_flows)`
                 
