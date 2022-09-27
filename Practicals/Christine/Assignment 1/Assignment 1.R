
# Assignment 1 
# Monte Carlo simulation exercise 

library(ggplot2)
library(kableExtra)

# ---- Perform a small simulation that does the following: ----

# Sample 100 samples from a standard normal distribution.

# For each of these samples, calculate the following statistics for the mean:
# absolute bias
# standard error
# lower bound of the 95% confidence interval
# upper bound of the 95% confidence interval

stats <- vector("list", 100)
nsim <- length(stats)

set.seed(123)

stats <- lapply(1:nsim, function(x) { 
  
  draw <- rnorm(1000, mean = 0, sd = 1)
  mbar <- mean(draw)
  bias <- mbar - 0
  se <- 1/sqrt(length(draw))
  df <- length(draw) - 1
  lower <- mbar - qt(.975, df) * se
  upper <- mbar + qt(.975, df) * se
  
  return(c("mean" = mbar, "bias" = bias, 
           "se" = se, "lower" = lower, "upper" = upper))
  }
)

stats_df <- as.data.frame(do.call(rbind, stats))


# ---- Create a plot that demonstrates the following:----

# “A replication of the procedure that generates a 95% confidence interval that is centered around the sample mean would cover the population value at least 95 out of 100 times” (Neyman, 1934)

# flag samples where population mean is covered by CI
stats_df$covered <- with(stats_df, ifelse(lower <= 0 & 0 <= upper, TRUE, FALSE))

# order samples by amount of bias
stats_df_ord <- stats_df[order(stats_df$bias), ] 
                    
# plot means with errorbars     
ggplot(data = stats_df_ord, 
       aes(x = mean, y = 1:100, color = covered)) +
  geom_vline(aes(xintercept = 0)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.3) +
  labs(title = "Means and 95% confidence intervals of one hundred samples drawn from a standard normal dsitribution.",
       x = "Deviation from population mean", y = "Sample",
       color = "Mean") +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.9), 
        legend.background = element_rect(fill = "transparent", 
                                         colour = "transparent"),
        plot.title = element_text(hjust = 0.5)) 


# ---- Present a table containing all simulated samples for which the resulting confidence interval does not contain the population value.-----

kable(stats_df[stats_df$covered == FALSE, ], 
      format = "simple", 
      digits = 3)
