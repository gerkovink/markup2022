# a. Sample 100 samples from a standard normal distribution.
set.seed(7079540)
samples <- list()
for (i in 1:100) {
samples[[i]] <- rnorm(1000)
}

# b. For each of these samples, calculate the following statistics for the mean:
# › the sample mean 
# › absolute bias
# › standard error
# ›lower bound of the 95% confidence interval
# › upper bound of the 95% confidence interval
results <- list()
for (i in 1:100) {
  M <- mean(samples[[i]])
  df <- length(samples[[i]]) - 1
  SE <- 1 / sqrt(length(samples[[i]]))
  t <- qt(.975, df) * SE   
  results[[i]] <- (c(M, M - 0, SE, M - t, M + t))
}

# Summary of the 100 samples:
stats <- data.frame(matrix(unlist(results), nrow = length(results), byrow = T))
colnames(stats) <- c("x_bar", "bias", "SE", "lower", "upper")
colMeans(stats)

# c. Create a plot that demonstrates the following:

# “A replication of the procedure that generates a 95% confidence interval that is centered around the sample mean would cover the population value *at least* 95 out of 100 times” (Neyman, 1934)
library(magrittr)
stats %<>% mutate(covered = lower < 0 & 0 < upper)
library(ggplot2)
stats %>% 
  ggplot(aes(y = x_bar, x = 1:100, colour = covered)) + 
  geom_point() +
  geom_pointrange(aes(ymax = stats$upper, ymin = stats$lower)) +
  geom_hline(aes(yintercept = 0), color = "black", size = 1) +
  scale_color_manual(values = c("#FF0000", "#33CC00"))

# d. Present a table containing all simulated samples for which the resulting confidence interval does not contain the population value.

table(stats[stats$covered == F, ]) 
