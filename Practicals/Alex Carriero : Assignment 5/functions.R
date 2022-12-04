# Functions for Quarto Presentation 

scenario <- function(npred, ev = 0.5 , n.level = 1, dmu = 0.5){
  # this function takes each factor to be varied in simulation as input
  # and generates the mean and covariance matrices for each class.
  
  # it returns all information required for the data generating function: 
  # number of predictors, event fraction, n.level, sample size, 
  # and the mean and covariance matrices of each class. 
  
  # set up
  x      <- dmu                              # delta mu
  y      <- 0.5                              # delta sigma 
  z      <- 0.2                              # z 
  p      <- 0.8                              # proportion of correlated predictors
  npred  <- npred                            # number of predictors
  n      <- 10000                            # sample size calculation
  
  # set up correlations
  corr0  <-  matrix(0, npred, npred)         # matrix: set up for cov matrix, 0 on diagonals
  corr0[1:npred*p, 1:npred*p] = z            # class 0 
  diag(corr0) = 0
  
  corr1  <-  matrix(0, npred, npred)         # matrix: set up for cov matrix, 0 on diagonals
  corr1[1:npred*p, 1:npred*p] = (1-y)*z      # class 1
  diag(corr1) = 0
  
  # mean structures
  mu0    <-  c(rep(0,npred))                 # vector: class 0 means
  dmu    <-  c(rep(x,npred))                 # vector: difference in means between classes 
  mu1    <-  mu0 - dmu                       # vector: class 1 means 
  
  # covariance structures
  sigma0 <-  diag(npred)  + corr0            # matrix: cov matrix of class 0 
  dsig   <-  diag(c(rep(y, npred)))          # matrix: difference in variances between classes 
  sigma1 <-  diag(npred) - dsig + corr1      # matrix: cov matrix of class 1
  
  return(list(npred, ev, n.level, n, mu0, mu1, sigma0, sigma1))
}



generate_data <- function(inn){
  # this function generates one data set with the specifications 
  # given by the input scenario.
  
  npred   = inn[[1]]
  ev      = inn[[2]]
  n.level = inn[[3]]
  n       = inn[[4]]
  mu0     = inn[[5]]
  mu1     = inn[[6]]
  sigma0  = inn[[7]]
  sigma1  = inn[[8]]
  
  # positive class
  n1      <- rbinom(1, n, ev)
  class_1 <- mvrnorm(n1, mu1, sigma1)
  
  # negative class
  n0      <- n - n1
  class_0 <- mvrnorm(n0, mu0, sigma0)
  
  outcome <- c(rep(1, n1), rep(0, n0))
  
  # format data frame
  df <- cbind(rbind(class_1, class_0), outcome)%>% 
    as.data.frame() %>% 
    mutate(outcome = as.factor(outcome))
  
  return(df)
}


visualize <- function(df){
  df <- df 
  df %>%
    arrange(outcome)%>%
    ggplot(aes(x = V1, y = V2, col = outcome)) + 
    geom_point(alpha = 0.7) + 
    theme_minimal() + 
    ggtitle("Imbalanced Data:") + 
    scale_color_manual("Class", values = c("goldenrod1", "darkblue")) + 
    xlab("Predictor 1") +
    ylab("Predictor 2")
}