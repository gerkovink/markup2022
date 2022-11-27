# mini simulation: 
# test different versions of penalties for delta+ and delta-. The goal of the 
# penalties is to weigh cases that are added to the audit sample more heavily
# then cases that are removed from the audit sample. 

# package for solving constrained minimization problem
library(nloptr)

# set seed for simulation study
set.seed(123)

# specify probabilities for the relationships between W, X, Y and Z
WX <- matrix(c(.9/3, .05/3, .05/3, 
               .1/3, .8/3, .1/3,
               .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)
WY <-  matrix(c(.8/3, .1/3, .1/3, 
                .1/3, .8/3, .1/3,
                .1/3, .1/3, .8/3), nrow = 3, ncol = 3, byrow = TRUE)
XZ <- matrix(c(.97/3, .018,
               .97/3, .010,
               .97/3, .002), nrow = 3, ncol = 2, byrow = TRUE)

# list the number of categories for each variable
X <- c(1,2,3)
Y <- c(1,2,3)
W <- c(1,2,3)
Z <- c(0,1)

# create grid 
variables <- list(X = X, Y = Y, W = W, Z=Z)
XYWZ <- expand.grid(variables)

# find probabilties
for (i in 1:nrow(XYWZ)){
  # if X=... and W=... find the corresponding probability in the matrix WX
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"X"] == 1){XYWZ[i,"WX"] = WX[1,1]}
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"X"] == 2){XYWZ[i,"WX"] = WX[1,2]}
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"X"] == 3){XYWZ[i,"WX"] = WX[1,3]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"X"] == 1){XYWZ[i,"WX"] = WX[2,1]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"X"] == 2){XYWZ[i,"WX"] = WX[2,2]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"X"] == 3){XYWZ[i,"WX"] = WX[2,3]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"X"] == 1){XYWZ[i,"WX"] = WX[3,1]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"X"] == 2){XYWZ[i,"WX"] = WX[3,2]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"X"] == 3){XYWZ[i,"WX"] = WX[3,3]}
  # if X=... and Y=... find the corresponding probability in the matrix WY
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"Y"] == 1){XYWZ[i,"WY"] = WY[1,1]}
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"Y"] == 2){XYWZ[i,"WY"] = WY[1,2]}
  if(XYWZ[i,"W"] == 1 & XYWZ[i,"Y"] == 3){XYWZ[i,"WY"] = WY[1,3]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"Y"] == 1){XYWZ[i,"WY"] = WY[2,1]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"Y"] == 2){XYWZ[i,"WY"] = WY[2,2]}
  if(XYWZ[i,"W"] == 2 & XYWZ[i,"Y"] == 3){XYWZ[i,"WY"] = WY[2,3]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"Y"] == 1){XYWZ[i,"WY"] = WY[3,1]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"Y"] == 2){XYWZ[i,"WY"] = WY[3,2]}
  if(XYWZ[i,"W"] == 3 & XYWZ[i,"Y"] == 3){XYWZ[i,"WY"] = WY[3,3]}
  # If Y=.. and Z=... find the corresponding probability in the matrix WZ
  if(XYWZ[i,"X"] == 1 & XYWZ[i,"Z"] == 0){XYWZ[i,"XZ"] = XZ[1,1]}
  if(XYWZ[i,"X"] == 1 & XYWZ[i,"Z"] == 1){XYWZ[i,"XZ"] = XZ[1,2]}
  if(XYWZ[i,"X"] == 2 & XYWZ[i,"Z"] == 0){XYWZ[i,"XZ"] = XZ[2,1]}
  if(XYWZ[i,"X"] == 2 & XYWZ[i,"Z"] == 1){XYWZ[i,"XZ"] = XZ[2,2]}
  if(XYWZ[i,"X"] == 3 & XYWZ[i,"Z"] == 0){XYWZ[i,"XZ"] = XZ[3,1]}
  if(XYWZ[i,"X"] == 3 & XYWZ[i,"Z"] == 1){XYWZ[i,"XZ"] = XZ[3,2]}
}

# compute final probabilities 
XYWZ[, "prob"] <- (XYWZ[,"WX"]*XYWZ[,"WY"]*XYWZ[,"XZ"])*9

# generate data based on the final probabilities
generated_data <- cbind(XYWZ[,1:4], "freq" = rmultinom(n = 1, size = 10000, 
                                                       prob = XYWZ[, "prob"]))

# the code for the constrained minimization problem

# create tab
tab <- aggregate(generated_data[,5], by = list(X = generated_data$X, 
                                               Y = generated_data$Y, 
                                               Z = generated_data$Z), 
                 FUN  = sum, drop = TRUE)
colnames(tab) = c("X","Y","Z","freq")


# we want a non-linear deviance constraint: the deviance for each model should 
# be below a boundary that is specified by the user via alpha
alpha <- 0.05 
I <- 2 
J <- 2 
# compute the user-specified deviance threshold
bound <- qchisq(p = 1 - alpha, df = J * (I-1), lower.tail = TRUE)

# compute the constant term of the deviance for any solution to the problem 
# (this constant term does not change because it only depends on X and Y, 
# which do not change. Only Z changes)
tot  <- aggregate(tab$freq, by = tab[ , "Y", drop = FALSE], FUN = sum)
rtot <- aggregate(tab$freq, by = tab[ , c("X","Y")], FUN = sum)
cst <- 2 * sum(tot$x * log(tot$x), na.rm=TRUE) - 2 * sum(rtot$x * log(rtot$x), na.rm=TRUE)

# specify function for the non-linear constraint. This is a function of the 
# solution x and the user specified deviance threshold. The deviance for 
# solution x should fall below the threshold
deviance_bound <- function(x, bound) {
  deltaplus <- x[1:(length(x)/2)]
  deltamin <- x[(1+(length(x)/2)):length(x)]
  # compute the new frequency table under solution x
  tab$freq <- tab$freq +
    c(-deltaplus, -deltamin) + 
    c(deltamin, deltaplus)
  # calculate deviance under solution x using new frequency table
  ktot <- aggregate(tab$freq, by = tab[ , c('Y','Z')], FUN = sum)
  G2 <- cst + 2 * sum(tab$freq * log(tab$freq)) -
    2 * sum(ktot$x * log(ktot$x))
  # the constraint should be in the form hin(x) >= 0, therefore we return
  # the threshold - calculated deviance under the solution
  return(bound - G2)
}

# nloptr() function can only be used with numeric, tables not integers
# convert table to numeric:
tab[] <- lapply(tab, as.numeric)

# set varying weights for the delta+ values
plus_weight <- c(1,2,3,4,5,6,7,8,9,10)
# set varying weights for the delta- values
min_weight <- c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10)

# initialize convergence, added units and removed units
convergence <- c()
add_units <- c()
rem_units <- c()

# set the options for the optimization procedure
opts <- nl.opts()
opts$algorithm <- "NLOPT_LD_AUGLAG"
#SLSQP
opts$local_opts <- list("algorithm" = "NLOPT_LD_SLSQP", "eval_grad_f" = NULL, 
                        xtol_rel = 1e-6, "maxeval" = 1000, "tol_constraints_ineq" = 1e-2)
opts$tol_constraints_ineq <- 1e-2
hin <- function(x) deviance_bound(x, bound = bound)
.hin <- match.fun(hin)
hin <- function(x) (-1) * .hin(x)
hinjac <- function(x) nl.jacobian(x, hin)

# perform the procedure for every set of delta weights
for (i in 1:length(plus_weight)){
  sol <- nloptr(
    # specify starting values 
    x0 = c(rep(1, nrow(tab)/2), rep(0, nrow(tab)/2)),
    # specify target function: the sum of delta+ and delta- values
    eval_f = function(x) { plus_weight[i]*sum(x[1:(length(x)/2)]) +
        min_weight[i]*sum(x[(1+(length(x)/2)):length(x)])},
    # specify the derivative of the target function
    eval_grad_f = function(x) { c(rep(plus_weight[i], (nrow(tab)/2)), 
                                  rep(min_weight[i], (nrow(tab)/2))) },
    # specify lower bounds for the solution: the delta values cannot be below 0
    lb = rep(0, nrow(tab)),
    # specify upper bounds for the solution: there can't be more units moved 
    # from Z = 0 to Z = 1 then are initially in Z = 0, and vice versa
    ub = tab$freq[c(which(tab$Z == 0), which(tab$Z == 1))],
    # non-linear constraint on the deviance with the pre-specified function
    eval_g_ineq = hin,
    eval_jac_g_ineq = hinjac,
    # solver to the problem
    # localsolver = "LBFGS", 
    opts = opts)
  # save convergence, added units and removed units for every iteration
  convergence[i] <- sol$status
  add_units[i] <- sum(sol$solution[1:nrow(tab)/2])
  rem_units[i] <- sum(sol$solution[(1+nrow(tab)/2):nrow(tab)])
}

# print the results 
round(cbind(plus_weight, min_weight, convergence, add_units, rem_units), 3)

# from the results, we can conclude that as delta plus increases and delta min
# decreases, the number of added units decreases, and the number of removed units
# increases. This is what we expect to happen. 

# This simulation was done as an intermediate step of my thesis project




