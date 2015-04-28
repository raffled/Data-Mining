###################################################################
#####################  Assignment 5a: Splines  ####################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 

###### Set up the data
set.seed(100)
x <- runif(150, 0, 2*pi)
xgrid <- seq(0, 2*pi, length=500)
n <- length(xgrid)
ygrid <- vector(length=n)

###### Implement basis function, knots, and generate response.
f <- function(x) sin(2*x) + log(exp(x)+2)
psi1 <- psi1 <- 2.1
psi2 <- psi2 <- 4.15
y <- f(x) + rnorm(150, , 0.1)

#### Compute Hat Matrix (get H)
get.h <- function(x, mod){
    switch(mod,
           ##Linear (intercpt)
           "Linear Constant" = {
               h1 <- function(x) ifelse(x < psi1, 1, 0)
               h2 <- function(x) ifelse(x > psi1 & x < psi2, 1, 0)
               h3 <- function(x) ifelse(x > psi2, 1, 0)
               H <- cbind(h1(x), h2(x), h3(x))
           },
           ## Linear (discontinuous)
           "Linear Discontinuous" = {
               h1 <- function(x) ifelse(x < psi1, 1, 0)
               h2 <- function(x) ifelse(x > psi1 & x < psi2, 1, 0)
               h3 <- function(x) ifelse(x > psi2, 1, 0)
               H <- cbind(h1(x), h2(x), h3(x), x*h1(x), x*h2(x), x*h3(x))
           },
           ## Linear (continuous)
           "Linear Continuous" = {
               h3 <- function(x) (x - psi1) * ifelse(x >= psi1, 1, 0)
               h4 <- function(x) (x - psi2) * ifelse(x >= psi2, 1, 0)
               H <- cbind(1, x, h3(x), h4(x))
           },
           ##Quadratic (continuous)
           Quadratic = {
               h4 <- function(x) (x - psi1)^2 * ifelse(x >= psi1, 1, 0)
               h5 <- function(x) (x - psi2)^2 * ifelse(x >= psi2, 1, 0)
               H <- cbind(1, x, x^2, h4(x), h5(x))
           },
           ##Cubic (continuous)
           Cubic = {
               h5 <- function(x) (x - psi1)^3 * ifelse(x >= psi1, 1, 0)
               h6 <- function(x) (x - psi2)^3 * ifelse(x >= psi2, 1, 0)
               H <- cbind(1, x, x^2, x^3, h5(x), h6(x))
           }, stop(paste("Invalid spline choice: \"", mod, "\"", sep=""))
    )
    return(H)
}

## plot function
plot.pred <- function(ygrid, mod){
    ## prep plot
    plot(x, y, pch=16, main=mod)
    abline(v=c(psi1, psi2))
    lines(xgrid, f(xgrid), col=c("blue"))
    #### plot preds
    ## [0, psi1]
    ind <- xgrid < psi1
    lines(xgrid[ind], ygrid[ind], col=c("red"))
    ## (psi1, psi2]
    ind <- xgrid > psi1 & xgrid < psi2
    lines(xgrid[ind], ygrid[ind], col=c("red"))
    ## (psi2, inf]
    ind <- xgrid > psi2
    lines(xgrid[ind], ygrid[ind], col=c("red"))
}

mod.vec <- c("Linear Constant", "Linear Discontinuous", "Linear Continuous",
             "Quadratic", "Cubic")

## predict and plot
pdf("assorted_splines.pdf")
for(mod in mod.vec){
    ## get H & BLS
    H <- get.h(x, mod)
    bls <- solve(t(H) %*% H, t(H) %*% y)
    ## predict
    ygrid <- as.vector(get.h(xgrid, mod) %*% bls)
    plot.pred(ygrid, mod)
}
dev.off()

