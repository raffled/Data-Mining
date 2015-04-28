###################################################################
#################  Assignment 5b: Natural Splines  ################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 

######
## f(x) Regression Example
######
#### Create function to find greater than region for basis functions
tpls <- function(x, psi, tau){
    v <- (x - psi)^tau
    v * as.numeric(v > 0)
}

#### Create basis function
dk<-function(x, psi.k, tau){
    (tpls(x, psi.k, tau) - tpls(x, max(psi), tau))/(bpsi[2] - psi.k)
}


#### function to make predictions and generate plots depending on the
#### whether to predict past end knots
natural.splines <- function(first){
    #### Generate the data, knots, etc.
    set.seed(100)
    x <- runif(150,0,2*pi)
    f <- function(x){sin(2*x)+log(exp(x)+2)}
    y <- f(x) + rnorm(150,,0.1)
    psi <- c(1, 2.1, 4.15, 5.5)
    bpsi <- range(x)

    ifelse(first, xgrid <- seq(0, 2*pi, length=500),
                  xgrid <- seq(-pi/2, 5*pi/2, length=500))

    n <- length(xgrid)
    ygrid <- vector(length=n)

    ## Compute BS spline
    h <- cbind(1, x, x^2, x^3)
    h0 <- cbind(1, xgrid, xgrid^2, xgrid^3)
    for(i in 1:length(psi)){
        h <- cbind(h, tpls(x, psi[i], 3))
        h0 <- cbind(h0, tpls(xgrid, psi[i], 3))
    }

    bls <- solve(t(h)%*%h, t(h)%*%y)
    ygrid1 <- as.vector(h0%*%bls)

    ## Compute NS spline to get ygrid2
    psi <- c(bpsi[1], psi, bpsi[2])
    K <- length(psi)
    N <- cbind(1, x)
    n0 <- cbind(1, xgrid)
    for(k in 1:(K-2)){
        N <- cbind(N, dk(x, psi[k], 3) - dk(x, psi[5], 3))
        n0 <- cbind(n0, dk(xgrid, psi[k], 3) - dk(xgrid, psi[5], 3))
    }
    bls.n <- solve(t(N) %*% N, t(N) %*% y)
    ygrid2 <- as.vector(n0 %*% bls.n)

    ## Plot Result
    ifelse(first, m <- "[0, 2pi]", m <- "[-pi/2, 5pi/2]")
    plot(xgrid, ygrid1, type="n", pch=16, ylim=range(c(ygrid1, ygrid2, y)), xlim=range(c(xgrid, x)),
         main=m)
    abline(v=psi, lty=2, col="gray")
    lines(xgrid, f(xgrid), col=c("blue"))
    points(x, y, pch=20)
    lines(xgrid, ygrid1, col="red")
    lines(xgrid, ygrid2, col="orange", lwd=2)
}

pdf("natural_splines.pdf")
natural.splines(first = TRUE)
natural.splines(first = FALSE)
dev.off()
