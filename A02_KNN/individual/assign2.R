###################################################################
#######################  Assignment 2: KNN  #######################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 


#### Create Functions to find predictions of BLS and KNN
bls <- function(x0,x,y){
    x0 %*% (solve(t(x) %*% x) %*% t(x) %*% y)
}

knn <- function(x0, x, y, k){
    dis <- apply(as.matrix(x), 1, function(r) sum((x0 - r)^2))
    mean(y[head(order(dis), k)])
}

## Get parallel package to save some time
library(parallel)
## set a seed for reproducibility
set.seed(100)

###################################################################
#################### sin(x) Regression Example ####################
###################################################################
#### Create a function to implement six(x) example
#### for knn and and bls.
sinx <- function(BLS_Flag = TRUE, save.plot = FALSE){
    ## generate the data.
    n <- 500
    x <- runif(100,0,2*pi)
    y <- sin(x)+rnorm(100,,0.1)
    xgrid <- seq(0,2*pi,length=n)
    ygrid <- vector(length=n)
    k=15

    ## Get our predictions
    ygrid <- unlist(mclapply(1:n, function(i){
                 ifelse(BLS_Flag,
                        bls(xgrid[i], x, y),
                        knn(xgrid[i], x, y, k)
                 )
    }))

    ## Get plot names
    plot.name <- ifelse(BLS_Flag, "sinx_bls.png", "sinx_knn.png")
    plot.title <- ifelse(BLS_Flag, "K-NN sin(x)", "LS sin(x)")

    ## Plot the data, conditionally saving to make presentation
    if(save.plot) png(plot.name)
    plot(x,y,pch=16, main=plot.title)
    lines(xgrid,ygrid,col=c("red"))
    lines(xgrid,sin(xgrid),col=c("blue"))
    if(save.plot) dev.off()
}
sinx(TRUE)
sinx(FALSE)


#################################################################
######################## Simulated Example ######################
#################################################################
sim <- function(BLS_Flag = TRUE, save.plot = FALSE){
    ## Generate data
    x <- read.table("dat_2.txt", FALSE)
    y <- x[,3]
    x <- x[,-3]
    x <- as.matrix(x)
    n <- 100
    xgrid1 <- seq(min(x[,1]),max(x[,1]),length=n)
    xgrid2 <- seq(min(x[,2]),max(x[,2]),length=n)
    zgrid <- matrix(0,n,n)
    k <- 10

    ## Get the predictions
    zgrid <- t(matrix(unlist(mclapply(1:n, function(i){
                 sapply(1:n, function(j){
                            ifelse(BLS_Flag,
                                   bls(c(xgrid1[i], xgrid2[j]), x, y),
                                   knn(c(xgrid1[i], xgrid2[j]), x, y, k)
                            )
                 })
    })), n, n))

    ## Plot the data, optionally saving for presentation.
    plot.name <- ifelse(BLS_Flag, "sim_bls.png", "sim_knn.png")
    plot.title <- ifelse(BLS_Flag, "K-NN Simulation", "LS Simulation")
    if(save.plot) png("sim_knn.png")
    plot(x, col=c("orange","blue")[y+1], pch=16, xlab="x1", ylab="x2", main="K-NN Simulation")
    for(i in 1:n){
	val<-as.numeric(zgrid[,i] >= 0.5) + 1
	points(xgrid1,rep(xgrid2[i],n), pch = ".", col = c("orange", "blue")[val])
    }
    contour(x=xgrid1, y=xgrid2, z=zgrid, levels=0.5, add=TRUE, drawlabels=FALSE)
    if(save.plot) dev.off()
}
sim(TRUE)
sim(FALSE)
