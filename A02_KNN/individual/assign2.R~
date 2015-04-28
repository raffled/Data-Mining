bls <- function(x0,x,y){
    x0 %*% (solve(t(x) %*% x) %*% t(x) %*% y)
}

knn <- function(x0, x, y, k){
    dis <- apply(as.matrix(x), 1, function(r) sum((x0 - r)^2))
    mean(y[head(order(dis), k)])
}

###### set up clusters
library(parallel)

###################################################
############ sin(x) Regression Example ############
###################################################
BLS_Flag=TRUE

set.seed(100)
x=runif(100,0,2*pi)
y=sin(x)+rnorm(100,,0.1)

xgrid=seq(0,2*pi,length=500)
n=length(xgrid)
ygrid=vector(length=n)
k=15

cl.glob <- makeCluster(detectCores())
clusterExport(cl.glob, varlist=c(ls(), ls(2)))
system.time(
    ygrid <- parSapply(cl.glob, 1:n, function(i){
        ifelse(BLS_Flag,
               bls(xgrid[i], x, y),
               knn(xgrid[i], x, y, k)
        )
    })
)
stopCluster(cl.glob)


## system.time(
##     for(i in 1:n){
##         if(BLS_Flag){
##             zgrid[i] <- bls(xgrid[i], x, y)
##         } else{
##             zgrid[i] <- knn(xgrid[i], x, y, k)
##         }
##     }
## )

#png("sinx_knn.png")
plot(x,y,pch=16, main="K-NN sin(x)")
lines(xgrid,ygrid,col=c("red"))
lines(xgrid,sin(xgrid),col=c("blue"))
#dev.off()

BLS_Flag=FALSE

#################################################
################ Simulated Example ##############
#################################################
BLS_Flag=TRUE
x<-read.table("dat_2.txt",F)
y<-x[,3]
x<-x[,-3]
x=as.matrix(x)

xgrid1<-seq(min(x[,1]),max(x[,1]),length=100)
xgrid2<-seq(min(x[,2]),max(x[,2]),length=100)
n=length(xgrid1)
zgrid=matrix(0,n,n)
k=10


zgrid=matrix(0,n,n)

cl.glob <- makeCluster(detectCores())
clusterExport(cl.glob, varlist=c(ls(), ls(2)))
system.time(
    zgrid <- t(parSapply(cl.glob, 1:n, function(i){
        sapply(1:n, function(j){
            ifelse(BLS_Flag,
                   bls(c(xgrid1[i], xgrid2[j]), x, y),
                   knn(c(xgrid1[i], xgrid2[j]), x, y, k)
            )
        })
    }))
)
stopCluster(cl.glob)

## system.time(
##     for(i in 1:n){
##         for(j in 1:n){
##             if(BLS_Flag){
##                 zgrid[i,j] <- bls(c(xgrid1[i], xgrid2[j]), x, y)
##             }else{
##                 zgrid[i,j]=knn(c(xgrid1[i], xgrid2[j]), x, y, k)
##             }
##         }
##     }
## )


#png("sim_knn.png")
plot(x, col=c("orange","blue")[y+1], pch=16, xlab="x1", ylab="x2", main="K-NN Simulation")
for(i in 1:n){
	val<-as.numeric(zgrid[,i] >= 0.5) + 1
	points(xgrid1,rep(xgrid2[i],n), pch = ".", col = c("orange", "blue")[val])
}
contour(x=xgrid1, y=xgrid2, z=zgrid, levels=0.5, add=TRUE, drawlabels=FALSE)
#dev.off()
BLS_Flag=FALSE


