###################################################################
####################  Assignment 11: AdaBoost  ####################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 

library(rpart)

## set up data
x <- matrix(rnorm(2000*10), , 10)
dat <- data.frame(y=sign(rowSums(x^2) - 9.34), x)
n <- nrow(dat)

test.x <- matrix(rnorm(5000*10), , 10)
test.dat <- data.frame(y=sign(rowSums(test.x^2) - 9.34), test.x)

## init
w <- rep(1/n, n)
M <- 400
f <- rep(0, 2000)
f.test <- rep(0, 5000)
err.mat <- matrix(nrow=M, ncol=2)
for(m in 1:M){
    ## fit the model
    h.m <- rpart(y ~ ., dat, w, control=rpart.control(cp=-1, minsplit=1, maxdepth=1))
    y.hat <- sign(predict(h.m))
    err <- sum(w * ifelse(y.hat == dat$y, 0, 1))/sum(w)
    beta.m <- log((1-err)/(err))
    w <- w*exp(beta.m * ifelse(y.hat == dat$y, 0, 1))

    f <- f + beta.m * y.hat
    f.test <- f.test + beta.m * sign(predict(h.m, newdata=test.dat))

    err.mat[m,] <- c(table(sign(f) * dat$y)[1]/2000,
                     table(sign(f.test) * test.dat$y)[1]/5000)

}

h6 <- rpart(y ~ ., dat, control=rpart.control(cp=-1, minsplit=6, maxdepth=6))
f.6 <- sign(predict(h6))
f.test.6 <- sign(predict(h6, newdata=test.dat))
err.train.6 <- table(f.6 * dat$y)["-1"]/2000
err.test.6 <- table(f.test.6 * test.dat$y)["-1"]/5000

matplot(err.mat, type="l", col=c("blue", "red"),
        xlab="Iteration", ylab="Error Rate")
abline(h=err.train.6, col="blue", lty=3)
abline(h=err.test.6, col="red", lty=4)
legend("topright", legend=c("Training", "Testing", "6-Split Train",
                            "6-Split Test"), col=c("blue", "red"), lty=1:4)
