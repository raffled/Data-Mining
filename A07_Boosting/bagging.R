###################################################################
####################  Assignment 7b: Boosting  ####################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 

library(rpart); library(faraway)
data(pima)
head(pima)
pima$test <- 2*pima$test - 1

pdf("bagging_boosting.pdf", width=768/72, height=768/72)
#### Bagging.
n <- nrow(pima)/2
test <- pima[-c(1:n),]
train <- pima[(1:n),]

err <- matrix(ncol=2, nrow=100)
p.train <- p.test <- matrix(0, nrow=n, ncol=100)

for(i in 1:100){
    PI <- sample(1:n, n, replace=TRUE)
    foo <- rpart(test ~ ., train[PI,])
    ## get training & testing p.hats for current tree
    p.train[,i] <- predict(foo, newdata=train)
    p.test[,i] <- predict(foo, newdata=test)

    ## get preds.
    y.tr <- sign(rowSums(p.train)/i)
    y.te <- sign(rowSums(p.test)/i)

    #### get error rates (train, test)
    err[i,] <- c(table(y.tr*train$test)[1]/n, table(y.te*test$test)[1]/n)
}

### plot
matplot(err, type="l", col=c("blue", "red"), main="Bagging Error by Number of Trees",
        ylab="Error Rate", xlab="ntrees")
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1:2)

##### Boost it. (and also learning rate).
boost <- function(v, N, M){
    f <- f2 <- rep(0, n)
    err.boost <- matrix(nrow=N, ncol=2)
    for(m in 1:N){
        ## get weights
        w <- abs(train$test - f)
        w <- w/sum(w)

        ## get h(x) w/ weights
        g <- rpart(test~., train, w, control=rpart.control(cp=-1, minsplit=M, maxdepth=M))
        h.tr <- predict(g)
        h.te <- predict(g, newdata=test)

        ## line search for beta hat
        b.hat <- solve(t(h.tr) %*% h.tr) %*% t(h.tr) %*% (train$test - f)

        ## update
        f <- f + v*b.hat*h.tr
        f2 <- f2 + v*b.hat*h.te

        ## get classifications for this tree
        y.tr <- ifelse(f < 0, -1, 1)
        y.te <- ifelse(f2 < 0, -1, 1)
        m.tr <- y.tr*train$test
        m.te <- y.te*test$test

        ## get error for this tree
        err.boost[m,] <- c(length(m.tr[m.tr==-1])/n, length(m.tr[m.te==-1])/n)
    }
    ## Title.  LS or w/ learning rate?
    titl <- ifelse(v == 1, paste("LS Boosting:, ntree = ", N, ", M = ", M, sep=""),
                   paste("Boosting: v=", v, ", ntree = ", N, ", M = ", M, sep=""))
    ## Plot.
    matplot(err.boost, type="l", col=c("blue", "red"), main=titl, ylab="Error Rate",
            xlab="Iteration")
    ## Legend.
    legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1:2)
}

## Least Squares Boosting
par(mfrow=c(1,1))
boost(1, 500, 3)

## Jumping through v
par(mfrow=c(2,2))
sapply(c(0.05, 0.1, 0.5, 0.75), function(v)  boost(v, 500, 1))
sapply(c(0.05, 0.1, 0.5, 0.75), function(v)  boost(v, 500, 2))
sapply(c(0.05, 0.1, 0.5, 0.75), function(v)  boost(v, 500, 3))
sapply(c(0.05, 0.1, 0.5, 0.75), function(v)  boost(v, 500, 5))
dev.off()

