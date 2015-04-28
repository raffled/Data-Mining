###################################################################
#################  Assignment 7a: Regression Tree  ################
###################################################################
#####  Doug Raffle
#####  Fall 2014
#####  STAT 745 - Data Mining 

library(rpart)
library(faraway)

### Get data, subset, and get rid of NAs
data(airquality)
dat <- airquality[,1:4]
dat$Ozone <- dat$Ozone^(1/3)
dat <- na.omit(dat)
rownames(dat) <- 1:nrow(dat)
head(dat)

#### for verification
(tree.r <- rpart(Ozone ~ ., dat, control=rpart.control(cp=-1, minsplit=3, maxdepth=3)))
plot(tree.r); text(tree.r)

######### Fun stuff
## function to calculate best split points given
## one variable and y
get.splits <- function(x, y){
    inds <- order(x)
    y <- y[inds]; x <- x[inds]
    s.mat <- t(sapply(1:(length(x) - 1), function(i){
        split.p <- median(c(x[i], x[i+1]))
        l.y <- y[x < split.p]
        l.err <- sum((l.y - mean(l.y))^2)
        r.y <- y[x >= split.p]
        r.err <- sum((r.y - mean(r.y))^2)
        t.err <- r.err+l.err
        cbind(split.p, t.err)
    }))
    (split.p <- s.mat[which.min(s.mat[,2]), ])
}

## find one tree.
my.tree <- function(form, dat, m){
    ## pull out response & predictors.  keep 'em separate
    ## since we operate on 'em separately
    y <- model.frame(form, dat)[,1]
    x <- model.frame(form, dat)[,-1]

    ## set up the initial tree data.frame w/ root node.
    tree <- data.frame(node="root", N_m=nrow(dat), Parent="",
                       Error=sum((y-mean(y))^2), Y.hat=mean(y),
                       stringsAsFactors=F)

    ## recursive function to get (sub)tree(s)
    get.tree <- function(form, dat, m, parent="root"){
        ## pull out response & predictors.  keep 'em separate since we operate on 'em separately
        y <- model.frame(form, dat)[,1]
        x <- model.frame(form, dat)[,-1]

        ## if we need to split, do it.  If not we have a terminal node & we're done.
        if(nrow(x) > 2){
            ## find the best splits for each var & select the best var.  Keep name for ref.
            split.mat <- apply(x, 2, function(x) get.splits(y, x))
            best.split <- split.mat[, which.min(split.mat[2,])]
            s.var <- colnames(split.mat)[which.min(split.mat[2,])]

            ## subset data into left & right-hand regions
            inds <- x[,s.var] < best.split[1]

            ### left side
            left.y <- y[inds]
            left.hat <- mean(left.y)
            left.rse <- sum((left.y - left.hat)^2)
            ## Add left node to data.frame
            tree <<- rbind(tree, c(paste(s.var, "<", best.split[1]), length(left.y), parent,
                                   left.rse, left.hat))
            ## down the rabbit hole on the left
            if(m-1 > 0){
                get.tree(form, dat[inds, ], m-1, paste(s.var, "<", best.split[1]))
            }

            ### right side
            right.y <- y[!inds]
            right.hat <- mean(right.y)
            right.rse <- sum((right.y - right.hat)^2)
            ## add right side to data.frame
            tree <<- rbind(tree, c(paste(s.var, ">=", best.split[1]), length(right.y),
                                   parent, right.rse, right.hat))
            ## annnddddd down the other rabit hole.
            if(m-1 > 0){
                get.tree(form, dat[!inds, ], m-1, paste(s.var, ">=", best.split[1]))
            }
        }
    }

    get.tree(form, dat, m, "root")
    tree[,4:5] <- apply(tree[,4:5], 2, function(col) round(as.numeric(col), 5))

    ## give it a class for better printing.
    class(tree) <- c("mytree", "data.frame")
    return(tree)
}

print.mytree <- function(tree){
    ## tell 'em what to expect
    cat("\nnode number) Split  N_m  Error  Y.hat  (* if terminal)\n\n")

    ## climb down the tree to find distance of node to root
    node.dist <- function(node){
        if(node$node == "root") d <- 0
        else{
            parent <- tree[tree$node == node$Parent,]
            d <- 1 + node.dist(parent)
        }
        return(d)
    }

    ## find the distance from each node to root
    root.dist <- sapply(1:nrow(tree), function(i)
        node.dist(tree[i,]))

    ## create a col. to distinguish terminal nodes
    tree$term <- ifelse(sapply(tree$node, function(node)
        !(node %in% tree$Parent)), "*", "")

    ## number nodes based on level
    node.nums <- rank(root.dist, ties.method="first")

    ##pretty hierarchical printing of tree
    cat("1)", tree[1,]$node, tree[1,]$N_m, tree[1,]$Error, tree[1,]$Y.hat, "\n")
    invisible(sapply(2:nrow(tree), function(i){
        cat(rep("\t", root.dist[i]), paste(node.nums[i], ")", sep=""),
            tree[i,]$node, tree[i,]$N_m, tree[i,]$Error,
            tree[i,]$Y.hat, tree$term[i], "\n")
    }))
}

tree.r
(tree <- my.tree(Ozone~., dat, 3))

