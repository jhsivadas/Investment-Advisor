library(quantmod)
library(ggplot2)
library(finreportr)
library(dplyr) 

# Get the return data on stock portfolio.
getStocksInformation <-function(names) {
    Symbols <- names
    # create environment to load data into
    Data <- new.env()
    getSymbols(Symbols, from="2016-01-01", env=Data)    
    # calculate returns, merge, and create data.frame (eapply loops over all
    # objects in an environment, applies a function, and returns a list)
    Returns <- eapply(Data, function(s) ROC(Ad(s), type="discrete"))
    ReturnsDF <- as.data.frame(do.call(merge, Returns))
    # adjust column names are re-order columns
    colnames(ReturnsDF) <- gsub(".Adjusted","",colnames(ReturnsDF))
    ReturnsDF <- ReturnsDF[,Symbols]
    ReturnsDF <- ReturnsDF[complete.cases(ReturnsDF),]
    ReturnsDF
}

# Get the covariance matrix
getCov <- function(names) {
    stocks <- getStocksInformation(names)
    covs <- cov(stocks, y=stocks, use="all.obs")
    covs <- data.matrix(covs, rownames.force = NA)
}

# Getting the weights for each individual security in portfolio given the 
# Mean-Variance Analysis.
getWeights <- function(names) {
    Sigma <- getCov(names)
    Sigma <- diag(diag(Sigma))
    Sigma_inv = solve(Sigma)
    mu <- colMeans(getStocksInformation(names))
    weights <- Sigma_inv %*% mu
    weights <- weights / sum(weights)
    weights <- as.data.frame(weights)
    names(weights) <- (c("Weights"))
    row.names(weights) <- names(getStocksInformation(names))
    weights
}


# Mean, Volatility, and Sharpe Ratio of Portfolio
getMeanReturn <- function(names) {
    df <- getWeights(names)
    mu <- colMeans(getStocksInformation(names))
    mean_return <- t(as.matrix(mu)[,1]) %*% t(as.matrix(df)[,1])
    mean_return[1, 1]
}

getVol <- function(names) {
    Sigma <- getCov(names)
    Sigma <- diag(diag(Sigma))
    Sigma_inv = solve(Sigma)
    mu <- colMeans(getStocksInformation(names))
    vol <- sqrt(t(mu) %*% Sigma_inv %*% mu)
    vol[1, 1]
}

getSharpe <- function(names) {
    mean <- getMeanReturn(names)
    vol <- getVol(names)
    sharpe <- mean / vol
    sharpe
}

getInfo <- function(names) {
    vol <- getVol(names)
    mean <- getMeanReturn(names)
    sharpe <- getSharpe(names)
    info <- c(vol, mean, sharpe)
    df <- data.frame(info)
    print(df)
}
