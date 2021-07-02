library(quantmod)
library(ggplot2)
library(finreportr)
library(DT)

risky <- c("TSLA", "NIO", "PLTR", "BIDU", "NVDA", "AMD", "SQ", "SHOP", "FB", "MARA", "BAC")
med <- c("PYPL", "OGIG", "AAL", "JETS", "XOP", "GS", "WMT", "AAPL", "MSFT", "ARES", "PM")
small <- c("SPY", "VOO", "QQQ")

curr_date <- Sys.Date()
new_date <- as.POSIXlt(curr_date)
new_date$year <- new_date$year - 1

# Creates a vector portfolio of recommended stocks
createStockVector <- function(num_risky, num_med, num_small) {
    high_risk <- sample(1:11, num_risky, replace=FALSE)
    med_risk <- sample(1:11, num_med, replace=FALSE)
    small_risk <- sample(1:3, num_small, replace=FALSE)
    stocks <- vector(mode='character', length=num_risky+num_med+num_small)
    
    # Assigning Stocks to index in Vector
    for (i in 1:num_risky) {
        stocks[i] = risky[high_risk[i]]
    }
    index = num_risky + 1
    for (k in index:(index + num_med)) {
        stocks[k] = med[med_risk[k - index + 1]]
    }
    index = num_risky + num_med + 1
    for (j in index:(index + num_small - 1)) {
        stocks[j] = small[small_risk[j - index + 1]]
    }
    
    stocks
}

# Creates a vector of current prices of the stock portfolio
createPriceVector <- function(stocks) {
    price <- vector(mode='numeric', length=length(stocks))
    for (i in 1:length(stocks)) {
        ticker <- stocks[i]
        curr_stock <- getSymbols(ticker, auto.assign = F, from = new_date, return.class = 'data.frame')
        curr_stock <- tail(curr_stock, 1)[,4]
        price[i] = curr_stock
    }
    price
}

# Converts digits to percent format.
percent <- function(x, digits = 2, format = "f", ...) {  
    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

# Creates a vector for yearly returns for each security in portfolio
createReturnsVector <- function(stocks) {
    returns <- vector(mode='numeric', length = length(stocks))
    for (i in 1:length(stocks)) {
        stock <- getSymbols(stocks[i], auto.assign = F, from = new_date)[,4]
        return = periodReturn(stock, period="yearly", type="log")[2, 1]
        returns[i] = return
    }
    returns <- percent(returns)
    returns
}

# Creates a dataframe of securities, current prices, and yearly returns.
createTable <- function(num_risky, num_med, num_small) { 
    if (num_risky == 0 && num_med == 0 && num_small == 0) {
        df <- data.frame(numeric(), numeric(), numeric())
    } else {
        stocks <- createStockVector(num_risky, num_med, num_small)
        prices <- createPriceVector(stocks)
        returns <- createReturnsVector(stocks)
        df <- data.frame(stocks, prices, returns)
    }
    colnames(df) <- c('Stocks', 'Current Price', '1-Year Returns')
    df
}

# Graph Stock data
graphStock <- function(stock) {
    data <- getSymbols(stock, src="yahoo", auto.assign = F, from = as.Date(new_date))
    ggplot(data, aes(x = as.Date(index(data)), y = data[,4])) + 
        geom_line(color = "darkblue") + ggtitle("1-Year Stock prices") + 
        xlab("Date") + ylab("Price") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_date(date_labels = "%b %y", date_breaks = "3 months")
}

# create the stock portfolio recommendations curated out of the 
# Risky, medium-risk, and low-risk securities.
createRec <- function(risk, length, retire, invested, income, bonus) {
    risky = 0
    med = 0
    low = 0
    
    # Total bucket of risk
    retire <- retire / 10
    invested <- invested / 100000
    income <- income / 1000000
    bonus <- bonus / 1000000
    risk <- risk * 5
    total = retire + invested + income + bonus + length + risk
    
    # Algorithm to choose
    if (risk == 0 && length == 0 && retire == 0 && invested == 0 &&
        income == 0 && bonus == 0) {
        risky = 0
        med = 0
        low = 0
        print("hi")
    } else if (total >= 70) {
        risky = 9
        med = 1
    } else if (total > 40) {
        risky = 7
        med = 1
    } else if (total > 30) {
        risky = 5
        med = 4
        low = 1
    }else if (total > 20) {
        risky = 2
        med = 5
        low = 3
    } else if (total > 10) {
        med = 3
        low = 3
    } else {
        low = 3
    }
    
    df <- createTable(risky, med, low)
}