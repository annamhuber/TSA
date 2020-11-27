### Do it yourself! - Linear Regression ###

# Loading required packages
#install.packages("quantmod")
#install.packages("lmtest")
#install.packages("tseries")
library(quantmod)
library(lmtest)
library(tseries)

# Downloading required data via Yahoo Finance API
# "^GSPC" is the ticker for the S&P500
# "^TNX" is the ticker for the 10-year US Treasury Yields
data <- NULL
tickers_index <- c("^GSPC", "^TNX")

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="1970-01-01", to="2020-11-25", periodicity = "monthly",
                                 auto.assign=FALSE)[,6])
}

colnames(data)<-c("S&P 500", "Treasury Yield 10 Years")

# a) Testing for stationarity and creating stationary series
adf.test(data$`S&P 500`)
adf.test(data$`Treasury Yield 10 Years`)
plot(data$`S&P 500`, main="S&P 500")
plot(data$`Treasury Yield 10 Years`, main="Treasury Yield 10 Years")

returns <- na.omit(diff(log(data)))
plot(returns)
addLegend(legend.loc="bottomleft", legend.names=colnames(returns), 
               lty = 1, col=1:ncol(returns), text.col=1:nrow(returns), bg="white", bty=1)
adf.test(returns$`S&P 500`)
adf.test(returns$`Treasury Yield 10 Years`)

# b) Running the linear regression
lin_reg <- lm(`S&P 500` ~ `Treasury Yield 10 Years`, data = returns)
coefficients <- lin_reg$coefficients  
coefficients

# c) Testing for significance of the regression coefficients
coeftest(lin_reg)

# d) Deriving R-squared
summary(lin_reg)$r.squared 

# e) Calculating and plotting the regression residuals
resid <- returns$`S&P 500` - coefficients[1] - coefficients[2]*returns$`Treasury Yield 10 Years` 
resid <- lin_reg$residuals
plot(y=resid, x=as.Date(time(returns)), ylab="Residuals", xlab="Year", type="l", main="Regression residuals") +
  grid()
