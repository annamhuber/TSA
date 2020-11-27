### Do it yourself! - MPT ###

# Loading required package
#install.packages("quantmod")
library(quantmod)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
        getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}

colnames(data) <- c("Stocks", "Bonds")

log_Returns <- na.omit(diff(log(data)))
risk_free_rate <-0


# a) Calculating the Sharpe-ratios for bonds and stocks
# I) Expected returns
er_bonds <- mean(log_Returns$Bonds)
er_stocks <- mean(log_Returns$Stocks)

# II) Standard deviations
sd_bonds <- sd(log_Returns$Bonds)
sd_stocks <- sd(log_Returns$Stocks)

# III) Sharpe-rations
sr_bonds <- (er_bonds-risk_free_rate)/sd_bonds
sr_stocks <- (er_stocks-risk_free_rate)/sd_stocks


# b) Plotting efficient frontier
# Creating 1000 portfolio weights and calculating the correlation between stocks and bonds returns
x_weights <- seq(from = 0, to = 1, length.out = 1000)
cor_bs <- cor(log_Returns$Bonds, log_Returns$Stocks)

# Creating a data.frame that contains the weights for the two asset and empty columns for the portfolio return, standard deviation and Sharpe-rations
pf <- data.frame(w_bonds = x_weights, w_stocks = 1 - x_weights, er_p=NA, sd_p=NA, sr_p=NA)

# Calculating the expected returns and standard deviations for the 1000 portfolios
for(i in 1:nrow(pf)){
  pf$er_p[i] <- pf$w_bonds[i] * er_bonds + pf$w_stocks[i] * er_stocks # Formula for calculating portfolio returns
  pf$sd_p[i] <- sqrt(pf$w_bonds[i]^2 * sd_bonds^2 + 
                  pf$w_stocks[i]^2 * sd_stocks^2 + 
                    2 * pf$w_bonds[i] * pf$w_stocks[i] * sd_bonds * sd_stocks* cor_bs) # Formula for calculating portfolio standard deviation
}

# Plotting the efficient frontier
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio")
grid()


# c) Deriving the weightings of the market portfolio, i.e. the one that maximizes the Sharpe-ratio
## Identifying the market portfolio
# Calculating the Sharpe-ratio per portfolio
pf$sr_p <- (pf$er_p-risk_free_rate)/pf$sd_p

# Identifying the index of the market portfolio, i.e. the row number of the Sharpe-ratio-maximizing portfolio
indx_mp <- which.max(pf$sr_p)

# Identifying the weightings of the market portfolio
weightings <- cbind(pf$w_stocks[indx_mp], pf$w_bonds[indx_mp])
colnames(weightings) <- c("Stocks", "Bonds")
pie(weightings, labels = paste(round(weightings*100), "% ", colnames(weightings),sep = ""), main = "Asset allocation of market portfolio")


# d) Extracting the Sharpe-ratio of the market portfolio
sr_mp <- pf$sr_p[indx_mp]
cbind(sr_stocks, sr_bonds, sr_mp)
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio", main="Efficient frontier")
abline(a=risk_free_rate, b=sr_mp, lty=2, col="red")
grid()


### Alternative proceeding using the package fPortfolio
## Package loading, data import
# install.packages("fPortfolio")
# install.packages("quantmod")
library(fPortfolio)
library(quantmod)

# Downloading Prices via Yahoo Finance API
portfolio_index <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  portfolio_index <- cbind(portfolio_index,
                           getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}

colnames(portfolio_index) <- c("Stocks", "Bonds")

# Calculating log-returns and definition of risk-free rate
log_Returns<-diff(log(portfolio_index))[-1,]
risk_free_rate<-0

# Weekly Sharpe-Ratios of Stocks and Bonds
SR_Bonds<-(mean(log_Returns$Bonds)-risk_free_rate)/sd(log_Returns$Bonds)
SR_Stocks<-(mean(log_Returns$Stocks)-risk_free_rate)/sd(log_Returns$Stocks)

# Efficient Frontier
Spec<-portfolioSpec() # Creating variable for the following portfolio optimization specification
setRiskFreeRate(Spec)<-risk_free_rate # Defining risk-free rate
setTargetRisk(Spec)<-"Sigma" # Defining target risk to be optimized, here Sigma = Standard deviation
setNFrontierPoints(Spec)<-1000  # Defining number of efficient portfolios to be constructed

# Definition of constraints
constraint1<-"maxW[]<=1" # Constraint that the maximum weighting of a single asset is equal or smaller than 100%
constraint2<-"LongOnly" # Constraint that investors can't take short positions
constraint3<-"maxsumW[]=1" # Constraint that the sum of all weightings needs to equal 100% 

# Calculating and plotting efficient frontier and tangency line
pf<-portfolioFrontier(as.timeSeries(log_Returns), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
frontierPlot(pf, return="mean", risk="Sigma")
grid()
tangencyLines(pf,col="blue")

# Alternatively: Shortcut
tailoredFrontierPlot(pf, return="mean", risk="Sigma", sharpeRatio = FALSE, twoAssets = TRUE)

# Deriving tangency portfolio (TP) and corresponding weights of individual assets included
tp<-tangencyPortfolio(as.timeSeries(log_Returns), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
weightsPie(tp)

# Sharpe-Ratio of tangency portfolio
SR_Tangency<-(getPortfolio(tp)$targetReturn[1]-risk_free_rate)/getPortfolio(tp)$targetRisk[1]
names(SR_Tangency)<-"Sharpe-Ratio TP"
SR_Tangency

