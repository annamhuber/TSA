### Do it yourself! - Value-at-Risk ###

# Loading required package
#install.packages("quantmod")
library(quantmod)

# Downloading levels of SMI ("^SSMI") via Yahoo Finance API
data <- NULL
tickers_index <- c("^SSMI")

for (Ticker in tickers_index){
  data <- cbind(data,
                  getSymbols.yahoo(Ticker, from="2000-01-01", periodicity = "monthly",
                                   auto.assign=FALSE)[,6])
}

plot(data)

# Calculating continuous returns ("stetige Renditen")
SMI_Returns<-diff(log(data))[-1,]
colnames(SMI_Returns)<-"SMI Returns"
plot(SMI_Returns)

# Parameterizing Value-at-Risk
inv_volume <- 1000       # Investment volume
hp <- 1             # Holding period
alpha <- 0.05            # Confidence level

# Calculating historical Value-at-Risk
SMI_Returns_sorted<-sort(as.numeric(SMI_Returns), decreasing=FALSE)

position_quantil<-floor(length(SMI_Returns_sorted)*alpha)
alpha_quantil<-SMI_Returns_sorted[length(SMI_Returns)*alpha]
hvar <- alpha_quantil*inv_volume


ecdf <- 1:length(SMI_Returns_sorted) / length(SMI_Returns_sorted)
plot(x=SMI_Returns_sorted, y=ecdf, xlab="SMI Returns", ylab="ECDF", main = "ECDF of SMI Returns")
abline(v=alpha_quantil, col="red")

# Calculating historical Value-at-Risk
vola <- sd(SMI_Returns)   # Standard deviation of SMI returns
mean <- mean(SMI_Returns)   # Average return of SMI
parvar<-(mean*hp-qnorm(1-alpha,0,1)*vola*sqrt(hp))*inv_volume

# Plotting estimated VaRs
hist(SMI_Returns*inv_volume, breaks=30, xlab = "Profit/Loss per Month (Returns*Investment Volume)", main = "")
abline(v=hvar, col="red")
abline(v=parvar, col="blue")
legend("topleft", legend = c("Historical VaR", "Parametric VaR"), col=c("red","blue"), lty=1)

abs(hvar)-abs(parvar)

#Interpretation: The historcial (parametric) Value-at-Risk indicates that there is a 5% chance of having losses that exceed CHF 71.8553 (CHF 62.348764) over a monthly period. Since the absolute historical VaR is larger than the parametric, it can be interpreted as a more conservative measure of risk.