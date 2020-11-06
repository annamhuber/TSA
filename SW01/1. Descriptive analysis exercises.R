### Do it yourself 1! ###

# Create new working directory
#dir.create("~/TSA in Finance") # Creates new folder in home directory ("Documents" folder by default)
#setwd("~/TSA in Finance") # Sets working directory to the newly created folder

# Alternatively: Set working directory to an exising folder, for example:
setwd("C:/Users/annam/OneDrive/Documents/HSLU/MSC/HS20/TSA/Exercises_TSA/TSA/SW01")


# Exercise a): Importing datasets
#install.packages("tidyverse")
library(tidyverse)

SPI<-read_csv2("SPI.csv", 
               col_names = TRUE, col_types = cols(
                 Date = col_date(format="%d.%m.%Y"),
                 SPI = col_double()
               )
)

M1_Colombia<-read_csv2("M1 of Colombia.csv", 
                       col_names = TRUE, col_types = cols(
                         Date = col_date(format="%d.%m.%Y"),
                         M1_Colombia_bn = col_double()
                         ))

GDP_CH<-read_csv2("GDP CH (in CHF bn).csv", 
                  col_names = TRUE, col_types = cols(
                    Date = col_date(format="%d.%m.%Y"),
                    GDP_CH_bn
                    = col_double()
                  ))

# Exercise b): Transformation into time series class ("ts")
SPI<-ts(SPI[,-1], start = 2000, frequency = 12)
M1_Colombia<-ts(M1_Colombia[,-1], start = 2000, frequency = 12)
GDP_CH<-ts(GDP_CH[,-1], start = 1980, frequency = 4)# Note: Time series starts in the year 1980 with a quarterly frequency

# Exercise c): Plotting Swiss GDP
plot(GDP_CH, main="Quarterly GDP of Switzerland", ylab="In CHF mio", xlab="Year")

# Alternatively:
#install.packages("zoo")
library(zoo)
ggplot(data.frame(GDP_CH), aes(x = as.Date(time(GDP_CH)), y = c(GDP_CH))) +
  geom_bar(stat='identity',fill=rep(c("deepskyblue4","deepskyblue3","deepskyblue2","deepskyblue1"),ceiling(length(GDP_CH)/4))[1:length(GDP_CH)]) +
  labs(x="Year", y="Quarterly GDP of Switzerland in CHF bn")

# Exercise d): Decomposing Swiss GDP
GDP_decomposed<-decompose(GDP_CH)

plot(GDP_decomposed, xlab="Year")
names(GDP_decomposed)
seasonality<-GDP_decomposed$seasonal
GDP_seasonally_adjusted<-GDP_CH-seasonality
plot(GDP_CH-seasonality, main="Seasonally adjusted GDP", ylab="In CHF bn", xlab="Year")


### Do it yourself 2! ###
# Exercise a): Calculating continuous SPI returns
SPI_returns<-???

# Exercise b): Plotting histogram of SPI returns
hist(???)

# Exercise c): Calculating central moments of SPI returns
#install.packages("fBasics")
library(fBasics)
basicStats(???)

# Alternative visualization: Boxplot
boxplot(SPI_returns, main="Boxplot of SPI Returns")


### Do it yourself 3! ###
# Exercise a): Plotting SPI and M1 of Colombia
plot(???)
plot(???)

# Exercise b): Scatterplotting SPI and M1 of Colombia and calculating correlation
plot(x=???, y=???)
cor(???)

# Exercise c): Scatterplotting SPI returns and growth rates of M1 of Colombia and calculating correlation
M1_Colombia_growth<-???
plot(x=???, y=???)
cor(???,???)

# Exercise d): Replicating the ACF of the SPI and testing for stationarity
acf(???)
acf(???)

#install.packages("tseries")
library(tseries)
adf.test(???)
adf.test(???)