library(quantmod)
library(zoo)
library(lubridate)
library(jrvFinance)
library(readxl)
library(tidyverse) 
library(gridExtra)
library(datasets)
library(dplyr)
library("astsa")

setwd("E:/UofT/apm466/a1")
bdata <- read_excel("Data.xlsx")

#4a

# Define bond data
maturity_dates <- bdata$`MATURITY DATE`
valuation_dates <- c("2024-01-16", "2024-01-17", "2024-01-18", "2024-01-19", "2024-01-20", "2024-01-23", "2024-01-24", "2024-01-25", "2024-01-26", "2024-01-27")
price_matrix <- matrix(c(bdata$`45299`, bdata$`45300`, bdata$`45301`, bdata$`45302`, bdata$`45303`, bdata$`45306`, bdata$`45307`, bdata$`45308`, bdata$`45309`, bdata$`45310`), nrow = 10, ncol = 10, byrow = TRUE)
coupon_rates <- as.numeric(bdata$COUPON)
time_to_maturity <- matrix(nrow = 10, ncol = 10)

# Calculate time to maturity for each bond and date
for (bond_idx in 1:10) {
  for (date_idx in 1:10) {
    time_to_maturity[date_idx, bond_idx] <- as.numeric(interval(as.Date(valuation_dates[date_idx]), as.Date(maturity_dates[bond_idx]))) / 365
  }
}

# Calculate YTM for each bond and date
ytm_matrix <- matrix(nrow = 10, ncol = 10)
for (bond_idx in 1:10) {
  for (date_idx in 1:10) {
    ytm_matrix[date_idx, bond_idx] <- bond.yield(settle = as.Date(valuation_dates[date_idx]),mature = as.Date(maturity_dates[bond_idx]),coupon = coupon_rates[bond_idx],price = price_matrix[date_idx, bond_idx],freq = 2,convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),comp.freq = 2,redemption_value = 100)
  }
}

# Plot yield curve
years_to_maturity <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
color_for_plot <- c("black","red", "blue", "green", "orange", "purple", "cyan", "magenta", "yellow", "darkgreen")
plot(years_to_maturity, ytm_matrix[1,], type = "l", main = "Yield Curve", col = "black", xlab = "Years to Maturity", ylab = "YTM", lwd = 0.9)
for (bond_idx in 2:10) {
  lines(years_to_maturity, ytm_matrix[bond_idx,], type = "l", col = color_for_plot[bond_idx], lwd = 0.9)
}
legend("topright", lty = 1, legend = valuation_dates, col = color_for_plot, lwd = 0.9, cex = 0.7, bty = "n")


# estimating YTM using linear interpolation
years_to_maturity2 <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
ytm_interpolation <- matrix(nrow=10, ncol=10)
for (i in 1:10) {
  for (j in 1:9) {
    ytm_interpolation[i,j] = as.numeric(ytm_matrix[i,j])+((as.numeric(ytm_matrix[i,j+1])-as.numeric(ytm_matrix[i,j]))/(time_to_maturity[i,(j+1)]-time_to_maturity[i,j]))*(years_to_maturity2[j]-time_to_maturity[i,j])
  }
}
ytm_interpolation[,10] <- ytm_matrix[,10]

plot(year, ytm_interpolation[1,], type = "l", main = 'Yield Curve with Linear interpolation', col = "black", xlab="Years to maturity", ylab = "YTM", lwd=0.9)
for (bond_idx in 2:10) {
  lines(years_to_maturity, ytm_matrix[bond_idx,], type = "l", col = color_for_plot[bond_idx], lwd = 0.9)
}

legend("topright",lty = 1,legend=valuation_dates,
       col=color_for_plot,lwd=0.9,cex=0.7, bty = "n")

#4b
n <- length(years_to_maturity)
m <- ncol(ytm_matrix)
spot_rate <- matrix(nrow=n, ncol=m)

#4c





#5
log_y<-matrix(nrow=9,ncol=5)
for(i in c(1:9)){
  for(j in c(1:5)){
    log_y[i,j]<-log(as.numeric(ytm_interpolation[(i+1),j])/as.numeric(ytm_interpolation[i,j]))
  }
}

cov_y <- cov(log_y)
cov_y

#6
print(eigen(cov_y)$values)
print(eigen(cov_y)$vectors)

