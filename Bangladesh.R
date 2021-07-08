library(readxl)
library(tidyverse)
library(dplyr)
setwd("/Users/priya/Desktop")
my_data <- read_excel("/Users/priya/Desktop/XN /Data/BGD.xlsx")

df_ma <- read_excel("/Users/priya/Desktop/XN /Data/BGD.xlsx")
head(df_ma)

colSums(is.na(df_ma))



df_ma1 <- data.frame(
  sapply(
    df_ma,
    function(x) ifelse (is.na(x), mean(x,na.rm = TRUE),x)
  )
)

head(df_ma1)

df_ma1$Fiscal.balance..percent.of.GDP <- 						as.numeric(df_ma1$Fiscal.balance..percent.of.GDP)
df_ma1$Inflation..percent.change.in.the.Consumer.Price.Index <- 	as.numeric(df_ma1$Inflation..percent.change.in.the.Consumer.Price.Index)
df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar <- 		as.numeric(df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar)
df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP <-	as.numeric(df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP)
df_ma1$Foreign.exchange.Reserves <- 	as.numeric(df_ma1$Foreign.exchange.Reserves)
df_ma1$Bank.return.on.assets..in.percent <- 			as.numeric(df_ma1$Bank.return.on.assets..in.percent)
df_ma1$Bank.return.on.equity..in.percent <- 						as.numeric(df_ma1$Bank.return.on.equity..in.percent)
df_ma1$Stock.price.volatility..percent <- 	as.numeric(df_ma1$Stock.price.volatility..percent)
df_ma1$Private.sector.credit <- 								as.numeric(df_ma1$Private.sector.credit)
df_ma1$Current.account.to.GDP <- 							as.numeric(df_ma1$Current.account.to.GDP)
df_ma1$M2.Money.supply <- 							as.numeric(df_ma1$M2.Money.supply)

str(df_ma1)
S <- cor(df_ma1[,4:14]) #correlation matrix
S.eigen <- eigen(S) #applying eigen
S.eigen$values

stress.pca.scaled <- prcomp(df_ma1[,4:14],scale. = TRUE) #using PCA with scaling

summary(stress.pca.scaled)
S.eigen$vectors

print(stress.pca.scaled)


