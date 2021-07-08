library(readxl)
library(tidyverse)
library(dplyr)
setwd("/Users/priya/Desktop")
my_data <- read_excel("/Users/priya/Desktop/XN /Data/Nigeria.xlsx")

df_ma <- read_excel("/Users/priya/Desktop/XN /Data/Nigeria.xlsx")
head(df_ma)

colSums(is.na(df_ma))



df_ma1 <- data.frame(
  sapply(
    df_ma,
    function(x) ifelse (is.na(x), mean(x,na.rm = TRUE),x)
  )
)

head(df_ma1)

df_ma1$Inflation..percent.change.in.the.Consumer.Price.Index <- 	as.numeric(df_ma1$Inflation..percent.change.in.the.Consumer.Price.Index)
df_ma1$Current.account.balance.as.percent.of.GDP <- 							as.numeric(df_ma1$Current.account.balance.as.percent.of.GDP)
df_ma1$Interbank.rates <- 			as.numeric(df_ma1$Interbank.rates)
df_ma1$Bank.return.on.assets..in.percent <- 			as.numeric(df_ma1$Bank.return.on.assets..in.percent)
df_ma1$Bank.return.on.equity..in.percent <- 						as.numeric(df_ma1$Bank.return.on.equity..in.percent)
df_ma1$Bank.credit.to.the.private.sector.as.percent.of.GDP <- 						as.numeric(df_ma1$Bank.credit.to.the.private.sector.as.percent.of.GDP)
df_ma1$Short.term.external.debt..percent.of.international.reserves <- 	as.numeric(df_ma1$Short.term.external.debt..percent.of.international.reserves)
df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar <- 		as.numeric(df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar)
df_ma1$Stock.market.return..percent <-	as.numeric(df_ma1$Stock.market.return..percent)
df_ma1$Foreign.exchange.reserves.including.gold..billion.USD <- 	as.numeric(df_ma1$Foreign.exchange.reserves.including.gold..billion.USD)
df_ma1$Real.interest.rate..Bank.lending.rate.minus.inflation <- 			as.numeric(df_ma1$Real.interest.rate..Bank.lending.rate.minus.inflation)

str(df_ma1)
S <- cor(df_ma1[,4:14]) #correlation matrix
S.eigen <- eigen(S) #applying eigen
S.eigen$values

stress.pca.scaled <- prcomp(df_ma1[,4:14],scale. = TRUE) #using PCA with scaling

summary(stress.pca.scaled)
S.eigen$vectors

stress.pca.scaled

