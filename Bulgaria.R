library(readxl)
library(tidyverse)
library(dplyr)
setwd("/Users/priya/Desktop")
my_data <- read_excel("/Users/priya/Desktop/XN /Data/Bulgaria.xlsx")

df_ma <- read_excel("/Users/priya/Desktop/XN /Data/Bulgaria.xlsx")
head(df_ma)

colSums(is.na(df_ma))



df_ma1 <- data.frame(
  sapply(
    df_ma,
    function(x) ifelse (is.na(x), mean(x,na.rm = TRUE),x)
  )
)

head(df_ma1)

df_ma1$External.debt..percent.of.Gross.National.Income <- 						as.numeric(df_ma1$External.debt..percent.of.Gross.National.Income)
df_ma1$Short.term.external.debt..percent.of.international.reserves <- 	as.numeric(df_ma1$Short.term.external.debt..percent.of.international.reserves)
df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar <- 		as.numeric(df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar)
df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP <-	as.numeric(df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP)
df_ma1$Financial.system.deposits..percent.of.GDP <- 	as.numeric(df_ma1$Financial.system.deposits..percent.of.GDP)
df_ma1$Foreign.bank.assets.as.percent.of.the.total.bank.assets <- 			as.numeric(df_ma1$Foreign.bank.assets.as.percent.of.the.total.bank.assets)
df_ma1$Stock.market.return..percent <- 						as.numeric(df_ma1$Stock.market.return..percent)
df_ma1$Interest.rates <- 	as.numeric(df_ma1$Interest.rates)
df_ma1$Foreign.exchange.reserves.including.gold..billion.USD <- as.numeric(df_ma1$Foreign.exchange.reserves.including.gold..billion.USD)

str(df_ma1)
S <- cor(df_ma1[,4:12]) #correlation matrix
S.eigen <- eigen(S) #applying eigen
S.eigen$values

stress.pca.scaled <- prcomp(df_ma1[,4:12],scale. = TRUE) #using PCA with scaling

summary(stress.pca.scaled)
S.eigen$vectors

stress.pca.scaled
