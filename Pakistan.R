library(readxl)
library(tidyverse)
library(dplyr)
setwd("/Users/priya/Desktop")
my_data <- read_excel("/Users/priya/Desktop/XN /Data/Pakistan.xlsx")

df_ma <- read_excel("/Users/priya/Desktop/XN /Data/Pakistan.xlsx")
head(df_ma)

colSums(is.na(df_ma))



df_ma1 <- data.frame(
  sapply(
    df_ma,
    function(x) ifelse (is.na(x), mean(x,na.rm = TRUE),x)
  )
)

head(df_ma1)

df_ma1$Bank.return.on.equity..in.percent <- 						as.numeric(df_ma1$Bank.return.on.equity..in.percent)
df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar <- 		as.numeric(df_ma1$Exchange.rate..local.currency.units.per.U.S..dollar)
df_ma1$Stock.market.return..percent <-	as.numeric(df_ma1$Stock.market.return..percent)
df_ma1$Foreign.exchange.reserves.including.gold..billion.USD <- 	as.numeric(df_ma1$Foreign.exchange.reserves.including.gold..billion.USD)
df_ma1$Stock.market.value.traded..percent.of.GDP <- 			as.numeric(df_ma1$Stock.market.value.traded..percent.of.GDP)
df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP <-	as.numeric(df_ma1$Domestic.credit.to.the.private.sector..percent.of.GDP)

str(df_ma1)
S <- cor(df_ma1[,4:9]) #correlation matrix
S.eigen <- eigen(S) #applying eigen
S.eigen$values

stress.pca.scaled <- prcomp(df_ma1[,4:9],scale. = TRUE) #using PCA with scaling

summary(stress.pca.scaled)
S.eigen$vectors

stress.pca.scaled

