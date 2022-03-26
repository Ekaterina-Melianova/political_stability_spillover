
library(tidyverse)
library(Spillover)
library(vars)
library(zoo)

# Data
index_raw <- read.csv2('index.csv', sep = ',')
index <- index_raw %>%
  filter(Series.Name == 'Political Stability and Absence of Violence/Terrorism: Estimate') %>%
  dplyr::select(-c(Series.Name, Series.Code, ï..Country.Name))

# prepossessing
index <- as.data.frame(t(index))
colnames(index) <- index['Country.Code',]
index <- index[-1,]
Date <- c(1996, 1998, 2000, 2002:2020)
index[index == '..'] <- NA
index <- sapply(index, as.numeric)
index <- index[ , colSums(is.na(index)) == 0]
#rownames(index) <- c(1996, 1998, 2000, 2002:2020)
index <- cbind.data.frame(Date, index)

# interpolation
z <- read.zoo(index, FUN = as.numeric)
m <- na.approx(merge(z, zoo(, c(kronecker(time(z), 0:11/12, "+")))))
time(m) <- as.Date(as.yearmon(time(m)), frac = 1)
index_interpolated <- as.data.frame(m)
index_interpolated_zoo <- as.zoo(index_interpolated)

# VAR and spillover index
VAR.1 <- VAR(index_interpolated_zoo)
O.spillover(VAR.1, n.ahead=1)
O.spillover(VAR.1, n.ahead=2, ortho.type = "partial")
O.spillover(VAR.1, n.ahead=2, ortho.type = "total")




