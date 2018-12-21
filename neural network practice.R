#neural network example
library(datasets)

names(infert)

#  *------------------------------------------------------------------*
#  | train the network
#  *------------------------------------------------------------------* 

library(neuralnet)

nn <- neuralnet(
  case~age+parity+induced+spontaneous,
  data=infert, hidden=2, err.fct="ce",
  linear.output=FALSE)

################
# output training results
###############

# basic
nn


# reults options
names(nn)


# result matrix

nn$result.matrix

# The given data is saved in nn$covariate and
# nn$response as well as in nn$data for the whole data
# set inclusive non-used variables. The output of the
# neural network, i.e. the fitted values o(x), is provided
# by nn$net.result:

out <- cbind(nn$covariate,nn$net.result[[1]])

dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))

head(out)

# generalized weights

# The generalized weight expresses the effect of each
# covariate xi and thus has an analogous interpretation
# as the ith regression parameter in regression models.
# However, the generalized weight depends on all
# other covariates. Its distribution indicates whether
# the effect of the covariate is linear since a small variance
# suggests a linear effect

# The columns refer to the four covariates age (j =
# 1), parity (j = 2), induced (j = 3), and spontaneous (j=4)

head(nn$generalized.weights[[1]])

# visualization

plot(nn)


#########
#With EU Data
#########


rm(list = ls())

setwd("C:/Users/dholder/Desktop/Practice")
power <- read_excel("EU daily data.xlsx", sheet=1)

library(neuralnet)

power <- filter(power, DE>-10000)
power <- filter(power, carbon>-10000)
power <- filter(power, TTFPrice>-10000)
power <-filter(power, coal >-1000)

n <- neuralnet(
  DE~carbon+TTFPrice+coal+month,
  data=power, hidden=2, err.fct="ce",
  linear.output=FALSE)

nn <- neuralnet(
  DE~carbon+TTFPrice+coal+month,
  data=power, hidden=4)

#the compute function is essentially the forecast function
#inputForecast is the four input figures carbon, TTF, coal, month
inputForecast10 <-c(4,14.5,55,1,4,14.5,55,2,4,14.5,55,3)
inputForecast50 <-c(6,15,65,1,6,16,65,2,6,15.5,65,3)
inputForecast90 <-c(9,18,80,1,9,19,80,2,9,18,80,3)
forecast <- compute(nn, covariate=matrix(inputForecast50, byrow=TRUE, ncol=4))
print(forecast)
prediction()
plot(nn)
