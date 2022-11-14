### Homework 3 - Problem 4

#packages 
install.packages('deSolve')
install.packages('stats')
install.packages('ModelMetrics')
install.packages("MASS") 
install.packages("reshape2") 
install.packages("reshape") 
#libraries
library(ggplot2)
library(caret)
library(tidyverse)
library(dplyr)
library(deSolve)
library(stats)
library(ModelMetrics)
library(MASS)
library(reshape2)
library(reshape)

# 1 )  load data
#Separate the table into times when the observations are recoded and the states of the population Z_data.

Z_data<-read.csv("sird_data.csv", header = TRUE)

#View(Z_data)

time <- seq(0, 150, by = 1)

# try to define Z_150 as a matrix including S,I,R,D
# 2) Initialize your "wild guess" of the parameters that you think are true

params_0 <- c(
  beta = 0.7, # infection rate 
  gamma = 0.3,# recovery rate
  mu = 0.01) # mortality rate

#params_0
# 3) initialize Z0 and times from 0-150 days

Z_0 <- c(S=8756000,
I =44000,
R=0,
D=0)

N = Z_data$S[1] + Z_data$I[1] + Z_data$R[1] + Z_data$D[1]



# 4) define function sird_model() that returns derivative over time of S(t), I(t), R(t), D(t)
sird_model <- function(time, Z, par) {
  beta = par[1]
  gamma = par[2]
  mu = par[3]
  
  S = Z[1]
  I = Z[2]
  R = Z[3]
  D = Z[4]
  N = sum(Z)
  
  dS = -beta*S/N*I
  dI = beta*S/N*I - gamma*I - mu*I
  dR = gamma*I
  dD = mu*I
  
  return(list(c(dS, dI, dR, dD)))
}


#5 ) solve the model using ode() function to get the population dynamics over time with your "wild guess"ed parameters
pt_5<-ode(y = Z_0, times = time, func = sird_model, parms = params_0)
#pt_5
PTS_55 <-melt(pt_5)
#PTS_55


ggplot(PTS_55)+
geom_line(aes(x=X1,y=value,colour =X2,group =X2))

# we can see that R and D are 0 compared to the data. 

#Compare the obtained solution with the given data.'''

#6 ) It is time to find the best parameters that characterize the disease. Define the loss function as the sum of squared errors across all population groups


loss <- function(parameters){
  names(parameters) <- c("beta", "gamma", "mu") # parameters must be named
  solution <- ode(y = Z_0, times = time, func = sird_model, parms = parameters) #fited
  S<- solution[, 2]
  I <- solution[, 3] # 3rd column of ODE solution
  R<- solution[, 4]
  D <- solution[, 5] # 5th column of ODE solution
  return(sum((I-Z_data$I)^2 + (D-Z_data$D)^2+(R-Z_data$R)^2+(S-Z_data$S)^2))
}


#7 minimze loss function with optim() function. Use method = "L-BFGS-B" and restrict parameters to be positive lower = 
params_opt <- optim(par = params_0, fn = loss, method = "L-BFGS-B", lower = c(0,0,0), upper = c(1,1,1))
params_opt

#8) run ode (Z0,times,sird model,params_opt) and plot the zfit and Z_data
Z_exo <- melt(Z_data) # melt Z_data better layout for plotting
Z_2<-Z_exo[152:nrow(Z_exo),] # remove t values

Zfit_1 <- ode(y = Z_0, times = time, func = sird_model, parms = c(0.199993809,0.049990838,0.001011214 ))  %>% melt()

Zfit_2<-Zfit_1[152:755,] # remove time


plot_final <- ggplot(Zfit_2)+
    geom_line(aes(x = X1, y = value, color =X2,group = X2),linetype = 'dotted')+ # model fitted
    geom_line(data = Z_2, aes(x = Zfit_2$X1, y = value, color = variable,group=variable),linetype = 'solid')+ #Z_data
    labs(x = "Time", y = "Population", title = "SIRD model")+
    theme(plot.title = element_text(hjust = 0.5))

plot_final
