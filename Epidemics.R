---
title: "Epidemics"
author: "Omid Parvizi"
date: "21 3 2020"
output: html_document
source: https://link.springer.com/chapter/10.1007/978-3-319-97487-3_1
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Required Library
```{r}
#install.packages("epimdr")
library(epimdr)
```
#-----------------------------
## Chapter 1
#-----------------------------
```{r}
#Persistence of measles against population size for 954 cities and villages in prevaccination England and Wales (1944–1964). Communities below 500k exhibited occasional or frequent (depending on size) local extinction of the virus
data(ccs) 
plot(ccs$size, ccs$ext*100, log="x", xlab= "Community size", ylab="Percent of time extinct")
```

```{r}
#Incidence of (a) weekly incidence of gonorrhea in Massachusetts (2006–2015)
plot(magono$time, magono$number, ylab="Cases", xlab="Year") 
lines(lowess(x=magono$time, y=magono$number, f=.4))
```

```{r}
# Incidence of monthly average (±SE) mortality from cholera in the Dacca district (1891–1940)
#sapply: returns a list of the same length as X
#split: divides the data in the vector x into the groups defined by f
#na.rm: remove NA values
#ui/li: The absolute upper/lower limit of the confidence region
data(cholera) 
ses=sesdv=rep(NA, 12) 
ses[c(7:12, 1:6)]=sapply(split(cholera$Dacca, cholera$Month), mean, na.rm=TRUE)
sesdv[c(7:12, 1:6)]=sapply(split(cholera$Dacca, cholera$Month), sd, na.rm=TRUE)/
  sqrt(length(split(cholera$Dacca, cholera$Month))) 
#install.packages("plotrix")
library(plotrix)
plotCI(x=1:12, y=ses, ui=ses+sesdv, li=ses-sesdv, xlab="Month", ylab="Deaths")
lines(x=1:12, y=ses)
```

#-----------------------------
## Chapter 2
#-----------------------------
```{r}
## The SIR Model (The closed SIR epidemic with leftt and right  axes and effective #reproductive ratio RE. The epidemic turns over at RE = 1)

## deSolve: Numerical Integration of the SIR Model ##
library(deSolve)
sirmod = function(t, y, parms) { 
# Pull state variables from y vector 
  S = y[1] 
  I = y[2]
  R = y[3] 
# Pull parameter values from parms vector 
  beta = parms["beta"] 
  mu = parms["mu"]
  gamma = parms["gamma"]
  N = parms["N"] 
# Define equations 
  dS = mu * (N - S) - beta * S * I/N 
  dI = beta * S * I/N - (mu + gamma) * I 
  dR = gamma * I - mu * R 
  res = c(dS, dI, dR) 
# Return list of gradients 
  list(res) 
}

#States of the system (here we use 26 weeks with 10 time-increments per week as speciﬁed in the vector times), the parameter values (in this case as speciﬁed in the vector parms), and starting conditions (speciﬁed in start).
#In this case we model the fraction of individuals in each class, so we set N =1, and consider a disease with an infectious period of 2 weeks ( γ = 1/2), no births or deaths ( μ = 0) and a transmission rate of 2 ( β =2). For our starting conditions we assume that 0.1% of the initial population is infected and the remaining fraction is susceptible.

times = seq(0, 26, by = 1/10) 
parms = c(mu = 0, N = 1, beta = 2, gamma = 1/2) 
start = c(S = 0.999, I = 0.001, R = 0)

# ode: General Solver for Ordinary Differential Equations

out=ode(y=start, times=times, func=sirmod, parms= parms) 
out=as.data.frame(out) 
head(round(out, 3))

plot(x=out$time, y=out$S, ylab="Fraction", xlab= "Time", type="l") 
lines(x=out$time, y=out$I, col="red") 
lines(x=out$time, y=out$R, col="green")

##we can conﬁrm that the turnover of the epidemic happens exactly when RE = R0s=1, where s is the fraction of remaining susceptibles.

#Calculate R0: basic reproductive ratio, ), deﬁned as the expected number of secondary #infections
R0=parms["beta"]/(parms["gamma"]+parms["mu"])
#Adjust margins to accommodate a second right axis 
par(mar = c(5,5,2,5)) 
#Plot state variables 
plot(x=out$time, y=out$S, ylab="Fraction", xlab="Time", type="l") 
lines(x=out$time, y=out$I, col="red") 
lines(x=out$time, y=out$R, col="green") 
#Add vertical line at turnover point 
xx=out$time[which.max(out$I)] 
lines(c(xx,xx), c(1/R0,max(out$I)), lty=3)
#prepare to superimpose 2nd plot 
par(new=TRUE) 
#plot effective reproductive ratio (w/o axes) 
plot(x=out$time, y=R0*out$S, type="l", lty=2, lwd=2, col="black", axes=FALSE, xlab=NA, ylab=NA, ylim=c(-.5, 4.5)) 
lines(c(xx, 26), c(1,1), lty=3) 
#Add right-hand axis for RE 
axis(side = 4) 
mtext(side = 4, line = 4, expression(R[E])) 
#Add legend 
legend("right", legend=c("S", "I", "R", expression(R[E])), lty=c(1,1,1, 2), col=c("black", "red", "green", "black"))
```

```{r}
## Final Epidemic Size (The ﬁnal epidemic size as a function of R0)

#So for these parameters, 2% of susceptibles are expected to escape infection #altogether and 98%—the ﬁnal epidemic size—are expected to be infected during the #course of the epidemic.
#install.packages("rootSolve")
library(rootSolve) 
# runsteady: Dynamically runs a system of ordinary differential equations (ODE) to steady-state
equil=runsteady(y=c(S=1-1E-5, I=1E-5, R=0), times=c(0,1E5), 
                func=sirmod, parms=parms) 
round(equil$y, 3)

#Candidate values for R0 and beta 
R0 = seq(0.1, 5, length=50) 
betas= R0 * 1/2 
#Vector of NAs to be filled with numbers 
f = rep(NA, 50) 
#Loop over i from 1, 2, ..., 50 
for(i in seq(from=1, to=50, by=1)){ 
  equil=runsteady(y=c(S=1-1E-5, I=1E-5, R=0), times=c(0,1E5), func=sirmod, parms=c(mu=0, N=1,
    beta=betas[i], gamma=1/2)) 
  f[i]=equil$y["R"] 
  } 
plot(R0, f, type="l", xlab=expression(R[0])) 
curve(1-exp(-x), from=1, to=5, add=TRUE, col="red")

#The black line is the solution based on numerically integrating the closed epidemic, #and the red line is the approximation f=1−exp(−R0)
```

```{r}
## For the closed epidemic SIR model, there is an exact mathematical solution to the #fraction of susceptibles that escapes infection (1−f) given by the implicit equation #f =exp(−R0(1−f)) or equivalently exp(−R0(1−f))−f =0

#Define function 
fn=function(x, R0){ 
  exp(-(R0*(1-x))) - x
}

# uniroot: ﬁnds numerical solutions to equations with one unknown variable (which has to be named x)
1-uniroot(fn, lower = 0, upper = 1-1E-9, tol = 1e-9, R0=2)$root
#check accuracy of approximation: 
exp(-2)-uniroot(fn, lower = 0, upper = 1-1E-9, tol = 1e-9, R0=2)$root

```

```{r}
##  The open SIR epidemic ("endemicequilibrium”werethepathogenandhost coexist)

#Let’s assume that 19% of the initial population issusceptible and 1% is infected and numerically integrate the model for 50 years

times = seq(0, 52*50, by=.1) 
parms = c(mu = 1/(50*52), N = 1, beta = 2, gamma = 1/2) 
start = c(S=0.19, I=0.01, R = 0.8) 
out = as.data.frame(ode(y=start, times=times, func=sirmod, parms=parms))
par(mfrow=c(1,2)) 
# The fraction infected over time 
plot(times, out$I, ylab="Fraction", xlab="Time", type="l") 
# The joint time series of infectedsandsusceptiblesintheS-Iphaseplane
plot(out$S, out$I, type="l", xlab="Susceptible", ylab="Infected")

```

```{r}
## Phase Analyses 

simod = function(t, y, parameters) { 
  S = y[1] 
  I = y[2]
  beta = parameters["beta"] 
  mu = parameters["mu"] 
  gamma = parameters["gamma"] 
  N = parameters["N"]
dS = mu * (N - S) - beta * S * I/N 
dI = beta * S * I/N - (mu + gamma) * I 
res = c(dS, dI) 
list(res)
}
#install.packages("phaseR")
library(phaseR) 
#Plot vector field 
fld=flowField(simod, xlim=c(0.15,0.35), ylim=c(0,.01), parameters=parms, system="two.dim", 
              add=FALSE, ylab="I", xlab="S") 
#Add trajectory 
out = as.data.frame(ode(y = c(S=0.19, I=0.01), times= seq(0, 52*100, by=.1), func=simod, 
                        parms=parms)) 
lines(out$S, out$I, col="red")

#Add S-isocline 
curve(parms["mu"]*(1/x-1)/parms["beta"], 0.15, 0.35, xlab="S", ylab="I", add=TRUE) 
#Add I-isocline 
shat=(parms["gamma"]+parms["mu"])/parms["beta"] 
lines(rep(shat, 2), c(0,0.01))
legend("topright", legend=c("Transient", "Isoclines"), lty=c(1, 1), col=c("red", "black"))

```

```{r}
## Stability and Periodicity

# Pull values from parms vector 
gamma = parms["gamma"] 
beta = parms["beta"] 
mu = parms["mu"] 
N = parms["N"]
# Endemic equilibrium
Sstar = (gamma + mu)/beta 
Istar = mu * (beta/(gamma + mu) - 1)/beta 
eq1 = list(S = Sstar, I = Istar)
# Define equations 
dS = expression(mu * (N - S) - beta * S * I/N) 
dI = expression(beta * S * I/N - (mu + gamma) * I) 
## calculate the elements of the Jacobian 
# Differentiate w.r.t. S and I 
j11 = D(dS, "S") 
j12 = D(dS, "I") 
j21 = D(dI, "S") 
j22 = D(dI, "I")
##passthevaluesforS∗ andI∗ intheeq1-listtotheJacobian,8 anduseeigenfunction to calculate the eigenvalues
#Evaluate Jacobian at equilibrium 
J=with(data=eq1, expr=matrix(c(eval(j11),eval(j12), eval(j21),eval(j22)), nrow=2, byrow=TRUE)) #Calculate eigenvalues 
eigen(J)$values
##For the endemic equilibrium, the eigenvalues are a pair of complex conjugates which real parts are negative, so it is a stable focus. The period of the inwards spiral is:
2 * pi/(Im(eigen(J)$values[1]))

eq2=list(S=1,I=0) 
J=with(eq2, matrix(c(eval(j11),eval(j12),eval(j21), eval(j22)), nrow=2, byrow=TRUE)) 
eigen(J)$values

```

```{r}
## chain-SIR model (More Realistic Infectious Periods)

chainSIR=function(t, logx, params){ 
  x=exp(logx) 
  u=params["u"] 
  S=x[1] 
  I=x[2:(u+1)] 
  R=x[u+2] 
  with(as.list(params),{ 
    dS = mu * (N - S) - sum(beta * S * I) / N 
    dI = rep(0, u) 
    dI[1] = sum(beta * S * I) / N - (mu + u*gamma) * I[1] 
    if(u>1){ 
      for(i in 2:u){ 
        dI[i]= u*gamma * I[i-1] - (mu+u*gamma)* I[i] 
      } 
    } 
    dR = u*gamma * I[u] - mu * R 
    res=c(dS/S, dI/I, dR/R) 
    list(res) 
  }) 
}

times = seq(0, 10, by=1/52) 
paras2 = c(mu = 1/75, N = 1, beta = 625, gamma = 365/14, u=1) 
xstart2 = log(c(S=.06, I=c(0.001, rep(0.0001, paras2["u"]-1)), R = 0.0001)) 
out = as.data.frame(ode(xstart2, times, chainSIR, paras2)) 
plot(times, exp(out[,3]), ylab="Infected", xlab= "Time", ylim=c(0, 0.01), type='l')

paras2["u"] =2 
xstart2 = log(c(S=.06, I=c(0.001, rep(0.0001/ paras2["u"], paras2["u"]-1)), R = 0.0001)) 
out2 = as.data.frame(ode(xstart2, times, chainSIR, paras2)) 
lines(times, apply(exp(out2[,-c(1:2,length(out2))]), 1 ,sum), col='blue')

paras2["u"] =73 
xstart2 = log(c(S=.06, I=c(0.001, rep(0.0001/ paras2["u"], paras2["u"]-1)), R = 0.0001)) 
out3 = as.data.frame(ode(xstart2, times, chainSIR, paras2)) 
lines(times, apply(exp(out3[,-c(1:2,length(out3))]), 1, sum), col='red', lwd=2, lty=2)

paras2["u"] =500 
xstart2 = log(c(S=.06, I=c(0.001, rep(0.0001/ paras2["u"], paras2["u"]-1)), R = 0.0001)) 
out4 = as.data.frame(ode(xstart2, times, chainSIR, paras2)) 
lines(times, apply(exp(out4[,-c(1:2,length(out4))]),
                   1,sum, na.rm=TRUE), col='green')
legend("topright", legend=c("SIR", "u=2", "u=500", "u=73 (H-S)"), lty=c(1,1,1,2), lwd=c(1,1,1, 2), col=c("black", "blue", "green", "red"))

```

##ShinyApp
```{r}
require(shiny) 
require(deSolve) 
require(phaseR)
#This creates the User Interface (UI) 
ui = pageWithSidebar(
  #The title 
  headerPanel("The SIR model"), 
  #The sidebar for parameter input 
  sidebarPanel( 
  #Sliders: 
    sliderInput("beta", "Transmission (yrˆ-1):", 300, min = 0, max = 1000), 
    sliderInput("infper", "Infectious period (days)", 5, min = 1, max = 100), 
    sliderInput("mu", "birth rate:", 5, min = 0, max = 100), 
    sliderInput("T", "Time range:", min = 0, max = 1, value = c(0,1))
  ), 
  #Main panel for figures and equations 
  mainPanel( 
    #Multiple tabs in main panel 
    tabsetPanel( 
      #Tab 1: Time plot (plot1 from server) 
      tabPanel("Time", plotOutput("plot1")), 
      #Tab 2: Phase plot (plot2 from server) 
      tabPanel("Phase plane", plotOutput("plot2", height = 500)), 
      #Tab 3: MathJax typeset equations 
      tabPanel("Equations", 
               withMathJax( 
                 helpText("Susceptible $$\\frac{dS}{dt} = \\mu (N - S) - \\frac{\\beta I S}{N}$$"), 
                 helpText("Infecitous $$\\frac{dI}{dt} = \\frac{\\beta I S}{N} -( \\mu+\\sigma) I$$"),
                 helpText ("Removed $$\\frac{dR}{dt} = \\gamma I - \\mu R$$"), 
                 helpText("Reproductive ratio $$R_0 = \\frac{1}{\\gamma+\\mu} \\frac{\\beta N}{N}$$")
                 ))
  ))) #End of ui()
  # This creates the ’behind the scenes’ code (Server) 
server = function(input, output) { 
  #Gradient function for SIR model 
  sirmod=function(t, x, parms){ 
    S=x[1] 
    I=x[2] 
    R=x[3]
  beta=parms["beta"] 
  mu=parms["mu"] 
  gamma=parms["gamma"] 
  N=parms["N"] 
  dS = mu * (N - S) - beta * S * I / N 
  dI = beta * S * I / N - (mu + gamma) * I 
  dR = gamma * I - mu * R 
  res=c(dS, dI, dR) 
  list(res)
} 
  #Gradient function used for phaseR phase-plot 
  simod=function(t, y, parameters){ 
    S=y[1] 
    I=y[2] 
    beta=parameters["beta"] 
    mu=parameters["mu"] 
    gamma=parameters["gamma"] 
    N=parameters["N"] 
    dS = mu * (N - S) - beta * S * I / N 
    dI = beta * S * I / N - (mu + gamma) * I 
    res=c(dS, dI) 
    list(res)
  } 
  #Plot1: renderPlot to be passed to UI tab 1 
  output$plot1 = renderPlot({ 
  #input \ $xx’s are pulled from UI 
    times = seq(0, input$T[2], by=1/1000) 
    parms = c(mu = input$mu, N = 1, beta = input$beta, gamma = 365/input$infper) 
    start = c(S=0.999, I=0.001, R = 0) 
    R0 = round(with(as.list(parms), beta/(gamma+mu)), 1)
  #Integrate ode with parameters pulled from UI 
    out=ode(y=start, times=times, func=sirmod, parms=parms) 
    out=as.data.frame(out)
  #Plot1 
    sel=out$time>input$T[1]&out$time<input$T[2] 
    plot(x=out$time[sel], y=out$S[sel], ylab="fraction", xlab="time", type="l", ylim=range(out[sel,-c(1,4)]))
    title(paste("R0=", R0))
  lines(x=out$time[sel], y=out$I[sel], col="red") 
  lines(x=out$time[sel], y=out$R[sel], col="green") 
  legend("right", legend=c("S", "I", "R"), lty=c(1,1,1), col=c("black", "red", "green")) 
  }) 
  #Plot2: renderPlot to be passed to UI tab 2 
  output$plot2 = renderPlot({ 
    times = seq(0, input$T[2], by=1/1000) 
    parms = c(mu = input$mu, N = 1, beta = input$beta, gamma = 365/input$infper) 
    start = c(S=0.999, I=0.001, R = 0) 
    R0 = round(with(as.list(parms), beta/(gamma+mu)), 1)
  #Integrate simod 
  out=ode(y=start[-3], times=times, func=simod, parms=parms) 
  out=as.data.frame(out)
  #Plot2 
  plot(x=out$S, y=out$I, xlab="Fraction suceptible", ylab="Fraction infected", type="l") 
  title(paste("R0=", R0)) 
  #Add vector field 
  fld=flowField(simod, x.lim=range(out$S), y.lim= range(out$I), parameters=parms, system="two.dim", add=TRUE, ylab="I", xlab="S") 
  #Add isoclines 
  abline(v=1/R0, col="green") 
  curve(parms["mu"]*(1-x)/(parms["beta"]*x), min(out$S), max(out$S), add=TRUE, col="red") 
  legend("topright", legend=c("I-socline", "S-isocline"), lty=c(1,1), col=c("red", "green")) 
  }) 
} #End of server()
shinyApp(ui, server)

```

###############################################################
#Primacy of R0 (expected number of secondary cases) 
###############################################################

#Estimating R0 from a Simple Epidemic
```{r}
require(epimdr)
data(niamey) 
head(niamey[, 1:5])

par(mar = c(5,5,2,5)) 
plot(niamey$absweek, niamey$tot_cases, type="b", xlab="Week", ylab="Incidence") 
par(new=T)
# Weekly incidence of measles in Niamey, Niger during the 2003–2004 outbreak
plot(niamey$absweek, niamey$cum_cases, type="l", col="red", axes=FALSE, xlab=NA, ylab=NA, log="y") 
axis(side = 4) 
mtext(side = 4, line = 4, "Cumulative incidence") 
legend("topleft", legend=c("Cases", "Cumulative"), lty=c(1,1), pch=c(1,NA), col=c("black", "red"))

##serial interval (V) which is the average time between infection and reinfection
# V= 1.5- 1.8

fit=lm(log(cum_cases)~absweek, subset=absweek<7, data=niamey) 
r=fit$coef["absweek"] 
V=c(1.5, 1.8) 
V*r+1


V = c(1.5, 1.8) 
f = (5/7)/V 
V * r + 1 + f * (1 - f) * (V * r)^2
```

# Maximum Likelihood: The Chain-Binomial Model
 
```{r}
#likelihood-function for the chain-binomial model 
llik.cb = function(S0, beta, I) { 
  n = length(I) 
  S = floor(S0 - cumsum(I[-n])) 
  p = 1 - exp(-beta * (I[-n])/S0) 
  L = -sum(dbinom(I[-1], S, p, log = TRUE)) 
  return(L) } 
# S0=Least case during the epidemics (aggregate the data into 2-week intervals)
twoweek = rep(1:15, each = 2) 
y = sapply(split(niamey$cases_1[1:30], twoweek), sum) 
sum(y)
#parameterization RE=β and calculate the likelihood 
S0cand=6500 
betacand=seq(0,10, by=.1) 
ll=rep(NA, length(betacand)) 
for(i in 1:length(betacand)){ 
  ll[i]=llik.cb(S0=S0cand, beta=betacand[i], I=y) 
} 
plot(ll~betacand, ylab="Neg log-lik", xlab= expression(beta)) 
betacand[which.min(ll)]

##If our S0 guess is right, then β should be around 2.3 
# We can do a similar check for S0 (assuming β is 2.3). The grid-value associated with the highest likelihood value is 7084.8 (Fig.3.3), so our original S0 guess was good but not perfect.

betacand=2.3 
S0cand=seq(5920,8000, length=101) 
ll=rep(NA, length=101) 
for(i in 1:101){ 
  ll[i]=llik.cb(S0=S0cand[i], beta=betacand, I=y) 
} 
plot(ll~S0cand, ylab="Neg log-lik", xlab= expression(S[0])) 
S0cand[which.min(ll)]

#minimize the negative log-likelihood using the generic optim-function or the mle2-function

require(bbmle) 
fit=mle2(llik.cb, start=list(S0=7085, beta=2.3), method="Nelder-Mead",data = list(I = y)) 
summary(fit)
# S0=(CI:?-?)
confint(fit)

## it is conceivable that similar epidemic trajectories can arise from having a large number of initial susceptibles and a low transmission rate, or a more moderate number of susceptibles and a higher transmission rate. We can quantify this through considering the correlation matrix among the parameters of our likelihood analysis; vcov calculates their variance-covariance matrix from which we can calculate standard errors according to sqrt(diag(vcov(fit))) and cov2cor converts this to a correlation matrix. As intuition suggested there is a strong negative correlation between the estimates of the β and S0 parameters.

cov2cor(vcov(fit))

```

## Stochastic Simulation

```{r}
sim.cb=function(S0, beta, I0){
  I=I0 
  S=S0 
  i=1 
  while(!any(I==0)){
   i=i+1
   I[i]=rbinom(1, size=S[i-1], prob=1-exp(-beta*I[i-1]/S0))
   S[i]=S[i-1]-I[i]
  }
  out=data.frame(S=S, I=I)
  return(out)
}
plot(y, type="n", xlim=c(1,18), ylab="Predicted/observed", xlab="Week")
for(i in 1:100){
  sim=sim.cb(S0=floor(coef(fit)["S0"]), beta=coef(fit)["beta"], I0=11)
  lines(sim$I, col=grey(.5))
  }
points(y, type="b", col=2)
```
# Further Examples 

```{r} 
##(Inﬂuenza A/H1N1 1977)

# Daily number of children conﬁned to bed in a boarding school in North England during an outbreak in 1978 of the reemerging A/H1N1 strain
data(flu)
plot(flu$day, flu$cases, type="b", xlab="Day", ylab="In bed", log="y")
tail(flu)
# estimate of R0 
fit=lm(log(cases)~day, subset=day<=5, data=flu)
lambda=fit$coef["day"]
V=c(2,3)
V*lambda+1
#This is higher than most estimates of R0 of pandemic ﬂu (which typically lies in the 1.5–2.5 interval). However, contact rates within a boarding school is likely to be higher than average across human populations as a whole

##(Ebola Sierra Leone 2014–2015)
data(ebola)
par(mar = c(5,5,2,5))
plot(ebola$day, ebola$cases, type="b", xlab="Week", ylab="Incidence")
par(new=T)
plot(ebola$day, ebola$cum_cases, type="l", col="red", axes=FALSE, xlab=NA, ylab=NA, log="y")
axis(side = 4)
mtext(side = 4, line = 4, "Cumulative incidence")
legend("right", legend=c("Cases", "Cumulative"), lty=c(1,1), pch=c(1,NA), col=c("black", "red"))
tail(ebola)

#use the regression method with Lipsitch’s correction
V = 15
f = 0.5
V * lambda + 1 + f * (1 - f) * (V * lambda)^2

# aggregate the data in 2-week increments roughly corresponding to the serial interval
cases=sapply(split(ebola$cases, floor((ebola$day-.1)/14)), sum)
sum(cases)

#Removal MLE
fit=mle2(llik.cb, start=list(S0=20000, beta=2), method="Nelder-Mead",data = list(I = cases))
summary(fit)
confint(fit, std.err=c(100,0.1))

##(Ebola DRC 1995)
names(ferrari)
ferrari$Ebolacases95
sum(ferrari$Ebolacases95, na.rm = TRUE)
y = c(na.omit(ferrari$Ebolacases95))
fit=mle2(llik.cb, method="Nelder-Mead", start=list(S0=300, beta=2), data = list(I = y))
fit
confint(fit, std.err=2)

```

## Other Rules of Thumb

```{r}
require(statnet)
data(gonnet)
#visualize the chains of transmission
nwt = network(gonnet, directed = TRUE)
plot(nwt, vertex.col = c(0, rep(1, 17), rep(2, 71)))

```

## Advanced: The Next-Generation Matrix
# SEIR

```{r}
#Step 1: Infected classes are E and I, let us label them 1 and 2. 
#Step 2: All new infections: dE/dt = β SI/N, dI/dt = 0
F1 = quote(beta * S * I/N) 
F2 = 0
#Step 3: All losses dE/dt =( μ+σ )E, dI/dt =( μ+α+γ )I
Vm1 = quote(mu * E + sigma * E)
Vm2 = quote(mu * I + alpha * I + gamma * I)
#Step 4: All gained transfers dE/dt = 0, dI/dt =( σ )E
Vp1 = 0 
Vp2 = quote(sigma * E)
#Step 5: Subtract Vp from Vm
V1 = substitute(a - b, list(a = Vm1, b = Vp1)) 
V2 = substitute(a - b, list(a = Vm2, b = Vp2))
#Step 6: Generate the partial derivatives for the two Jacobians
f11 = D(F1, "E"); f12 = D(F1, "I") 
f21 = D(F2, "E"); f22 = D(F2, "I")

v11 = D(V1, "E"); v12 = D(V1, "I")
v21 = D(V2, "E"); v22 = D(V2, "I")
#Step 7: Assuming N=1, the disease free equilibrium (dfe) is S = 1,E = 0,I = 0,R = 0. We also need values for other parameters. Assuming a weekly time-step and something chickenpox-like we may use μ = 0, α = 0, β = 5, γ = .8, σ = 1.2, and N = 1.
paras=list(S=1, E=0, I=0, R=0, mu=0, alpha=0, beta=5, gamma=.8, sigma=1.2, N=1)
f=with(paras, matrix(c(eval(f11),eval(f12),eval(f21), eval(f22)), nrow=2, byrow=TRUE))
v=with(paras, matrix(c(eval(v11),eval(v12),eval(v21), eval(v22)), nrow=2, byrow=TRUE))
#Step8: Calculate the largest eigenvalue of f ×inverse(v). Note that the function for inverting matrices in R is solve.
max(eigen(f %*% solve(v))$values)
# Let us check that the next-generation method and the “ﬂow” method are in agreement recalling that for the SEIR-ﬂow R0 = σ/σ + μ  β/γ + μ + α
with(paras, sigma/(sigma + mu) * beta/(gamma + mu + alpha))

```

## SEIHFR
# There are four infected compartments (E, I, H, and F), thus F, V−, and V+ will be 4×1 matrices, and f and v will be 4×4 matrices. 
```{r}
#Step 1: Infected classes are E, I, H, and F, and let us label them 1–4. 
#Step 2: All new infections dE/dt = β SI/N, dI/dt = 0

F1=expression(betai * S * I / N + betah* S * H / N + betaf * S * F / N) 
F2=0 
F3=0 
F4=0
#Step 3: All losses
Vm1=quote(sigma * E) 
Vm2=quote(Theta * gammah * I + (1 - Theta) * (1 - Lambda) * gammar * I + (1 - Theta) * Lambda * gammaf * I) 
Vm3=quote(Lambda * etaf * H + (1 - Lambda) * etar * H) 
Vm4=quote(chi * F)
#Step 4: All gained transfers
Vp1=0 
Vp2=quote(sigma * E)
Vp3=quote(Theta * gammah * I) 
Vp4=quote((1 - Theta) * (1 - Lambda) * gammar * I+ Lambda * etaf * H)
#Step 5: Subtract Vp from Vm
V1 = substitute(a - b, list(a = Vm1, b = Vp1)) 
V2 = substitute(a - b, list(a = Vm2, b = Vp2)) 
V3 = substitute(a - b, list(a = Vm3, b = Vp3)) 
V4 = substitute(a - b, list(a = Vm4, b = Vp4))
#Step 6: Generate the partial derivatives for the two Jacobians
f11 = D(F1, "E"); f12 = D(F1, "I"); f13 = D(F1, "H") 
f14 = D(F1, "F") 
f21 = D(F2, "E"); f22 = D(F2, "I"); f23 = D(F2, "H") 
f24 = D(F2, "F") 
f31 = D(F3, "E"); f32 = D(F3, "I"); f33 = D(F3, "H") 
f34 = D(F3, "F")
f41 = D(F4, "E"); f42 = D(F4, "I"); f43 = D(F4, "H") 
f44 = D(F4, "F")
v11 = D(V1, "E"); v12 = D(V1, "I"); v13 = D(V1, "H") 
v14 = D(V1, "F")
v21 = D(V2, "E"); v22 = D(V2, "I"); v23 = D(V2, "H") 
v24 = D(V2, "F") 
v31 = D(V3, "E"); v32 = D(V3, "I"); v33 = D(V3, "H") 
v34 = D(V3, "F")
v41 = D(V4, "E"); v42 = D(V4, "I"); v43 = D(V4, "H") 
v44 = D(V4, "F")
#Step 7: Disease free equilibrium: the dfe is S=1,E =0,I =0,H =0,F =0,R= 0. We also need values for other parameters. We use the estimates from the DRC 1995 outbreak scaled as weekly rates from tables and appendices of Legrand et al. (2007).
gammah = 1/5 * 7
gammaf = 1/9.6 * 7
gammar = 1/10 * 7 
chi = 1/2 * 7 
etaf = 1/4.6 * 7 
etar = 1/5 * 7

paras=list(S=1,E=0, I=0, H=0, F=0,R=0, sigma=1/7*7, Theta=0.81, Lambda=0.81, betai=0.588, betah=0.794, betaf=7.653,N=1, gammah=gammah, gammaf=gammaf, gammar=gammar, etaf=etaf, etar=etar, chi=chi)

f=with(paras, 
       matrix(c(eval(f11),eval(f12),eval(f13),eval(f14), 
                eval(f21),eval(f22),eval(f23),eval(f24), 
                eval(f31),eval(f32),eval(f33),eval(f34), 
                eval(f41),eval(f42),eval(f43),eval(f44)), 
              nrow=4, byrow=T))

v=with(paras, 
       matrix(c(eval(v11),eval(v12),eval(v13),eval(v14),
                eval(v21),eval(v22),eval(v23),eval(v24),
                eval(v31),eval(v32),eval(v33),eval(v34), 
                eval(v41),eval(v42),eval(v43),eval(v44)), 
              nrow=4, byrow=T)) 
#Step 8: Calculate the largest eigenvalue of f ×inverse(v) 
max(eigen(f %*% solve(v))$values)

```
###############################################################
##FoI and Age-Dependent Incidence
###############################################################

```{r}
## Probability of Infection at Age: The Catalytic Model
data(black) 
black
#subsetting age brackets
b2=black[-c(1,8,9),]
#Estimate log-FoI 
fit=glm(cbind(pos,neg) ~ offset(log(mid)), family=binomial(link="cloglog"), data=b2) 
#Plot predicted and observed 
phi=exp(coef(fit))
curve(1-exp(-phi*x), from=0, to=60, ylab='Seroprevalence', xlab='Age')
points(black$mid, black$f, pch='*', col='red') 
points(b2$mid, b2$f, pch=8) 
exp(fit$coef)

```
#########################################################
# More Flexible φ-Functions

```{r}
data(rabbit) 
head(rabbit)
rabbit$notinf=rabbit$n-rabbit$inf 
#Binomial regression 
fit=glm(cbind(inf, notinf)~offset(log(a)), family=binomial(link="cloglog"), data=rabbit, subset=a<12) 
#Plot data 
symbols(rabbit$inf/rabbit$n~rabbit$a, circles=rabbit$n, inches=.5, xlab="Age", ylab="Prevalence") 
#Predicted curves for <1 and all 
phi=exp(coef(fit)) 
curve(1-exp(-phi*x), from=0, to=12, add=TRUE) 
curve(1-exp(-phi*x), from=0, to=30, add=TRUE, lty=2) 
1/phi                  

```
#########################################################
# Calculate the average FoI from the binomial regression scheme 
```{r}
data(rabbit)
#  Deﬁne a function for the integrand which takes the argument a for age, up is a vector of the upper cut-offs for each age bracket, and foi is the vector of age-speciﬁc FoIs: 
integrandpc=function(a, up, foi){ 
  #Find which interval a belongs to 
  wh=findInterval(a, sort(c(0,up))) 
  #Calcultae duration of each interval
  dur=diff(sort(c(0,up))) 
  #Evaluate integrand 
  inte=ifelse(wh==1, foi[1]*a, 
              sum(foi[1:(wh-1)]*dur[1:(wh-1)])+ foi[wh]*(a-up[wh-1])) 
  return(inte)
}
# The negative log-likelihood function for the piecewise constant model
llik.pc = function(par, age, num, denom, up) { 
  ll = 0 
  for (i in 1:length(age)) { 
    p = 1 - exp(-integrandpc(a=age[i], up = up, foi = exp(par))) 
    ll = ll + dbinom(num[i], denom[i], p, log = T) } 
  return(-ll) 
} 
#cut-off points for the age categories and assign arbitrary initial values of 0.1 for each piece  of the FoI-function:
x = c(1, 4, 8, 12, 18, 24, 30) 
para = rep(0.1, length(x))
# ﬁnd maximum likelihood estimates:
est = optim(par=log(para),fn=llik.pc, age=rabbit$a, num=rabbit$inf, denom=rabbit$n, up=x, method="Nelder-Mead", control=list(trace=2))
# Associated age-speciﬁc FoIs 
round(exp(est$par), 6) 

##  predict the age-prevalence curve 
# Make space for left and right axes 
par(mar = c(5,5,2,5)) 
# Add beginning and ends to x and y for step plot 
xvals=c(0,x) 
yvals=exp(c(est$par, est$par[7])) 
plot(xvals, yvals, type="s", xlab="age", ylab="FoI")
# Superimpose predicted curve 
par(new=T) 
p = rep(0, 28) 
for (i in 1:28) { 
  p[i] = 1 - exp(-integrandpc(a=i, up = x, foi = exp(est$par))) 
  } 
plot(p~c(1:28), ylim=c(0,1), type="l", col="red", axes=FALSE, xlab=NA, ylab=NA)
# Add right axis and legend
axis(side = 4) 
mtext(side = 4, line = 4, "Prevalence") 
legend("right", legend=c("FoI", "Prevalence"), lty=c(1,1), col=c("black", "red"))

```
#########################################################
# A Log-Spline Model

```{r}
require(splines) 
data(rabbit) 
x = c(1, 4, 8, 12, 18, 24, 30)
# Degrees-of-freedom 
df = 7 
# Construct dummy lm-object 
dl = lm(inf ~ bs(a, df), data = rabbit)
#predict the spline on a log-transformed scale to ensure that the force-of-infection (FoI) is strictly positive
tmpfn = function(x, dl) { 
  x = predict(dl, newdata = data.frame(a = x)) 
  exp(x) 
} 
# calculates the negative log-likelihood of the FoI 
tmpfn2=function(par,data, df){ 
  #Dummy lm-object 
  dl=lm(inf~bs(a,df), data=data) 
  #Overwrite spline coefficients with new values 
  dl$coefficients=par 
  #Calculate log-likelihood 
  ll=0 
  for(i in 1:length(data$a)){ 
    p = 1 - exp(-integrate(tmpfn, 0, i,dl = dl)$value)
    ll=ll+dbinom(data$inf[i],data$n[i],p,log=T)
    } 
  return(-ll) 
} 
# arbitrary initial values and minimize the negative log-likelihood 
para=rep(-1, df +1) 
dspline = optim(par=para, fn=tmpfn2, data=rabbit, df=df, method="Nelder-Mead", control= list(trace=2, maxit=2000))
#Room for two axes
par(mar = c(5,5,2,5))
 #Overwrite dummy-objects coefficients with MLEs
dl$coefficients=dspline$par 
#Age-prevalce plot 
plot(tmpfn(rabbit$a,dl)~rabbit$a, type="l", ylab="FoI", xlab="Age (mos)", las=1)
#Overlay FoI 
par(new=T) 
p = rep(0, 28) 
for (i in 1:28) {
  p[i] = 1 - exp(-integrate(tmpfn, 0, i, dl = dl) $value)
} 
plot(p~c(1:28), ylim=c(0,1), type="l", col="red", axes=FALSE, xlab=NA, ylab=NA) 
axis(side = 4, las=1) 
mtext(side = 4, line = 4, "Prevalence") 
legend("topright", legend=c("FoI", "Prevalence"), lty=c(1,1), col=c("black", "red"))

```
#########################################################
#  Rubella: a relatively mild, vaccine-preventable infection.
#  Age-speciﬁc incidence and cumulative incidence of rubella in Peru 1997–2009.

```{r}
data(peru) 
head(peru)
#Calculate cumulative incidence 
peru$cumulative=cumsum(peru$incidence) 
#Define denominator 
peru$n=sum(peru$incidence) 
#Make room for two axes and plot 
par(mar = c(5,5,2,5)) 
#Plot incidence with cumulative overlaid 
plot(peru$incidence~peru$age, type="b", xlab="Age", ylab="Incidence")
par(new=T) 
plot(peru$cumulative~peru$age, type="l", col="red", axes=FALSE, xlab=NA, ylab=NA) 
axis(side = 4)
mtext(side = 4, line = 4, "Cumulative") 
legend("right", legend=c("Incidence", "Cumulative"), lty=c(1,1), col=c("black", "red"))
#######################################################
# The relative age-speciﬁc FoI of rubella in Peru as estimated using the piecewise-constant model

#Upper age cut-offs 
up=c(1:20,30, 40, 50, 60, 70,100) 
para=rep(.1,length(up)) 
#Inital values 
#Minimize log-likelihood 
est2 = optim(par=log(para),fn=llik.pc, age=peru$age, num=peru$cumulative, denom=peru$n, up=up, method="Nelder-Mead", control= list(trace=2, maxit=2000)) 
#Step plot 
x=c(0, up) 
y=exp(c(est2$par, est2$par[26])) 
plot(x, y, ylab="Relative FoI", xlab="Age", type="l", ylim=c(0,0.25), xlim=c(0,80))


```
#########################################################
