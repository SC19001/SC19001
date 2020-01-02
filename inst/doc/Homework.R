## ----mtcars--------------------------------------------------------------
summary(mtcars)

## ------------------------------------------------------------------------
knitr::kable(mtcars[,1:7])

## ------------------------------------------------------------------------
boxplot(mpg ~ cyl,data = mtcars,
        main="Car Mileage Data",
        xlab = "Number of cylinders",
        ylab = "Miles/(US) gallon")

## ----states--------------------------------------------------------------
states <- as.data.frame(state.x77[,c("Population","Murder","Illiteracy","Income","Frost")])
summary(states)

## ------------------------------------------------------------------------
knitr::kable(states)

## ------------------------------------------------------------------------
lm.D1 <- lm(Income~Illiteracy,data = states)
summary(lm.D1)$coef

## ------------------------------------------------------------------------
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(lm.D1)

## ----sunspotData---------------------------------------------------------
y <-1700:1988
sunspotData <- data.frame(y,sunspot.year)
summary(sunspotData)

## ------------------------------------------------------------------------
knitr::kable(sunspotData[1:20,])

## ------------------------------------------------------------------------
par(mar=c(1,1,1,1))
par(mar=c(5,4,2,0.1) + 0.1)
plot(y,sunspot.year,type = "b",lty=1,pch=21,bg="red",
     main = "Yearly Sunspot Data, 1700-1988")

## ------------------------------------------------------------------------
n <- 1000
set.seed(1235)
u <- runif(n)
sigma <- 1
x <- sigma*sqrt(-2*log(u)) # F(x) = 1-exp(-x^2/(2*sigma^2))
hist(x, prob = TRUE, breaks = seq(0, 4, .1),
     main = expression(f(x)== x/(sigma^2)*e(-x^2/(2*sigma^2))),
     col = "yellow"
     ) 
y <- seq(0, 4, .01)
lines(y, y/(sigma^2)*exp(-y^2/(2*sigma^2)),col = "red")

## ------------------------------------------------------------------------
n <- 1000
set.seed(1234)
u <- runif(n)
sigma <- 5
x <- sigma*sqrt(-2*log(u)) # F(x) = 1-exp(-x^2/(2*sigma^2))
hist(x, prob = TRUE, breaks =  seq(0, 20, .4),
     main = expression(f(x)== x/(sigma^2)*e(-x^2/(2*sigma^2))),
     col = "yellow"
) 
y <- seq(0, 20, .01)
lines(y, y/(sigma^2)*exp(-y^2/(2*sigma^2)),col = "red")

## ------------------------------------------------------------------------
n <- 1000
set.seed(1235)
u <- runif(n)
sigma <- 10
x <- sigma*sqrt(-2*log(u)) # F(x) = 1-exp(-x^2/(2*sigma^2))
hist(x, prob = TRUE, breaks = seq(0,40, 1),
     main = expression(f(x)== x/(sigma^2)*e(-x^2/(2*sigma^2))),
     col = "yellow"
) 
y <- seq(0,40, .01)
lines(y, y/(sigma^2)*exp(-y^2/(2*sigma^2)),col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.75
p2 <- 1 - p1
set.seed(1234)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 8, .2),
     main = expression(f(x)== (0.75/sqrt(2*pi))*e(-x^2/2)+
                         (0.25/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 8, .01)
lines(y, 0.75/sqrt(2*pi)*exp(-y^2/2)+
        0.25/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.1
p2 <- 1 - p1
set.seed(8)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 10, .25),
     main = expression(f(x)== (0.1/sqrt(2*pi))*e(-x^2/2)+
                         (0.9/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 10, .01)
lines(y, 0.1/sqrt(2*pi)*exp(-y^2/2)+
        0.9/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.3
p2 <- 1 - p1
set.seed(6)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 10, .3),
     main = expression(f(x)== (0.3/sqrt(2*pi))*e(-x^2/2)+
                         (0.7/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 10, .01)
lines(y, 0.3/sqrt(2*pi)*exp(-y^2/2)+
        0.7/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.5
p2 <- 1 - p1
set.seed(12)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 8, .3),
     main = expression(f(x)== (0.5/sqrt(2*pi))*e(-x^2/2)+
                         (0.5/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 8, .01)
lines(y, 0.5/sqrt(2*pi)*exp(-y^2/2)+
        0.5/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.6
p2 <- 1 - p1
set.seed(1234)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 8, .3),
     main = expression(f(x)== (0.6/sqrt(2*pi))*e(-x^2/2)+
                         (0.4/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 8, .01)
lines(y, 0.6/sqrt(2*pi)*exp(-y^2/2)+
        0.4/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.8
p2 <- 1 - p1
set.seed(19)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 6, .15),
     main = expression(f(x)== (0.8/sqrt(2*pi))*e(-x^2/2)+
                         (0.2/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 6, .2)
lines(y, 0.8/sqrt(2*pi)*exp(-y^2/2)+
        0.2/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
n <- 1000
p1 <- 0.9
p2 <- 1 - p1
set.seed(5)
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,3,1)
r <- sample(c(1,0),n,replace=TRUE,prob = c(p1,p2))
Z1 <- r*X1+(1-r)*X2
hist(Z1, prob = TRUE, breaks = seq(-4, 6, .25),
     main = expression(f(x)== (0.9/sqrt(2*pi))*e(-x^2/2)+
                         (0.1/sqrt(2*pi))*e(-(x-3)^2/2)),
     col = "blue"
) 
y <- seq(-4, 6, .01)
lines(y, 0.9/sqrt(2*pi)*exp(-y^2/2)+
        0.1/sqrt(2*pi)*exp(-(y-3)^2/2),
      col = "red")

## ------------------------------------------------------------------------
set.seed(1234)
fun <- function(d,n){
  T = matrix(0,nrow=d,ncol=d)
  for(j in 1:d){ 
    for(i in 1:d){
      if(i>j) {T[i,j] <- rnorm(1)}
      else if(i==j) {T[i,j] <- sqrt(rchisq(1,n-i+1))}
      else {T[i,j]=0}
       } 
  }
  T
}#generate the matrix A
 A = fun(4,6) #d=4,n=6
 sigma=matrix(c(1,0.7,0.3,0.9,
                0.7,1,0.2,0.6,
                0.3,0.2,1,0.5,
                0.9,0.6,0.5,1),nrow = 4)
 L = t(chol(sigma))#Bartlett’s decomposition.
 W = L%*%A%*%t(A)%*%t(L)
 W

## ------------------------------------------------------------------------
m <- 1e4 ; 
set.seed(1234)
t <- runif(m, min = 0, max = pi/3)
theta.hat <- mean(sin(t))*(pi/3)
print(c(theta.hat, cos(0)-cos(pi/3)))

## ------------------------------------------------------------------------
set.seed(1234)
MC.f <- function(t, n = 10000, antithetic = TRUE) {
  p <- runif(n/2,0,t)
  if (!antithetic) q <- runif(n/2) 
  else q <- 1 - p #generate antithetic variables
  p <- c(p, q)
  cdf <- numeric(length(t))
  for (i in 1:length(t)) {
    g <- t[i] * exp(-p )/(1+p^2)
    cdf[i] <- mean(g)
  }
  cdf
}
m <- 1000
MC <- MCa <- numeric(m)
t <- 1
for (i in 1:m) {
  MC[i] <- MC.f(t, n = 1000, anti = FALSE)
  #Use Monte Carlo integration without antithetic variables 
  MCa[i] <- MC.f(t, n = 1000)
  #Use Monte Carlo integration with antithetic variables 
}

## ------------------------------------------------------------------------
mean(MCa)
c(var(MC),var(MCa),var(MC)-var(MCa))

## ------------------------------------------------------------------------
M <- 10000; k <- 5 
r <- M/k #replicates per stratum
N <- 50 #number of times to repeat the estimation
T <- numeric(k)
S <- numeric(k)
P <- matrix(0, N, 1)
Q <- matrix(0, N, 1)
set.seed(1234)
for (i in 1:N) {
    for(j in 1:k){
        u <- runif(r,min = j-1, max = j)
        #Generate random numbers on 5 intervals Respectively
          x <- -log(1-(1-exp(-1))*u/5)
          #F(x)=5(1-exp(-x))/(1-exp(-1))
          #Generate random samples corresponding to 5 intervals
        g <- function(x)((exp(-x)/(1+x^2))/(exp(-x)/(1-exp(-1))))
        T[j] <- mean(g(x))#Generate the mean of each interval
        S[j] <- sd(g(x))#Generate the standard deviation of each interval
            }
        P[i,1] <- mean(T)
        Q[i,1] <- mean(S)
}
esm <- mean(P)
ess <- mean(Q)
  print(c(esm,ess))

## ------------------------------------------------------------------------
n <- 20
alpha <- .05
m <- 10000
set.seed(123)
xbar <- numeric(m)
es <- numeric(m)
for (i in 1:m) {
   xbar[i] <- mean(rchisq(n, df = 2))
   es[i] <- qt((1-alpha/2),n-1)*sd(rchisq(n, df = 2))/sqrt(n)
}
# The symmetric t-interval to estimate a mean is xbar +(-) t(1-α/2)(n-1)s/sqrt(n)
p <- mean((xbar-es)<2 & 2<(xbar+es))
#Judging  whether the mean of the populations falling within the confidence interval generated by the samples subject to the chi-square distribution, thus obtaining the probability that the confidence interval covers the mean
p

## ------------------------------------------------------------------------
 n<-1000
 v <- xbar <- m3 <- m2 <- numeric(n)
 m <- 1000
 set.seed(12345)
 for (i in 1:n) {
  x <- rnorm(m,mean = 0, sd = sqrt(6/n))
  xbar[i] <- mean(x)
  m3[i] <- mean((x - xbar[i])^3)
  m2[i] <- mean((x - xbar[i])^2)
  v[i] <- m3[i] / ((m2[i])^1.5)
  }
  u <- v[order(v)]
  #Generate quantiles through empirical distribution
  #Produce skewness coefficients from 1000 sets of samples from a normal distribution，and sort the 1000 data in ascending order to get the quantile
  q <- c(0.025,0.05,0.95,0.975)
  gq <- lsq <- f <- ssd <- numeric(4)
  #gq represents the estimated value corresponding to 0.025,0.05 ,0.95,0.975 quantile of the skewness sqrt(b1) under normality by a Monte Carlo experiment. 
  #lsq represents quantiles of the large sample approximation sqrt(b1) ≈ N (0, 6/n).
  #ssd represents the standard error of the estimates from (2.14) using the normal approximation for the density (with exact variance formula)
  for (j in 1:4) {
    gq[j] <- u[n*q[j]]
    lsq[j] <- qnorm(q[j],mean = 0,sd = sqrt(6/n))
    f[j] <- (1/sqrt(2*pi*(6/n)))*exp(-gq[j]^2/(2*(6/n)))
    ssd[j] <- sqrt(q[j]*(1-q[j])/(n*f[j]^2))
    #ssd=sqrt(q*(1-q)/(n*f(xq)^2))
  }
  comparison <- data.frame(gq,lsq,ssd)
  knitr::kable(comparison)
  

## ------------------------------------------------------------------------
alpha <- 20
n <- c(10,20,30,50,100,500) #sample sizes
p.reject <- p.heavy <- cv <- xbar <-ybar <- m31 <- m32 <- 
  m21 <- m22 <- u <- v <- numeric(length(n)) 
#to store sim. results 
m <- 10000 #num. repl. each sim.
sktests <- heavy <- numeric(m) #test decisions 
set.seed(12345)
for (i in 1:length(n)) {
  for (j in 1:m) {
    cv[i] <- qnorm(.975, 0,sqrt(6*(n[i]-2) / ((n[i]+1)*(n[i]+3))))  
    #crit. values for each n
    x <- rbeta(n[i],alpha,alpha)
    y <- rt(n[i],2)
    xbar[i] <- mean(x)
    ybar[i] <- mean(y)
    m31[i] <- mean((x - xbar[i])^3)
    m32[i] <- mean((y - ybar[i])^3)
    m21[i] <- mean((x - xbar[i])^2)
    m22[i] <- mean((y - ybar[i])^2)
    u[i] <- m31[i] / ((m21[i])^1.5)
    v[i] <- m32[i] / ((m22[i])^1.5)
    sktests[j] <- as.integer(abs(u[i])>= cv[i] ) 
    heavy[j] <- as.integer(abs(v[i])>= cv[i] ) 
  }
  p.reject[i] <- mean(sktests) #proportion rejected 
  p.heavy[i] <- mean(heavy)
}
comparison <- data.frame(n,p.reject, p.heavy)
knitr::kable(comparison)

## ------------------------------------------------------------------------
alpha <- 20
n <- c(1000,1500,2000) #sample sizes
p.reject <- p.heavy <- cv <- xbar <-ybar <- m31 <- m32 <- 
  m21 <- m22 <- u <- v <- numeric(length(n)) 
#to store sim. results 
m <- 10000 #num. repl. each sim.
sktests <- heavy <- numeric(m) #test decisions 
set.seed(1234)
for (i in 1:length(n)) {
  for (j in 1:m) {
    cv[i] <- qnorm(.975, 0, sqrt(6/n[i]))   
    #crit. values for each n
    x <- rbeta(n[i],alpha,alpha)
    y <- rt(n[i],2)
    xbar[i] <- mean(x)
    ybar[i] <- mean(y)
    m31[i] <- mean((x - xbar[i])^3)
    m32[i] <- mean((y - ybar[i])^3)
    m21[i] <- mean((x - xbar[i])^2)
    m22[i] <- mean((y - ybar[i])^2)
    u[i] <- m31[i] / ((m21[i])^1.5)
    v[i] <- m32[i] / ((m22[i])^1.5)
    sktests[j] <- as.integer(abs(u[i])>= cv[i] ) 
    heavy[j] <- as.integer(abs(v[i])>= cv[i] ) 
  }
  p.reject[i] <- mean(sktests) #proportion rejected 
  p.heavy[i] <- mean(heavy)
}
comparison <- data.frame(n,p.reject, p.heavy)
knitr::kable(comparison)

## ------------------------------------------------------------------------
alpha1 <- 4 
alpha2 <- 10
n <- 300
m <- 1500
set.seed(1234)
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05)) 
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv <- qnorm(0.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
for (j in 1:N) { #for each epsilon 
  e <- epsilon[j]
sktests <- xbar <- m3 <- m2 <- sk <- numeric(m)
for (i in 1:m) { #for each replicate
alpha <- sample(c(alpha1, alpha2), replace = TRUE, 
                size = n, prob = c(1-e, e))
x <- rbeta(n, alpha, alpha)
xbar[i] <- mean(x)
m3[i] <- mean((x-xbar[i])^3)
m2[i] <- mean((x-xbar[i])^2)
sk[i] <- m3[i] / ((m2[i])^1.5)
sktests[i] <- as.integer(abs(sk[i]) >= cv) }
        pwr[j] <- mean(sktests)
}
#plot power vs epsilon 
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon)) 
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors 
lines(epsilon, pwr+se, lty = 3,col = "red")
lines(epsilon, pwr-se, lty = 3,col = "red")

## ------------------------------------------------------------------------
n1 <- 4 
n2 <- 40
n <- 300
m <- 1500
set.seed(1234)
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05)) 
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv <- qnorm(0.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
for (j in 1:N) { #for each epsilon 
  e <- epsilon[j]
sktests <- xbar <- m3 <- m2 <- sk <- numeric(m)
for (i in 1:m) { #for each replicate
nn <- sample(c(n1, n2), replace = TRUE, 
                size = n, prob = c(1-e, e))
x <- rt(n, nn)
xbar[i] <- mean(x)
m3[i] <- mean((x-xbar[i])^3)
m2[i] <- mean((x-xbar[i])^2)
sk[i] <- m3[i] / ((m2[i])^1.5)
sktests[i] <- as.integer(abs(sk[i]) >= cv) }
        pwr[j] <- mean(sktests)
}
#plot power vs epsilon 
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon)) 
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors 
lines(epsilon, pwr+se, lty = 3,col = "red")
lines(epsilon, pwr-se, lty = 3,col = "red")

## ------------------------------------------------------------------------
n <- 200 
alpha <- c(0.01, 0.05, 0.1)
#Investigating whether the empirical Type I er- ror rate of the t-test is approximately equal to the nominal significance level at different nominal level α,  when the sampled population is non-normal.
mu01 <- mu02 <- mu03 <- 1 
#All of the means of the three distributions are 1
m <- 10000 #number of replicates
p1 <- p2 <- p3 <- matrix(0,3,m)  #storage for p-values
p1.hat <- p2.hat <- p3.hat <- numeric(3) 
#storage for the observed Type I error rate
se1.hat <- se2.hat <- se3.hat <- numeric(3)
#storage for the standard error of the estimate
set.seed(1234)
for (i in 1:3) {
  for (j in 1:m) {
    x <- rchisq(n,1)
    y <- runif(n,min = 0, max = 2)
    z <- rexp(n,1)
    ttest1 <- t.test(x, alternative = "two.side", mu = mu01)
    ttest2 <- t.test(y, alternative = "two.side", mu = mu02)
    ttest3 <- t.test(z, alternative = "two.side", mu = mu03)
    p1[i,j] <- ttest1$p.value
    p2[i,j] <- ttest2$p.value
    p3[i,j] <- ttest3$p.value
  }
p1.hat[i] <- mean(p1[i,] <= alpha[i])
se1.hat[i] <- sqrt(p1.hat[i] * (1 - p1.hat[i]) / m) 
p2.hat[i] <- mean(p2[i,] <= alpha[i])
se2.hat[i] <- sqrt(p2.hat[i] * (1 - p2.hat[i]) / m) 
p3.hat[i] <- mean(p3[i,] <= alpha[i])
se3.hat[i] <- sqrt(p3.hat[i] * (1 - p3.hat[i]) / m) 
}
Sampled.population <- c("chisq(1)", "Uniform(0,2)", "exp(1)",
                        "chisq(1)", "Uniform(0,2)", "exp(1)",
                        "chisq(1)", "Uniform(0,2)", "exp(1)")
Nominal.level <- c(alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3])
Observed.t1e <- c(p1.hat[1], p2.hat[1], p3.hat[1],
                  p1.hat[2], p2.hat[2], p3.hat[2],
                  p1.hat[3], p2.hat[3], p3.hat[3])
Estimate.se <- c(se1.hat[1], se2.hat[1], se3.hat[1],
                 se1.hat[2], se2.hat[2], se3.hat[2],
                 se1.hat[3], se2.hat[3], se3.hat[3])
result <- data.frame(Sampled.population, Nominal.level, Observed.t1e, Estimate.se)
knitr::kable(result) 

## ------------------------------------------------------------------------
library(bootstrap)
library(boot)
s <- original <- bias <- se <- numeric(4)
par(mar=c(1,1,1,1))
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,4))
for (i in 1:4) {
  for (j in (i+1):5) {
    plot(scor[[i]],scor[[j]],"p",
         xlab = colnames(scor)[i], ylab = colnames(scor)[j], col = "blue")
  }
}
par(opar)
cor(scor) #generate the sample correlation matrix. 
p <- c(1,3,3,4)
q <- c(2,4,5,5)
for (k in 1:4) {
b.cor <- function(scor,i) cor(scor[i,p[k]],scor[i,q[k]])
 obj <- boot(data = scor, statistic = b.cor, R = 2000) 
 round(c(original[k] <- obj$t0, bias[k] <- mean(obj$t) - obj$t0, se[k] <- sd(obj$t)),3)
}
estimates <- c("mec~vec", "alg~ana", "alg~sta","ana~sta")
result <- data.frame(estimates,original, bias, se)
knitr::kable (result)

## ------------------------------------------------------------------------
skewness <- function(x,i) {
  #computes the sample skewness coeff.
  xbar <- mean(x[i])
  m3 <- mean((x[i] - xbar)^3)
  m2 <- mean((x[i] - xbar)^2)
  return( m3 / m2^1.5 )
}
s <- 0
n <- 20
m <- 1000
set.seed(1234)
library(boot)
nor.norm <- nor.basic <- nor.perc <- matrix(0, m, 2)
for (i in 1:m) {
  data.nor <- rnorm(n, 0, 4)
  nor.ske <- boot(data.nor, statistic = skewness, R=1000)
  nor <- boot.ci(nor.ske, type=c("norm","basic","perc"))
  nor.norm[i,] <- nor$norm[2:3]
  nor.basic[i,] <- nor$basic[4:5]
  nor.perc[i,] <- nor$percent[4:5]
}
#Calculate the coverage probability of a normal distribution
    norm1 <- mean(nor.norm[,1] <= s & nor.norm[,2] >= s)
    basic1 <- mean(nor.basic[,1] <= s & nor.basic[,2] >= s)
    perc1 <- mean(nor.perc[,1] <= s & nor.perc[,2] >= s)
#Calculate the probability of the left side of the normal distribution
    norm1.left <- mean(nor.norm[,1] >= s )
    basic1.left <- mean(nor.basic[,1] >= s )
    perc1.left <- mean(nor.perc[,1] >=s )
#Calculate the right side probability of a normal distribution
    norm1.right <- mean(nor.norm[,2] <= s )
    basic1.right <- mean(nor.basic[,2] <= s )
    perc1.right <- mean(nor.perc[,2] <= s)
s<-sqrt(8/5)
n<-20
m<-1000
set.seed(12345)
library(boot)
chi.norm<-chi.basic<-chi.perc<-matrix(0, m, 2)
for (i in 1:m) {
  data.chisq<-rchisq(n,5)
  chisq.ske<-boot(data.chisq,statistic=skewness, R=1000)
  chi<- boot.ci(chisq.ske,type=c("norm","basic","perc"))
  chi.norm[i,]<-chi$norm[2:3];
  chi.basic[i,]<-chi$basic[4:5];
  chi.perc[i,]<-chi$percent[4:5];
}
#Calculate the coverage probability of the chi-square distribution
    norm2 <- mean(chi.norm[,1] <= s & chi.norm[,2] >= s)
    basic2 <- mean(chi.basic[,1] <= s & chi.basic[,2] >= s)
    perc2 <- mean(chi.perc[,1] <= s & chi.perc[,2] >= s)
#Calculate the probability of the left side of the chi-square distribution
    norm2.left <- mean(chi.norm[,1] >= s )
    basic2.left <-mean(chi.basic[,1] >= s )
    perc2.left <- mean(chi.perc[,1] >=s )
#Calculate the right side probability of the chi-square distribution
    norm2.right <- mean(chi.norm[,2] <= s )
    basic2.right <- mean(chi.basic[,2] <= s )
    perc2.right <- mean(chi.perc[,2] <= s)
Distribution <- c("N(0,16)","chisq(5")
Type <- c("basic", "norm", "perc")
Left <- c(norm1.left, norm2.left, basic1.left, 
          basic2.left, perc1.left, perc2.left)
Right <- c(norm1.right, norm2.right, basic1.right, 
          basic2.right, perc1.right, perc2.right)
P.coverage <- c(norm1, norm2, basic1, basic2, perc1, perc2)
result <- data.frame(Distribution, Type, Left, Right, P.coverage)
knitr::kable(result) 

## ------------------------------------------------------------------------
library(bootstrap)
sigma <- cov(scor)
s <- eigen(sigma)
v <- s$values[order(-(s$values))]
theta <- v[1]/sum(v)
#measures the proportion of variance explained by the first principal component of the original sample.
n <- 88
theta.hat <- numeric(n)
for (i in 1:n) {
  scor.jack <- scor[-i, ]
  sigma.hat <- cov(scor.jack)
  ss <- eigen(sigma.hat)
  vv <- ss$values[order(-(ss$values))]
  theta.hat[i] <- vv[1]/sum(vv)
}
bias.jack <- (n-1)*(mean(theta.hat)-theta)
#Obtain the jackknife estimates of bias of θ.hat
se.jack <- sqrt((n-1)*mean((theta.hat-theta)^2))
#Obtain the jackknife estimates of standard error of θ.hat
round(c(original = theta, bias.jack = bias.jack, se.jack = se.jack), 4)

## ------------------------------------------------------------------------
library(DAAG,quietly=TRUE)
attach(ironslag)

## ------------------------------------------------------------------------
n <- length(magnetic)   #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)
yhat1 <- yhat2 <- yhat3 <- yhat4 <- numeric(n)
SSR1 <- SSR2 <- SSR3 <- SSR4 <- numeric(n)
SST1 <- SST2 <- SST3 <- SST4 <- numeric(n)
ybar <-  mean(magnetic)
# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  
  J1 <- lm(y ~ x) #Linear model
  yhat1[k] <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1[k]
  SSR1[k] <- (yhat1[k]-ybar)^2
  SST1[k] <- (magnetic[k]-ybar)^2
  
  J2 <- lm(y ~ x + I(x^2)) #Quadratic model
  yhat2[k] <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2[k]
  SSR2[k] <- (yhat2[k]-ybar)^2
  SST2[k] <- (magnetic[k]-ybar)^2
  
  J3 <- lm(log(y) ~ x) #Exponential model
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3[k] <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3[k]
  SSR3[k] <- (yhat3[k]-ybar)^2
  SST3[k] <- (magnetic[k]-ybar)^2
  
  J4 <- lm(y ~ x + I(x^2) + I(x^3)) #Cubic polynomial model.
  yhat4[k] <- J4$coef[1] + J4$coef[2] * chemical[k] +
  J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3
  e4[k] <- magnetic[k] - yhat4[k]
  SSR4[k] <- (yhat4[k]-ybar)^2
  SST4[k] <- (magnetic[k]-ybar)^2
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ------------------------------------------------------------------------
c(51/50*sum(SSR1)/sum(SST1), 51/49*sum(SSR2)/sum(SST2), 
  51/50*sum(SSR3)/sum(SST3), 51/48*sum(SSR4)/sum(SST4)) 
# R^2=(n-1)/(n-p-1)*SSR/SST  n=52

## ------------------------------------------------------------------------
n1 <- 100; n2 <- 150
set.seed(12345)
m <- 500
count5test <- function(x, y, s) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0) 
return(as.integer(max(c(outx, outy)) > s))
}
x <- rnorm(n1)
y <- rnorm(n2)
s <- 5:15
R <- 100
q <- numeric(R)
alphahat <- pwr <- numeric(length(s))
for (j in 1:length(s)) {
  ss <- s[j]
  alphahat[j] <- count5test(x, y, ss) 
  z <- c(x, y)
  K <- 1:(n1+n2); n<-length(x)
  for (i in 1:R) {
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]; y1 <- z[-k] #complement of x1
  x1 <- x1 - mean(x1) 
  #centered by sample mean 
  y1 <- y1 - mean(y1)
  q[i] <- count5test(x1, y1, ss)
 }
 pwr[j] <- mean(c(alphahat[j], q))
}
plot(s, pwr, col = "red")

## ------------------------------------------------------------------------
library(boot)
library(MASS)
library(Ball)
dCov <- function(x, y) { 
  x <- as.matrix(x);  y <- as.matrix(y)
  n <- nrow(x); m <- nrow(y)
  if (n != m || n < 2) stop("Sample sizes must agree") 
  if (! (all(is.finite(c(x, y)))))
    stop("Data contains missing or infinite values")
  Akl <- function(x) {
    d <- as.matrix(dist(x))
    m <- rowMeans(d); M <- mean(d)
    a <- sweep(d, 1, m); b <- sweep(a, 2, m) 
    b + M
  }
  A <- Akl(x);  B <- Akl(y)
  sqrt(mean(A * B)) 
}
ndCov2 <- function(z, ix, dims) {
  #dims contains dimensions of x and y 
  p <- dims[1]
  q <- dims[2]
  d <- p + q
  x <- z[ , 1:p] #leave x as is
  y <- z[ix, -(1:p)] #permute rows of y 
  return(nrow(z) * dCov(x, y)^2)
}
set.seed(123)
ss <- seq(10, 200, 10) 
N <- 50
P <- matrix(0, nrow = length(ss), ncol = 2)
for(j in 1:N) {
    p <- matrix(0, nrow = length(ss), ncol = 2)
  for(i in 1:length(ss)) {
    # data generate
    n <- ss[i]
    x <- mvrnorm(n, c(0,0), matrix(c(1,0,0,1), nrow = 2))
    e <- mvrnorm(n, c(0,0), matrix(c(1,0,0,1), nrow = 2))
    # Model2
    y <- x / 4 + e
    z <- cbind(x, y)
    # Ball test
    p[i, 1] <- bcov.test(z[ ,1:2], z[ ,3:4],               num.permutations=50, seed=i*j)$p.value
    # permutatin: resampling without replacement
    boot.obj <- boot(data = z, statistic = ndCov2, R = 100, 
                     sim = "permutation", dims = c(2, 2))
    tb <- c(boot.obj$t0, boot.obj$t)
    p[i, 2] <- mean(tb >= tb[1])
  }
  P <- P + (p <= 0.05)
}
P <- P / N
plot(ss, P[, 1], "b", col = "green", ylim = c(0, 1), ylab = "power1", main = "Model1: Power")
lines(ss, P[, 2], "b", col = "red")
legend("topleft", legend = c("Ball", "Cor"), lty = 1, col = c("green", "red"))

## ------------------------------------------------------------------------
P <- matrix(0, nrow = length(ss), ncol = 2)
for(j in 1:N) {
  p <- matrix(0, length(ss), 2)
  for(i in 1:length(ss)) {
    # data generate
    n <- ss[i]
    x <- mvrnorm(n, c(0,0), matrix(c(1,0,0,1), nrow = 2))
    e <- mvrnorm(n, c(0,0), matrix(c(1,0,0,1), nrow = 2))
    # Model2
    y <- x / 4 * e
    z <- cbind(x, y)
    # Ball test
    p[i, 1] <- bcov.test(z[, 1:2], z[, 3:4], 
    num.permutations = 50, seed=i*j)$p.value
    # permutatin: resampling without replacement
    boot.obj <- boot(data = z, statistic = ndCov2, R = 100, 
                     sim = "permutation", dims = c(2, 2))
    tb <- c(boot.obj$t0, boot.obj$t)
    p[i, 2] <- mean(tb >= tb[1])
  }
  P <- P + (p <= 0.05)
}
P <- P / N
plot(ss, P[, 1], "b", col = "green", ylim = c(0, 1), ylab = "power2", main = "Model2: Power")
lines(ss, P[, 2], "b", col = "red")
legend("bottomright", legend = c("Ball", "Cor"), lty = 1, col = c("green", "red"))

## ------------------------------------------------------------------------
 set.seed(12345)
    rw.Metropolis <- function(sigma, x0, N) {
        x <- numeric(N)
        x[1] <- x0
        u <- runif(N)
        k <- 0
        for (i in 2:N) {
            y <- rnorm(1, x[i-1], sigma)
             if (u[i] <= ((0.5*exp(-abs(y))) / (0.5*exp(-abs(x[i-1])))))
        # Target distribution: The density of the standard Laplace distribution is f(x) = 0.5*exp(-abs(x))
                    x[i] <- y  
                else {
                    x[i] <- x[i-1]
                    k <- k + 1
                }
            }
        return(list(x=x, k=k))
    }
    N <- 4000
    sigma <- c(.05, .5, 5,  20)
    x0 <- 10
    rw1 <- rw.Metropolis(sigma[1], x0, N)
    rw2 <- rw.Metropolis(sigma[2], x0, N)
    rw3 <- rw.Metropolis(sigma[3], x0, N)
    rw4 <- rw.Metropolis(sigma[4], x0, N)
    #number of candidate points rejected
    accept.rate <- c(1-(rw1$k)/N, 1-(rw2$k)/N, 1-(rw3$k)/N, 1-(rw4$k)/N)
    accept <- data.frame(sigma = sigma, no.reject=c(rw1$k, rw2$k, rw3$k, rw4$k), accept.rate = accept.rate)
    knitr::kable(accept)

## ------------------------------------------------------------------------
 par(mar=c(1,1,1,1))
 par(mfrow=c(2,2))  #display 4 graphs together
    refline <- c(log(0.05), -log(0.05))
    rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
        abline(h=refline)
    }
    par(mar=c(1,1,1,1))
    par(mfrow=c(1,1)) #reset to default

## ------------------------------------------------------------------------
a <- c(.05, seq(.1, .9, .1), .95)
Q <- numeric(length(a))
    for (i in 1:11) {
      if(i <=6)
      Q[i] <- log(2*a[i])
      else
      Q[i] <- -log(2 - 2*a[i])
    }
#Calculate the corresponding quantile points according to the Laplace distribution function
    rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
    mc <- rw[501:N, ]
    Qrw <- apply(mc, 2, function(x) quantile(x, a))
    qq <- data.frame(round(cbind(Q, Qrw), 3))
    names(qq) <- c('True','sigma=0.05','sigma=0.5','sigma=2','sigma=16')
    knitr::kable(qq)

## ------------------------------------------------------------------------
x <- seq(1, 22, 1)
log(exp(x)) == exp(log(x))

## ------------------------------------------------------------------------
isTRUE(all.equal(log(exp(x)),exp(log(x))))

## ------------------------------------------------------------------------
m <- c(5:25, 100, 500, 1000)
n <- c(4:25, 100)
A <- numeric(24)
B <- numeric(23)
#Exercise 11.4
for (k in m) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}
#Coefficient before integration
g <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2),
lower = sqrt((a^2)*(k-1)/(k-a^2)), upper = Inf)$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), 
lower = sqrt((a^2)*k/(k+1-a^2)), upper = Inf)$value))
}
A[k] <- uniroot(g, lower = 0.01, upper = 1+sqrt(k)/2)$root
}
#Find the intersection points A(k) for k = 4 : 25, 100, 500, 1000
#Exercise 11.5
for (k in n) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}
h <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2), lower = 0, upper = sqrt((a^2)*(k-1)/(k-a^2)))$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), lower = 0, upper = sqrt((a^2)*k/(k+1-a^2)))$value))
}
B[k] <- uniroot(h, lower = 0.01, upper = 1+sqrt(k)/2)$root
}
#Find the intersection points B(k) for k = 4 : 25, 100, 500, 1000
A <- c(NA, A[m])
B <- c(B[n], NA, NA)
k <- c(4:25, 100, 500, 1000)
result <- data.frame(k, A, B)
knitr::kable(result)

## ------------------------------------------------------------------------
nA. <- 28; nB. <- 24; nOO <- 41; nAB <- 70
p <- q <- r <- numeric(100)
p[1] <- 0.2; q[1] <- 0.2; r[1] <- (1- p[1]- q[1])
   #Given initial value of iteration
f <- function(a,b) {
  return((nB.*b/(2-b-2*a)+nB.+nAB)/(nA.*a/(2-a-2*b)+nA.+nAB))
}
g <- function(a,b) {
 return(((1-a/(2-a-2*b))*nA.+(1-b/(2-b-2*a))*nB.+2*nOO)/((nB.*b/(2-b-2*a)+
                          nB.+nAB)))
}
threshold <- 1e-5
#Given the threshold
for (k in 2:100) {
   p[k] <- 1/(1+f(p[k-1],q[k-1])*(1+g(p[k-1],q[k-1])))
   q[k] <- f(p[k-1],q[k-1])/(1+f(p[k-1],q[k-1])*(1+g(p[k-1],q[k-1])))
   r[k] <- 1- p[k] - q[k]
   #Through the theoretical steps of the EM algorithm, the relationship between the iteration value at each step and the previous iteration value is obtained.
   if((p[k]-p[k-1] <= threshold) & (q[k]-q[k-1] <= threshold) &
      (r[k]-r[k-1] <= threshold))
   #If the difference between two iterations of p, q, r is less than a given threshold, stop iteration
       {print(c(k, p[k], q[k],r[k]))
       break
    }
}

## ------------------------------------------------------------------------
x <- seq(1,k,1)
plot(x, p[1:k], "b", col = "red",ylim=c(0,0.6), main = "The log-maximum likelihood values in M-steps" , xlab = "The number of iteration", ylab = "The value of iteration")
lines(x, q[1:k], "b", col = "blue")
lines(x, r[1:k], "b", col = "green")
legend("topright", legend = c("p", "q", "r"),lty = 1, col = c("red", "blue", "green"))

## ------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
v <- numeric(4)
for (i in 1:4)
{
v[i] <- lapply(i,function(i) {lm(formulas[[i]], data = mtcars)})
}
v

## ------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
         rows <- sample(1:nrow(mtcars), rep = TRUE)
         mtcars[rows, ]
})
u <- numeric(10)
for (i in 1:10) {
u[i] <- lapply(bootstraps[i], lm, formula = mpg ~ disp)
}
u

## ------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
v <- numeric(4)
rsq <- function(mod) summary(mod)$r.squared
for (i in 1:4)
{
v[i] <- lapply(i,function(i) {rsq(lm(formulas[[i]], data = mtcars))}
               )
}
v

## ------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
         rows <- sample(1:nrow(mtcars), rep = TRUE)
         mtcars[rows, ]
})
u <- numeric(10)
rsq <- function(mod) summary(mod)$r.squared
for (i in 1:10) {
u[i] <- lapply(i, function(i) rsq(lm(mpg ~ disp, data = bootstraps[[i]])))
}
u

## ------------------------------------------------------------------------
trials <- replicate(
         100,
         t.test(rpois(10, 10), rpois(7, 10)),
         simplify = FALSE
       )
u <- numeric(100)
for (i in 1:100) {
u[i] <- sapply(i, function(i) trials[[i]]$p.value)
}
u

## ------------------------------------------------------------------------
sapply(trials, "[[", 3)

## ------------------------------------------------------------------------
library(parallel)
# mcsapply()
mcsapply<-function(k,f){
cl <- makeCluster(getOption("cl.cores", 4))
result<-parLapply(cl,k,f) 
stopCluster(cl) 
return(unlist(result))
} 
trials <- replicate(
         5000,
         t.test(rpois(12, 22), rpois(4, 34)),
         simplify = FALSE
       )
system.time(mcsapply(trials,function(x) unlist(x)[3]))
system.time(sapply(trials,function(x) unlist(x)[3]))
#The mcsapply function has a significantly higher operating efficiency than sapply function

## ------------------------------------------------------------------------
set.seed(42)
rw_MetropolisR <- function(sigma, x0, N) 
{
  #Metropolis Randomwalk using R
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(-(abs(y) - abs(x[i-1]))))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}

## ------------------------------------------------------------------------
library(Rcpp)
#// This is the rw_MetropolisC.cpp
#include <Rcpp.h>
#using namespace Rcpp;
#// [[Rcpp::export]]
cppFunction('NumericVector rw_MetropolisC(double sigma, double x0, int N) 
{
  //Metropolis Randomwalk using C
  NumericVector x(N);
  x[0] = x0;
  double u, y;
  int k = 0;
  for (int i = 1; i < N; i++) 
  {
    y = rnorm(1, x[i-1], sigma)[0];
    u = runif(1)[0];
    if (u <= exp(-(abs(y) - abs(x[i-1])))) 
    {
      x[i] = y; 
    }
    else 
    {
      x[i] = x[i-1];
      k++;
    }
  }
  return x;
}')

## ------------------------------------------------------------------------
library(microbenchmark)
N = 2000
sigma <- c(0.5,1,10,100)
x0 = 25
for (i in 1:length(sigma)) {
ts = microbenchmark(rwR = rw_MetropolisR(sigma[i], x0, N)$x, 
                    rwC = rw_MetropolisC(sigma[i], x0, N))
print(summary(ts)[, c(1,3,5,6)])
rwR = rw_MetropolisR(sigma[i], x0, N)$x
rwC = rw_MetropolisC(sigma[i], x0, N)
par(mar=c(1,1,1,1))
par(mfrow = c(2, 2))
b <- 1000 #discard the burnin sample
y <- (rwR)[b:N]
a <- ppoints(500)
QR <- ifelse(a <= 1/2, log(2*a), -log(2-2*a)) #quantiles of Laplace
Q1 <- quantile(rwR, a)
qqplot(QR, Q1, main=paste("R  sigma=",sigma[i]) , xlab="Laplace Quantiles", ylab="Sample Quantiles", col = "red")
abline(a=0, b=1)
y <- (rwC)[b:N]
a <- ppoints(500)
QR <- ifelse(a <= 1/2, log(2*a), -log(2-2*a)) #quantiles of Laplace
Q2 <- quantile(rwC, a)
qqplot(QR, Q2, main=paste("C  sigma=",sigma[i]) , xlab="Laplace Quantiles", ylab="Sample Quantiles", col = "red")
abline(a=0, b=1)
qqplot(Q1, Q2, main=paste("C-R  sigma=",sigma[i]) , xlab="C Quantiles", ylab="R Quantiles", col = "blue")
abline(a=0, b=1)
}

