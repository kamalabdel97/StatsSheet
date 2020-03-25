library(ggplot2)

#Simulate Perfect positive correlation
n <- 100; m1 <- 8; m2 <- 2; s1 <- 1;s2 <- 3;p <- 1

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

PerfPosCor <- data.frame(x, y)

##################################################################
#Simulate Slightly Postive Correlation

n <- 100; m1 <-8; m2 <- 2; s1 <- 1;s2 <- 3;p <- .5

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

SlightPosCor <- data.frame(x, y)

##################################################################
#Simulate No Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p<- 0

x<- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

NoCor <- data.frame(x, y)

##################################################################
#Simulate Slightly Negative Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p<- -.5

x<- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

SlightNegCor <- data.frame(x, y)

##################################################################
#Simulate Perfect Negative Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p <- -1

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

PerfNegCor <- data.frame(x, y)

