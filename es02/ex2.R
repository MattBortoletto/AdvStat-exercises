## 0 ##
xn <- seq(-10,10,0.1)
d_norm <- dnorm(x,mean=0,sd=2)
p_norm <- pnorm(x,0,2)
plot(xn, d_norm,
     type = "l", # Line plot
     main = "pdf of the normal distribution",
     xlab = "x")
plot(xn, p_norm,
     type = "l", # Line plot
     main = "cdf of the normal distribution",
     xlab = "x")

xp <- seq(-100,100,1)
d_pois <- dpois(xp,lambda = 40)
p_pois <- ppois(xp,lambda = 40)
plot(xp, d_pois,
     type = "l", # Line plot
     main = "pdf of the Poisson distribution",
     xlab = "x")
plot(xp, p_pois,
     type = "l", # Line plot
     main = "cdf of the Poisson distribution",
     xlab = "x")

xg <- seq(0,100,1)
d_geom <- dgeom(xg,prob=0.3)
p_geom <- pgeom(xg,prob=0.3)
plot(xg, d_geom,
     type = "l", # Line plot
     main = "pdf of the geometric distribution",
     xlab = "x")
plot(xg, p_geom,
     type = "l", # Line plot
     main = "cdf of the geometric distribution",
     xlab = "x")

xe <- seq(0,100,1)
d_exp <- dexp(xe,rate=0.2)
p_exp <- pexp(xe,rate=0.2)
plot(xe, d_exp,
     type = "l", # Line plot
     main = "pdf of the exponential distribution",
     xlab = "x")
plot(xe, p_exp,
     type = "l", # Line plot
     main = "cdf of the exponential distribution",
     xlab = "x")

xb <- seq(0,200,1)
d_bin <- dbinom(xb,size=80,prob=0.3)
p_bin <- pbinom(xb,size=80,prob=0.3)
plot(xb, d_bin,
     type = "l", # Line plot
     main = "pdf of the binomial distribution",
     xlab = "x")
plot(xb, p_bin,
     type = "l", # Line plot
     main = "cdf of the binomial distribution",
     xlab= "x")

## 1 ##
x <- c(15.58, 15.9, 16, 16.1, 16.2)
p1 <- c(0.15, 0.21, 0.35, 0.15, 0.14)
p2 <- c(0.14, 0.05, 0.64, 0.08, 0.09)
df <- data.frame(x, p1, p2)
m1 <- sum(df[,1]*df[2])
m2 <- sum(df[,1]*df[3])
v1 <- sum(df[,1]**2*df[,2]) - m1**2
v2 <- sum(df[,1]**2*df[,3]) - m2**2
paste("Method 1. E[X] =", m1, "Var(X) =", v1)
paste("Method 2. E[X] =", m2, "Var(X) =", v2)

## 2 ##
# a)
m <- 30 
r <- 1/30
people <- 50
x <- seq(0,120,10)
wait_t <- rexp(people,rate=r)
hist(wait_t, breaks = x, xlab="wait time")

# b)
min <- 10
p_less10 <- pexp(x[2], r)
paste("The probability that a person waits for less than 10 minutes is", p_less10)

# c)
m <- mean(wait_t)
paste("The mean is", p_less10)

# d)
p_more60 <- 1-pexp(x[7], r)
paste("The probability that a person waits for more than 1 hour is", p_more60)

## 3 ##
typo <- 1/3
prob_atleastonetypo <- 1 - ppois(0,lambda = typo)
paste("The probability that there is at least one typo on a specific page is", prob_atleastonetypo)
xp <- seq(0,7,1)
d_pois <- dpois(xp,lambda = typo)
p_pois <- ppois(xp,lambda = typo)
plot(xp, d_pois,
     type = "b",
     main = "pdf of the Poisson distribution",
     xlab = "errors in a page",
     ylab = "probability")
plot(xp, p_pois,
     type = "b", 
     main = "cdf of the Poisson distribution",
     xlab = "error in a page",
     ylab = "probability")

## 4 ##
cards <- 52
p_ace <- 4/52
draws <- 10
pr <- pbinom(1,10,p_ace)
paste("The probability that at least 10 draws are needed to find an ace is", pr)

## 5 ## 
# 
sindaci <- read.csv2("sindaciincarica.csv", skip=2)
head(sindaci)

#
sex <- table(sindaci$sesso)
barplot(sex)

#
town_prov <- table(sindaci$codice_provincia)
town_prov
barplot(town_prov, 
        xlab = "Province ID", 
        ylab = "Number of cities",
        main = "Number of cities per province",
        col = "darkolivegreen3",
        density = 70)

town_reg <- table(sindaci$codice_regione)
town_reg
barplot(town_reg, 
        xlab = "Region ID", 
        ylab = "Number of cities",
        main = "Number of cities per region",
        col = "darkorchid3",
        density = 70)

#
sindaci_t <- format(as.Date(sindaci$data_nascita, format="%d/%m/%Y"),"%Y")
birth <- table(sindaci_t)
barplot(birth, 
        xlab = "Year", 
        ylab = "Number of mayors born",
        main = "Distribution of the age (years only) of the mayors",
        col = "deepskyblue3",
        density = 70)

#
library(lubridate)
sindaci_ch <- as.POSIXct(sindaci$data_entrata_in_carica, format="%d/%m/%Y")
sindaci_ch
today <- as.POSIXct(Sys.Date(), tz = "CEST")
format(today, format="%Y-%m-%d")
today
#sindaci_ch <- as.numeric(sindaci$data_entrata_in_carica, format="%d/%m/%Y")
#charge_time <- as.numeric(difftime(today(),sindaci$data_entrata_in_carica))
charge_time <- difftime(today, sindaci_ch,units = "auto", format = )
charge_time
chtime <- table(charge_time)
barplot(chtime, 
        xlab = "Time in charge", 
        ylab = "Number of mayors",
        main = "Distribution of the mayors' time in charge",
        col = "plum3",
        density = 70)
