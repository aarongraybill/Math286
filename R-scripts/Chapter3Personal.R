rm(list=ls())


d <- read.csv("Data/TV.csv")
attach(d)

plot(d$X..of.people.per.TV.set,d$Life.expectancy.in.years)
plot(log(d$X..of.people.per.TV.set),d$Life.expectancy.in.years)

out1 <- 
  lm(Life.expectancy.in.years~X..of.people.per.TV.set,data=d)
summary(out1)

out2 <- 
  lm(Life.expectancy.in.years~log(X..of.people.per.TV.set),data=d)
summary(out2)

out3 <- 
  lm(Life.expectancy.in.years~log(1/X..of.people.per.TV.set),data=d)
summary(out3)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(d[,2:3],diag.panel = panel.hist)


d <- 
  read.table("data/responsetransformation.txt",header=T)

plot(d$x,d$y)


out1 <- 
  lm(y~x+I(x^2),data=d)

out2 <- 
  lm(y^(1/3)~x,data=d)

car::inverseResponsePlot(out1)
car::inverseResponsePlot(out2)

#to search specific lamda use the following:

temp <- 
  car::inverseResponsePlot(out1,lambda=seq(-10,10,.01))

plot(temp$lambda[2:2002],temp$RSS[2:2002],type="l")

test <- 
  MASS::boxcox(out1,seq(1/3-.1,1/3+.1,.01))

