## Simultaneous Example

rm(list=ls())

d <- 
  read.csv("data/Toluca.csv")

#a. A linear model seems okay
plot(d$Size,d$Hours)

#b. Find OLS regression
out <- lm(Size~Hours,data=d);summary(out)

#c. Find confint
confint(out,"confidence",level=.95)
confint(out,"confidence",level=.9)


