## Simultaneous Example

rm(list=ls())

d <- 
  read.csv("data/Toluca.csv")

#a. A linear model seems okay
plot(d$Size,d$Hours)

#b. Find OLS regression
out <- lm(Hours~Size,data=d);summary(out)

#c. Find confint
confint(out,"confidence",level=.95)
confint(out,"confidence",level=.9)


#i.
confint(out,level = 1-.1/2)

#j.
data=data.frame(Size=c(30,65,100))
prediction <- 
  predict(out,data,interval="confidence",se.fit=T,level=.9)

ses <- 
  sqrt(12.4^2+prediction$se.fit^2)

W <- sqrt(2*qf(1-.1,2,23))

predict(out,data,interval = "confidence",level=.9)

data=data.frame(Size=c(80,100))
predict(out,data,se.fit=T)

W=sqrt(2*qf(1-.05,2,23))
t=qt(1-.05/4,23)



#Prediction interval



# question 2 ----
d <- read.csv("Data/Charles.csv")
plot(d$Work.Units,d$Cost)

out <- 
  lm(Cost~Work.Units-1,data=d)

plot(d$Work.Units,d$Cost)
abline(out)

#the relationship looks pretty good

confint(out)

data=data.frame(Work.Units=d$Work.Units)

predict(out,data,level=.9,interval = 'confidence',se.fit=T)
