library(alr4)

attach(caution)


d <- caution




d <- read.csv("data/manager.csv")

out <- lm()
plot(d$income,stdres(out))

# 3.	Use the Example of menu pricing in a new Italian Restaurant. Data: nyc.cvs (from the book website)
# a.	Find the least square line with all the 4 predictors: Food, Décor, Service and East.
# b.	Look at the residual plot.  Does the plot suggest any violation of assumptions?
#   c.	Do you think we should keep Service in the model, with all other three variables already in? Carry-out a t-test.
# d.	Look at the Added Variable Plot. What can you conclude?
#   e.	What’s your final model?


d <- read.csv("data/nyc.csv")
out1 <- 
  lm(Price~Food+Decor+Service+East,data=d)
summary(out1)


car::avPlot(out1,"East")


out2 <- 
  lm(Price~Food+Decor+East,data=d)

anova(out2,out1)





d <- read.table("data/defects.txt",header=T)
pairs(d[,2:5])


out1 <- 
  lm(Defective~Rate+Density+Temperature,data=d)
car::inverseResponsePlot(out1,lambda=seq(0,10,length=100))

box_out <- MASS::boxcox(out1)
box_out$x[box_out$y==max(box_out$y)]

out2 <- 
  lm(I(sqrt(Defective))~Rate+Density+Temperature,data=d)

out3 <- 
  lm(I(sqrt(Defective))~Density+Temperature,data=d)



d <- read.csv("data/magazines.csv")
out1 <- 
  lm(AdRevenue~NewsRevenue+SubRevenue+AdPages,data=d)
summary(out1)

car::powerTransform(d[,3:5])

d$logNewsRevenue <- log(d$NewsRevenue)
d$logSubRevenue <- log(d$SubRevenue)
d$logAdPages <- log(d$AdPages)

out3 <- lm(AdRevenue~logNewsRevenue+logSubRevenue+logAdPages,data=d)
car::inverseResponsePlot(out3)



out2 <- 
  lm(I(log(AdRevenue))~I(log(NewsRevenue))+I(log(SubRevenue))+I(log(AdPages)),data=d)

out4 <- 
  lm(I(log(AdRevenue))~I(log(SubRevenue))+I(log(AdPages)),data=d)

car::avPlot(out2,"I(log(NewsRevenue))")



summary(out2)
plot(out2)
