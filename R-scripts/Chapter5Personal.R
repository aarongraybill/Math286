d <- read.csv("data/MEDDICORP4.csv")
attach(d)
out1 <- lm(SALES~ADV+BONUS,data=d)


data_in <- 
  data.frame(ADV=c(50000),BONUS=c(25000))

predict(out1,data_in,interval="confidence",se.fit=T)
predict(out1,data_in,interval="predict",se.fit=T)

# i.	Use the reduced model to predict the average sales for companies who spend $50,000 in advertising and $25,000 in bonus. Find the 95% confidence interval for your prediction.
# j.	Use the reduced model to predict the sales for a company who spend $50,000 in advertising and $25,000 in bonus. Find the 95% prediction interval for your prediction.



# 2.	A company that provides transportation services uses a telemarketing 
# division to help sell its services. The division manager is interested 
# in the time spent on the phone by the telemarketers in the division. 
# Data on the number of months of employment and the number of calls paced 
#per day (an average for 20 working days) is recorded for 20 employees. 
#The data are in the file: TELEMARK5.csv
# a.	Look at the scatter plot of the data. Put the least-square line on the scatter plot. What’s the R2 of the regression that describes the relationship between CALLS and MONTHS? Get the residual plot for the regression that describes the relationship between CALLS and MONTHS.
# b.	Get the relationship that describes the CALLS and MONTH. (polynomial)
# c.	Test whether the second order is significant or not at 5% level of significance.
# d.	Analyze the residual plot for your new model.


d <- read.csv("data/TELEMARK5.csv")
plot(d$MONTHS,d$CALLS)
abline(lm(CALLS~MONTHS,data=d))

out1 <- 
  lm(CALLS~MONTHS+I(MONTHS^2),data=d)

summary(out1)

d <- 
  read.csv("data/HeightWeight.csv")


# They are the same
out1 <- 
  lm(Weight~Height,data=d)

out2 <- 
  lm(Weight~Height+Female,data=d)

summary(out1)
summary(out2)

out3 <- 
  lm(Weight~Height*Female,data=d)

anova(out3,out1)

plot(d$Height,d$Weight)


