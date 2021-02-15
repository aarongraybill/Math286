d <- 
  read.csv("Data/FieldGoals2003to2006.csv")
#Compute dimensions:
obs <- 
  dim(d)[1]

col <- 
  dim(d)[2]


#Plot t-1 against t
cor(d$FGtM1,d$FGt)
#wow, it's negative, is that significant?
cor.test(d$FGtM1,d$FGt)
#no, not really

#let's visualize:
plot(d$FGtM1,d$FGt,
     main = 
       paste0("Unadjusted Correlation: ",round(cor(d$FGtM1,d$FGt),4)),
     xlab = "FG % t-1",
     ylab = "FG % t"
     )

# Let's test for significance
mean_FG <- mean(d$FGtM1)
sd_FG <- sd(d$FGAtM1)

t_test_FG <- 
  t.test(d$FGAtM1,mu=81)

# so it's not significantly different than 81,
# but is it significantly greater?

t.test(d$FGAtM1,mu=81,alternative = "greater")

# How many kickers are there?
n_kickers <- length(unique(d$Name))

#but being more sophisticated
players <- 
  as.factor(d$Name)
n_kickers <- 
  nlevels(players)

#obs per player:
table(players)

#Regression with Dummy Variables
fit.1 <- lm(FGt~FGtM1+Name+FGtM1:Name,data = d)

#Do ANOVA on that fit
anova.1 <- 
  anova(fit.1)


#Pull the desired Coefficient:
fit.2 <- lm(FGt~Name+FGtM1,data = d)
test <- fit.2$coef[["FGtM1"]]



