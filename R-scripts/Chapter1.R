#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files

kicker <- read.csv("Data/FieldGoals2003to2006.csv",header=T)

attach(kicker)

#Figure 1.1 on page 2
plot(kicker$FGtM1,kicker$FGt,
main="Unadjusted Correlation = -0.139",
xlab="Field Goal Percentage in Year t-1",ylab="Field Goal Percentage in Year t")

#p-values on page 3
fit.1 <- lm(FGt~FGtM1 +Name +FGtM1:Name,data=kicker)
anova(fit.1)

#slope and interecepts of lines in Figure 1.2 on page 3
fit.2 <- lm(FGt ~ Name + FGtM1,data=kicker)
fit.2

#Figure 1.2 on page 3
plot(kicker$FGtM1,kicker$FGt,
main="Slope of each line = -0.504",
xlab="Field Goal Percentage in Year t-1",
ylab="Field Goal Percentage in Year t")
tt <- seq(60,100,length=1001)
slope.piece <- summary(fit.2)$coef[20]*tt
lines(tt,summary(fit.2)$coef[1]+slope.piece,lty=2)
for (i in 2:19)
{lines(tt,summary(fit.2)$coef[1]+summary(fit.2)$coef[i]+slope.piece,lty=2)}

detach(kicker)


circulation <- read.table("Data/circulation.txt", header=TRUE, sep="\t")
attach(circulation)

#Dimensions in data:
circ_dim <- 
  dim(circulation)

names(circulation)

#how many zeros and ones on the dummy
  #using the table command
table(circulation$Tabloid.with.a.Serious.Competitor)

#Figure 1.3 on page 5
plot(Weekday,Sunday,xlab="Weekday Circulation",ylab="Sunday Circulation",
pch=Tabloid.with.a.Serious.Competitor+1,col=Tabloid.with.a.Serious.Competitor+1)
legend(110000, 1600000,legend=c("0","1"),
pch=1:2,col=1:2,title="Tabloid dummy variable")

#Figure 1.4 on page 5
plot(log(Weekday),log(Sunday),xlab="log(Weekday Circulation)",ylab="log(Sunday Circulation)",
pch=Tabloid.with.a.Serious.Competitor+1,
col=Tabloid.with.a.Serious.Competitor+1)
legend(11.6, 14.1,legend=c("0","1"),pch=1:2,col=1:2,
title="Tabloid dummy variable")

#Compare Sunday circulation to Weekday circ
sumstats <- 
  list(list(mean(Weekday),median(Weekday),sd(Weekday)),list(mean(Sunday),median(Sunday),sd(Sunday)))
boxplot(Weekday,Sunday,names=c("Weekday","Sunday"))

t.test(Sunday,Weekday,alternative = 'greater',paired = TRUE)

detach(circulation)


nyc <- read.csv("Data/nyc.csv",header=TRUE)
attach(nyc)

#Figure 1.5 on page 7
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(Price~Food+Decor+Service,data=nyc,gap=0.4,
cex.labels=1.5,diag.panel = panel.hist)

library(dplyr)

e_price <- 
  nyc$Price[East==1]
ne_price <- 
  nyc$Price[East==0]
summary(nyc$Price[East==1])
summary(nyc$Price[East==0])

boxplot(e_price,ne_price,names=c('east','not east'))
boxplot(nyc$Price~nyc$Decor)


t.test(e_price,ne_price)


#Figure 1.6 on page 10
boxplot(Price~East,ylab="Price",
xlab="East (1 = East of Fifth Avenue)")

detach(nyc)


Bordeaux <- read.csv("Data/Bordeaux.csv", header=TRUE)
attach(Bordeaux)

#Figure 1.7 on page 10
pairs(Price~ParkerPoints+CoatesPoints,data=Bordeaux,gap=0.4,cex.labels=1.5)

#Figure 1.8 on page 11
par(mfrow=c(2,3))
boxplot(Price~P95andAbove,ylab="Price",xlab="P95andAbove")
boxplot(Price~FirstGrowth,ylab="Price",xlab="First Growth")
boxplot(Price~CultWine,ylab="Price",xlab="Cult Wine")
boxplot(Price~Pomerol,ylab="Price",xlab="Pomerol")
boxplot(Price~VintageSuperstar,ylab="Price",xlab="Vintage Superstar")

#Figure 1.9 on page 12
par(mfrow=c(1,1))
pairs(log(Price)~log(ParkerPoints)+log(CoatesPoints),data=Bordeaux,gap=0.4,cex.labels=1.5)

#Figure 1.10 on page 13
par(mfrow=c(2,3))
boxplot(log(Price)~P95andAbove,ylab="log(Price)",
xlab="P95andAbove")
boxplot(log(Price)~FirstGrowth,ylab="log(Price)",
xlab="First Growth")
boxplot(log(Price)~CultWine,ylab="log(Price)",
xlab="Cult Wine")
boxplot(log(Price)~Pomerol,ylab="log(Price)",
xlab="Pomerol")
boxplot(log(Price)~VintageSuperstar,ylab="log(Price)",
xlab="Vintage Superstar")

detach(Bordeaux)

