d <-  read.table("Data/salarygov.txt")


# a. I conclude non-linearity
par(mfrow=c(1,2))
plot(density(d$Score))
plot(density(d$MaxSalary))

par(mfrow=c(1,1))
plot(d$Score,d$MaxSalary)

#b.
#sqrt seems to be appropriate for x
plot(density(sqrt(d$Score)))

#c.
out1 <- 
  lm(d$MaxSalary~I(sqrt(d$Score)))
car::inverseResponsePlot(out1)
#r seems to think -1/5

#inverse response
out1 <- 
  lm(d$MaxSalary^(-1/5)~I(sqrt(d$Score)))

out2 <- 
  lm(log(d$MaxSalary)~I(sqrt(d$Score)))

out3 <- 
  lm(I(log(d$MaxSalary)^(1/4))~d$Score)

out4 <- 
  lm(log(d$MaxSalary)~d$Score)

out4 <- 
  lm(d$MaxSalary~d$Score)

plot(out3)





