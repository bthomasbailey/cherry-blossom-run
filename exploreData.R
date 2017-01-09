setwd("C:/MyGitRepos/cherry-blossom-run/Data")

library(RColorBrewer)

#List functions available in the package
ls("package:RColorBrewer")

display.brewer.all()
Purples8 <- brewer.pal(9, "Purples")[8]

#Make Purples8 more transparent
Purples8A <- paste(Purples8, "14", sep = "")

#Produce smooth density representation of scatter plot with smoothScatter()
smoothScatter(y = cbMen$runTime, x = cbMen$age, 
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")

###Create subgroups of runners by age
#First cut out records of runners who don't have an age or are 15 or younger or
#who have unrealistically low run times
cbMenSub <- cbMen[cbMen$runTime > 30 & !is.na(cbMen$age) & cbMen$age > 15, ]
ageCat <- cut(cbMenSub$age, breaks = c(seq(15, 75, 10), 90))
table(ageCat)

plot(cbMenSub$runTime ~ ageCat, xlab = "Age (years)", ylab = "Run Time (minutes)")


###Try simple linear model of runTime vs. age
lmAge <- lm(runTime ~ age, data = cbMenSub)
summary(lmAge)

#plot residuals of linear model against age and add horizontal line at 0 to see if there is any
#curvature in residuals
smoothScatter(x = cbMenSub$age, y = lmAge$residuals, xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

#Use loess() function to further examine residuals
resid.lo <- loess(resids ~ age, 
                  data = data.frame(resids = residuals(lmAge),
                                    age = cbMenSub$age))

#Use resid.lo variable with predict.loess() function (we just have to call predict()
#and it will automatically call predict.loess())
age20to80 <- 20:80
resid.lo.pr <- predict(resid.lo, newdata = data.frame(age = age20to80))

#Add resid.lo.pr line to original plot, and see that linear model is not
#appropriate b/c it underestimates run times for runners over 60 years old
lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 3, lty = 2)


###Try piecewise linear model by taking local weighted averages of time as age varies
#use loess function
menRes.lo <- loess(runTime ~ age, cbMenSub)
menRes.lo.pr <- predict(menRes.lo, data.frame(age = age20to80))