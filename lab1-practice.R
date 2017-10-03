library(DAAG)
str(nihills)
lognihills <- log(nihills)
names(lognihills) <- c("ldist", "lclimb", "ltime", "ltimef")
View(nihills)
library(lattice)
lognihills <- log(nihills)
names(lognihills) <- paste("l", names(nihills), sep="")
lognihills.lm <- lm(ltime ~ ldist + lclimb, data=lognihills)
round(coef(lognihills.lm),3)
nihills$gradient <- with(nihills, climb/dist)
lognihills <- log(nihills)
names(lognihills) <- paste("l", names(nihills), sep="")
lognigrad.lm <- lm(ltime ~ ldist + lgradient, data=lognihills)
round(coef(lognigrad.lm),3)
par(mfrow=c(1,2))
termplot(lognigrad.lm, col.term="gray",partial=TRUE, col.res="black",smooth=panel.smooth)
View(oddbooks)

#the above codes are practice codes
#assignment codes are shown below:

oddbooks$density <- oddbooks$weight/(oddbooks$thick*oddbooks$height*oddbooks$breadth)
View(oddbooks)
pairs(oddbooks)  #as can be seen from the scatterplot matrix
# height and breadth show the strongest relationship
# breadth and weight, thick and height, thick and breadth, height and weight
#also show significant relationship

round(cor(oddbooks), 2)

logoddbooks <- log(oddbooks)
names(logoddbooks) <- paste("l", names(oddbooks), sep="")
logoddbooks.lm <- lm(ltime ~ ldist + lclimb, data=logoddbooks)

logoddbooks.lm <- lm(lweight ~ lthick, data=logoddbooks)
round(coef(logoddbooks.lm),3)
# regression relationships log(weight) on log(thick):
# (Intercept)      lthick 
#   9.692          -1.073 

logoddbooks.lm <- lm(lheight ~ lbreadth, data=logoddbooks)
round(coef(logoddbooks.lm),3)
# regression relationships log(height) and log(breadth):
# (Intercept)    lbreadth 
#    0.834         0.842 

logoddbooks.lm <- lm(lweight ~ lthick + I(0.5*(lheight+lbreadth)), data=logoddbooks)
round(coef(logoddbooks.lm),3)
# regression relationships log(weight) on log(thick) and 0.5*(log(height) + log(breadth))
# (Intercept)     lthick    I(0.5 * (lheight + lbreadth)) 
#   -1.593        0.483               2.191



