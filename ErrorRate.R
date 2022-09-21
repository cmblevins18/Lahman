library(Lahman)
library(dplyr)

load("/Users/cfay/Research/Lahman-master/data/Fielding.RData")
rf <- select(Fielding,playerID, yearID, POS, G, PO, A, E)
rf2 <- mutate(rf,playerID, yearID, POS, G, PO, A, E, rfact=(PO+A)/G, rfacta=(PO+A-E)/G, Chance=PO+A+E, E.rate=E/Chance)

png(file="EvA.png")
plot(x=rf2$Chance,y=rf2$E, xlab="Chances", ylab="Errors", main="Errors vs Chances all players all positions")

rfOF<-filter(rf2,G>100,POS=="OF")
png(file="EvA-OF.png")
plot(x=rfOF$Chance,y=rfOF$E, xlab="Chances", ylab="Errors", main="Errors vs Chances all players OF >100 Games")

# Lets look at all 2B with greater than 100 games in a season

rf2B<-filter(rf2,G>100,POS=="2B")
png(file="EvA-2B.png")
plot(x=rf2B$Chance,y=rf2B$E, xlab="Chances", ylab="Errors", main="Errors vs Chances all players 2B >100 Games")

# Find a regression line relating the number of chances to errors - sort of an expectedr number of errors per chance

x<-rf2B$Chance
y<-rf2B$E
plot(x=x,y=y, xlab="Chances", ylab="Errors", main="Errors vs Chances all players 2B >100 Games")
relation<-lm(y~x)
relation<-lm(formula = y~x)
print(relation)
summary(relation)

png(file="EvA-2B-linear.png")
plot(x=x,y=y, xlab="Chances", ylab="Errors", main="Errors vs Chances all players 2B >100 Games",col="blue",abline(lm(y~x)),cex=1.3,pch=16)
dev.off()

#Find the difference between the errors committed and the expected errors

error.res=resid(relation)
rf2Bres<-cbind(rf2B,error.res)
print(rf2Bres[order(rf2Bres$error.res), ] )