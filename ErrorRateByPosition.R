

rf <- select(Fielding,playerID, yearID, POS, G, PO, A, E)
rf2 <- mutate(rf,playerID, yearID, POS, G, PO, A, E, rfact=(PO+A)/G, rfacta=(PO+A-E)/G, Chance=PO+A+E, E.r
              ate=E/Chance)

#Choose Position and make a variable based on the position select number of games
GG=82
ppos="OF"
str1<-"rf"
dfname<-paste(str1,ppos,sep="")
#filter the rangefactor by position and number of games
assign(dfname,filter(rf2,G>GG,POS==ppos))
#print(get(dfname))

x<-get(dfname)$Chance
y<-get(dfname)$E
graphlabel<-paste("Errors vs Chances all players",ppos,">",GG,"Games",sep=" ")
plot(x=x,y=y, xlab="Chances", ylab="Errors", main=graphlabel)
relation<-lm(y~x)
relation<-lm(formula = y~x)
print(relation)
summary(relation)

graphlabel<-paste("Errors vs Chances all players",ppos,">",GG,"Games",sep=" ")
graphname<-paste("EvA",ppos,"linear.png",sep="-")
png(file=graphname)
plot(x=x,y=y, xlab="Chances", ylab="Errors", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
dev.off()

#dfnameres <- paste(get(dfname),"res",sep=".")
error.res=resid(relation) #error residue == difference between linear model and number of errors
residues<-cbind(get(dfname),E.res=error.res)
print(residues[order(residues$E.res), ] )
pE<-c(residues$error.res/residues$Chance) #What percent of chances is the error residue
residues<-cbind(residues,pE)

