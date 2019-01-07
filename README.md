# anadatr
Regression analysis
a= read.table ( file= "xxxxxxxxxxxx.csv" , sep= ",", header= TRUE)
View (a)

library (moments)
library(car)
library (vegan)
library(ade4)
library(gclus)
library(ape)
library(FactoMineR)
vif(M)


hist(a$LONG, main="Histogram of LONG",xlab="LONG (deg)", ylab="Frequency")
hist(a$LAT, main="Histogram of LAT",xlab="LAT (deg)", ylab="Frequency")
hist(a$C3, main="Histogram of C3",xlab="C3 (relative abundance)", ylab="Frequency")

skewness(a$LONG, na.rm = FALSE)

kurtosis(a$LONG, na.rm = FALSE)
curvilinear relationships
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(~LONG+LAT+LatLong, data=a,
      lower.panel=panel.smooth, upper.panel=panel.cor,
      pch=20, main="Correlation between LONG LAT and LatLong")
      
      attach(a)
M = lm(a$LC3_new~a$LAT+a$LONG+a$LatLong)
library(gvlma)
p= gvlma(M)
summary(p)
M
summary(M)
attach(a)
M1 = lm(a$LC3_new~a$LAT+a$LONG)  (just LAT)
library(gvlma)
p= gvlma(M1)
summary(p)
M1
summary(M1)

vif (M1)

par(mfrow=c(2,2))
plot(M1)

anova (M1)

M3<-glm(b$Pres~b$Pcov+b$El+b$dist, data=b, family=binomial)
library(gvlma)
p= gvlma(M3)
summary(p)
M3


vif (M3)

M4<-glm(b$Pres~b$Pcov, data=b, family=binomial)
library(gvlma)
p= gvlma(M4)
summary(p)
M4 
residuals(M4, "deviance")
hist(residuals(M4))
residuals(M4, "pearson")
hist(residuals(M4, "pearson"))

///NMDS
dev.new(title="NMDS on molluscs species - Percentage difference")
plot(moll.nmds, display="sites", type="t", main=paste("NMDS/Percentage difference - Stress =",round(moll.nmds$stress,3))) 





