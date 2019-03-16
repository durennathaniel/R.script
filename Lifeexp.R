#Reads in the table.

life <- read.table(file="https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/5%20-%20LifeExpectancy/Data/LifeExpectancy2.txt", header=TRUE, stringsAsFactors=FALSE)

#Changes the Group class into factors, a categorical class.

life$Group <- factor(life$Group, levels=c("other", "africa", "oecd"))

#Plots the relationship between Per-Person GDP on the logarithmic scale and Life Expectancy.  OECD countries are blue dots,
#africa countries are red dots, and other countries are yellow dots.

categories <- interaction(life$Group)
colors <- c("yellow", "red", "blue")
plot(log(life$PPGDP), life$LifeExp, xlab="Log Per Person GDP", ylab="Life Expectancy", pch=19, col=colors[categories])
legend("topleft", legend=c("other", "africa", "oecd"), col=c("yellow", "red", "blue"), lty=1, lwd=1)

#Defines which rows of the table belong to which Group category.

other.life <- life[which(life$Group == "other"),]
africa.life <- life[which(life$Group == "africa"),]
oecd.life <- life[which(life$Group == "oecd"),]

#Performs regression on three different data sets for the three different Group categories of "other",
#"africa", and "oecd."

colnames(life)
other.fm <- lm(LifeExp ~ log(PPGDP), data=other.life)
africa.fm <- lm(LifeExp ~ log(PPGDP), data=africa.life)
oecd.fm <- lm(LifeExp ~ log(PPGDP), data=oecd.life)

#This looks at the correlation between Life Expectancy and Per-Person GDP for 
#every Group.

c(other = coef(other.fm)[2], africa=coef(africa.fm)[2], oecd=coef(oecd.fm)[2])
cor(life$LifeExp, life$PPGDP)
cor(other.life$LifeExp, other.life$PPGDP)
cor(africa.life$LifeExp, africa.life$PPGDP)
cor(oecd.life$LifeExp, oecd.life$PPGDP)

#This is a regression model assuming that there is an interaction between 
#Group and the logarithm of Per-Person GDP.  In other words, we are 
#assuming that the average amount Life Expectancy rises when 
#the logarithm of Per-Person GDP rises changes depending on the Group.

full.fm <- lm(LifeExp ~ log(PPGDP)*Group, data=life)

#Here we are testing are conditions for multiple linear regression.  They are met.

res <- residuals(full.fm)
fitted <- predict.lm(full.fm)
hist(res, xlab="Residuals", main="Histogram")
plot(fitted, res, xlab="Fitted Values for Life Exp", ylab="Residuals", pch=19)
abline(h=0, lwd=3)
co <- coef(full.fm)
sd(res)

#In conclusion, the data tends to show that Life Expectancy will rise more quickly
#for African countries and other countries as the logarithm of Per-Person GDP rises than for 
#OECD countries, but Life Expectancy in general is higher for OECD countries regardless.
#Further tests of significance need to be undergone.  

abline(a=(co[1] + co[3]), b=(co[2] + co[5]), col="red")
abline(a=co[1], b=co[2], col="yellow")
abline(a=(co[1] + co[4]), b=(co[2] + co[6]), col="blue")










