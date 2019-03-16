life <- read.table(file="https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/5%20-%20LifeExpectancy/Data/LifeExpectancy2.txt", header=TRUE, stringsAsFactors=FALSE)
life$Group <- factor(life$Group, levels=c("other", "africa", "oecd"))
categories <- interaction(life$Group)
colors <- c("yellow", "red", "blue")
plot(log(life$PPGDP), life$LifeExp, xlab="Log Per Person GDP", ylab="Life Expectancy", pch=19, col=colors[categories])
legend("topleft", legend=c("other", "africa", "oecd"), col=c("yellow", "red", "blue"), lty=1, lwd=1)
other.life <- life[which(life$Group == "other"),]
africa.life <- life[which(life$Group == "africa"),]
oecd.life <- life[which(life$Group == "oecd"),]
colnames(life)
other.fm <- lm(LifeExp ~ log(PPGDP), data=other.life)
africa.fm <- lm(LifeExp ~ log(PPGDP), data=africa.life)
oecd.fm <- lm(LifeExp ~ log(PPGDP), data=oecd.life)
c(other = coef(other.fm)[2], africa=coef(africa.fm)[2], oecd=coef(oecd.fm)[2])
cor(life$LifeExp, life$PPGDP)
cor(other.life$LifeExp, other.life$PPGDP)
cor(africa.life$LifeExp, africa.life$PPGDP)
cor(oecd.life$LifeExp, oecd.life$PPGDP)
full.fm <- lm(LifeExp ~ log(PPGDP)*Group, data=life)
res <- residuals(full.fm)
fitted <- predict.lm(full.fm)
hist(res, xlab="Residuals", main="Histogram")
plot(fitted, res, xlab="Fitted Values for Life Exp", ylab="Residuals", pch=19)
abline(h=0, lwd=3)
co <- coef(full.fm)
sd(res)
abline(a=(co[1] + co[3]), b=(co[2] + co[5]), col="red")
abline(a=co[1], b=co[2], col="yellow")
abline(a=(co[1] + co[4]), b=(co[2] + co[6]), col="blue")










