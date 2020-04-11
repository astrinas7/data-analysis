NBA.data<-read.table("data.txt")

names(NBA.data)<-c("name","heigth","num.match","time.min","age","points","assist","rebound","per.in.area","per.free.throws")

library(sjPlot)

tab_df(NBA.data)

#elenxw ton tupo thas kathe metavliths

is.numeric(NBA.data$name)
length(NBA.data$name)
mode(NBA.data$name)
class(NBA.data$name)
is.character(NBA.data$name)
is.factor(NBA.data$name)

is.numeric(NBA.data$heigth)
length(NBA.data$heigth)
mode(NBA.data$heigth)
class(NBA.data$heigth)



is.numeric(NBA.data$num.match)
length(NBA.data$num.match)
mode(NBA.data$num.match)
class(NBA.data$num.match)

is.numeric(NBA.data$time.min)
length(NBA.data$time.min)
mode(NBA.data$time.min)
class(NBA.data$time.min)

is.numeric(NBA.data$age)
length(NBA.data$age)
mode(NBA.data$age)
class(NBA.data$age)

is.numeric(NBA.data$points)
length(NBA.data$points)
mode(NBA.data$points)
class(NBA.data$points)

is.numeric(NBA.data$assist)
length(NBA.data$assist)
mode(NBA.data$assist)
class(NBA.data$assist)

is.numeric(NBA.data$rebound)
length(NBA.data$rebound)
mode(NBA.data$rebound)
class(NBA.data$rebound)

is.numeric(NBA.data$per.in.area)
length(NBA.data$per.in.area)
mode(NBA.data$per.in.area)
class(NBA.data$per.in.area)

###vriskw vasika perigrafika metra

tab_df(summary(NBA.data[,-1]))## συνολικά

tab_df(t(t(summary(NBA.data[,-1]))[,c(-2,-3,-5)]))#perigrafika mono gia meso min kai max

library(psych)
describe(NBA.data[,-1])

par(mfrow=c(2,2))

boxplot(NBA.data[,2],ylim=range(NBA.data[,2]),main="boxplot for heigth")
boxplot(NBA.data[,3],ylim=range(NBA.data[,3]),main="boxplot for num.match")
boxplot(NBA.data[,4],ylim=range(NBA.data[,4]),main="boxplot for time.min")
boxplot(NBA.data[,5],ylim=range(NBA.data[,5]),main="boxplot for age")
 
par(mfrow=c(2,2))
 
boxplot(NBA.data[,6],ylim=range(NBA.data[,6]),main="boxplot for points")
boxplot(NBA.data[,7],ylim=range(NBA.data[,7]),main="boxplot for assist")
boxplot(NBA.data[,8],ylim=range(NBA.data[,8]),main="boxplot for rebound")
boxplot(NBA.data[,9],ylim=range(NBA.data[,9]),main="boxplot for per.in.area")


###istograma metavlhtwn

par(mfrow=c(2,2))

hist(NBA.data[,2],ylim = c(0,35),main = "Ιστόγραμμα για ύψος",xlab = "Υψος",ylab="Συχνότητα",las=1,col="dark blue")
hist(NBA.data[,3],ylim = c(0,40),main = "Ιστόγραμμα για αριθμό αγώνων",xlab = "Αριθμός αγώνων",ylab="Συχνότητα",las=1,col="dark blue")
hist(NBA.data$time.min,ylim = c(0,25),main = "Ιστόγραμμα αγωνιστικών λεπτών",xlab = "Χρόνος σε λεπτά",ylab="Συχνότητα",col="dark blue")
hist(NBA.data$per.free.throws,main = "Ιστόγραμμα ελευθέρων βολών",xlab = "Ελεύθερες βολές",ylab="Συχνότητα",col="dark blue",ylim = c(0,50),las=1)

par(mfrow=c(2,2))

hist(NBA.data$points,main = "Ιστόγραμμα για πόντους",xlab = "Πόντοι",ylab="Συχνότητα",las=1,ylim=c(0,35),col="dark blue")
hist(NBA.data$assist,main="Ιστόγραμμα για assist",xlab = "asssist",ylab="Συχνότητα",col="dark blue")
hist(NBA.data$rebound,main="Ιστόγραμμα για rebound",xlab="rebound",ylab="Συχνότητα",ylim=c(0,35),col="dark blue")
hist(NBA.data$per.in.area,main = "Ιστόγραμμα ποσοστού εντός τριπόντου",xlab="ποσοστού εντός τριπόντου",ylab="Συχνότητα",ylim=c(0,50),col="dark blue")



par(mfrow=c(1,2))
boxplot(NBA.data[,10],ylim=range(NBA.data[,10]),main="boxplot for per.free.throws")
hist(NBA.data$age,main="Ιστόγραμμα για ηλικία",xlab="Ηλικία",ylim=c(0,25),col="dark blue")


par(mfrow=c(1,2))

qqnorm(NBA.data$heigth,main="QQ plot for heigth")
qqline(NBA.data$heigth)

qqnorm(NBA.data$num.match,main="QQ plot for number of much")
qqline(NBA.data$num.match)

qqnorm(NBA.data$time.min,main="QQ plot for time at min")
qqline(NBA.data$time.min)

qqnorm(NBA.data$age,main="QQ plot for age")
qqline(NBA.data$age)

qqnorm(NBA.data$points,main="QQ plot for points")
qqline(NBA.data$points)

qqnorm(NBA.data$assist,main="QQ plot for assist")
qqline(NBA.data$assist)

qqnorm(NBA.data$rebound,main="QQ plot for rebound")
qqline(NBA.data$rebound)

qqnorm(NBA.data$per.in.area,main="QQ plot for per.in.area")
qqline(NBA.data$per.in.area)

par(mfrow=c(1,1))
qqnorm(NBA.data$per.free.throws,main="QQ plot for per.free.throws")
qqline(NBA.data$per.free.throws)

classage<-cut(NBA.data$age,quantile(NBA.data$age),include.lowest = T,)
classage

classheight<-cut(NBA.data$heigth,quantile(NBA.data$heigth),include.lowest = T)
classheight

par(mfrow=c(1,2))
barplot(table(classage),main = "Ραβδόγραμμα για ομαδοποιημένες ηλικίες",xlab = "ομαδοποιημένες ηλικίες",ylab="Συχνότητα",col = "red")
barplot(table(classheight),ylim=c(0,35),xlab = "ομαδοποιημένα ύψη",ylab = "Συχνότητα",col="red",main = "Ραβδόγραμμα για ομαδοποιημένα ύψη")

###boxplot ana 2 mia katigorikh age kai mia posostikh
par(mfrow=c(2,2))

boxplot(NBA.data$num.match~classage,xlab="clasess of age",main="number of matches~age")

boxplot(NBA.data$time.min~classage,xlab="clasess of age",main="time at min ~ age")

boxplot(NBA.data$points~classage,xlab="clasess of age",main="points ~ age")

boxplot(NBA.data$assist~classage,xlab="clasess of age",main="assist ~ age")

par(mfrow=c(1,3))

boxplot(NBA.data$rebound~classage,xlab="clasess of age",main="rebound~ age")

boxplot(NBA.data$per.in.area~classage,xlab="clasess of age",main="per.in.area~ age")

boxplot(NBA.data$per.free.throws~classage,xlab="clasess of age",main="per.free.throws~ age")

dev.new()
pairs(NBA.data)

###pinakas susxetisewn
install.packages("corrplot")

r<-cor(NBA.data[2:9],use = "pairwise.complete.obs")
par(mfrow=c(1,1))
dev.new()
require(corrplot)
corrplot(r,type = "lower",order = "AOE",method="ellipse",addCoef.col = "black")

##elenxoi kanonikothtas

shapiro.test(NBA.data$heigth) ##P-value<0.05 aporiptw Ho ara

shapiro.test(NBA.data$num.match) ## P value<0.05 aporiptw Ho

shapiro.test(NBA.data$time.min) ## P value<0.05 aporiptw Ho

shapiro.test(NBA.data$age) ## P value<0.05 aporiptw Ho

shapiro.test(NBA.data$points) ## P value<0.05 aporiptw Ho

shapiro.test(NBA.data$assist) ## P value<0.05 apor Ho

shapiro.test(NBA.data$rebound) ## P value<0.05 apor Ho

shapiro.test(NBA.data$per.in.area) ## P value>0.05 DEN APORIPTW Ho

shapiro.test(NBA.data$per.free.throws) ## P value<0.05 aporiptw Ho


#######σχεσεις ανα δυο############
install.packages("FSA")
library(FSA)

library(car)
classage
is.factor(classage)
#------------------------------------pontoi-----------------------------------
anova1<-aov(NBA.data$points~classage,data = NBA.data)
summary(anova1)


### gia kanonikotita kataloipwn
shapiro.test(anova1$res)#aporiptw ho p-value<0.05
#ara aporiptw ipothesi kanonikotitas kataloipwn 

## ARA KRUSKAL TEST

kruskal.test(NBA.data$points~classage,data = NBA.data)

#aporiptw ho
#yparxei diafora stis diamesous gia thn metavlhth pontoi analoga me thn hlikia

dunnTest(NBA.data$points~classage,data = NBA.data,method = "bonferroni") 

## ara stis ilikiakes omades (27,30] kai [22,25] parathritai diafora stis diammesous

##allos tropos
pairwise.wilcox.test(NBA.data$points,classage,p.adjust.method = "bonferroni")
##diafora se [22,25]-(27,30]

#------------------------------------assist------------------------------------
anova2<-aov(NBA.data$assist~classage,data=NBA.data)
summary(anova2)

#gia kanonikotita kataloipwn
shapiro.test(anova2$res)### aporiptw ho p-value<0.05
##aporiptw ipothsi kanonikotitas katalipwn

kruskal.test(NBA.data$assist~classage,data = NBA.data)##aporiptw ho
#yparxei diafora stis diamesous tvn metablhtwn mas assist kai classage

dunnTest(NBA.data$assist~classage,data = NBA.data,method = "bonferroni") 
#diafora stois (27,30]-(30,37] kai (27,30]-[22,25]

##allos tropos
pairwise.wilcox.test(NBA.data$assist,classage,p.adjust.method = "bonferroni")
#idia diafora


#----------------------------------rebound-------------------------------------

anova3<-aov(NBA.data$rebound~classage,data=NBA.data)
summary(anova3)

#gia kanonikotita kataloipwn
shapiro.test(anova3$res)### aporiptw ho p-value<0.05
##aporiptw ipothsi kanonikotitas kataloipwn

kruskal.test(NBA.data$rebound~classage,data = NBA.data)##aporiptw ho
#yparxei diafora stis diamesous ton metavlitwn rebound kai classage

dunnTest(NBA.data$rebound~classage,data = NBA.data,method = "bonferroni")
#diafora stis ilikiakes omades (27,30]-[22,25]

#allos tropos
pairwise.wilcox.test(NBA.data$rebound,classage,p.adjust.method = "bonferroni")
#diafora stis (27,30]-(30,37] kai [22,25]-(27,30]


#-------------------------------per.in.area------------------------------------

anova4<-aov(NBA.data$per.in.area~classage,data=NBA.data)
summary(anova4)

#gia kanonikotita kataloipwn
shapiro.test(anova4$res)###DEN aporiptw ho p-value>0.05 (pvalue=0.3031)
##DEN aporiptw ipothsi  kanonikotitas kataloipwn

###gia diakimanseis
leveneTest(anova4)##DEN aporiptw ho
###ara den aporiptw ipothesi iswn diakimansewn

#apo anova den aporiptw ho ara den uparxei diafora sthn meses times twn ilikiakwn 
#omadwn gia to pososto kaliathiwn entos tripontou

#----------------------------per.free.throws-----------------------------------

anova5<-aov(NBA.data$per.free.throws~classage,data=NBA.data)
summary(anova4)

#gia kanonikotita kataloipwn
shapiro.test(anova5$res)##aporiptw ho p-value>0.05 
##aporiptw ipothsi  kanonikotitas kataloipwn

kruskal.test(NBA.data$per.free.throws~classage,data = NBA.data)
##uparxei diafora stis diamesous

dunnTest(NBA.data$per.free.throws~classage,data = NBA.data,method = "bonferroni")

pairwise.wilcox.test(NBA.data$per.free.throws,classage,p.adjust.method = "bonferroni")
##diafora se [22,25]-(27,30] kai (25,27]-(27,30]


#-------------------------------time.min----------------------------------------
anova6<-aov(NBA.data$time.min~classage,data=NBA.data)
summary(anova6)

#gia kanonikotita kataloipwn
shapiro.test(anova6$res)###DEN aporiptw ho p-value>0.05 (ORIAKA P-VALUE=0.0542)
##DEN aporiptw ipothsi omoskedastikotitas


###gia diakimanseis
leveneTest(anova6)##DEN aporiptw ho
###ara den aporiptw ipothesi iswn diakimansewn


TukeyHSD(anova6)
#diafora gia (27,30]-[22,25] kai (30,37]-(27,30]

#------------------------------num.much------------------------

anova7<-aov(NBA.data$num.match~classage,data=NBA.data)
summary(anova7)
#gia kanonikotita kataloipwn 
shapiro.test(anova7$residuals)#aporiptw ho
#aporoiptw ipothesi kanonikotitas kataloipwn
kruskal.test(NBA.data$num.match~classage,data = NBA.data)
#uparxei diafora stis diamesous
dunnTest(NBA.data$num.match~classage,data=NBA.data)
pairwise.wilcox.test(NBA.data$num.match,classage,data=NBA.data)
#diafora gia (27,30]-[22,25]

####grammiko#######
sjt.corr(NBA.data[,2:10],corr.method = "pearson",triangle = "lower",digits = 2)
sjt.corr(NBA.data[,2:10],corr.method = "spearman",triangle = "lower",digits = 2)
sjt.corr(NBA.data[,2:10],corr.method = "kendall",triangle = "lower",digits = 2)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(NBA.data[,2:10],histogram = T,pch=19)


lm1<-lm(NBA.data$time.min ~ NBA.data$heigth + NBA.data$num.match + NBA.data$age + 
          NBA.data$points + NBA.data$assist + NBA.data$rebound + NBA.data$per.in.area +
          NBA.data$per.free.throws, data= NBA.data)

summary(lm1)

shapiro.test(rstandard(lm1))

leveneTest(rstandard(lm1))

stp1<-step(lm1,birection="both")

summary(stp1)

plot(lm1)

vif(stp1)

##kentropoihmenes metavlites
point.centered<-NBA.data$points-mean(NBA.data$points)

rebound.centered<-NBA.data$rebound-mean(NBA.data$rebound)

assist.centered<-NBA.data$assist-mean(NBA.data$assist)

heigth.centered<-NBA.data$heigth-mean(NBA.data$heigth)

num.much.centered<-NBA.data$num.match-mean(NBA.data$num.match)

age.centered<-NBA.data$age-mean(NBA.data$age)

area.centered<-NBA.data$per.in.area-mean(NBA.data$per.in.area)

frthr.centered<-NBA.data$per.free.throws-mean(NBA.data$per.free.throws)

lm2<-lm(NBA.data$time.min ~ point.centered + rebound.centered + assist.centered
        +heigth.centered + num.much.centered + age.centered +area.centered + frthr.centered)

summary(lm2)

stp2<-step(lm2,direction = "both")

summary(stp2)

shapiro.test(rstandard(stp2))

plot(stp2)

vif(stp2)
