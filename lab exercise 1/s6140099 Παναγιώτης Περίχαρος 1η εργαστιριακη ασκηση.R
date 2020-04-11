

###ERWTHMA A,B
#orizw to parakatw pinaka me to onoma x kai onomata sthlwn baros, ypsos, fylo, kai ta antistoixa onomata apo thn  entolh row.names=...

baros<-c(70,63,80,90,84,52,89,95,98,58)
ypsos<-c(1.71,1.67,1.69,1.80,2.01,1.60,1.73,1.82,1.91,1.63)
fylo<-ordered(c(0,0,1,1,1,0,1,1,1,0),labels=c("female","male"))
x<-data.frame(baros,ypsos,fylo,row.names = c("helen","kate","john","peter","george","maria","tom","andrew","david","sophie"))
x

###ERWTHMA C
#dhmiourgw monadiaio dianisma me length=10, to prosthetw mw thn entolh cbind kai to onomazw X
mon<-rep(1,10)
X<-cbind(mon,x)
X

###ERWTHMA D
#dhmhourgw dianisma y me  tis times ths metablhths baros kai aferw tis times tou baros apo to X
y<-x$baros
X<-X[-2]

###ERWTHMA E,F
#orizw ton tupo twn metavlhtwn mou kai orizw ta katalhla epipeda gia thn katigorikh mou metavlith 1= female 2=male
is.numeric(X$mon)
length(X$mon)
mode(X$mon)
class(X$mon)

is.numeric(X$ypsos)
length(X$ypsos)
mode(X$ypsos)
class(X$ypsos)

is.numeric(y)
length(y)
mode(y)
class(y)

length(X$fylo)
is.numeric(X$fylo)
is.character(X$fylo)
is.factor(X$fylo)
as.numeric(X$fylo)-1
mode(X$fylo)
class(X$fylo)

###ERWTHMA G
#vriskw periliptika perigrafika metra gia tis 3 metavlites
summary(x)

###ERWTHMA H 
#gia thn baros dimiourgoume nea katigorikh metavlith me 3 katigories me A=(50,65] , B=(65,80] ,C=(80,100] xrisimopoiwntas thn entolh cut 
baros2<-cut(x$baros,breaks = c(50,65,80,100),labels = c("A","B","C"))
baros2

###ERWTHMA I
#kataskeuazw pinaka sinafeias gia tis dio katigorikes metavlites fylo kai baros2 kai kataskeuazw ta antistoixa rabdogrammata
xtabs(~fylo+baros2)

par(mfrow=c(1,2))
barplot(table(x$fylo))
barplot(table(baros2))

###ERWTHMA J
tab<-xtabs(~fylo+baros2)

#sunolikes sxetikes suxnothtes
prop.table(tab)

#pithanothtes kata gramh
round(prop.table(tab,1),2)

#pithanothtes kata sthlh
prop.table(tab,2)

