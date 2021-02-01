library(tree)
library(ISLR)

attach (Carseats)

#pravi se binarna zavisna promenljiva High
High<-ifelse (Sales <=8," No"," Yes ")

Carseats<-data.frame(Carseats ,High)

#funkcije tree kreira stablo
tree.carseats<-tree(High~.-Sales ,Carseats)

#summary ispisuje koje su sve promenljive bile koriscene
#za podelu prostora na regione, broj listova i greske
summary (tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty =0)
#sa grafika je moguce videti kojim redosledom je deljen prostor
#tj. koje promenljive su najbitnije u toj podeli

#izdvaja se skup za trening i skup za test
set.seed(4)
train<-sample (1: nrow(Carseats ), 200)
Carseats.test<-Carseats [-train ,]
High.test<-High[-train ]


tree.carseats<-tree(High~.-Sales ,Carseats ,subset =train )
tree.pred<-predict(tree.carseats ,Carseats.test ,type ="class")
table(tree.pred,High.test)
mean(tree.pred==High.test)
#oko 70.5% je uspesno klasifikovano koristeci celo stablo

#pravi se podstablo
set.seed(4)
cv.carseats<-cv.tree(tree.carseats ,FUN=prune.misclass )

#ispisuje broj listova i odgovarajuce greske (izmedju ostalog)
cv.carseats

par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")


#greska je bila najmanja za stablo sa 10 listova
prune.carseats<-prune.misclass(tree.carseats ,best =10)
plot(prune.carseats)
text(prune.carseats ,pretty =0)

#predikcija pomocu podstabla
tree.pred<-predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
mean(tree.pred==High.test)
#procenat uspeha je porastao na 75% sto pokazuje da
#je bolje koristiti adekvatno krace stablo



##############
#primenjeno na prethodni zadatak
library(rpart)

data<-data.frame(X1,X2,X3,Y)


treefit<-rpart(Y~X1+X2+X3,data = data,method = "class")
plot(treefit)


treefit.pred<-predict(treefit)
#rezultat je matrica koja ima 1 u onoj koloni koja odgovara klasi
#u koju je smestena opservacija iz tog reda


treefitpred<-c()

for(i in 1:nrow(treefit.pred)){
  treefitpred[i]<-as.integer(colnames(treefit.pred)[treefit.pred[i,]==1])  
}

table(treefitpred,Y)
mean(treefitpred==Y)

#test slike

#promenljive ucitane pokretanjem linija iz prethodnog zadatka

treefit.pred.test<-predict(treefit,newdata = data.frame(X1, X2))


treefitpred<-c()

for(i in 1:nrow(treefit.pred.test)){
  treefitpred[i]<-as.integer(colnames(treefit.pred.test)[treefit.pred.test[i,]==1])  
}

table(treefitpred,Y)
mean(treefitpred==Y)

#100%


