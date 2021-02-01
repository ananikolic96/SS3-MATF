library(tsne)
library(cluster)

#ucitani iz 4. zadatka
prediktori<-data.frame(X1,X2,X3)
prediktori<-scale(prediktori)

#pravimo 7 klastera
klasteri = kmeans(prediktori, centers=7)

M<-table(klasteri$cluster,Y)

#menjam Y tako da oznake polja budu broj onog klastera u koji je to polje najcesce upalo
Yk<-Y
for(i in unique(Y)){
  Yk[Y==i]<-which.is.max(M[,which(colnames(M)==i)])
}

mean(Yk==klasteri$cluster)
unique(Yk) #cesto ih ima 5

#ima tendenciju da razdvoji podatke u 5 klastera 
# sto bi se i dobilo kao optimalan broj klastera
# tj onaj sa najvecom siluetom, ali ovde to nije cilj
#mada cilj svakako bas i nije dostignut


tsne.model = tsne(X = prediktori,
                  k = 2,
                  epoch = 200,
                  perplexity = 5,
                  max_iter = 6600)

Y[Y==-100]<-7
Y[Y==0]<-3
Y[Y==10]<-1
Y[Y==20]<-4
Y[Y==30]<-5
Y[Y==40]<-6
Y[Y==100]<-2

par(mfrow = c(1,2))
plot(tsne.model, col = klasteri$cluster)
plot(tsne.model, col = Y)

#prazno i 4 ume da klasifikuje, ostale mesa (crveno i roze na grafiku)