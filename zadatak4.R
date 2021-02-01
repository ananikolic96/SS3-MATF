library(imager)
library(nnet)
library(MASS)


image_list<-load.dir("C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\cetvrti_zadatak_obucavanje")
slike<-list()

for(i in 1:length(image_list)){
  slike[[i]]<-image_list[[i]][ , ,1,1]+image_list[[i]][ , ,1,2]+image_list[[i]][ , ,1,3]
}  


# nalazenje ivica

slika<-slike[[1]]
(slika[1:25,1:25])

# neka bude 0.1. Debljina ivice onda bude 4 piksela sto deluje razumno

(slika[1:33,1:33]<0.1) #dosta lepo izgleda


# ako je preko 40% piksela crno, moze se reci da je to deo granice
granice_vrsta<-function(matrica,preciznost){
  
  grvr<-c()
  for(i in 1:dim(matrica)[1]){
    if(mean(matrica[i,]<preciznost)>=0.4){
      grvr<-c(grvr,i)
    }
  }
  return(grvr)
}
#slicno kao za vrste
granice_kolona<-function(matrica,preciznost){
  
  grkl<-c()
  
  for(i in 1:dim(matrica)[2]){
    if(mean(matrica[,i]<preciznost)>=0.4){
      grkl<-c(grkl,i)
    }
  }
  return(grkl)
}

# zelim sto siri interval za ivice polja (otud samo 0.4)

# trazim pocetak i kraj ivica da bih kasnije imala polje u koje
# ne upadaju delovi ivica (ili barem manje upadaju)

pocetak<-function(gr){
  
  start<-c()
  start[1]<-gr[1]
  
  for(i in 2:length(gr)){
    if((gr[i]-gr[i-1])>15){
      start<-c(start,gr[i])
    }
  }
  return(start)
}

kraj<-function(gr){
  
  end<-c()
  
  for(i in 1:(length(gr)-1)){
    if((gr[i+1]-gr[i])>15){
      end<-c(end,gr[i])
    }
  }
  end<-c(end,gr[length(gr)])
  return(end)
}

vrste_pocetak<-pocetak(granice_vrsta(slika,0.1))
vrste_kraj<-kraj(granice_vrsta(slika,0.1))
kolone_pocetak<-pocetak(granice_kolona(slika,0.1))
kolone_kraj<-kraj(granice_kolona(slika,0.1))

#polje (8,4)
s<-array(image_list[[1]][(vrste_kraj[4]+4):(vrste_pocetak[5]-1),(kolone_kraj[8]+4):(kolone_pocetak[9]-1),1,1:3],c(vrste_pocetak[5]-vrste_kraj[4]-4,kolone_pocetak[9]-kolone_kraj[8]-4,1,3))
plot(as.cimg(s))

#u sustini ovo sa pocetkom i krajem ivice bas i nije moralo da se radi ali nema veze


#narednih 180 linija je iskopirano od kolege, priznajem :D

y <- c( 100,  100,  100,  100,  100,  100,   10,    0,    0,
         10,   10,  100,  100,  100,   10,   20,    0,    0,
         0,   10,  100,  100,  100,   20,    0,    0,    0,
         0,   10,  100,  100,  100,   20,    0,   30,   10,
         0,   10,   10,   10,  100,   10,   10,   10,  100,
         0,    0,    0,   10,  100,  100,  100,  100,  100,
         0,    0,    0,   10,  100,   10,   10,   10,  100,
         0,    0,    0,   10,   10,   10,    0,   20,   10,
         0,    0,    0,    0,    0,    0,    0,    0,    0)

y2 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,   40,   30,   40,    0,    0,
           0,    0,    0,    0,   20,  100,   10,   20,    0,
           0,    0,    0,    0,   20,  100,  100,   10,    0,
           0,    0,    0,    0,   20,   20,   20,   20,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y3 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,   30,   10,   30,    0,    0,    0,    0,    0,
           0,   20,  100,   10,   20,   20,    0,    0,    0,
           10,   10,  100,  100,  100,   10,    0,    0,    0,
           100,  100,  100,   10,   20,   40,    0,    0,    0,
           10,   20,   10,   20,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,   40,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y4 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,   30,    0,
           0,    0, -100, -100, -100, -100,   30,    0,    0,
           0,    0,   40,   40,   30,   30,   30,    0,    0,
           0,    0, -100,   20,  100,   10, -100,   30, -100,
           0,    0, -100,   20,  100,   10,   10,   20,   10,
           0,    0,   40,   20,  100,  100,  100,   10,   10,
           0, -100, -100,   20,   10,   10,  100,   10, -100,
           0, -100,   30,   20, -100,   10,  100,   10,   10)

y5 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,   40,   10,   30,    0,    0,    0,
           0,    0,    0,   30,  100,   30,    0,    0,    0,
           0,    0,    0,   30,  100,   30,    0,    0,    0,
           0,    0,    0,   30,   20,   40,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y6 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,   40,   20,   40, -100,    0,    0,
           0,    0,    0,   20,  100,   30, -100,    0,    0, 
           0,    0,    0,   40,   20,   30, -100,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y7 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0, 
           0,    0,    0,   30,   20,   30,    0,    0,    0,
           0,    0,    0,   20,  100,   20,    0,    0,    0,
           0,    0,    0,   40,   30,   40,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y8 <- c(   0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,   40,   30,   30,    0,    0,
           0,    0,    0,    0,   10,  100,   20,    0,    0,
           0,    0,    0,    0,   40,   30,   40,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0)

y9 <- c(   0,    0,   20,   10,  100,   10, -100,    0,    0,
           0,    0, -100,   20,  100,   20,   40,    0,    0,
           0,    0, -100,   30,  100,   10, -100,    0,    0,
           0,    0, -100,   30,   20,   30,   40,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,  
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,    0,    0,    0,    0,    0,    0,    0,    0,
           0,   40,    0,    0,    0,    0,    0,    0,    0)

y10 <- c(100,  100,  100,  100,   10,    0,   10,  100,  100,
         100,  100,  100,  100,   10,   10,   20,   10,   10,
         100,  100,  100,  100,  100,  100,   10,    0,   10,
         10,   10,   10,  100,  100,  100,   10,   10,   10,
         20,    0,   10,  100,  100,  100,   10,   10,   10,
         0,   40,   30,   10,  100,  100,   10,    0,    0,
         20,    0,    0,   30,   30,   20,   20,   10,   10,
         10,   20,   30,    0,    0,    0,   10,  100,  100,
         100,  100,   10,   20,   30,   20,   10,  100,  100)

y11 <- c(  0,    0,    0,    0,    0,    0,    0,    0,    0, 
           0,    0,    0,   20,   10,   10, -100,   10,    0,
           0,    0,   30, -100,   10,   10,   10,   10,    0,
           20,   30, -100,   20,   10,  100,   10,   10,    0,
           -100,   20,   10,   10,  100,  100,   10, -100,    0,
           10,   10,  100,  100,   10,   10,   20,   10,    0,
           100,   10,   10,   10,   10, -100,   20,   10,    0,
           100,   10, -100,    0,    0,    0,    0,    0,    0,
           100,   10,    0,    0,    0,    0,    0,    0,    0)

y12 <- c(  0,    0,    0,   10,  100,   10,   10,   10,  100,
           0,   20,    0,   20,   10,   10, -100,   10,  100,
           0,    0,   30, -100,   10,   10,   10,   10,  100, 
           20,   30, -100,   20,   10,  100,   10,   10,   10,
           -100,   20,   10,   10,  100,  100,   10, -100,   10,
           10,   10,  100,  100,   10,   10,   20,   10,   10,
           100,   10,   10,   10,   10, -100,   20,   10,  100,
           100,   10, -100,   10,   10,   20,    0,   10,  100,
           100,   10,   10,   10,  100,   10,    0,   10,  100)

y13 <- c(100,  100,  100,  100,  100,  100,   10,   10,   10,
         10,   10,  100,  100,  100,   10,   20, -100,   20,
         -100,   10,  100,  100,  100,   20, -100,   40, -100,
         10,   10,  100,  100,  100,   20, -100,   30,   10,
         100,   10,   10,   10,  100,   10,   10,   10,  100,
         100,   10, -100,   10,  100,  100,  100,  100,  100,
         100,   10,   10,   10,  100,   10,   10,   10,  100,
         10,   10,   20,   10,   10,   10, -100,   20,   10,
         10, -100,   20, -100,   10,   10,   10,   20, -100)

y14 <- c(100,  100,  100,  100,  100,  100,  100,   10,    0,
         10,   10,  100,  100,  100,  100,  100,   10,    0,
         0,   10,  100,  100,   10,   20,   30,   30,    0,
         20,   20,  100,  100,   10, -100, -100, -100,   10,
         0,   10,  100,  100,   10,   20,   30,   20,   10,
         0,   10,   10,   10,   10,  100,  100,  100,  100,
         10,   10,   10,    0,   20,   10,   10,  100,  100,
         0,    0,   20,   10,   20,    0,   10,  100,  100,
         0,    0,   10,  100,   10,    0,   10,  100,  100)

y15 <- c(100,  100,  100,  100,  100,  100,  100,  100,  100,
         100,  100,  100,  100,  100,   10,   10,   10,  100,
         100,  100,  100,  100,  100,   20,    0,   30,   10,
         10,   10,  100,  100,  100,   20,    0,    0,    0,
         0,   10,  100,   10,   20,   30,   30,   30,    0,
         10,   10,  100,   10,    0,    0,   10,   10,    0,
         100,   10,   10,   20,   20,   20,   10,   10,   10,
         10,   20,    0,   10,  100,  100,  100,   10,    0,
         0,   20,   10,   10,  100,  100,  100,   10,    0)

y16 <- c(100,  100,  100,  100,  100,  100,   10,   20,    0,
         20,   20,   20,   10,   10,  100,   10,    0,   20,
         -100, -100,    0,    0,   30,   10,   10,   10,   10,
         20,   20,   30,    0,    0,   10,  100,  100,  100,
         100,  100,   10,   20,   20,   10,  100,  100,  100,
         100,   10,   10,   10,  100,  100,  100,  100,  100,
         100,   10,    0,   10,  100,  100,  100,  100,  100,
         100,   20,   20,   30,   10,   10,  100,  100,  100,
         100,   10,    0,   20,    0,   10,  100,  100,  100)

y17 <- c(100,  100,  100,  100,   10,    0,   20,   10,  100,
         100,  100,  100,  100,   10,   30,    0,   20,  100,
         100,  100,  100,  100,  100,   20,    0,   20,  100,
         100,  100,  100,  100,  100,   10,   20,   20,   10,
         10,   10,   10,   10,   10,  100,   10,    0,   10,
         0,    0,    0,    0,   10,  100,   10,   10,   10,
         0,    0,    0,    0,   20,   10,  100,  100,  100,
         0,    0,    0,    0,    0,   10,   10,   10,   10,
         0,    0,    0,    0,    0,    0,    0,    0,    0)

y18 <- c(100,   10,   10,   10,  100,  100,  100,  100,  100,
         100,   10,    0,   10,  100,  100,   10,   20,   20,
         10,   20,   10,   10,   10,   20,   30,    0,    0,
         0,   10,  100,  100,   10,    0,    0,   40,   30,
         10,   10,   10,   10,   20,   20,   20,   20,    0,
         100,  100,   10,    0,   20,   10,  100,   10,   10,
         100,  100,   10,   20,    0,   10,  100,  100,  100,
         100,  100,  100,   10,   10,   10,   10,   10,   10,
         100,  100,  100,  100,  100,  100,   10,    0,    0)



#prvi prediktor koji meri zastupljenost plave boje na slici
#zatvorena polja su vise plava, otvorena manje plava
# jedinice su vise plave od drugih brojeva

plavo<-function(im,vrstepocetak,vrstekraj,kolonepocetak,kolonekraj){
  
  x<-c()
  l=1
  
  for(j in 1:9){
    for(i in 1:9){
      s<-array(im[(vrstekraj[j]+4):(vrstepocetak[j+1]-1),(kolonekraj[i]+4):(kolonepocetak[i+1]-1),1,1:3],c(vrstepocetak[j+1]-vrstekraj[j]-4,kolonepocetak[i+1]-kolonekraj[i]-4,1,3))
      nova<-cimg(s)

      red<-as.vector(nova[ , ,1,1])
      green<-as.vector(nova[ , ,1,2])
      blue<-as.vector(nova[ , ,1,3])
      
      brojac<-mean(blue/(red+green+blue),na.rm = TRUE)
      x[l]<-brojac
      
      l<-l+1
    }
  }
  return(x)
}

# prediktor koji predstavlja disperziju polja
disperzija<-function(im,vrstepocetak,vrstekraj,kolonepocetak,kolonekraj){

  x<-c()
  l=1
  
  for(j in 1:9){
    for(i in 1:9){
      
      s<-array(im[(vrstekraj[j]+4):(vrstepocetak[j+1]-1),(kolonekraj[i]+4):(kolonepocetak[i+1]-1),1,1:3],c(vrstepocetak[j+1]-vrstekraj[j]-4,kolonepocetak[i+1]-kolonekraj[i]-4,1,3))
      nova<-cimg(s)
      
      x[l]<-var(as.vector(grayscale(cimg(s))[,,,1]))
      l<-l+1
    }
  }
  return(x)
}

x1<-plavo(image_list[[1]],vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
x2<-disperzija(image_list[[1]],vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)

model.multi<-multinom(y~x1+x2)
model.multi.pred<-predict(model.multi)
table(model.multi.pred,y)

model.lda<-lda(y~x1+x2)
model.lda.pred<-predict(model.lda)
table(model.lda.pred$class,y)

model.qda<-qda(y~x1+x2)
model.qda.pred<-predict(model.lda)
table(model.qda.pred$class,y)

#ni jedan ne ume 3 da prepozna, tj prepozna je kao 2 jer ni 2 ni 3 nisu plavi
#uvedimo jos jedan prediktor crveno


crveno<-function(im,vrstepocetak,vrstekraj,kolonepocetak,kolonekraj){
  
  x<-c()
  l=1
  
  for(j in 1:9){
    for(i in 1:9){
      s<-array(im[(vrstekraj[j]+4):(vrstepocetak[j+1]-1),(kolonekraj[i]+4):(kolonepocetak[i+1]-1),1,1:3],c(vrstepocetak[j+1]-vrstekraj[j]-4,kolonepocetak[i+1]-kolonekraj[i]-4,1,3))
      nova<-cimg(s)
      
      red<-as.vector(nova[ , ,1,1])
      green<-as.vector(nova[ , ,1,2])
      blue<-as.vector(nova[ , ,1,3])
      
      brojac<-mean(red/(red+green+blue),na.rm = TRUE)
      x[l]<-brojac
      
      l<-l+1
    }
  }
  return(x)
}

x3<-crveno(image_list[[1]],vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)

model.multi<-multinom(y~x1+x2+x3)
model.multi.pred<-predict(model.multi)
table(model.multi.pred,y)

model.lda<-lda(y~x1+x2+x3)
model.lda.pred<-predict(model.lda)
table(model.lda.pred$class,y)

model.qda<-qda(y~x1+x2+x3)
model.qda.pred<-predict(model.lda)
table(model.qda.pred$class,y)

#pokidase

#ovo sve je bilo igranje sa jednom slikom, a sad sledi sa svim iz skupa za obucavanje

X1<-c()
X2<-c()
X3<-c()

Y <- c(y, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18)


for(i in 1:length(image_list)){
  slika<-slike[[i]]
  im<-image_list[[i]]
  
  vrste_pocetak<-pocetak(granice_vrsta(slika,0.1))
  vrste_kraj<-kraj(granice_vrsta(slika,0.1))
  kolone_pocetak<-pocetak(granice_kolona(slika,0.1))
  kolone_kraj<-kraj(granice_kolona(slika,0.1))
  
  x1<-plavo(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  x2<-disperzija(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  x3<-crveno(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  
  X1<-c(X1,x1)
  X2<-c(X2,x2)
  X3<-c(X3,x3)
}


model.multi<-multinom(Y~X1+X2+X3)
model.multi.pred<-predict(model.multi)
table(model.multi.pred,Y)

model.lda<-lda(Y~X1+X2+X3)
model.lda.pred<-predict(model.lda)
table(model.lda.pred$class,Y)

model.qda<-qda(Y~X1+X2+X3)
model.qda.pred<-predict(model.lda)
table(model.qda.pred$class,Y)


#sva tri kidaju.. ostaje samo da vidimo dal neki od modela moze i sa manje prediktora
#kasnije


#provera kvaliteta modela na slikama koje nisu ucestvovale u pravljenju modela:

image_list_test<-load.dir("C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\cetvrti_zadatak_kontrolni")
slike_test<-list()

for(i in 1:length(image_list_test)){
  slike_test[[i]]<-image_list_test[[i]][ , ,1,1]+image_list_test[[i]][ , ,1,2]+image_list_test[[i]][ , ,1,3]
} 


X1<-c()
X2<-c()
X3<-c()

for(i in 1:length(image_list_test)){
  slika<-slike_test[[i]]
  im<-image_list_test[[i]]
  
  vrste_pocetak<-pocetak(granice_vrsta(slika,0.1))
  vrste_kraj<-kraj(granice_vrsta(slika,0.1))
  kolone_pocetak<-pocetak(granice_kolona(slika,0.1))
  kolone_kraj<-kraj(granice_kolona(slika,0.1))
  
  x1<-plavo(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  x2<-disperzija(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  x3<-crveno(im,vrste_pocetak,vrste_kraj,kolone_pocetak,kolone_kraj)
  
  X1<-c(X1,x1)
  X2<-c(X2,x2)
  X3<-c(X3,x3)
}

Ytest1 <- c(  10,   10,   20, -100,   10,  100,  100,  100,  100,
           20, -100,   30,   10,   10,  100,  100,  100,  100,
           20, -100,   20,  100,  100,   10,   10,   10,  100,
           0,   20,   10,  100,  100,   10, -100,   20,   10,
           -100,   10,  100,   10,   10,   20,   20,    0,    0,
           10,   10,  100,   10, -100,   10,   10,    0,    0,
           100,  100,  100,   10,   10,   10,   10,   20,    0,
           100,  100,  100,  100,   10,   10,   20,    0,    0,
           100,  100,  100,  100,   10,    0,   20,    0,    0)

Ytest2 <- c(   0,    0,   10,    0,    0, -100,   10,  100,  100,
            10,   10,   10,   10,   20,   20,   10,   10,   10,
            100,  100,  100,  100,  100,  100,  100,   10,    0,
            100,  100,  100,  100,  100,  100,  100,   10,   10,
            100,  100,  100,  100,  100,   10,   10,   20,   10,
            100,  100,  100,  100,  100,   20, -100,   30, -100,
            100,  100,  100,   10,   10,   40, -100,   40,   10,
            100,  100,  100,   20, -100,   40, -100,   20,  100,
            100,  100,  100,   20, -100,   30,   10,   10,  100)

Y<-c(Ytest1,Ytest2)

multipred<-predict(model.multi, newdata = data.frame(X1,X2,X3))
table(multipred, Y)
mean(multipred == Y)

ldapred<-predict(model.lda, newdata = data.frame(X1,X2,X3))
table(ldapred$class, Y)
mean(ldapred$class == Y)

qdapred<-predict(model.qda, newdata = data.frame(X1,X2,X3))
table(qdapred$class, Y)
mean(qdapred$class == Y)

#sva tri rade savrseno

#####################

#ako se treniraju samo sa plavo i disp.
#ponovo se ucitaju X i Y za trening


model.multi<-multinom(Y~X1+X2)
model.multi.pred<-predict(model.multi)
table(model.multi.pred,Y)
mean(model.multi.pred == Y)
#0.9993141

model.lda<-lda(Y~X1+X2)
model.lda.pred<-predict(model.lda)
table(model.lda.pred$class,Y)
mean(model.lda.pred$class==Y)
#0.973251

model.qda<-qda(Y~X1+X2)
model.qda.pred<-predict(model.lda)
table(model.qda.pred$class,Y)
mean(model.lda.pred$class==Y)
#0.973251

#ispade multinomni najbolji

#na test setu
#ucita se ponovo X i Y za test

multipred<-predict(model.multi, newdata = data.frame(X1,X2))
table(multipred, Y)
mean(multipred == Y)
#1

ldapred<-predict(model.lda, newdata = data.frame(X1,X2))
table(ldapred$class, Y)
mean(ldapred$class == Y)
#0.9382716

qdapred<-predict(model.qda, newdata = data.frame(X1,X2))
table(qdapred$class, Y)
mean(qdapred$class == Y)
#1

#ali na testiranju je i qda podjednako dobar kao multinomni
