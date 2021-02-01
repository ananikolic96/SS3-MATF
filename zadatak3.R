library(imager)
library(plotrix)

path<-"C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\treci_zadatak"
setwd(path)

slika1<-load.image("nasa.jpg")
slika2<-load.image("nasa1.jpg")

slika1<-rm.alpha(slika1)
slika2<-rm.alpha(slika2)

razlike<-renorm(slika2)-renorm(slika1)


razlike<-razlike[ , ,1,1]+razlike[ , ,1,2]+razlike[ , ,1,3]


# trazimo koordinate mesta na kojima je razlika dve slike velika
mesta_razlike<-function(slika){
  
  koordinate<-list()
  ind<-1
  
  for(i in 1:nrow(slika)){
    for(j in 1:ncol(slika)){
      if(slika[i,j]>10){ #kada bi ovde bilo >0 dobilo bi se da postoji
        # preko 700 piksela na kojima je razlika veca od 0
        #nema potrebe da ih bude toliko a prag 10 je vio dovoljno veliki da izbaci sve 
        #koji nisu znacajni
        
        koordinate[[ind]]<-c(i,j)
        ind<-ind+1
      }
    }
  }
  return(koordinate)  
}

koord<-mesta_razlike(razlike)

par(mfrow = c(1,2))

plot(slika1)
#slike imaju 4 razlike pa je dovoljno nacrtati elipse oko samo 16 tacaka

for (i in seq(1, length(koord), length(koord)/16)){
  draw.ellipse(koord[[i]][1],koord[[i]][2],15,15,border =  "red")
}


plot(slika2)
for (i in seq(1, length(koord), length(koord)/16)){
  draw.ellipse(koord[[i]][1],koord[[i]][2],15,15,border = "red")
}





