# 1. zadatak

library(imager)

#sve slike iy foldera se ucitavaju u listu
image_list<-load.dir("C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\prvi_zadatak")
slike<-list()

#koordinate
dole<-list()
desno<-list()

#slike se iz cimg formata prebacuju u matricu
for(i in 1:length(image_list)){
  slike[[i]]<-image_list[[i]][ , ,1,1]+image_list[[i]][ , ,1,2]+image_list[[i]][ , ,1,3]
  slike[[i]]<-renorm(slike[[i]])


  ind<-slike[[i]][1,1] #boja pozadine
  
  #posmatraju se pikseli redom duz slike i prvi koji je razlicite boje od
  #pozadine se uzme kao jedno teme
  for(k in 1:ncol(slike[[i]])){
    for(j in 1:nrow(slike[[i]])){
      if(slike[[i]][j,k]!=ind){
        dole[[i]]<-c(j,-k)
        break
      }
    }
  }
  
  #slicno kao prvo teme se nadje i drugo
  #temena su sacuvana kao koordinate u odnosu na koordinadni pocetak 
  #u gornjem levom uglu slike
  #redovi u matrici rastu duz x ose a kolone duz y ose
  
  for(j in 1:nrow(slike[[i]])){
    for(k in 1:ncol(slike[[i]])){
      if(slike[[i]][j,k]!=ind){
        desno[[i]]<-c(j,-k)
        break
      }
    }
  }
    
}

#a)
uglovi<-c()

#uglovi u stepenima pravih odredjenih susednim temenima pravougaonika
for(i in 1:length(image_list)){
  if(sum(desno[[i]]==dole[[i]])==0){
    uglovi[i]<-atan2(desno[[i]][2]-dole[[i]][2],desno[[i]][1]-dole[[i]][1]) * 180/pi
  }else{
    uglovi[i]<-0
  }
}

image_list_rotated<-list()

#imrotate prima uglove u stepenima
for(i in 1:length(image_list)){
  #b)
  image_list_rotated[[i]]<-imrotate(image_list[[i]],uglovi[i])
  #v)
  save.image(image_list_rotated[[i]],paste(toString(i), "rotirana.jpeg", sep=" "))
}