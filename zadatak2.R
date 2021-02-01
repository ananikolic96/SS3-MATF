library(imager)
library(magick)
library(purrr)

path<-"C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\drugi_zadatak\\pravougaonici"

slike<- list.files(path)
setwd(path)

images <- map(slike, image_read)


#filter za detekciju vertikalnih ivica
edgev = matrix(c(1,1,1,0,0,0,-1,-1,-1), 3, 3)
#filter za detekciju horizontalnih ivica
edgeh = t(edgev)


#igranjem sa filterima sam na kraju dosla do toga da se najlepsi rezultat dobije
#ako se za jednu sliku bias postavi na 100% a za drugu na 0%
#tako jedna slika ima crnu a druga belu pozadinu zbog cega se od jedne uzme negativ
#pa se tako dobijene slike saberu

# verovatno bi se uz jos igranja nasli filteri koji ovaj posao lepo rade za 
# horizontalne i vertikalne odjednom, ali ovo je za sada najbolje...

ivice<-function(slika){

  image_convolve(slika,edgev,bias = "100%") -> imgEdgev100
  image_convolve(slika,edgeh,bias = "100%") -> imgEdgeh100
  
  image_convolve(slika,edgev,bias = "0%") %>% image_negate()-> imgEdgev0
  image_convolve(slika,edgeh,bias = "0%") %>% image_negate()-> imgEdgeh0

  slikav100<-magick2cimg(imgEdgev100, alpha = "rm")
  slikah100<-magick2cimg(imgEdgeh100, alpha = "rm")
  
  slikav0<-magick2cimg(imgEdgev0, alpha = "rm")
  slikah0<-magick2cimg(imgEdgeh0, alpha = "rm")

  slikav100<-renorm(slikav100)
  slikah100<-renorm(slikah100)
  slikav0<-renorm(slikav0)
  slikah0<-renorm(slikah0)
  
  im<-renorm(slikav100+slikah100+slikav0+slikah0)
  im<-as.cimg(im<250) #da pozadina bude crna
  
  im<-im[ , ,1,1]+im[ , ,1,2]+im[ , ,1,3]
  im<-renorm(im)
  
  return(im)
}

#naredne 4 funkcije traze temena pravougaonika na slici
#ili us lucaju kruga, traze skroz donju, gornju, levu i desnu tacku na krugu

levo<-function(slika){
  ind<-slika[1,1]
  for(j in 1:nrow(slika)){
    for(k in 1:ncol(slika)){
      if(slika[j,ncol(slika)+1-k]!=ind){
        left<-c(j,-ncol(slika)-1+k)
        return(left)
      }
    }
  }
}


dole<-function(slika){
  ind<-slika[1,1]
  for(k in 1:ncol(slika)){
    for(j in 1:nrow(slika)){
      if(slika[nrow(slika)+1-j,ncol(slika)+1-k]!=ind){
        down<-c(nrow(slika)+1-j,-ncol(slika)-1+k)
        return(down)
      }
    }
  }
}

desno<-function(slika){
  ind<-slika[1,1]
  for(j in 1:nrow(slika)){
    for(k in 1:ncol(slika)){
      if(slika[nrow(slika)+1-j,k]!=ind){
        right<-c(nrow(slika)+1-j,-k)
        return(right)
      }
    }
  }
}

gore<-function(slika){
  ind<-slika[1,1]
  for(k in 1:ncol(slika)){
    for(j in 1:nrow(slika)){
      if(slika[j,k]!=ind){
        up<-c(j,-k)
        return(up)
      }
    }
  }
}


jedan_pravougaonik<-function(orig){
  

  slika<-ivice(orig)
  
  #temena pravougaonika
  a<-levo(slika)
  b<-dole(slika)
  c<-desno(slika)
  d<-gore(slika)
  
  
  #ako znamo tri temena, cetvrto se moze odrediti pomocu njih
  tacka<-c()
  
  tacka[1]<-c[1]-b[1]+a[1]
  tacka[2]<-c[2]-b[2]+a[2]
  
  #ako se cetvrto teme, odredjeno pomocu preostala 3 ne poklapa sa temenom
  #odredjenom primenom funkcije gore, onda na slici ne moze biti samo 1 pravougaonik
  
  if(sqrt((tacka[1]-d[1])^2 + (tacka[2]-d[2])^2)>5){
    return("Na slici ima vise od jednog pravougaonika1")
  }else{
   
    #dok ako se poklapa sa temenom odredjenom pomocu funkcije gore
    # onda i dalje mozemo imati slucaj da postoji vise pravougaonika
    # ali takvih da se mogu upisati u jedan veci
    # zato proveravamo da li prava koja sadrzi dijagonalu tog opisanog pravougaonika
    # sece neku od ivica pravougaonika u unutrasnjosti tog opisanog
    
    suma<-0
    k<-(c[2]-a[2])/(c[1]-a[1])
    
    for(i in 8:(c[1]-a[1]-8)){
      x<-a[1]+i
      y<-floor(a[2]+k*(x-a[1]))
      
      suma<-suma+slika[x,-y]
    }
    if(suma>0){
      return("Na slici ima vise od jednog pravougaonika2")
    }
  }
  
  # ako je na slici 1 pravougaonik, ostaje jos da se proveri da li je crn, beo ili nijedno
  orig<-magick2cimg(orig,alpha = "rm")
  
  orig<-orig[ , ,1,1]+orig[ , ,1,2]+orig[ , ,1,3]
  orig<-renorm(orig)
  
  #pod pretpostavkom da je pravougaonik jednobojan, uzme se tacka iz centra i odredi 
  #se koje je ona boje
  
  if(orig[(a[1]+c[1])/2,-(a[2]+c[2])/2]!=0 & orig[(a[1]+c[1])/2,-(a[2]+c[2])/2]!=255){
    return("Pravougaonik je neke druge boje")
  }else{
    return(c(a,b,c,d))
  }
  
  
  return("greska")
}


path<-"C:\\FAKS\\SS3\\Seminarski\\fajlovi\\Prva Grupa\\drugi_zadatak\\krugovi"

slike<- list.files(path)
setwd(path)

images <- map(slike, image_read)


duzina<-function(a,b){
  return((sqrt((a[1]-b[1])^2 + (a[2]-b[2])^2)))
}

jedan_krug<-function(orig){
  
  
  slika<-ivice(orig)
  
  a<-levo(slika)
  b<-dole(slika)
  c<-desno(slika)
  d<-gore(slika)
  
  
  #ako je na slici jedan krug
  # tacke a, b, c i d treba da budu temena kvadrata
  
  if(duzina(a,b)-duzina(a,d) > 5 | duzina(c,b)-duzina(c,d) > 5 | duzina(a,b)-duzina(b,c) > 5 | duzina(a,c)-duzina(b,d) > 5){
    return("Na slici ima vise od jednog kruga1")
  }else{
    #slicno kao kod pravougaonika, cak i ako ta temena cine kvadrat
    # mozda samo postoje npr. 4 kruga koji dodiruju da temena, u tom slucaju 
    # bi prava koja spaja tacke a i c morala preseci neku ivicu kruga
    
    suma<-0
    k<-(c[2]-a[2])/(c[1]-a[1])
    
    for(i in 8:(c[1]-a[1]-8)){
      x<-a[1]+i
      y<-floor(a[2]+k*(x-a[1]))
      
      suma<-suma+slika[x,-y]
    }
    if(suma>0){
      return("Na slici ima vise od jednog kruga2")
    }
  }
  
  orig<-magick2cimg(orig,alpha = "rm")
  
  orig<-orig[ , ,1,1]+orig[ , ,1,2]+orig[ , ,1,3]
  orig<-renorm(orig)
  
  
  #ako je krug jednobojan, proverava se koje je boje tacka u sredini
  if(orig[(a[1]+c[1])/2,-(a[2]+c[2])/2]!=0 & orig[(a[1]+c[1])/2,-(a[2]+c[2])/2]!=255){
    return("Krug je neke druge boje")
  }else{
    #centar i poluprecnik
    return(c(round((a+c)/2),round(duzina(a,c)/2)))
  }
  
  return("greska")
}


