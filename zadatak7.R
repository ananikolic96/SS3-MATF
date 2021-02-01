
prava_matrica<-function(matrica,dimenzija,broj_mina){
  
  #boji se broj mina u matrici i poredi sa zadatim
  brmina<-0
  for(i in 1:dimenzija[1]){
    for(j in 1:dimenzija[2]){
      if(matrica[i,j]==-100){
        brmina<-brmina+1
      }
      
      #za svaku tacku se od svih mogucih tacaka u okolini biraju one 
      #koje ne prelaze granice table 
      okolina<-c()
      moguce<-c(i-1,j-1,i-1,j,i-1,j+1,i,j+1,i+1,j+1,i+1,j,
                   i+1,j-1,i,j-1)
      for(k in 1:8){
        if(moguce[2*k-1]>0 & moguce[2*k-1]<=dimenzija[1] & moguce[2*k]>0 & moguce[2*k]<=dimenzija[2]){
          okolina<-c(okolina,moguce[2*k-1],moguce[2*k])
        }
      }
      
      #broje se mine u okolini svakog polja i poredi se da li taj broj odgovara
      #broju upisanom u dato polje
      sum<-0
      for(l in 1:(length(okolina)/2)){
        if(matrica[okolina[2*l-1],okolina[2*l]] == -100){
          sum<-sum+1
        }
      }
      #pored zatvorenog polja ne sme biti mina
      if(matrica[i,j]==100 & sum!=0){
        return(0)
      }
      #oko polja u kome je upisan broj mora biti taj broj mina
      if(matrica[i,j]!=100 & matrica[i,j]!=-100 & matrica[i,j]!= sum*10){
        return(0)
      }
    }
  }
  if(brmina!=broj_mina){
    return(0)
  }
  return(1)
}


generator_table <- function(dimenzija, broj_mina){
  
  #tabla je na pocetku popunjena nulama
  matrica<-matrix(rep(0,dimenzija[1]*dimenzija[2]),nrow = dimenzija[1])
  
  #mine se postavljaju na proizvoljna mesta na tabli
  mine<-sample(1:(dimenzija[1]*dimenzija[2]),size = broj_mina,replace = FALSE)
  mine<-sort(mine)
  
  f=1
  for(i in 1:dimenzija[1]){
    for(j in 1:dimenzija[2]){
      
      if(dimenzija[2]*(i-1)+j == mine[f]){
        matrica[i,j]<--100
        if(f<length(mine)){
          f<-f+1
        }
        #slicno kao malo pre, odredjuje se okolina svake mine
        okolina<-c()
        moguce<-c(i-1,j-1,i-1,j,i-1,j+1,i,j+1,i+1,j+1,i+1,j,
                  i+1,j-1,i,j-1)
        for(k in 1:8){
          if(moguce[2*k-1]>0 & moguce[2*k-1]<=dimenzija[1] & moguce[2*k]>0 & moguce[2*k]<=dimenzija[2]){
            okolina<-c(okolina,moguce[2*k-1],moguce[2*k])
          }
        }
        
        #u okolini svake mine se na vec postojeci broj dodaje 10
        # time ce broj upisan u polje odgovarati broju okolina u koje je to polje upalo
        #tj. broju mina u okolini tog polja
        for(l in 1:(length(okolina)/2)){
          if(matrica[okolina[2*l-1],okolina[2*l]]!= -100){
            matrica[okolina[2*l-1],okolina[2*l]]<-10+matrica[okolina[2*l-1],okolina[2*l]]
          }
        }
        
      }
      
      
    }
  }
  #sva ostala polja postaju samo otvorena i prazna
  for(i in 1:dimenzija[1]){
    for(j in 1:dimenzija[2]){
      if(matrica[i,j]==0){
        matrica[i,j]<-100
      }
    }
  }
  
  return(matrica)
}



sakrivanje_polja <- function(matrica, broj_polja){
  
  mine<-c()
  
  #prvo se zatvore sva polja na kojima su mine
  for(i in 1:nrow(matrica)){
    for(j in 1:ncol(matrica)){
      if(matrica[i,j]==-100){
        matrica[i,j]<-0
        
        #pamte se polja na kojima su bile mine
        mine<-c(mine,ncol(matrica)*(i-1)+j)
      }
    }
  }
  #od preostalih otvorenih polja, biraju se ona koja ce da se zatvore
  polja<-sample(setdiff(1:(ncol(matrica)*nrow(matrica)),mine),size = broj_polja,replace = FALSE)
  polja<-sort(polja)
  
  if(broj_polja!=0){
    f=1
    for(i in 1:nrow(matrica)){
      for(j in 1:ncol(matrica)){
      
        if(ncol(matrica)*(i-1)+j == polja[f]){
          matrica[i,j]<-0
          if(f<length(polja)){
            f<-f+1
          }
        }
      }
    }
  }
  
  return(matrica)
}



#pomocna funkcija koja prosledjenu delimicno popunjenu tablu popunjava do kraja
popuni<-function(matrica,broj_mina,zatvorena,orig_brmina){
  
  
  M<-matrica
  
  n<-nrow(matrica)
  m<-ncol(matrica)

  #medju poljima koja nisu vec otvorena se biraju mesta za mine
  mine<-sample(zatvorena,size = broj_mina,replace = FALSE)
  mine<-sort(mine)
  
  f=1
  for(i in 1:n){
    for(j in 1:m){
      
      if(m*(i-1)+j == mine[f]){
        matrica[i,j]<--100
        if(f<length(mine)){
          f<-f+1
        }
        okolina<-c()
        moguce<-c(i-1,j-1,i-1,j,i-1,j+1,i,j+1,i+1,j+1,i+1,j,
                  i+1,j-1,i,j-1)
        for(k in 1:8){
          #da bi polje pripadalo okolini dodaje se uslov da ono nije medju vec
          #otvorenima poljima u prvobitnoj tabli
          if(moguce[2*k-1]>0 & moguce[2*k-1]<=n & moguce[2*k]>0 & moguce[2*k]<=m){
            if((m*(moguce[2*k-1]-1)+moguce[2*k]) %in% zatvorena){
              okolina<-c(okolina,moguce[2*k-1],moguce[2*k])
            }
          }
        }
        #iz istih razloga kao u kod funkcije generisi_tablu
        #dodaje se 10 na vec postojeci broj u okolini svake mine
        if(length(okolina)>0){
          for(l in 1:(length(okolina)/2)){
            if(matrica[okolina[2*l-1],okolina[2*l]]!= -100){
              matrica[okolina[2*l-1],okolina[2*l]]<-10+matrica[okolina[2*l-1],okolina[2*l]]
            }
          }
        }
      }
    }
  }
  for(i in 1:n){
    for(j in 1:m){
      if(matrica[i,j]==0){
        matrica[i,j]<-100
      }
    }
  }
  
#samo ako je na ovaj nacin popunjena tabla jedna validna tabla minolovca
  #funkcija vraca poziciju dopisanih mina na ovoj tabli
  if(prava_matrica(matrica,c(n,m),orig_brmina)){
    return(mine)
  }
}


MK_simulacija <- function(matrica, broj_mina, N){
  n<-nrow(matrica)
  m<-ncol(matrica)
  
  #broji se koliko mina je preostalo da se pronadje da bi se taj broj
  #kasnije prosledio funkciji popuni
  brmina<-0
  for(i in 1:n){
    for(j in 1:m){
      if(matrica[i,j]==-100){
        brmina<-brmina+1
      }
    }
  }
  brmina<-broj_mina-brmina #broj preostalih mina na tabli
  
  zatvorena<-c()
  
  #traze se zatvorena polja
  for(i in 1:n){
    for(j in 1:m){
      if(matrica[i,j]==0){
        zatvorena<-c(zatvorena,m*(i-1)+j)
      }
    }
  }
  
  #matrica pomocu koje ce da se broje pozicije mina u mogucim popunjavanjima table
  verovatnoce<-matrix(nrow=2,ncol=length(zatvorena))
  verovatnoce[1,]<-zatvorena
  verovatnoce[2,]<-rep(0,length(zatvorena))
  
  
  for(g in 1:N){
    
    mines<-popuni(matrica,brmina,zatvorena,broj_mina)
    
    #ako je popunjena tabla validna, mesta na kojima su bile preostale mine se zapisuju
    for(z in 1:length(mines)){
      poz<-which(verovatnoce[1,]==mines[z])
      verovatnoce[2,poz]<-verovatnoce[2,poz]+1
    }
  }
  
  if(max(verovatnoce[2,])==0){
    return(c(0,0))
  }
  
  max<-which.max(verovatnoce[2,])
  min<-which.min(verovatnoce[2,])
  
  max.i<-ceiling(verovatnoce[1,max]/m)
  max.j<-verovatnoce[1,max] - (max.i-1)*m
  
  min.i<-ceiling(verovatnoce[1,min]/m)
  min.j<-verovatnoce[1,min] - (min.i-1)*m
  
  
  print(verovatnoce)
  
  #vraca broj vrste pa broj kolone za najvecu verovatnocu pa isto to za najmanju
  return(c(max.i,max.j,min.i,min.j))
}

