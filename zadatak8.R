igra_determinanta<-function(N, matrica, ind_prvog)
{
  n<-nrow(matrica)
  # od matrice napravimo vektor
  vektorizacija = as.vector(matrica)
  
  #matrica u koju ce se upisivati verovatnoce
  M = matrix(rep(0, n^4), ncol=(n*n), nrow=n*n)
  
  # moguca_mesta - mesta koja jos nisu popunjena u matrici.
  moguca_mesta = which(vektorizacija==0)
  # Brojevi koji jos nisu iskorisceni
  moguci_brojevi = setdiff(1:(n*n),vektorizacija)
  for(i in moguci_brojevi)
  {
    for(j in moguca_mesta)
    {
      # Kako smo fiksirali jedno mesto i jedan broj, moramo ih izbaciti sa raspolaganja.
      moguca = moguca_mesta[moguca_mesta!=j]
      moguci = moguci_brojevi[moguci_brojevi!=i]
      
      for(k in 1:N){
        
        # Dobijemo neku slucajnu permutaciju mogucih mesta i samim tim i brojeva.
        if(length(moguca) > 1){
          x = sample(moguca, length(moguca), replace=FALSE)
        }else{
          x = moguca 
        }
        y = moguci
        
        # Ubacimo sve u nasu vektorizovanu matricu.
        for(l in 1:length(moguci)){
          
          #na svako od mogucih mesta se upisuje po jedan nasumicni broj od preostalih
          # mogucih brojeva
          vektorizacija[x[l]]=y[l]
        }
        #ovaj je izbacen iz mogucih brojeva pre nego sto je upisan pa se dodaje
        vektorizacija[j] = i
        matr = vektorizacija
        dim(matr) = c(n,n)
        
        #u ovom slucaju oba igraca igraju optimalno
        #prvi igrac pobedjuje ako je determinanta pozitivna
        #drugi igrac pobedjuje ako je determinanta negativna
        
        d = det(matr)
        if(d>0 & ind_prvog==1){
          M[i, j] = M[i, j]+1
        }
        if(d<0 & ind_prvog==0){
          M[i, j] = M[i, j]+1
        }
      }
    }
  }
  M=M/N
  return(M)
}



pusti_simulaciju <- function(n)
{
  matrica = matrix(rep(0, n*n), ncol=n)
  for(i in 1:(n*n))
  {
    message("Pocinje ", i, ". potez.")
    
    m = igra_determinanta(N = 3000, matrica = matrica, ind_prvog = i %% 2)
    
    #igrac bira broj i poziciju za koje ima najvecu verovatnocu da pobedi
    verovatnoca = m[which.max(m)]
    broj_pozicija = which(verovatnoca == m, arr.ind = TRUE)
    broj = broj_pozicija[1, 1]
    pozicija = broj_pozicija[1, 2]
    
    if(verovatnoca == 0){
      if(i %% 2 == 0){
        #ako je drugi igrac na potezu, a verovatnoca da on pobedi
        #za bilo koji izbor broja i pozicije je 0
        #onda pobedjuje prvi
        igrac_koji_pobedjuje = 'prvi'
      }else{
        igrac_koji_pobedjuje = 'drugi'
      }
      message('Nema smisla nastavljati, pobedio je: ', igrac_koji_pobedjuje, ' igrac!')
      return()
    }
    # upisemo trazeni broj
    matrica[pozicija] = broj
    message("Posle ", i, "-tog poteza:")
    print(matrica)
  }
  det(matrica)
  
}
