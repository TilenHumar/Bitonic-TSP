source("pomozne_funkcije.R")


najdi_razdaljo = function(seznam_tock){
  # Kot argument dobi tabelo s stolpcema x in y
  
  # Najprej uredimo vrstice v vhodni tabeli
  seznam_tock = seznam_tock[order(seznam_tock$x),]
  
  # Zdaj imamo tocke a_1, a_2, ... , a_n, urejene po velikosti x koordinate od najmanjše do največje.
  
  # Ustvarimo matriko B. Element v i-ti vrstici in j-tem stolpcu (B[i,j]) je najkrajša bitonična pot, ki se začne v vozlišču a_i in gre najprej strogo levo do vozlišča a_1, potem pa še strogo desno do vozlišča a_j.
  N = nrow(seznam_tock)
  B = matrix(0, nrow = N, ncol = N)
  
  # Najbolj enostavna taka pot je pot od a_1 do a_2, za katero poznamo ceno (razdaljo)
  B[1,2] = razdalja(seznam_tock[1,], seznam_tock[2,])

  for (j in 3:N){
    
    for (i in 1:(j-2)){
      B[i,j] = B[i,(j-1)] + razdalja(seznam_tock[(j-1),], seznam_tock[j,])
    }
    
    B[(j-1),j] = B[i,j-1] + razdalja(seznam_tock[i,], seznam_tock[j,])
    
    for (k in 2:(j-2)){
      trenutno = B[i,j]
      B[i,j] = min(trenutno, B[k,(j-1)] + razdalja(seznam_tock[k,], seznam_tock[j,]))
    }
  }
  
  B[N,N] = B[(N-1),N] + razdalja(seznam_tock[(N-1),], seznam_tock[N,])
  
  return(B)
  }

primer = generiraj_tocke(0,8,0,5,56)
primer

tocke = data.frame(x = c(0,1,2,3),
                   y = c(0,1,0,1))

TESTNA = najdi_razdaljo(primer)
TESTNA
