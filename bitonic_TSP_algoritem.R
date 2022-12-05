source("pomozne_funkcije.R")

najdi_razdaljo = function(seznam_tock){
  # Kot argument dobi tabelo s stolpcema x in y
  
  # Najprej uredimo vrstice v vhodni tabeli
  seznam_tock = seznam_tock[order(seznam_tock$x),]
  N = nrow(seznam_tock)
  # Zdaj imamo tocke a_1, a_2, ... , a_n, urejene po velikosti x koordinate od najmanjše do največje.
  
  # Ustvarimo matriko B. Element v i-ti vrstici in j-tem stolpcu (B[i,j]) je dolžina najkrajše poti, ki se začne v vozlišču a_i in gre najprej strogo levo do vozlišča a_1, potem pa še strogo desno do vozlišča a_j.
  B = matrix(0, nrow = N, ncol = N)
  
  # Ustvarimo matriko C. Element v i-ti vrstici in j-tem stolpcu (C[i,j]) je predzadnje vozlišče v bitonični poti, ki se začne v vozlišču a_i in konča v vozlišču a_j. To je torej predhodnik vozlišča a_j na tej poti.
  C = matrix(0, nrow = N, ncol = N)
  
  # Najbolj enostavna taka pot je pot od a_1 do a_2, za katero poznamo ceno (razdaljo)
  B[1,2] = razdalja(seznam_tock[1,], seznam_tock[2,])
  C[1,2] = 1
  
  for (j in 3:N){
    
    for (i in 1:(j-2)){
      B[i,j] = B[i,(j-1)] + razdalja(seznam_tock[(j-1),], seznam_tock[j,])
      C[i,j] = j-1
    }
    
    B[(j-1),j] = B[i,j-1] + razdalja(seznam_tock[i,], seznam_tock[j,])
    C[(j-1),j] = i
    
    for (k in 1:(j-2)){
      trenutno = B[k,(j-1)] + razdalja(seznam_tock[k,], seznam_tock[j,])
      if (trenutno < B[j-1,j]){
        B[j-1,j] = trenutno
        C[j-1,j] = k
      }
    }
  }
  
  # Povezava med a_n-1 in a_n bo gotovo v ciklu
  B[N,N] = B[(N-1),N] + razdalja(seznam_tock[(N-1),], seznam_tock[N,])
  C[N,N] = C[(N-1),N]  
  
  
  seznam = list("rezultat" = B[N,N], "dolzine" = B, "predniki" = C)
  return(seznam)
}

izpisi_cikel = function(C, seznam_tock){
  N = nrow(seznam_tock)
  cikel = c(N, N-1)
  
  trenutni = C[N-1, N]
  
  while (trenutni > 1) {
    cikel = c(trenutni, cikel)
    if (trenutni < cikel[2] - 1) {
      cikel = rev(cikel)
    }
    trenutni = C[cikel[length(cikel)], cikel[1]]
  }
  return(c(setdiff(N:1, cikel), cikel))
}


# Še 2 testa

# preprost
test = data.frame("x" = c(1,2,3,4),
                  "y" = c(0,10,0,10)
                  )
narisi(test)

test_funkcije = najdi_razdaljo(test)

test_funkcije$dolzine
test_funkcije$predniki

cikel = izpisi_cikel(test_funkcije$predniki, test)

narisi_cikel(test,cikel)

# zahteven
test1 = generiraj_tocke(1,80,0,10,60)
narisi(test1)

test_funkcije1 = najdi_razdaljo(test1)

test_funkcije1$dolzine
test_funkcije1$predniki

cikel1 = izpisi_cikel(test_funkcije1$predniki, test1)

narisi_cikel(test1,cikel1)

