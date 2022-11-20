source("pomozne_funkcije.R")
# https://sandipanweb.wordpress.com/2020/12/08/travelling-salesman-problem-tsp-with-python/
# https://github.com/sandipan/UMBC/blob/master/CMSC%20641%20-%20Design%20and%20Analysis%20of%20Algorithms/HW-1.pdf

najdi_razdaljo = function(seznam_tock){
  # Kot argument dobi tabelo s stolpcema x in y
  
  # Najprej uredimo vrstice v vhodni tabeli
  seznam_tock = seznam_tock[order(seznam_tock$x),]
  N = nrow(seznam_tock)
  # Zdaj imamo tocke a_1, a_2, ... , a_n, urejene po velikosti x koordinate od najmanjše do največje.
  
  # Ustvarimo matriko B. Element v i-ti vrstici in j-tem stolpcu (B[i,j]) je dolžina najkrajše bitonične poti, ki se začne v vozlišču a_i in konča v vozlišču a_j.
  B = matrix(0, nrow = N, ncol = N)
  
  # Ustvarimo matriko C iz katere bomo prebrali cikel
  C = matrix(0, nrow = N, ncol = N)
  
  # Cikel bo gotovo vseboval povezavo med vozliščema a_(n-1) in a_n 
  B[(N-1),N] = razdalja(seznam_tock[(N-1),], seznam_tock[N,])
  C[(N-1),N] = N
  
  for (i in (N-2):1){
    minimum = Inf
    for (k in (i+2):N){
      if (minimum > B[(i+1),k] + razdalja(seznam_tock[i,], seznam_tock[k,])){
        minimum = B[(i+1),k] + razdalja(seznam_tock[i,], seznam_tock[k,])
        minimum_k = k
      }
    }
    B[i,(i+1)] = minimum
    C[i,(i+1)] = minimum_k
    for (j in (i+2):N){
      B[i,j] = B[(i+1),j] + razdalja(seznam_tock[i,], seznam_tock[(i+1),])
      C[i,j] = i+1
    }
  }
    
  B[1,1] = B[1,2] + razdalja(seznam_tock[1,], seznam_tock[2,])
  C[1,1] = 2
  
  seznam = list("dolzine" = B, "tocke" = C)
  return(seznam)
}

izpisi_pot = function(C,i,j){
  if (i < j){
    k = C[i,j]
    if (k != i){
      print(k)
    }
    if (k>1){
      izpisi_pot(C,i,k)
    }
  } else {
    k = C[j,i]
    if (k>1){
      izpisi_pot(C,k,j)
      print(k)
    }
  }
}

izpisi_cikel = function(C,N){
  print(N)
  print(N-1)
  k = C[N-1,N]
  izpisi_pot(C,k, N-1)
  print(k)
}

primer = generiraj_tocke(0,8,0,5,4)
primer

najdi_razdaljo(primer)
test = najdi_razdaljo(primer)$tocke

izpisi_cikel(test,4)



# druga koda za algoritem
najdi_razdaljo_checkpoint = function(seznam_tock){
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
    
    for (k in 2:(j-2)){
      trenutno = B[k,(j-1)] + razdalja(seznam_tock[k,], seznam_tock[j,])
      if (trenutno < B[i,j]){
        B[i,j] = trenutno
        C[i,j] = k
      }
    }
  }
  
  B[N,N] = B[(N-1),N] + razdalja(seznam_tock[(N-1),], seznam_tock[N,])
  C[N,N] = N-1
  
  seznam = list("dolzine" = B, "tocke" = C)
  return(seznam)
}

