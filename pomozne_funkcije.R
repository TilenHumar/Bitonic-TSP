# Pomozna funkcija generiraj_tocke generira tocke (x, y) v ravnini po katerih bomo iskali pot. Tu je x celo, y pa realno število.
# spodnja_x ... spodnja meja za x koordinato
# zgornja_x ... zgornja meja za x koordinato
# spodnja_y ... spodnja meja za y koordinato
# zgornja_y ... zgornja meja za y koordinato
# N ... želeno število točk


# Opomba: predpostavil sem, da bomo iskali pot na točkah z različnimi x koordinatami


generiraj_tocke = function(spodnja_x, zgornja_x, spodnja_y, zgornja_y, N) {
  
  # Preverimo, ali želeno število točk presega število razpoložljivih x koordinat
  if (N > (zgornja_x - spodnja_x + 1)){
    # Generiralo se bo največje možno število točk
    N = zgornja_x - spodnja_x + 1
    print("Število želenih točk presega število razpoložljivih koordinat na abscisni osi. Generirano je bilo največje možno število točk.")
    }
  
  # Najprej naredimo prazno tabelo. Prvi stolpec pove x, drugi pa y koordinato točke.
  seznam_tock = data.frame(matrix(nrow = N, ncol = 2))
  colnames(seznam_tock) = c("x", "y")
  
  # Naredimo vektor razpoložljivih x koordinat
  mozni_x = spodnja_x:zgornja_x
  izbrani = c()
  
  for (i in 1:N){
    x_koord = sample(mozni_x, 1)
    while (x_koord %in% izbrani){
      x_koord = sample(mozni_x, 1)    
    }
    izbrani = append(izbrani, x_koord)
    seznam_tock[i,1] = x_koord
    seznam_tock[i,2] = runif(1, min = spodnja_y, max = zgornja_y)
    }
    
  seznam_tock = seznam_tock[order(seznam_tock$x),]
  return(seznam_tock)
}

# Še funkcija narisi, ki bo vrnila graf točk
narisi = function(seznam){
  graf = plot(x = seznam$x,
              y = seznam$y,
              pch = 19,
              lwd=5.0,
              xlab = "x",
              ylab = "y",
              main = "Bitonic-TSP")
  return(graf)
}

# funkcija, ki izračuna razdaljo med dvema točkama
razdalja = function(tocka1, tocka2){
    razdalja = sqrt((tocka2$x - tocka1$x)^2 + (tocka2$y - tocka1$y)^2)
    return(razdalja)
  }


# funkcija, ki nariše cikel. Kot argument dobi seznam tock, ki je urejen glede na vrstni red vozlišč v ciklu
narisi_cikel = function(seznam_tock, seznam_indeksov){
  N = nrow(seznam_tock)
  seznam_tock = seznam_tock[seznam_indeksov,]
  narisi(seznam_tock)
  indeksi = 1:N
  arrows(seznam_tock$x[indeksi], seznam_tock$y[indeksi], seznam_tock$x[indeksi+1], seznam_tock$y[indeksi+1], length = 0.25, angle = 30, code = 2,
         lty = NULL, xpd = FALSE)
  
  arrows(seznam_tock$x[N], seznam_tock$y[N], seznam_tock$x[1], seznam_tock$y[1], length = 0.25, angle = 30, code = 2,
         lty = NULL, xpd = FALSE)
  }


