# Pomozna funkcija generiraj_tocke generira tocke (x, y) v ravnini po katerih bomo iskali pot. Tu je x naravno, y pa realno število.
# spodnja_x ... spodnja meja za x koordinato
# zgornja_x ... zgornja meja za x koordinato
# spodnja_y ... spodnja meja za y koordinato
# zgornja_y ... zgornja meja za y koordinato
# N ... želeno število točk


# Opomba: predpostavil sem, da bomo iskali pot na točkah z različnimi x koordinatami

generiraj_tocke = function(spodnja_x, zgornja_x, spodnja_y, zgornja_y, N) {
  
  # Najprej naredimo prazno tabelo. Prvi stolpec pove x, drugi pa y koordinato točke.
  seznam_tock = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(seznam_tock) = c("x", "y")
  
  # Naredimo vektor razpoložljivih x koordinat
  mozni_x = spodnja_x:zgornja_x
  
  for (i in 1:N){
    
    if (length(mozni_x) == 0L){
      print("Število želenih točk presega število razpoložljivih koordinat na abscisni osi. Generirano je bilo največje možno število točk.")
      break
    }
    
    else{
    x_koord = sample(mozni_x, 1)
    seznam_tock[i,1] = x_koord
    seznam_tock[i,2] = runif(1, min = spodnja_y, max = zgornja_y)
    mozni_x = mozni_x[!mozni_x == x_koord]}
    }
  
  seznam_tock = seznam_tock[order(seznam_tock$x),]
  return(seznam_tock)
}

# Še funkcija narisi, ki bo vrnila graf točk
narisi = function(seznam){
  graf = plot(x = seznam$x,
              y = seznam$y,
              pch = 19,
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
