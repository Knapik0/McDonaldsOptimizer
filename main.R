# Title     : TODO
# Objective : TODO
# Created by: Krzysztof
# Created on: 05.11.2020

#Instalujemy wymagane pakiety
install.packages("GA")
library(GA)

#Definiujemy zbiór danych
produkty <- data.frame(
  nazwa = c("Hamburger", "Cheeseburger","Big Mac", "McRoya", "WieśMac", "McChicken",
              "Filet-O-Fish", "Kurczakburger", "Jalapeño Burger", "Chikker", "McDouble", "McWrap Klasyczny",
              "McWrap Bekon DeLuxe", "Sałatka", "Sałatka Kurczak Premium",
              "6  McNuggets", "9  McNuggets", "20  McNuggets",
              "3 Chicken Strips", "5 Chicken Strips", "Marchewki", "Jabłka",
              "Frytki małe", "Frytki średnie", "Frytki duże",
              "Shake czekoladowy mały", "Shake czekoladowy duży", "Shake waniliowy mały", "Shake waniliowy duży",
              "Shake truskawkowy mały", "Shake truskawkowy duży", "Ciastko jabłkowe"),
  energia = c(1063, 1266, 2201, 2179, 2421, 1791, 1391, 1294, 1244, 1665, 1646,
              2181, 2723, 161, 1343, 1120, 1680, 3734, 1321, 2202, 147, 185, 968, 1379, 1815,
              859, 1375, 828, 1325, 834, 1335, 955),
  cena = c(40, 45, 122, 119, 119, 118, 106, 50, 45, 55, 64, 128, 132, 78, 157, 112, 132, 206,
           109, 144, 35, 50, 69, 78, 82, 56, 74, 56, 74, 56, 74, 45)
)
#portfel <- 120
#
##Definiujemy funkcję przystosowania
#fitnessFunc <- function(chr) {
#  calkowitaWartoscChr <- chr %*% produkty$energia
#  calkowitaCenaChr <- chr %*% produkty$cena
#  if (calkowitaCenaChr > portfel) return(-calkowitaWartoscChr)
#  else return(calkowitaWartoscChr)
#}
#
##Uruchamiamy algorytm genetyczny dla zadanych parametrów
#wyniki <- ga(type="binary", nBits=32, fitness=fitnessFunc, popSize=300,
#             pcrossover=0.55, pmutation=0.25, elitism=5, maxiter=1000, seed=100)
#
##Prezentacja najbardziej optymalnego zamówienia
#decode <- function(chr){
#  print( paste("Najbardziej optymalne zamówienie do kwoty",portfel/10,"zł:" ))
#  print( produkty[chr == 1, ] )
#  print( paste("Wartość zamówienia =",(chr %*% produkty$cena)/10,"zł") )
#  print( paste("Wartość energetyczna =", chr %*% produkty$energia, "kJ =", (chr %*% produkty$energia)*0.239, "kcal") )
#}
#decode(wyniki@solution[1,])

#Definiujemy minimalną wartość energetyczną zazmówienia
minKalorie <- 1000 / 0.239

#Definiujemy funkcję przystosowania
fitnessFunc <- function(chr) {
  calkowitaEnergia <- chr %*% produkty$energia
  calkowitaCena <- chr %*% produkty$cena
  if ((calkowitaEnergia < minKalorie) || calkowitaCena == 0) return(-calkowitaCena)
  #Zwracamy odwrotność całkowitej ceny (dzięki temu najniższa wartość ceny jest najlepiej przystosowana)
  else return(200/calkowitaCena)
}


#Uruchamiamy algorytm genetyczny dla zadanych parametrów
wyniki <- ga(type="binary", nBits=32, fitness=fitnessFunc, popSize=150,
             pcrossover=0.55, pmutation=0.25, elitism=5, maxiter=200, seed=100)

plot(wyniki)
#Prezentacja najbardziej optymalnego zamówienia
decode <- function(chr){
  print( paste("Najbardziej optymalne zamówienie na min" , minKalorie*0.239 , "kcal:" ))
  print( produkty[chr == 1 , ] )
  print( paste("Wartość zamówienia =" , (chr %*% produkty$cena)/10 , "zł") )
  print( paste("Wartość energetyczna =" , chr %*% produkty$energia , "kJ =" , (chr %*% produkty$energia)*0.239 , "kcal") )
}
decode(wyniki@solution[1,])
