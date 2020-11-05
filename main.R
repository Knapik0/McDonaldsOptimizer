# Title     : TODO
# Objective : TODO
# Created by: Krzysztof
# Created on: 05.11.2020

#RozwiÄ…zanie problemu plecakowego przy uĹĽyciu algorytmu genetycznego.

#zadanie:
#ZĹ‚odziej posiada plecak o udĹşwigu do 25 kg.
#Jakie przedmioty powinien ukraĹ›Ä‡ (do 25 kg), aby w sumie miaĹ‚y one majwiÄ™kszÄ… wartoĹ›Ä‡?

#Instalujemy/wĹ‚Ä…czamy wymagane pakiety
#install.packages("GA")
library(GA)

#Definiujemy zbiĂłr danych i limit plecaka
plecakDb <- data.frame(
  produkt = c("Hamburger", "Cheeseburger","Big Mac", "McRoya", "WieśMac", "McChicken",
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
portfel <- 700
#plecakDb

#Definiujemy funkcjÄ™ przystosowania
fitnessFunc <- function(chr) {
  calkowitaWartoscChr <- chr %*% plecakDb$energia
  calkowitaWagaChr <- chr %*% plecakDb$cena
  if (calkowitaWagaChr > portfel) return(-calkowitaWartoscChr)
  else return(calkowitaWartoscChr)
}

#Uruchamiamy algorytm genetyczny dla zadanych parametrĂłw
wyniki <- ga(type="binary", nBits=32, fitness=fitnessFunc, popSize=100,
             pcrossover=0.85, pmutation=0.05, elitism=5, maxiter=400, seed=10)

#Podsumawanie dziaĹ‚ania algorytmu genetycznego
summary(wyniki)
plot(wyniki)

#Prezentacja najbardziej optymalnego zamówienia
decode <- function(chr){
  print(paste("Najbardziej optymalne zamówienie do kwoty",portfel/10,"zł:" ))
  print( plecakDb[chr == 1, ] )
  print( paste("Wartość zamówienia =",(chr %*% plecakDb$cena)/10,"zł") )
  print( paste("Wartość energetyczna =", chr %*% plecakDb$energia, "kJ = ", (chr %*% plecakDb$energia)*0.239, "kcal") )
}
decode(wyniki@solution[1,])
