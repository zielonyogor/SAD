dane=read.csv("DwiePopulacje.csv", sep=";")

#1
#Badano zawartość procentową celulozy w drewnie pewnego gatunku pochodzącego z dwóch różnych
#regionów Polski. Dla regionu I poddano analizie 8 próbek drewna natomiast dla regionu II 
#przebadano 21 próbek drewna. Otrzymane wyniki zapisane zostały w pliku DwiePopulacje.csv. 
#Przyjmując normalność rozkładu zawartości celulozy w drewnie i poziom istotności 0,02:
#  (a) zweryfikuj hipotezę, że przeciętna zawartość celulozy dla regionu I różni się istotnie od 
#przeciętnej zawartości celulozy dla regionu II. Przyjmij jednorodność wariancji populacji i 
#normalność rozkładu badanej cechy;

#1. hipoteza:
#H0: mu1 - mu2 = 0
#H1: mu1 - mu2 =/= 0

reg1 = na.omit(dane$cel1)
reg2 = na.omit(dane$cel2)

n1 = length(reg1)
n2 = length(reg2)

x_bar1 = mean(reg1)
x_bar2 = mean(reg2)

var1 = var(reg1)
var2 = var(reg2)

Sp2 = ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2) #wspólna wariancja

alpha = 0.02

#2. wyznaczenie statystyki testowej
t = (x_bar1 - x_bar2)/(sqrt(Sp2*(1/n1 + 1/n2))) #nie odejmuje m0 bo to 0
#t = -1.5398

#3. wyznaczenie obszaru krytycznego
qt(1-alpha/2, n1+n2-2)
#R = (-inf ; 2.47266) + (2.47266 ; inf)

#4. podjecie decyzji
#t = -1.5398 nie należy do R  ->  brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 2% dane nie potwierdzają hipotezy, że przeciętna zawartość celulozy 
#dla regionu 1 różni się istotnie od przeciętnej zawartości celulozy w regione 2

#to samo tylko w R
#2. wyznaczenie statystyki testowej
t.test(reg1, reg2, var.equal = TRUE, mu = 0, alternative = "two.sided")

#3. wyznaczenie pvalue
#pval = 0.1352

#4. podjęcie decyzji
#alppha = 0.02 < 0.1352 = pval  ->  brak podstaw do odrzucenia H0

# (b) sprawdź, czy założenie o równości wariancji było słuszne;

#1. hipoteza
#H0: sigma^2_1 - sigma^2_2 = 0
#H1: sigma^2_1 - sigma^2_2 =/=

#2. wyznaczenie statystyki testowej 
F = var1/var2
#F = 0.4786012

#3. wyznaczenie przedziału krytycznego
L = qf(alpha/2, n1-1, n2-1)
P = qf(1 - alpha/2, n1-1, n2-1)
#R = (0 ; 0.16245) + (3.6987 ; inf)

#4. podjęcie decyzji
# F = 0.4786012 nie należy do R  ->  brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 2% dane nie potwierdzają hipotezy o różności wariancji, zatem założenie
#o jednorodności wariancji było słuszne

#to samo tylko w R

#2. wybór statystyki testowej
var.test (reg1, reg2, alternative = "two.sided")

#3. wyznaczenie pval
#pval = 0.3225

#4. podjęcie decyzji
#alpha = 0.02 < 0.3225 = pval  ->  brak podstaw do odrzucenia H0


# (c) oceń metodą przedziałową, ze współczynnikiem ufności 0,98, różnicę średnich. 
#Zinterpretuj wynik i na jego podstawie podejmij decyzję dotyczącą hipotezy z punktu (a). 

t.test(reg1, reg2, var.equal = TRUE ,conf.level = 1 - alpha)$conf.int
#na poziomie ufności 98% przedział (-13.52 ; 3.15) pokrywa pewną nieznaną prawdziwą 
#różnicę średnich zawartości celulozy w regionie1 i regionie2

#decyzja dotycząca hipotezy z punktu (a)
#0 mieści się w przedziale  ->  nie odrzucamy hipotezy H0
#na poziomie istotności 2% ponieważ przedział ufności pokrywa wartość 0 zatem nie mamy podstaw
#do odrzucenia H0

#PD: sposób ręczny
t = qt(1-alpha/2, n1+n2-2) * sqrt(Sp2*(1/n1 + 1/n2))
L = x_bar1 - x_bar2 - t
U = x_bar1 - x_bar2 + t
#na poziomie ufności 98% przedział (-13.52 ; 3.15) pokrywa pewną nieznaną prawdziwą 
#różnicę średnich zawartości celulozy w regionie1 i regionie2
#decyzja taka sama


#3.
#Bank chce sprawdzić, która metoda pozyskiwania pieniędzy (ze źródeł publicznych czy 
#prywatnych) prowadzi do pozyskania większego funduszu. W tym celu bank pobrał losową próbę 
#11 firm, które zaciągnęły kredyt tylko ze źródeł publicznych oraz próbę 16 firm, które 
#zaciągnęły kredyt tylko ze źródeł prywatnych. Otrzymane wyniki zapisano w pliku 
#DwiePopulacje.csv. Zakładając, że wysokość kredytów prywatnych i publicznych ma 
#rozkład normalny czy można stwierdzić, że publiczne źródła finansowania udzielają, 
#przeciętnie rzecz biorąc, mniejszych kredytów? Wnioskuj na poziomie istotności 0,1.

publiczny = na.omit(dane$publiczny)
prywatny = na.omit(dane$prywatny)

#najpierw hipoteza nt. równości wariancji
#1. hipoteza
#H0: sigma^2_1 - sigma^2_2 = 0
#H1: sigma^2_1 - sigma^2_2 =/=

#2. wyznaczenie statystyki testowej 
var.test(publiczny, prywatny, alternative = "two.sided")

#3. wyznaczenie pval
#pval = 0.08687

#4. podjęcie decyzji
#alpha = 0.1 > 0.08687 = pval -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 10% dane zaprzeczają jednorodności wariancji


#teraz hipoteza nt. zadania
#1. hipoteza
#H0: mu1 - mu2 >= 0
#H1: mu1 - mu2 < 0

#2. wyznaczenie statystyki testowej 
t.test(publiczny, prywatny, var.equal = FALSE, mu=0, alternative="less")$p.value

#3. wyznaczenie pval
#pval = 0.02300284

#4. podjęcie decyzji
#alpha = 0.1 > 0.02300284 = pval -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają, że przeciętnie rzecz biorąc publiczne źródła
#finansowania udzielają mniejszych kredytów od prywatnych źródeł


#4.
#Dla porównania regularności uzyskiwanych wyników sportowych dwóch zawodników w pewnym okresie, 
#wylosowano 12 wyników pierwszego zawodnika i 9 drugiego. Otrzymano następujące rezultaty (w m):
#  dla pierwszego: 7,6; 7,81; 8,01; 7,95; 7,15; 8,06; 7,9; 7,91; 7,56; 7,62; 7,85; 8,02;
#  dla drugiego: 7,5; 7,9; 8,0; 7,17; 7,28; 7,35; 7,73; 7,2; 7,98.
#Na poziomie istotności 0,05 zweryfikuj hipotezę o większej regularności wyników pierwszego
#zawodnika

zawodnik1 = na.omit(dane$zawodnik1)
zawodnik2 = na.omit(dane$zawodnik2)

#1. hipoteza
#H0: sigma^2_1 - sigma^2_2 >= 0
#H1: sigma^2_1 - sigma^2_2 < 0

#2. wyznaczenie statystyki testowej 
var.test(zawodnik1, zawodnik2, alternative = "less")$p.value

#3. wyznaczenie pval
#pval = 0.2107889

#4. podjęcie decyzji
#alpha = 0.05 < 0.2107889 = pval -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają hipotezy o większej regularności pierwszego
#zawodnika


#6
#W oparciu o badania przeprowadzone w Polsce i USA, dotyczące zadowolenia z pracy, uzyskano
#wyniki: spośród 1200 badanych Polaków 78% potwierdziło zadowolenie z pracy, natomiast spośród
#2000 Amerykanów 20% było niezadowolonych.

# (a) Porównaj za pomocą 90% przedziału ufności procent zadowolonych Polaków i Amerykanów.
#Napisz swoją własną funkcję, a następnie porównaj wyniki z otrzymanymi po zastosowaniu
#funkcji R

#PD: ręcznie
phat1 = t1/n1
phat2 = t2/n2

z = qnorm(1-alpha/2)
L= phat1 - phat2 - z*sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
U = phat1 - phat2 + z*sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
#na poziomie ufności 90% przedział (-4.46%  0.46%) pokrywa pewną nieznaną rzeczywistą różnicę
#proporcji Polaków i Amerykanów zadowolonych z pracy

#w R

t1 = 0.78*1200
t2 = 0.8*2000
n1 = 1200
n2 = 2000
alpha = 0.1

prop.test(c(t1, t2), c(n1, n2), conf.level = 1 - alpha)$conf.int

#na poziomie ufności 90% przedział (-4.53%  0.53) pokrywa pewną nieznaną rzeczywistą różnicę
#proporcji Polaków i Amerykanów zadowolonych z pracy

# (b) Czy opinia, że proporcja zadowolonych Polaków jest mniejsza niż zadowolonych Amerykanów 
#jest słuszna? Testuj na poziomie istotności 0,1.

#PD: ręcznie
#1. hipoteza
#H0: p1 - p2 >= 0
#H1: p1 - p2 < 0

#2. wyznaczenie statystyki testowej
phat = (t1 + t2)/(n1 + n2)
z = (phat1 - phat2)/sqrt(phat*(1-phat)*(1/n1 + 1/n2))
#z = -1.350678

#3. wyznaczenie obszaru krytycznego
-qnorm(1-alpha)
#R = (-inf ; 1.281552)

#4. podjęcie decyzji
#z nalezy do R  ->  odrzucamy H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają hipotezę, że proporcja zadowolonych Polaków jest
#mniejsza niż proporcja zadowolonych Amerykanów


#w R

#1. hipoteza
#H0: p1 - p2 >= 0
#H1: p1 - p2 < 0

#2. wyznaczenie statystyki testowej
prop.test(c(t1, t2), c(n1, n2), alternative = "less")$p.value

#3. wyznaczenie pvalue
#pval = 0.09583483

#4. podjęcie decyzji
#alpha = 0.1 > 0.09583483 = pval  ->  odrzucamy H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają hipotezę, że proporcja zadowolonych Polaków jest
#mniejsza niż proporcja zadowolonych Amerykanów

# (c) Socjolodzy twierdzili, że procent Polaków zadowolonych z pracy jest większy niż 75. 
#Czy próba potwierdza to przypuszczenie? Wnioskuj na poziomie istotności 0,1

#1. hipoteza
#H0: p1 <= 75%
#H1: p1 > 75%

#2. wyznaczenie statystyki testowej
prop.test(t1, n1, p = 0.75, alternative = "greater")$p.value

#3. wyznaczenie pvalue
#pval = 0.09583483

#4. podjęcie decyzji
#alpha = 0.1 > 0.09583483 = pval  ->  odrzucamy H0

#5. interpretacja


#9. - nie będzie na kolokwium
#Lekarz podejrzewa, że dany rodzaj leku zmienia wartości określonego parametru biochemicznego. I tak,
#u 9 pacjentów zmierzono poziom tego parametru przed i po podaniu leku:
#  Przed: 15 4 9 9 10 10 12 17 14
#  Po: 14 4 10 8 10 9 10 15 14
#Zakładając poziom istotności 5%, zweryfikuj podejrzenia lekarza. Załóż normalność poziomu
#parametru biochemicznego. 

przed = c(15, 4, 9, 9, 10, 10, 12, 17, 14)
po = c(14, 4, 10, 8, 10, 9, 10, 15, 14)
roznica = po - przed

#1. hipoteza
#H0: mu = 0
#H1: mu =/= 0

t.test(roznica)$p.value
#pval = 0.08052


#na poziomie istotności 5% dane nie potwierdzają hipotezy, że dany rodzaj leku zmienia wartości
#określonego parametru biochemicznego


#5
#Testowano działanie dwóch leków przeciwbólowych, przy czym spodziewano się, że pierwszy lek
#będzie działał dłużej. Podano 10 losowym pacjentom lek L1, a innej losowej grupie 15 pacjentów
#skarżących się również na bóle, lek L2. Otrzymane czasy działania (w godzinach) zapisano w 
#pliku DwiePopulacje.csv. Czy można twierdzić, że średni czas działania leku L1 jest istotnie 
#dłuższy niż dla leku L2? Testuj na poziomie istotności 10%.

alpha = 0.1

L1 = na.omit(dane$L1)
L2 = na.omit(dane$L2)

#najpierw hipoteza nt. równości wariancji
#1. hipoteza
#H0: sigma^2_1 = sigma^2_2
#H1: sigma^2_1 =/= sigma^2_2

#2. wyznaczenie statystyki testowej 
var.test(L1, L2, alternative = "two.sided")

#3. wyznaczenie pval
#pval = 0.6412

#4. podjęcie decyzji
#alpha = 0.1 < 0.6412 = pval -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają jednorodność wariancji

#tu zadanie
#1. hipoteza:
#H0: mu1 - mu2 <= 0
#H1: mu1 - mu2 > 0

#2. wyznaczenie statystyki testowej
t.test(L1, L2, var.equal = TRUE, mu = 0, alternative = "greater")

#3. wyznaczenie pvalue
#pval = 0.08234

#4. podjęcie decyzji
#alpha = 0.1 > 0.08234 = pval  ->  odrzucamy H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają, że średni czas działania leku L1 jest istotnie
#dłuższy niż dla leku L2


#2
#W budownictwie mieszkaniowym założono, że czas budowy metodą tradycyjną jest dłuższy niż czas
#budowy nową technologią. Wylosowano 10 obiektów wybudowanych metodą tradycyjną oraz 11 nową
#metodą i otrzymane czasy budowy zapisano w pliku DwiePopulacje.csv. Przyjmując, że rozkład
#czasu budowy zarówno metodą tradycyjną jak i nową jest normalny, zweryfikuj hipotezę, że
#średni czas budowy metodą tradycyjną jest dłuższy od średniego czasu budowy nową metodą. 
#Przyjmij poziom istotności 0,1.

alpha = 0.1
nowa = na.omit(dane$nowa)                #1
tradycyjna = na.omit(dane$tradycyjna)    #2

#najpierw hipoteza nt. równości wariancji
#1. hipoteza
#H0: sigma^2_1 = sigma^2_2
#H1: sigma^2_1 =/= sigma^2_2

#2. wyznaczenie statystyki testowej 
var.test(tradycyjna, nowa, alternative = "two.sided")

#3. wyznaczenie pval
#pval = 0.3613

#4. podjęcie decyzji
#alpha = 0.1 < 0.3613 = pval -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 10% dane potwierdzają jednorodność wariancji

#tu zadanie
#1. hipoteza:
#H0: mu1 - mu2 >= 0
#H1: mu1 - mu2 < 0

#2. wyznaczenie statystyki testowej
t.test(nowa, tradycyjna, var.equal = TRUE, mu = 0, alternative = "less")

#3. wyznaczenie pvalue
#pval = 0.6139

#4. podjęcie decyzji
#alpha = 0.1 < 0.6139 = pval  ->  brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 10% dane nie potwierdzają, że średni czas budowy nową metodą jest
#krótszy niż średni czas budowy metodą tradycyjną




#7.
#Badano czy częstość występowania malarii zależy od regionu. Przypadki występowania malarii w
#tropikalnych regionach były następujące: w Azji zanotowano 313 przypadków malarii typu A i 28
#przypadków malarii typu B; w Afryce zanotowano 145 przypadków malarii typu A i 56 typu B.
# (a) Czy częstość występowania malarii typu A zależy od regionu? Przyjmij poziom istotności 





# (b) Oceń metodą przedziałową, przyjmując współczynnik ufności 0,95, różnicę badanych 
#częstości występowania malarii typu A.


