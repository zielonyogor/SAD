#1
#Inżynier chemiczny chce sprawdzić, czy różne warunki ciśnieniowe mają wpływ na 
#średnią produkcję pewnego wyrobu z danego typu surowca. Poniższa tabela zawiera wyniki 
#przeprowadzonego eksperymentu (w g/l). Czy ciśnienie ma wpływ na wielkość produkcji? 
#Wykorzystaj funkcję pakietu R o nazwie anova. Wnioskuj na poziomie istotności 0,05. Analizę 
#wariancji poprzedź testem równości wariancji, np. z wykorzystaniem funkcji bartlett.test.

cisnienie = read.csv("Anova_cisnienie.csv", sep=";")
obiekty = rep(names(cisnienie), each = length(cisnienie$Niskie))
wyniki = c(cisnienie$Niskie, cisnienie$Srednie, cisnienie$Silne, cisnienie$BardzoSilne)
data.frame(obiekty, wyniki)

# średnie próbkowe
srednie = sapply(split(wyniki, obiekty), mean)
srednie

#hipoteza o jednorodności wariancji
#H0: sigmasq_1 = sigmasq_2 = sigmasq_3 = sigmasq_4
#H1: ~H0

bartlett.test(wyniki~obiekty)

#alpha = 0.05 < p-value = 0.5009  ->  brak podstaw do odrzucienie H0
#na poziomie istotności 5% możemy przeprowadzić analizę ANOVA

#hipoteza o wpływie ciśnienia
#H0: mu1 = mu2 = mu3 = mu4
#H1:~H0

anova(lm(wyniki~obiekty))

#1. sposób z pval
#alpha = 0.05 < 0.09735 = pval  ->  brak podstaw do odrzucenia H0

#2. sposób z F
alpha = 0.05
k = length(names(cisnienie))
n = length(wyniki)
qf(1 - alpha, k-1, n-k)
#F = 2.2665 < 2.866266 = qF  ->  brak podstaw do odrzucenia H0

#na poziomie istotności 5% dane nie potwierdzają, że ciśnienie ma istotny 
#wpływu na wielkość produkcji


#2.
#W doświadczeniu badano zawartość popiołu (części niepalnych) dla ekogroszku 
#wyprodukowanego na bazie węgla wysokogatunkowego pochodzącego z pięciu różnych kopalni.
#Czy średnie zawartości popiołu dla ekogroszku produkowanego w pięciu kopalniach można 
#uznać za jednakowe? Wykonaj analizę wariancji na poziomie istotności 0,01.

kopalnie = read.csv("Anova_kopalnie.csv", sep=";", dec=",")
obiekty = rep(names(kopalnie), each = length(kopalnie$K1))
wyniki = c(kopalnie$K1, kopalnie$K2, kopalnie$K3, kopalnie$K4, kopalnie$K5)
data.frame(obiekty, wyniki)

# średnie próbkowe
srednie = sapply(split(wyniki, obiekty), mean)
srednie

#hipoteza o jednorodności wariancji
#H0: sigmasq_1 = sigmasq_2 = ... = sigmasq_5
#H1: ~H0

bartlett.test(wyniki~obiekty)

#alpha = 0.01 < p-value = 0.03188  ->  brak podstaw do odrzucienie H0
#na poziomie istotności 1% możemy przeprowadzić analizę ANOVA

#hipoteza o średniej zawartości popiołu
#H0: mu1 = mu2 = ... = mu5
#H1:~H0

anova(lm(wyniki~obiekty))

#alpha = 0.01 < 0.4594 = pval  ->  brak podstaw do odrzucenia H0

#na poziomie istotności 1% dane nie potwierdzają, że średnie zawartości
#popiołu w pięciu kopalniach są różne


#3
#Każdym z trzech mikrometrów zmierzono kilkukrotnie ten sam przedmiot i uzyskano wyniki.
#Zakładając, że błędy pomiarów mają rozkłady normalne o takiej samej wariancji, na poziomie 
#istotności 0,05 zweryfikuj hipotezę, że wybór mikrometru ma wpływ na uzyskane wyniki.

mikrometr = read.csv("Anova_mikrometr.csv", sep=";", dec=",")
obiekty = rep(names(mikrometr), c(length(na.omit(mikrometr$mikrometrI)), 
          length(na.omit(mikrometr$mikrometrII)), length(na.omit(mikrometr$mikrometrIII))))
wyniki = c(na.omit(mikrometr$mikrometrI), na.omit(mikrometr$mikrometrII),
           na.omit(mikrometr$mikrometrIII))
data.frame(obiekty, wyniki)

srednie = sapply(split(wyniki, obiekty), mean)
srednie

#bez hipotezy o wariancji

#hipoteza o mikrometrze
#H0: mu1 = mu2 = mu3
#H1:~H0

anova(lm(wyniki~obiekty))

#alpha = 0.05 < 0.06859 = pval  ->  brak podstaw do odrzucenia H0

#na poziomie istotności 1% dane nie potwierdzają,że wybór mikrometru ma wpływ na uzyskane wyniki 


#4
#Populacja sportowców została ostrzeżona, że palenie papierosów opóźnia rozwój. Jedną z miar 
#wpływu palenia na rozwój jest badanie rytmu zatokowego serca. Przeprowadzono eksperyment, w 
#którym zbadano rytm zatokowy serca u 24 sportowców po zadanym wysiłku fizycznym i 5-min.

sportowcy = read.csv("Anova_sportowcy.csv", sep=";")
obiekty = rep(names(sportowcy), each=length(sportowcy$Niepalacy))
wyniki = c(sportowcy$Niepalacy, sportowcy$Lekkopalacy, sportowcy$Sredniopalacy, sportowcy$Duzopalacy)
data.frame(obiekty, wyniki)

alpha = 0.01

#  (a) Zakładając, że rytm serca u każdego rodzaju palaczy ma rozkład normalny, na poziomie 
#istotności 0,01, sprawdź czy palenie papierosów może wpływać na rytm zatokowy serca.

#hipoteza o jednorodności wariancji
#H0: sigmasq_1 = sigmasq_2 = sigmasq_3 = sigmasq_4
#H1: ~H0

bartlett.test(wyniki~obiekty)

#alpha = 0.01 < p-value = 0.8517  ->  brak podstaw do odrzucienie H0
#na poziomie istotności 1% możemy przeprowadzić analizę ANOVA

#hipoteza o paleniu papierosów
#H0: mu1 = mu2 = mu3 = mu4
#H1:~H0

anova(lm(wyniki~obiekty))

#alpha = 0.01 > 0.003979 = pval  ->  odrzucamy H0

#na poziomie isotności 1% dane potwierdzają, że średnie są różne, więc palenie papierosów może 
#wpływać na rytm zatokowy serca


#  (b) Zastosuj test uczciwych istotnych różnic (honest significant differences) Tukey’a do 
#wyznaczenia grup jednorodnych porównywanych średnich obiektowych.

TukeyHSD(aov(wyniki~obiekty),ordered=T,conf.level = 1-alpha)
# grupy jednorodne, gdy padj > alpha:
# S-N, D-N, D-S, L-D => S-N-D, L-D

# 2. sposób
obiekty = sapply(obiekty, function(slowo) substr(slowo, 1, 1))
plot(TukeyHSD(aov(wyniki~obiekty),ordered=T,conf.level = 1-alpha))
# grupy jednorodne gdy 0 należy do przedziału:
# S-N, D-N, D-S, L-D => S-N-D, L-D


#5
#Badano masę tarczycy chomików w zależności od ich poziomu homozygotyczności (inbredu), wyróżniając 
#cztery grupy (I – osobniki niezinbredowane, II – osobniki o poziomie inbredu z przedziału
#<0,01 – 0,10>, III – osobniki o poziomie inbredu z przedziału <0,11 – 0,20>, IV – osobniki o poziomie
#inbredu od 0,21).

chomiki = read.csv("Anova_chomiki.csv", sep=";")
obiekty = rep(names(chomiki), c(length(na.omit(chomiki$I)), length(na.omit(chomiki$II)), 
                                length(na.omit(chomiki$III)), length(na.omit(chomiki$IV))))
wyniki = c(na.omit(chomiki$I), na.omit(chomiki$II), na.omit(chomiki$III), na.omit(chomiki$IV))
data.frame(obiekty, wyniki)
alpha = 0.05

#  (a) Sprawdź (przyjmując poziom istotności 0,05) czy słuszne jest przypuszczenie, że masa gruczołu
#tarczycowego zależy od poziomu inbredu.

#hipoteza o jednorodności wariancji
#H0: sigmasq_1 = sigmasq_2 = sigmasq_3 = sigmasq_4
#H1: ~H0

bartlett.test(wyniki~obiekty)

#alpha = 0.05 < p-value = 0.2139  ->  brak podstaw do odrzucienie H0
#na poziomie istotności 5% możemy przeprowadzić analizę ANOVA

#hipoteza o tarczycy
#H0: mu1 = mu2 = mu3 = mu4
#H1:~H0

anova(lm(wyniki~obiekty))

#alpha = 0.05 > 0.02398 = pval  ->  odrzucamy H0

#na poziomie isotności 5% dane potwierdzają, że masa gruczołu zależy od poziomu inbredu


#  (b) Zastosuj test HSD Tukey’a do wyznaczenia grup jednorodnych porównywanych średnich
#obiektowych.

TukeyHSD(aov(wyniki~obiekty),ordered=T,conf.level = 1-alpha)
#grupy jednorodne, padj > alpha:
#II-I, III-I, III-II, IV-II, IV-III  ->  I-II-III, II-III-IV 


#6
#Student inżynierii przemysłowej pomógł zespołowi badawczemu ocenić różne strategie lokalizacji
#pułapek zapachowych na ćmy cygańskie. Zmienną odpowiedzi jest szacunkowy odsetek uwięzionej rodzimej 
#populacji płci męskiej.

pulapki = read.csv("Anova_pulapki.csv", sep=";")
obiekty = rep(names(pulapki), each=length(pulapki$rozsiany))
wyniki = c(pulapki$rozsiany, pulapki$skoncentrowany, pulapki$roslina.zywicielka, 
           pulapki$powietrzny, pulapki$gruntowy)
data.frame(obiekty, wyniki)
aplha = 0.05

#  (a) Czy strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich? Przyjmij
#poziom istotności 0,05 i normalność proporcji uwięzionych ciem cygańskich. Zweryfikuj założenie
#dotyczące jednorodności wariancji przed wykonaniem ANOVA.

#hipoteza o jednorodności wariancji
#H0: sigmasq_1 = sigmasq_2 = ... = sigmasq_5
#H1: ~H0

bartlett.test(wyniki~obiekty)

#alpha = 0.05 < p-value = 0.06804  ->  brak podstaw do odrzucienie H0
#na poziomie istotności 5% możemy przeprowadzić analizę ANOVA

#hipoteza o ćmach
#H0: mu1 = mu2 = ... = mu5
#H1:~H0

anova(lm(wyniki~obiekty))

#alpha = 0.05 > 3.252e-09 = pval  ->  odrzucamy H0

#na poziomie isotności 5% dane potwierdzają, że strategia lokalizacji może mieć wpływ na liczbę
#uwięzionych ciem

#  (b) Zastosuj test HSD Tukey’a do wyznaczenia grup jednorodnych porównywanych średnich
#obiektowych. 

TukeyHSD(aov(wyniki~obiekty),ordered=T,conf.level = 1-alpha)
#grupy jednorodne, padj > alpha:
#r-g, s-rz, p-rz, p-s  -> r-g, a-rz-p
