dane=read.csv("dane_hip.csv", sep=";", dec=",")


#1
#Aby mogły pracować urządzenia prądotwórcze elektrowni wiatrowej średnia prędkość 
#wiatru powinna przekraczać 4 m/s. W celu stwierdzenia czy sensowna jest budowa elektrowni 
#wiatrowej mierzono przez okres roku każdego miesiąca przeciętną prędkość wiatru w okolicach 
#Darłowa uzyskując wyniki (w m/s):
#  5,9 4,4 5,4 3,8 4,0 4,2 3,4 3,6 4,6 6,5 5,6 4,8.
#Zakładając, że prędkość wiatru jest zmienną losową o rozkładzie normalnym oraz przyjmując 
#poziom istotności α=0,05 sprawdź, czy okolice Darłowa nadają się do budowy elektrowni 
#wiatrowych. W tym celu skonstruuj odpowiednią procedurę testującą. Porównaj otrzymane wyniki z uzyskanymi po
#zastosowaniu odpowiedniej funkcji z pakietu R. 

#1. hipoteza:
#H0: mu <= 4 
#H1: mu > 4

wiatr = na.omit(dane$wiatr)
n = length(wiatr)

#2. t = (Xhat - mu0)/(S/sqrt(n))
t = (mean(wiatr) - 4)/(sd(wiatr)/sqrt(n))

#3. wyznaczanie obszaru krytycznego
alpha = 0.05
qt(1-alpha, n-1)

#R = (1.8 ; inf)

#4. podjęcie decyzji
#t = 2.41 nalezy do R -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają, że średnia prędkość wiatru przekracza 4 m/s

#odp:
#okolice Darłowa nadają się do budowy elektrowni wiatrowych

#wersja z funkcją w R (pamiętaj o 5 punktach)
#1. hipoteza:
#H0: mu <= 4 
#H1: mu > 4

#2.
t.test(wiatr, mu = 4, alternative="greater")$p.value

#3. wyznaczanie obszaru krytycznego
#pval = 0.017045 (lepiej zaokrąglać w dól lub wziąć już całą wartość jaka wychodzi)

#4. podjęcie decyzji
#alpha = 0.05 > 0.017 = pval -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają, że średnia prędkość wiatru przekracza 4 m/s

#odp:
#okolice Darłowa nadają się do budowy elektrowni wiatrowych


#2
#Przyjęto, że współczynnik efektywności pompy cieplnej COP jest zadawalający, 
#gdy jego średnia wartość wynosi co najmniej 3,5 (co oznacza, że ponad 70% dostarczonego 
#przez pompę ciepła pochodzi z naturalnego źródła ciepła, a reszta pochodzi z pracy sprężarki). 
#Potencjalny nabywca pompy ma wątpliwości i wysunął przepuszczenie, że współczynnik 
#efektywności pompy cieplnej w jego gospodarstwie domowym jest znacznie mniejszy niż 3,5. 
#Przez pewien okres mierzono współczynnik COP w tym gospodarstwie otrzymując następujące 
#wyniki:
#  3,5 3,2 3,6 3,0 3,3 3,8 2,5 3,0 3,7 3,9.
#Zakładając, że zmienna opisująca wartości współczynnika COP jest zmienną losową o rozkładzie
#normalnym i na podstawie powyższych wyników (przyjmując poziom istotności α=0,01) sprawdź, 
#czy wątpliwości nabywcy są słuszne.

alpha = 0.01

#1. hipoteza
#H0: mu >= 3.5
#H1: mu < 3.5

#2. wybór statystyki
pompa = na.omit(dane$pompa)
t.test(pompa, mu = 3.5, alternative = "less")$p.value

#3. wyznaczenie pval
#pval = 0.1520576

#4. podjecie decyzji
#alpha = 0.01 < 0.1520576 = pval -> nie ma podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 1% dane nie potwierdzają podejrzeń potencjalnego nabywcy, że 
#efektywność pompu cieplnej w jego gospodarstwie jest mniejszy niż 3.5

#odp
#wątpliwości nabywcy były bezpodstawne


#3.
#Wiadomo, że rozkład wyników pomiarów głębokości morza w pewnym rejonie jest normalny z
#odchyleniem standardowym 5 m. Dokonano 5 niezależnych pomiarów głębokości morza w pewnym
#rejonie i otrzymano następujące wyniki (w m):
#  862 870 876 866 871.
#Przyjmując poziom istotności α=0,05 zweryfikuj hipotezę, że średnia głębokość morza w tym 
#rejonie jest różna od 870 m. 

#1. hipoteza
#H0: mu = 870
#H1: mu =/= 870

#2. wybór statystyki
morze = na.omit(dane$morze)
library(BSDA)
z.test(morze, sigma.x = 5, mu = 870, alternative="two.sided")$p.value

#3. wyznaczenie pval
#pval = 0.6547208

#4. podjecie decyzji
#alpha = 0.05 < 0.6547208 = pval -> nie ma podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzaja, że średnia głębokość morza w tym rejonie 
#jest różna od 870 m

#odp.
#hipoteza jest nie prawdziwa


#4
#Automat produkuje blaszki określonych wymiarów o nominalnej grubości 0,04 mm. Wylosowana
#próba 40 blaszek dała następujące wyniki:
#  0,048 0,028 0,037 0,033 0,054 0,046 0,041 0,043 0,044 0,05 0,047 0,052 0,053 0,048
#0,027 0,056 0,058 0,039 0,026 0,034 0,043 0,042 0,047 0,022 0,046 0,04 0,036
#0,043 0,041 0,044 0,043 0,044 0,038 0,046 0,041 0,038 0,047 0,03 0,041 0,049.
#Przyjmując poziom istotności α=0,02 sprawdź poprawność twierdzenia, że produkowane przez 
#ten automat blaszki są grubsze niż nominalna grubość. 

#1. hipoteza
#H0: mu <= 0.04
#H1: mu > 0.04

#2. wybór statystyki
blaszki = na.omit(dane$blaszki)
zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = 0.04, alternative = "greater")$p.value

#3. wyznaczenie pval
#pval = 0.05041335

#4. podjecie decyzji
#alpha = 0.02 < 0.05041335 = pval -> nie ma podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 2% dane nie potwierdzają, że średnia grubość blaszek
#przekraczają nominalną grubość 0.04 mm


#5
#W próbce laboratoryjnej mleka spożywczego wykonano 10 oznaczeń (w %) zawartości tłuszczu 
#i uzyskano:
#  1,5 1,8 1,5 1,7 1,6 1,6 1,8 1,6 1,7 1,6.
#Przyjmując, że rozkład zawartości tłuszczu w mleku spożywczym jest normalny, odpowiedź na
#poniższe pytania (przyjmij poziom istotności α = 0,05):
mleko = na.omit(dane$mleko)
alpha = 0.05

# (a) Czy obserwacje przeczą hipotezie, że średnia zawartość tłuszczu w mleku wynosi 1,7 %?

#1. hipoteza
#H0: mu = 1.7
#H1: mu =/= 1.7

#2. wybór statystyki
t.test(mleko, mu = 1.7)$p.value #można bez two.sided

#3. wyznaczenie pval
#pval = 0.1113732

#4. podjecie decyzji
#alpha = 0.05 < 0.1113732 = pval -> nie ma podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają, żze średnia zawartość tłuszczu w mleku 
#wynosi 1.7%

#odp
#obserwacje nie przeczą hipotezie, że średnia zawartość tłuszczu w mleku wynosi 1.7

#(b) Czy można twierdzić, że wariancja zawartości tłuszczu w mleku jest mniejsza 
#niż 0,02 (%)2

#1. hipoteza
#H0: sigmasq >= 0.02
#H1: sigmasq < 0.02

#wzór własny
#2. wybór statystyki
n=length(mleko)
chi2 = (n-1)*(var(mleko)/0.02) 

#3.wyznaczenie przedziału krytycznego
qchisq(alpha,n-1)
#R = (0 ; 3.32)

#4. podjęcie decyzji
# chi2 nie nazleży do R -> nie ma podstaw do odrzucenia H0

#wersja w R
#2. wybór statystyki
library(TeachingDemos)
sigma.test(mleko, sigmasq = 0.02, alternative="less")$p.value

#3. wyznaczenie pval
#pval = 0.15

#4. podjecie decyzji
#alpha = 0.05 < 0.18 = pval -> nie ma podstaw do odrzucenia H0

#wspólna interpretacja:
#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają, że wariancja zawartości tłuszczu
#w mleku jest mniejsza niż 0.02%^2


#8
#Sondaż opinii publicznej na temat frekwencji oczekiwanej na wyborach wykazał, że w losowo
#wybranej grupie 2500 osób 1600 zamierza uczestniczyć w głosowaniu. Czy na poziomie 
#istotności równym 0,05 próba przeczy twierdzeniu, że 60% ogółu osób zamierza wziąć 
#udział w wyborach? Skonstruuj odpowiednią procedurę testującą. Porównaj otrzymane 
#wyniki z uzyskanymi po zastosowaniu testu dokładnego dostępnego w pakiecie R.
t = 1600 
n = 2500
alpha = 0.05
phat = t/n
p0 = 0.6

#1. hipoteza
#H0: p = 0.6
#H1: p =/= 0.6

#2. wybór statystyki
z = (phat - p0)/(sqrt(p0*(1-p0))/sqrt(n))

#3. wyznaczenie przedziału krytycznego
qnorm(1 - alpha/2)
#R = (-inf ; -1.96)+(1.96 ; inf) - uwaga na minusa, trzeba zmniejszyc przedzialy

#4. podjecie decyzji
#z = 4.082 nalezy do R - odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają hipotezę, że proporcja osób zamierzających 
#wziąć udział w wyborach jest różna od 60%

#funkcja w R:
binom.test(t, n, p=p0)$p.value
#alpha = 0.05 > 4.413383e-05 = pval -> odrzucamy H0
#reszta taka sama



#PD:
#reszta zadań do domu

#6
#Kukułki podrzucają swoje jaja różnym ptakom, między innymi małym strzyżykom. Obserwacje
#przyrodników wskazują, że kukułka potrafi znieść jajo wielkości podobnej do jaj „adopcyjnych
#rodziców”. Zmierzono długość [w mm] 21 jaj podrzuconych strzyżykom otrzymując wyniki:
#  17,93 18,52 19,66 14,30 17,52 20,76 20,26
#  19,82 21,40 16,54 18,64 17,62 20,79 19,14
#  16,74 14,93 18,56 15,43 15,19 21,05 20,79.
#Wiadomo, że średnia długość jaj strzyżyka wynosi 17 mm, a odchylenie standardowe 2,5 mm.
kukulki = na.omit(dane$kukulki)
#Zakładamy, że badana cecha ma rozkład normalny.
# (a) Na poziomie istotności 0,05 zweryfikuj stawianą przez przyrodników hipotezę dotyczącą:
#   (i) średniej długości podrzuconych jaj;

#1. hipoteza
#H0: mu = 17
#H1: mu =/= 17

#2. wybór statystyki
t.test(kukulki, mu = 17)$p.value #można bez two.sided

#3. wyznaczenie pval
#pval = 0.01011002

#4. podjecie decyzji
#alpha = 0.05 > 0.01011002 = pval -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają hipotezę, że kukułki zniosły jaja o średniej
#długości różnej od 17mm

#  (ii) wariancji długości podrzuconych jaj
#1. hipoteza
#H0: sigmasq = 2.5
#H1: sigmasq =/= 2.5

#2. wybór statystyki
sigma.test(kukulki, sigmasq = 2.5^2)$p.value

#3. wyznaczenie pval
#pval = 0.4983805

#4. podjecie decyzji
#alpha = 0.05 < 0.4983805 = pval -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają, że wariancja długości jajek kukułek jest
#różna od 2.5

#(b) Zbuduj 95% przedział ufności dla średniej długości jaj podrzucanych strzyżykom. 
#Jaki jest związek między skonstruowanym przedziałem ufności a decyzją podjętą przy 
#testowaniu hipotez?
t.test(kukulki, conf.level = 0.95)$conf.int
#z ufnością 95% przedział (17.36161 ; 19.36124) [mmw] pokrywa pewną nieznaną długość jajek 
#podrzucanych strzyżykom przez kukułki


#7.
#Agencja Ochrony Środowiska ustaliła dopuszczalne średnie zanieczyszczenie na terenach
#przemysłowych jako 55 miligramów na m3, (w promieniu 2 km od fabryki), przy odchyleniu
#standardowym 18 (miligramów na m3). Ekolog zmierzył poziom zanieczyszczeń na terenie
#przemysłowym 100 razy, w różnych dniach i nocach, a następnie obliczył średnią i odchylenie
#standardowe pomiarów: odpowiednio 60 i 20 miligramów na m3. Przyjmując poziom istotności 
#0,01, zweryfikuj, czy dane potwierdzają, że fabryka działa niezgodnie z prawem (w celu 
#weryfikacji hipotezy dotyczącej wariancji należy przyjąć normalność poziomu zanieczyszczeń).

n=100
alpha=0.01

#hipoteza średniej:
#1. hipoteza
#H0: mu <= 55
#H1: mu > 55

#2. wybór statystyki
zsum.test(60, 20, 100, mu=55, alternative="greater")$p.value

#3. wyznaczenie pval
#pval = 0.006209665

#4. podjecie decyzji
#alpha = 0.01 > 0.006209665 = pval -> odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają, że średnie zanieczyszczenie na terenie 
#przemysłowym jest większe niż 55mg na m3

#hipoteza wariancji:
#1. hipoteza
#H0: sigmasq <= 18
#H1: sigmasq > 18

#2. wybór statystyki
chi2 = (n-1)*(20^2/18^2)#chi2 = 122.222

#3.wyznaczenie przedziału krytycznego
qchisq(1 - alpha,n-1)
#R = (134.6416 ; inf)

#4. podjecie decyzji
#chi2 nie należy do R  -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają, że odchylenie standardowe zanieczyszczenia 
#na terenie przemysłowym jest większe niż dopuszczalne 18[mg na m3]

#odp:
#


#9.
#Przeprowadzono badanie jakości jaj kurzych pochodzących z pewnej fermy. Zakłada się z góry, 
#że 2% jaj jest złej jakości. Wylosowano 1200 jaj do zbadania i wśród nich 16 okazało się 
#złej jakości. Czy obserwacje przeczą przyjętemu założeniu i potwierdzają, że frakcja ta w 
#badanej fermie jest mniejsza? Wnioskuj na poziomie istotności 0,05. W tym celu skonstruuj 
#odpowiednią procedurę testującą. Porównaj otrzymane wyniki z uzyskanymi po zastosowaniu 
#testu dokładnego dostępnego w pakiecie R.

n=1200
t=16
phat=16/1200
p0=0.02
alpha=0.05

#1. hipoteza
#H0: p >= 0.02
#H1: p < 0.02

#2. wybór statystyki
z = (phat - p0)/(sqrt(p0*(1-p0))/sqrt(n)) #z = -1.6495

#3. wyznaczenie przedziału krytycznego
qnorm(1 - alpha)
#R = (-inf ; -1.644)

#4. podjecie decyzji
# z należy do R ->  odrzucamy H0

#5. interpretacja
#na poziomie istotności 5% dane potwierdzają hipotezę, że proporcja jajek złej jakości jest
#mniejsza niż 0.02

#funkcja w R
binom.test(t, n, p=p0, alternative="less")$p.value
#alpha = 0.05 < 0.05451192 = pval -> nie odrzucamy H0
#5. interpretacja
#na poziomie istotności 5% dane nie potwierdzają hipotezy, że proporcja jajek złej 
#jakości jest mniejsza niż 0.02


#10.
#W sondażu przeprowadzonym przez pracownię badania opinii społecznych wśród 1100 dorosłych
#Polaków, 1000 ankietowanych odpowiedziało, że w ubiegłym miesiącu nie przeczytało żadnej 
#książki. Pozostali twierdzili, że przeczytali przynajmniej jedną książkę. Na podstawie tych 
#danych, na poziomie 0,05, stwierdzić, czy opinia, że procent Polaków, którzy nie 
#przeczytali żadnej książki jest większy niż 90 jest uzasadniona?

n=1100
t=1000

#1. hipoteza
#H0: p0 <= 55
#H1: p0 > 55

#2. wybór statystyki
binom.test(t, n, p=0.9, alternative = "greater")$p.value

#3. wyznaczenie pval
#pval = 0.1700898

#4. podjecie decyzji
#alpha = 0.05 < 0.1700898 = pval -> brak podstaw do odrzucenia H0

#5. interpretacja
#na poziomie istotności 5% danie nie potwierdzają hipotezy, że procent Polaków, którzy nie 
#przeczytali żadnej książki jest większy niż 90%