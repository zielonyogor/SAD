#1
#Stowarzyszenie Russela Reynolda przeprowadziło ankietę wśród emerytowanych menedżerów wyższego
#szczebla, którzy wrócili do pracy. Ustalili, że po powrocie do pracy 38% było zatrudnionych w innej
#organizacji, 32% samozatrudnionych, 23% było freelancerami lub konsultantami, a 7% założyło własne firmy.
#Aby sprawdzić, czy te wartości procentowe są zgodne z odsetkami mieszkańców hrabstwa Allegheny, lokalny
#badacz przeprowadził ankietę wśród 300 emerytowanych menedżerów, którzy wrócili do pracy, i odkrył, że
#122 pracowało dla innej firmy, 85 prowadziło działalność na własny rachunek, 76 pracowało jako freelancer
#lub doradzało, a 17 założyło własne firmy. Czy przy istotności 10% dane potwierdzają, że odsetki
#poszczególnych zatrudnionych w hrabstwie Allegheny różnią się od ich odpowiedników w skali całego kraju?
 
#H0: rozkład częstotliwości emerytów,, którzy wrócili do pracy w hrabstwie Allegheny odpowiada
# ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reynolda
#H1: ~H0

alpha=0.1
obs = c(122, 85, 76, 17)
oczekiwane=c(0.38, 0.32, 0.23, 0.07)
chisq.test(obs, p=oczekiwane)

#alpha = 0.1  <  0.3485 = pval  ->  brak podstaw do odrzucenia H0
#na poziomie istotności 10% dane potwierdzają, że rozkład częstotliwości emerytówm którzy wrócili do 
#pracy w hrabstwie Allegheny odpowiada  ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reynolda


#2
#Badacz przeczytał w artykule, że liczba zgonów związanych z bronią palną wśród osób w wieku od 1 
#do 18 lat rozkładała się następująco: 74% to wypadki, 16% to zabójstwa, a 10% to samobójstwa. 
#W jej okręgu w ubiegłym roku doszło do 68 wypadków śmiertelnych, 27 zabójstw i 5 samobójstw. 
#Czy na poziomie istotności 10% dane potwierdzają, że odsetki poszczególnych zgonów różnią się 
#od przedstawionych w artykule?

#H0: rozkład częstotliwość zgonów związanych z bronią palną wśród osób w wieku 1-18 lat odpowiada
#ogólnemu rozkładowi z artykułu
#H1: ~H0

alpha=0.1
obs=c(68, 27, 5)
oczekiwane=c(0.74, 0.16, 0.1)
chisq.test(obs, p=oczekiwane)
#alpha = 0.1  >  0.005121  ->  odrzucamy H0
#na poziomie istotności 10% dane potwierdzają, że odsetki poszczególnych zgonów różnią się od tych
#przedstawionych w artykule


#3
#M&M/Mars, producent cukierków Skittles, twierdzi, że mieszanka smakowa wynosi 20% dla każdego smaku.
#Skittles to połączenie cukierków o smaku cytrynowym, limonkowym, pomarańczowym, truskawkowym i
#winogronowym. Poniższe dane przedstawiają wyniki czterech losowo wybranych torebek Skittles i ich
#mieszanek smakowych.
#Wykonaj test, aby porównać zaobserwowane (całkowite) wartości z wartościami oczekiwanymi. Załóżmy
#poziom istotności 0,05.

#H0: rozkład częstotliwości każdego smaku odpowiada rozkładowi od producenta Skittles
#H1: ~H0

alpha=0.05
obs=c(7+20+4+12, 20+5+16+9, 10+5+13+16, 7+13+21+3, 14+17+4+17)
oczekiwane=rep(0.2, 5)
chisq.test(obs, p=oczekiwane)
#alpha = 0.05 < 0.8369 = pval  ->  brak podstaw do odrzucenia H0
# na poziomie istotności 5% dane potwierdzają, że zaobserwowane częstotliwości odpowiadają rozkładowi
#firmy Skittles


#4
#Poniższe przykładowe dane przedstawiają stężenie ozonu (mierzone w częściach na 100 milionów) w
#powietrzu w centrum miasta przez 78 kolejnych letnich dni w 2004 roku:
#Powyższe dane można pogrupować w przedziały w następujący sposób:
#Należy pamiętać, że do ostatnich klas kwalifikuje się mniej niż 5 pomiarów. Na poziomie istotności 0,05
#sprawdź, czy stężenie ozonu ma rozkład normalny

#H0: stężenie ozonu w powietrzu w centrum mialsta ma rozkład normalny
#H1: stężenie nie ma rozkładu normalnego

#sposób z wykładu

dane=read.csv("normalnosc_ozon.csv", sep=";", dec=",")

br = seq(0,12, 2) #min,max
br
hist(dane$ozon, breaks=br, freq=F)

tbar=mean(dane$ozon)
sdt=sd(dane$ozon)

normprobabilities=c();
for(i in 1:length(br) - 1) {
  normprobabilities=c(normprobabilities, 
                      pnorm(br[i+1], tbar, sdt) - pnorm(br[i], tbar, sdt))
}
normprobabilities[1] = pnorm(br[2], tbar, sdt)
normprobabilities[6] = 1 - pnorm(br[6], tbar, sdt)
normprobabilities

obs= table(cut(dane$ozon, breaks=br))

normprobabilities[5] = normprobabilities[5] + normprobabilities[6]
normprobabilities=normprobabilities[-6]
obs[5] = obs[5] + obs[6]
obs = obs[-6]

chisq.test(obs, p=normprobabilities)
#alpha = 0.05 < 0.9206 = pval  ->  brak podstaw do odrzucenia H0
#na poziomie istotności 5% dane potwierdzają, że rozkład ozonu ma rozkład normalny

#sposoby na to w R
ozon = dane$ozon
pearson.test(ozon, adjust = TRUE) #wczesniej bylo FALSE
#alpha = 0.05  <  0.2689 = pval  ->  brak podstaw do odrzucenia H0
#na poziomie istotności 5% dane potwierdzają, że rozkład ozonu ma rozkład normalny

lillie.test(ozon)
#alpha = 0.05  <  0.2774 = pval  ->  nadal brak podstaw

shapiro.test(ozon)
#alpha = 0.05  <  0.1098 = pval  >  nadal brak podstaw


#5
#Poniższe dane dotyczą objętości guza (w mm^3) w grupie losowo wybranych 100 myszy:
#Na poziomie istotności 0.1 zweryfikuj czy objętość guza ma rozkład normalny.

obs = c(rep(1, 10), rep(3, 25), rep(5, 35), rep(7, 20), rep(9, 10))
br = seq(0, 10, 2)

tbar=mean(obs)
sdt=sd(obs)

normprobabilities=c();
for(i in 1:length(br) - 1) {
  normprobabilities=c(normprobabilities, 
                      pnorm(br[i+1], tbar, sdt) - pnorm(br[i], tbar, sdt))
}
normprobabilities[1] = pnorm(br[2], tbar, sdt)
normprobabilities[5] = 1 - pnorm(br[5], tbar, sdt)
sum(normprobabilities)

obserwowane= c(10, 25, 35, 20, 10)
chisq.test(obserwowane, p=normprobabilities)
#alpha = 0.1  <  0.9521 = pval  -> brak podstaw do odrzucenia H0
#na poziomie istotności 10% dane potwierdzają, że objętość guza ma rozkład normalny


#6
#Poniższe dane przedstawiają liczbę punktów uzyskanych przez grupę studentów na koniec semestru:
#Na poziomie istotności 0,01 sprawdź, czy oceny tej grupy uczniów mają rozkład normalny

dane = read.csv("normalnosc_punkty.csv", sep=";")

#H0: liczba punktów ma rozkład normalny
#H1: ~H0

shapiro.test(dane$punkty)
#alpha = 0.01  >  0.0006203 = pval  ->  odrzucamy H0

lillie.test(dane$punkty)
#alpha = 0.01 <  0.005197 = pval  ->  brak podstaw do odrzucenia


#7
#Socjolog pragnie sprawdzić, czy liczba lat nauki danej osoby ma związek z jej miejscem zamieszkania.
#Wybrano próbę 88 osób i sklasyfikowano, jak pokazano.
#Czy na poziomie istotności 0,05 socjolog może stwierdzić, że miejsce zamieszkania danej osoby 
#zależy od liczby lat studiów?

#H0: liczba lat studiów jest NIEZALEŻNA od miejsca zamieszkania
#H1: ~H0

miejski = c(15, 12, 8)
podmiejski = c(8, 15, 9)
wiejski = c(6, 8, 7)

TK = data.frame(miejski, podmiejski, wiejski)
chisq.test(TK)
#alpha = 0.05  <  0.5569 = pval  ->  brak podstaw do odrzucenia H0
#na poziomie istotności 5% dane nie potwierdzają, że liczba lat studiów jest ZALEŻNA od miejsca
#zamieszkania


#8
#Badacz wybrał 100 pasażerów każdej z 3 linii lotniczych i zapytał ich, czy linia lotnicza zgubiła 
#ich bagaż podczas ostatniego lotu. Dane przedstawiono w tabeli. Czy na poziomie istotności 0,05 
#dane potwierdzają, że odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, zależy od linii 
#lotniczej?

#H0: odsetek pasażerów, którzy zgubili bagaż jest NIEZALEŻNA od linii lotniczej
#H1: ~H0

tak = c(10, 7, 4)
nie = c(90, 93, 96)

TK = data.frame(tak, nie)
chisq.test(TK)
#alpha = 0.05  <  0.251 = pval  ->  brak podstaw do odrzucenia H0
#na poziomie istotności 5% dane nie potwierdzają, że odsetek pasażerów, którzy zgubili bagaż jest 
#ZALEŻNA od linii lotniczej


#9
#W Senacie planowane jest głosowanie nad projektem ustawy zezwalającej na instalowanie anten 
#satelitarnych dowolnej wielkości na obszarach objętych ograniczeniami wykonawczymi. Podobną 
#ustawę przyjęła Izba. Przeprowadzono badanie opinii publicznej, aby sprawdzić, czy odczucia danej 
#osoby w związku z ograniczeniami dotyczącymi anten satelitarnych są powiązane z jej wiekiem. 
#Czy dane potwierdzają, że opinia zależy od wieku? Załóżmy poziom istotności 0,05.

#H0: opinia jest NIEZALEŻNA od wieku
#H1: ~H0

za = c(96, 96, 90, 36)
przeciw = c(201, 189, 195, 234)
nie_wiem = c(3, 15, 15, 30)

TK = data.frame(za, przeciw, nie_wiem)
chisq.test(TK)  
#alpha = 0.05 > 0 ~= pval  ->  odrzucamy H0
#na poziomie istotności 5% dane potwierdzają, że opinia jest ZALEŻNA od wieku
