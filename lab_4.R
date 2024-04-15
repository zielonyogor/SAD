#Agencja reklamowa uruchomiła kampanię mającą wprowadzić na rynek nowy produkt.
#Na koniec kampanii przeprowadzono badanie na podstawie którego stwierdzono, że co
#najmniej 25% konsumentów kojarzy reklamowany produkt. Jeżeli 25% konsumentów
#rzeczywiście zna nowy produkt, to jakie jest prawdopodobieństwo, że nie więcej niż 232
#losowo wybranych konsumentów spośród 1000 zna produkt?

p=0.25
n=1000
t=232 #suma z sukcesu
#P(phat<=232)
#phat ma rozkład N(p,sqrt(p*(1-p)/n))
pnorm(t/n, p, sqrt(p*(1-p)/n))


###ESTYMACJA

dane=read.csv("dane_est.csv", sep=";", dec=",")

#1

#W celu oceny nowego procesu produkcji syntetycznych diamentów sprawdzono wagę [karaty] diamentów
#wyprodukowanych tą metodą uzyskując następujące wyniki:
#  0,46 0,61 0,52 0,48 0,57 0,54 0,47 0,63 0,51 0,49 0,58 0,55.
#Przyjmijmy, że badana zmienna ma rozkład normalny.

#(a) Określ populację, próbę i badaną zmienną
#populacja - wszytskie syntetyczne diamenty wyprodukowane nową metodą
#próba - te 12 diamentów wyprodukowanych nową metodą
#badana zmienna - waga syntetycznych diamentów wyprodukowanych nową metodą (w karatach)

#(b) Wyznacz oceny punktowe średniej, wariancji i odchylenia standardowego 
#wagi diamentów produkowanych tą metodą
diamenty=na.omit(dane$diamenty)

srednia=mean(diamenty)
wariancje=var(diamenty)
odchylenie=sd(diamenty)

#c) Oszacuj z 95% pewnością średnią wagę wszystkich syntetycznych diamentów 
#produkowanych badaną metodą (skonstruuj własną funkcję i porównaj 
#wynik z wynikami odpowiedniej funkcji dostępnej w R).

#przedziały ufności dla mu
ufnosc_srednia = function(avg, sd, sigma,n, alpha){
  L = 0
  P = 0
  if(n >= 30){ #duża próba
    L = mean(diamenty) - qnorm(1-alpha/2) * sd / sqrt(n)
    P = mean(diamenty) + qnorm(1-alpha/2) * sd / sqrt(n)
  }
  else{
    if(sigma){ #sigma - znane
      L = mean(diamenty) - qnorm(1-alpha/2) * sd / sqrt(n)
      P = mean(diamenty) + qnorm(1-alpha/2) * sd / sqrt(n)
    }
    else{ #sigma - nieznane
      L = mean(diamenty) - qt(1-alpha/2, n-1) * sd / sqrt(n)
      P = mean(diamenty) + qt(1-alpha/2, n-1) * sd / sqrt(n)
    }
  }
  print(paste("(", L, ":", P, ")"))
}

funkcja(mean(diamenty), sd(diamenty), F, 12, 0.05)
#interpretacja: z ufnością 95% przedział (49.8% ; 57%) pokrywa nieznaną 
#prawdziwą średnią wagę wszystkich syntetycznych diamentów

t.test(diamenty, conf.level=0.95) # to samo co naszą funkcją

#e) Wybierz współczynnik ufności i _oceń przedziałowo_ wariancję oraz odchylenie standardowe 
#wagi wszystkich diamentów syntetycznych produkowanych badaną metodą.

#odchylenie to pierwiastek wariancji

ufnosc_wariancja = function(n, sd, alpha){
  Lchi = (n-1)*sd^2/qchisq(1 - alpha/2, n-1)
  Pchi = (n-1)*sd^2/qchisq(alpha/2, n-1)
  Lz = (n-1)*sd^2/(n-1 + qnorm(1-alpha/2)*sqrt(2*(n-1))) #nie wiem skąd to jest
  Pz = (n-1)*sd^2/(n-1 - qnorm(1-alpha/2)*sqrt(2*(n-1)))
  if(n<30){
    L=Lchi #kwantyl rozkładu kwadrat
    P=Pchi
    
    print(paste(L, " ; ", P))
  }
  else{
    L=Lz #normalny
    P=Pz
    print(paste(L, " ; ", P))
  }
}

ufnosc_wariancja(length(diamenty), sd(diamenty), 0.05)

#z ufnością 95% przedział 0.1% ; 0.09% pokrywa nieznaną rzeczywistą wariancję 
#wagi wszytskich syntetycznych diamentów produkowanych nową metodą

C=sigma.test(diamenty, conf.level = 0.95)$conf.int #przedział ufności dla wariancji w R
L=sqrt(C[1]) #przedział ufności dla odchylenia standardowego
P=(sqrt(C[2]))
print(paste("(", L, ":", P, ")"))
#z ufnościa 95% przedział (0.039; 0.095) pokrywa nieznaną rzeczywistą wartość 
#odchylenia standardowego wagi wszystkich syntetycznych diamentów produkowanych nową metodą


#2

#Agencja Ochrony Środowiska jest zaniepokojona ilością PCB – toksycznej substancji
#chemicznej – w mleku matek karmiących piersią. W próbie 20 kobiet poziom PCB 
#(w liczbie cząsteczek na milion) był następujący:
#  16, 0, 0, 2, 3, 6, 8, 2, 5, 0, 12, 10, 5, 7, 2 , 3, 8, 17, 9, 1.
#Załóżmy, że rozkład analizowanej zmiennej losowej jest normalny.

kobiety=na.omit(dane$mleko)

#a) Zdefiniuj populację, próbkę i badaną zmienną.
#populacja - wszystkie matki karmiące piersią
#próba - 20 kobiet zbadanych
#poziom PCB (w liczbie cząsteczek na milion)

#b) Oblicz szacunkowy średni poziom PCB w mleku wszystkich matek karmiących piersią.
mean(kobiety)

#c) Oszacuj wariancję i odchylenia standardowe poziomu PCB w mleku 
#wszystkich matek karmiących piersią
C=sigma.test(kobiety, conf.level = 0.95)$conf.int #przedział ufności dla wariancji w R
print(paste("wariancja: (", C[1], ":", C[2], ")"))
print(paste("odchylenie standardowe: (",sqrt(C[1]), ":", sqrt(C[2]), ")"))

#d) Oceń z ufnością 95% średni poziom PCB w mleku wszystkich matek karmiących piersią. 
#Zinterpretuj wynik
#mu - nieznane, sigma - nieznane, n <= 30
alpha = 0.05
n = 20
L = mean(kobiety) - qt(1 - alpha/2, n-1)*sd(kobiety)/sqrt(n)
P = mean(kobiety) + qt(1 - alpha/2, n-1)*sd(kobiety)/sqrt(n)
print(paste("srednia: (", L, ":", P, ")")) 
#mozna też to zrobić wcześniejszą funkcją ufnosc_srednia lub gotowymi funkcjami w R
#z ufnością 95% przedział (3.42 : 8.18) pokrywa średnią ilość substancji
#toksycznej w mleko wszystkich kobiet karmiących piersią

#Oceń z ufnością 95% wariancję i odchylenie standardowe poziomu PCB w mleku 
#wszystkich matek karmiących piersią. Zinterpretuj wyniki.
C=sigma.test(kobiety, conf.level = 0.95)$conf.int

print(paste("wariancja: (", C[1], ":", C[2], ")"))
#z ufnością 95% przedział (14.95 : 55.16) pokrywa wariancję ilości substancji
#toksycznej w mleko wszystkich kobiet karmiących piersią

print(paste("odchylenie standardowe: (",sqrt(C[1]), ":", sqrt(C[2]), ")"))
#z ufnością 95% przedział (3.86 : 7.43) pokrywa odchylenie standardowe ilości substancji
#toksycznej w mleko wszystkich kobiet karmiących piersią


#3

#Aby oszacować średnią zawartość nikotyny w nowej marce papierosów, 
#wybrano 15 paczek papierosów i zbadano w nich zawartość nikotyny otrzymując dane (w mg)
#1,87 2,28 1,77 2,13 1,43 1,64 2,38 1,39 1,94 2,68 1,95 0,86 1,98 1,69 1,15.
#Z wcześniejszych badań wiadomo, że rozkład zawartości nikotyny jest normalny z
#odchyleniem standardowym równym 0,7 mg. 
sigma = 0.7
papierosy = na.omit(dane$papierosy)

#a) Oceń z ufnością 95% średnią zawartości nikotyny we wszystkich papierosach? 
C = z.test(papierosy, sigma.x = sigma, conf.level = 0.95)$conf.int
print(paste("średnia: (", C[1], ":", C[2], ")"))
#z ufnością 95% przedział (1.455 : 2.164) pokrywa średnią zawartość niktotyny 
#we wszystkich papierosach

#b) Jak duża próbka jest potrzebna, aby długość 95% przedziału ufności była nie 
#większa niż 0,3 mg?
n = (4*qnorm(1-0.05/2)^2 * sigma^2)/0.09 # n >= to co po prawo
#potrzeba n = 84 próbek

#c) Oblicz odchylenie standardowe z próby i porównaj wynik z podanym 
#odchyleniem standardowym populacji
sd(papierosy)
#odchylenie standardowe z próby jest prawie 2 razy mniejsze niż odchylenie populacji


#4

#Badacz zajmujący się możliwością zastosowania wodorostów do karmienia zwierząt badał 
#zawartość białka w wodorostach. Wyniki 18 pomiarów z 50-kilogramowych próbek 
#wodorostów przedstawiają się następująco:
#  4,28 3,3 4,22 2,77 2,75 2,93 3,86 3,05 4,12 2,88 3,94 4,99 2,08 4,35 2,7 4,09 2,81 2,82
#Przyjmijmy, że zawartość białka w wodorostach ma rozkład normalny.

wodorosty = na.omit(dane$wodorosty)
#a) Oszacuj średnią i wariancję populacji
mean(wodorosty)
var(wodorosty)

#b) Oceń z ufnością 90% prawdziwą średnią zawartość białka w 
#50-kilogramowych porcjach wodorostów.
C = t.test(wodorosty, conf.level = 0.90)$conf.int
print(paste("średnia: (", C[1], ":", C[2], ")"))
#z ufnością 90% przedział (3.115 : 3.767) pokrywa średnią zawartość białka wszystkich 
#50-kilogramowych porcji wodorostów

#c) Oceń z ufnością 90% wariancję populacyjną badanej zmiennej.
C=sigma.test(wodorosty, conf.level = 0.9)$conf.int
print(paste("wariancja: (", C[1], ":", C[2], ")"))
#z ufnością 90% przedział (0.388 : 1.235) pokrywa wariancję populacyjną zawartości 
#białka w 50-kilogramowych porcji wodorostów


#5

#Załóżmy, że jeśli sygnał o natężeniu μ pochodzi z lokalizacji A, to natężenie 
#zarejestrowane w lokalizacji B ma rozkład normalny ze średnią μ i odchyleniem 
#standardowym 3. Oznacza to, że z powodu „szumu” zarejestrowane
#natężenie różni się od rzeczywistego natężenia sygnału o wielkość będącą zmienną losową 
#o rozkładzie normalnym ze średnią 0 i odchyleniem standardowym 3. Aby zmniejszyć błąd, 
#ten sam sygnał jest niezależnie rejestrowany 10 razy. Jeżeli kolejne zarejestrowane 
#wartości to: 17, 21, 20, 18, 19, 22, 20, 21, 16, 19, oszacuj punktowo rzeczywiste 
#natężenie sygnału μ, a następnie oceń je przedziałowo z ufnością 95%. Zinterpretuj wynik.

sygnal = na.omit(dane$sygnal)

#oszacowanie punktowe rzeczywistego sygnału mu
mean(sygnal)

#ocena przedziałowa z ufnością 95%
C = z.test(sygnal, sigma.x = 3, conf.level = 0.95)$conf.int
print(paste("średnia: (", C[1], ":", C[2], ")"))
#z ufnościa 95% przedział ( 17.44 : 21.16 ) pokrywa rzeczywiste natężenie sygnału mu
#z lokalizacji A


#6

#Aby określić średni czas trwania połączenia telefonicznego realizowanego w godzinach 
#południowych, operator telefoniczny wybrał losowo próbę 1200 takich połączeń. 
#Obliczona średnia zmierzonego czasu trwania połączeń wynosi 4,7 minuty, a ich 
#odchylenie standardowe to 2,2 minuty. Oszacuj z 95% ufnością średnią długość trwania
#wszystkich takich połączeń oraz ich odchylenie standardowe. Zinterpretuj wyniki. 

#duża próba
C = zsum.test(4.7, 2.2, 1200, conf.level = 0.95)$conf.int
print(paste("średnia: (", C[1], ":", C[2], ")"))
#z ufnością 95% przedział (4.57 : 4.83) pokrywa średnią długość trwania 
#wszystkich takich połączeń

ufnosc_wariancja(1200, 2.2, 0.05) #tu coś nie wychodzi


#7

#Zużycie wody w fabryce podlega losowym wahaniom w kolejnych dniach roku.
#Na podstawie 365 obserwacji stwierdzono, że średnie dzienne zużycie wynosi 102 hl, 
#a wariancja 81 hl2

#a) Przyjmując współczynnik ufności 0,98 oceń przedziałowo średnie dzienne 
#zużycie wody w fabryce.

C = zsum.test(102, sqrt(81), 365, conf.level = 0.98)$conf.int
print(paste("średnia: (", C[1], ":", C[2], ")"))
#z ufnością 98% przedział ( 100.9 : 103.1 ) pokrywa średnie zużycie dzienne wody w fabryce

#b) W następnym roku cena wody ma wzrosnąć. Produkcja będzie musiała być ograniczona, 
#jeżeli średnie dzienne zużycie wyniesie co najmniej 122 hl. Czy na podstawie 
#uzyskanego wyniku jest to prawdopodobna sytuacja?

#nie, ponieważ z taką ufnością przedział nie pokrywa mu=122


#8

#Inżynier chce ustalić wielkość próbki niezbędną do uzyskania zadanej precyzji w 
#szacowaniu średniego czasu wiązania nowej mieszanki cementowej. Z dotychczasowych 
#doświadczeń wiadomo, że czas wiązania mieszanki cementowej jest zmienną losową o 
#rozkładzie normalnym i wariancji 25 (h^2). Jaka powinna być liczebność próby, aby 
#uzyskać 95% pewność, że błąd estymacji średniego czasu wiązania mieszanki nie przekroczy 1?

#jeśli dobrze rozumiem to:
#chcemy obliczyć z ee = qnorm(1-alpha/2)* sigma/sqrt(n)
# ee^2 = qnorm()^2 * sigma^2 / n
# n = to coś / ee^2

n = qnorm(1-0.05/2)^2 * 25 / 1^2
n
#trzeba zaokrąglic do góry (im mniejszy mianownik tym wieksze n)
#powinna być liczebność próby 97


#9

#Z wcześniejszych doświadczeń wiadomo, że waga łososia hodowanego w wylęgarni 
#komercyjnej jest zmienną losową o rozkładzie normalnym, przy czym średnia waga zmienia 
#się w zależności od sezonu, ale odchylenie standardowe pozostaje stałe na poziomie 0,3 
#funta. Jeśli chcemy mieć 90% ufności, że oszacowana średnia waga łososia jest prawidłowa 
#z dokładnością do ±0,1 funta, to jak dużą próbę należy pobrać? Jak zmieni się wynik,
#jeśli chcemy mieć 99% ufności? 

#to brzmi jak to co wyżej
n = qnorm(1-0.1/2)^2 * 0.3^2 / 0.1^2
n
#zaokrąglenie tak jak poprzednio
#powinna być liczebność 27

#dla ufności 99%
n = qnorm(1-0.01/2)^2 * 0.3^2 / 0.1^2
n
#potrzeba próby równej 60


#10

#Automat dozujący w browarze wymaga regulacji, gdy proporcja p niedopełnionych puszek 
#wynosi 1,5% lub więcej. Ponieważ skontrolowanie zawartości puszki powoduje jej zniszczenie, 
#nie ma możliwości wyznaczenia prawdziwej proporcji wszystkich niedopełnionych puszek. 
#Dlatego co jakiś czas wybiera się próbę 100 puszek i sprawdza się ich zawartość.
#W ostatnio pobranej próbie stwierdzono 4 niedopełnione puszki. Oceń z 95% ufnością 
#rzeczywisty odsetek niedopełnionych puszek. Napisz własną funkcję wyznaczającą oceniającą 
#proporcję niedopełnionych puszek, a następnie porównaj wynik z rezultatem funkcji 
#binom.test i prop.test w R. Zinterpretuj wynik.

phat = 4/100 #100 puszek i 4 niedopełnione, sukcesem jest niedopelnienie puszki
alpha=0.05
z=qnorm(1-alpha/2)
n=100

L=phat-z*sqrt(phat*(1-phat)/n)
P=phat+z*sqrt(phat*(1-phat)/n)
print(paste("(",L,":", P, ")" ))
#wynik = ( 0.15% : 7.85% ) z ufnością 95% ten przedział pokrywa nieznaną proporcję 
#niedopełnionych puszek w browarze
  
C = binom.test(4, 100, conf.level = 0.95)$conf.int
print(paste("(", C[1], ":", C[2], ")"))
# -||- ale przedział (1.1% : 9.93%)

C = prop.test(4, 100, conf.level = 0.95)$conf.int
print(paste("(", C[1], ":", C[2], ")"))
# -||- ale przedział ( 1.28% : 10.52% )


#11

#Asystent inżyniera przemysłowego przeprowadził 120 przypadkowych obserwacji zespołu 
#monterów tapicerek w zakładzie montażu samochodów. W 24 przypadkach zaobserwował, że 
#pracownicy układali materiały poza swoim stanowiskiem pracy (co może stwarzać 
#niebezpieczeństwo dla innych pracowników zakładu, a więc jest niezgodne z przepisami BHP). 
#Oceń z ufnością 90% prawdziwy odsetek monterów nie przestrzegających wspomnianych 
#przepisów BHP. Zinterpretuj wynik.

phat = 24/120
alpha=0.1
z=qnorm(1-alpha/2)
n=120

L=phat-z*sqrt(phat*(1-phat)/n)
P=phat+z*sqrt(phat*(1-phat)/n)
print(paste("(",L,":", P, ")" ))
#z ufnością 90% przedział ( 13.9% : 26.01%) pokrywa nieznaną proporcję monterów 
#nieprzestrzegających wspomnianych przepisów BHP

C = binom.test(24, 120, conf.level = 0.9)$conf.int
print(paste("(", C[1], ":", C[2], ")"))
# -||- ale przedział (14.1% : 27%)

C = prop.test(24, 120, conf.level = 0.9)$conf.int
print(paste("(", C[1], ":", C[2], ")"))
# -||- ale przedział (14.3% : 27.1%)


#12

#Badacz zainteresowany jest oszacowaniem frakcji osób mających problemy ze wzrokiem 
#w danej grupie wiekowej. Ile osób należy zbadać, aby na poziomie ufności 98% uzyskać 
#błąd oszacowania ±0,05 jeżeli:
ee = 0.05
z=qnorm(1-0.02/2)
#(a) z wcześniejszych doświadczeń wiadomo, że p wynosi 0,3

#sposób z wykładu
phat = 0.3
n = z*z * phat * (1-phat) / (ee*ee)
n

#potrzeba zbadać 455 osób

#(b) nic nie wiadomo o proporcji p.
phat = 0.5
n = z*z * phat * (1-phat) / (ee*ee)
n
