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
    print(paste(L, " ; ", P.))
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