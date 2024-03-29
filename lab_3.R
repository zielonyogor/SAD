#1

#Z badania zanieczyszczenia wody pitnej wynika, że 30% wszystkich studni w mieście jest
#zanieczyszczonych. Wybrano losowo pięć studni i sprawdzono jakość wody. Niech zmienna
#losowa S oznacza liczbę zanieczyszczonych studni spośród wybranych. 

#a) Jaki jest rozkład prawdopodobieństwa zmiennej losowej S? Podaj jego nazwę i przedstaw
#w formie tabeli a następnie narysuj liniowy wykres prawdopodobieństwa.

n = 5 #rozważamy 5 studni, włączamy 0 studni jako możliwość
p = 0.3 #prawdopodobieństwo zanieczyszczenia studni
x = c(0:5) #przypisanie wartości przyjmowanych przez zmienną losową
pr = dbinom(x, n, p) #rozklad pradopodobienstwa - gestosc

rbind(x, pr) #przedstawienie w formie tabeli (pradopodobienstwo ze jedna 0 studni jest zanieczyszczonych, ze 1 jest itd)

plot(x, pr, type="h", lwd = 3, ylab = "f(x)", xlab = "x", main ="binom(n,p)") 


#b) Korzystając z funkcji dostępnych w R oblicz prawdopodobieństwo, że:

#i) zmienna losowa przyjmuje wartość 3
#P(x=3) (tak zapisać na kolokwium) 
dbinom(3, n, p)

#ii) conajmniej 3 studnie są zanieczyszczone:
#P(X>=3) = P(X>2) = 1 - F(a)
1 - pbinom(2,n,p)

#iii) mniej niż 3 studnie są zanieczyszczone
#P(X<3) = P(X<=2)
pbinom(2,n,p)


#2
#Prawdopodobieństwo, że żarówka danego typu świeci przez przynajmniej 500 godzin wynosi 0,9.
#Niech B oznacza liczbę żarówek wśród 8 wylosowanych, których żywotność przekracza 500
#godzin. Podaj rozkład prawdopodobieństwa zmiennej losowej B i oblicz:
#  (a) P(B=8); (b) P(B=7); (c) P(B>5); (d) E(B); (e) SD(B).
#Zinterpretuj (d) i (e).


#rozkład dyskretny dwumianowy
p = 0.9 #prawdopodobieństwo sukcesu
n = 8 #liczba żarówek

x = c(0:8) #żarówki od 0 do 8
pr = dbinom(x,n,p)
rbind(x, pr)

#a) P(B=8) #prawdopodobieństwo że B(liczba żarówek) to 8
dbinom(8,n,p) #d od wartości gęstości

#c) P(B=7)
dbinom(7,n,p)

#c) P(B>5) = 1 - F(a)
1-pbinom(5, n, p) #p od wartości dystrybuanty = dbinom(0, n,p)+dbinom(1,n,p)

#d) E(B)
#wartość oczekiwana zmiennej losowej B/liczby żarówek, czyli E, suma kązdej zmiennej losowej * jej prawdopodobieństwo
expect = sum(x*pr)
#lub
#E(X) = n*p - druga możliwość na liczenie tego co wyżej
expect = n*p
#spodziewamy sie 7 zarowek

#SD(B)
#odchylenie standardowe zmiennej losowej - nie używać funkcji sd
#zmienna losowa to nie to samo co próba
s = sqrt(n*p*(1-p))
#możemy spodziewać się, że przeciętnie około 1 żarówka będzie odchylała się od średniej


#3
#Czas (w dniach) między awariami ogniw zasilających w satelicie jest zmienną losową o
#rozkładzie wykładniczym z λ = 0,01. Obecnie funkcjonują tylko 2 ogniwa. Są one ułożone
#równolegle i mają niezależne życie, dzięki czemu satelita może funkcjonować tak długo, jak
#działa co najmniej 1 ogniwo energetyczne. Narysuj funkcję gęstości czasu między awariami
#pojedynczego ogniwa korzystając z funkcji curve i dexp (dobierz odpowiednio przedział
#argumentów, aby zobaczyć kształt rozkładu wykładniczego). Zastosuj funkcję pexp aby
#wyznaczyć prawdopodobieństwo, że pojedyncze ogniwo:
#a) przeżyje co najmniej 200 dni,
#b) ulegnie awarii przed upływem 100 dni,
#c) ulegnie awarii przed upływem 500 dni

#rozkład wykładniczy, czyli rozkład ciągły
#lamba = 0.01 dla rozkładu wykładniczego

lambda = 0.01
curve(dexp(x, lambda), 0, 800) #ważny jest zakres x (0,800)

#a) P(X>=200) = P(X>200) = 1 - F(200) dystrybuanta - pexp
1 - pexp(200, lambda)

#b) P(X<100) = P(X<=100)
pexp(100, lambda)

#c) P(X<500)
pexp(500, lambda)


#4
#Siłę trzęsień ziemi (mierzoną w skali Richtera) zarejestrowanych w regionie Ameryki Północnej
#można modelować za pomocą rozkładu wykładniczego ze średnią 2,4 stopnia. Narysuj funkcję
#gęstości (dobierz odpowiednio przedział argumentów, aby zobaczyć kształt rozkładu
#wykładniczego). Oblicz prawdopodobieństwo, że nastąpi następne trzęsienie ziemi, które nawiedzi ten region
#a) przekracza 3 stopnie w skali Richtera,
#b) mieści się w przedziale od 2 do 3 stopnie w skali Richtera.
#Sprawdź, czy wartość oczekiwana wyliczona z definicji (za pomocą całki) jest równa 2,4.

#rozkład wykładniczy, ciągły

expect = 2.4 #średnia - wartość oczekiwana
lambda = 1/expect #ze wzoru 1/średnia

curve(dexp(x, lambda), 0, 15) #funkcja gęstości 

#a) P(X>3)
1 - pexp(3, lambda)

#b) P(2<x<3)
pexp(3, lambda) - pexp(2, lambda)

#wyliczenie wartości oczekiwanej z całki
f = function(x) {x*dexp(x,lambda)}
integrate(f,lower=0,upper = Inf) #wyliczenie całki


#5
#Przewody elektryczne przeznaczone do zastosowania w pewnym systemie komputerowym
#powinny mieć opór (rezystancję) pomiędzy 0,12 i 0,14 oma. Rezystancja przewodów
#produkowanych przez pewną firmę jest zmienną losową o rozkładzie normalnym ze średnią
##0,13 oma i odchyleniem standardowym 0,005 oma. Narysuj wykres funkcji gęstości rezystancji
#produkowanych przewodów (dobierz odpowiednio przedział argumentów funkcji, aby zobaczyć
#kształt rozkładu normalnego). Jakie jest prawdopodobieństwo, że losowo wybrany przewód
#produkowany przez tę firmę A spełnia wymagania stawiane przez system?

#średnia mi
mu = 0.13
sigma=0.005 #odchylenie standardowe

#twierdzenie o 3 sigmach - 99 obserwacji w przedziale
curve(dnorm(x, mu, sigma),mu - 3*sig, mu+3*sigma)

#P(0.12<x<0.14) - przewod spelnia wymagania
pnorm(0.14,mu,sigma) - pnorm(0.12,mu,sigma)


#6
#Czas schnięcia farby pewnego typu jest zmienną losową o rozkładzie normalnym z wartością
#oczekiwaną 2 godziny i odchyleniem standardowym 15 minut. Narysuj wykres funkcji gęstości
#czasu schnięcia badanej farby. Wyznacz prawdopodobieństwo, że farba schnie między 1h 51min
#i 2h 15 min.
# W minutach
mu = 120
sigma = 15
curve(dnorm(x,mu,sigma),mu-3*sigma,mu+3*sigma,xlab="czas schniecia")
# P(111<X<135) = F(135)-F(111)
pnorm(135,mu,sigma)-pnorm(111,mu,sigma)


#7
#Motorowery (małe motocykle o pojemności silnika poniżej 50cm3) cieszą się w Europie dużą
#popularnością ze względu na ich mobilność, łatwość obsługi i niski koszt. W pewnym
#specjalistycznym czasopiśmie opisano badanie przeprowadzone na stanowisku rolkowym mające
#na celu określenie maksymalnej prędkości pojazdu. Wywnioskowano, że maksymalna prędkość
#jest zmienną losową o rozkładzie normalnym z wartością oczekiwaną 46,8 km/h i odchyleniem
#standardowym 1,75 km/h.
#Rozważmy losowy wybór jednego takiego motoroweru. Oblicz prawdopodobieństwo, że jego
#maksymalna prędkość
#a) wynosi co najwyżej 50 km/h,
#b) wynosi co najmniej 48 km/h.
mu=46.8
sigma=1.75
#P(X<=50) = F(50)
pnorm(50, mu, sigma)
#P(X>=48) = 1 - F(48)
1- pnorm(48, mu, sigma)


#8
#Załóżmy, że 25% wszystkich studentów dużej uczelni publicznej otrzymuje stypendium. Niech
#X będzie liczbą studentów w losowej próbie o wielkości 100, którzy ubiegali się o przyznanie
#stypendium. Korzystając z rozkładu dokładnego zmiennej losowej X oraz jego przybliżenia
#rozkładem normalnym oblicz prawdopodobieństwo, że pomoc otrzyma co najwyżej 15 studentów.

#rozkład dwumianowy sukces/porażka

#liczebność studentow
n = 100
#prawdopodobieństwo sukcesu
p = 0.25

#P(X<=15) stypednium otrzyma co najwyżej 15 studentów
#rozkład dokładny
pbinom(15, n, p)

#przybliżenie rozkładem normalnym
pnorm(15,n*p,sqrt(n*p*(1-p)))


#9
#Rezystancja przewodników danego typu jest zmienną losową o rozkładzie normalnym ze średnią
#200 omów i odchyleniem standardowym 10 omów. W obwodzie użytych zostało 25 przewodników.
#Wyznacz prawdopodobieństwo, że

#a) średnia rezystancja wszystkich 25 przewodników zawiera się między 199 i 202 omów;
#prawdopodobienstwo że średnia zawiera się w danym zakresie

#średnia z próby ma rozkład normalny
mu = 200 #średnia
sigma = 10 #odchylenie standardowe

n = 25 #tyle rezystorów

#prawdopodob. P(199<avg<202) ze srednia jest taka
#avg ma rozkład N(mu, sigma/sqrt(n))

pnorm(202,mu,sigma/sqrt(n)) - pnorm(199, mu, sigma/sqrt(n))

#b) całkowita rezystancja wszystkich 25 przewodników nie przekracza 5100 omów. 

#suma z próby 25
#T = X1+X2+...+X25 ~ N(n*mu, sqrt(n)*sigma)
#P(T<=5100) = F(5100)
pnorm(5100, n*mu, sqrt(n)*sigma)


#10
#Poziom cholesterolu we krwi pracowników pewnej firmy jest zmienną losową, dla której średnia
#to 202 mg/dl, a odchylenie standardowe to 14 mg/dl (dl=decylitr). Oblicz prawdopodobieństwo,
#że średni poziom cholesterolu 64 wylosowanych do badania pracowników będzie zawierał się w
#przedziale między 198 a 206 mg/dl?

mu = 202
sigma = 14
n = 64

#czyli rozklad sredniej z proby znowu
pnorm(206, mu, sigma/sqrt(n)) - pnorm(198,mu, sigma/sqrt(n))


#11
#Wytrzymałość nici jest zmienną losową o średniej 0,5 kg i odchyleniu standardowym 0,2 kg.
#Załóżmy, że lina spleciona została ze 100 nici. Oblicz prawdopodobieństwo, że utrzyma ona
#ciężar 47 kg (Uwaga! Lina utrzyma ciężar, jeśli jej wytrzymałość jest od niego nie mniejsza).
mu = 0.5
sigma = 0.2
n = 100

#T=X1+...+X100 T ma rozkład N(n*mu, sig*sqrt(n))
#P(T>=47)
1 - pnorm(47, n*mu, sigma*sqrt(n))

