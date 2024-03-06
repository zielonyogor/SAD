#Laboratorium_1

#zad_1
#Oblicz:
sin(2*pi)
cos(3/4)
tan(pi)
log(100, 10)
log(15, exp(1))
log(1/7, 7)
exp(3)
64^(1/3)

#zad_2
#Utwórz wektor o składowych od 1 do 10. Zsumuj składowe wektora.
A = seq(1, 10, by=1)
sum(A)

#zad_3
#Utwórz wektor x którego składowymi są liczby parzyste od 2 to 20, a następnie
X = seq(2,20,by=2)

#a) zweryfikuj liczbę składowych wektora x - jest 10

#b) zdefiniuj nowy wektor, y, którego składowe sa takie same jak wektora x,
#tylko w odwrotnej kolejności
Y = rev(X)
#lub
Y = seq(20,2,by=-2)

#c) sprawdź, czym jest wynik działania x*x oraz x^2
X*X
X^2

#d) wyznacz długość (euklidesową) wektora x;
sqrt(sum(X^2))

#e) sprawdź, czym jest wynik mnożenia (macierzowego) transpozycji wektora x przez wektor y 
#oraz wektora x przez transpozycję wektora y.
t(X)%*%Y
X%*%t(Y)

#zad_4
#Utwórz wektor o 13 składowych, którego pierwsza składowa jest równa 5, ostatnia 10, 
#natomiast wszystkie pozostałe są równo oddalone od siebie.
A=seq(5,10,length=13)
A

#zad_5
#Utwórz wektory z1 i z2 będące odpowiednio 5-krotną replikacją wektora (1,2) 
#i 5-krotną replikacją składowych wektora (1,2). Wykonaj polecenia:
Z1 = rep(c(1,2), times=5) 
Z2 = rep(c(1,2), each=5)


#a) dodaj 4 do każdej składowej wektora z1;
Z1 = Z1+4

#b) zdefiniuj nowy wektor, z3, przez usunięcie ostatniej składowej wektora z2;
Z3 = Z2[-c(10,10)]

#c) zadeklaruj nowy wektor, c, jako sumę wektorów z1 i z3 i zweryfikuj wyniki
#C = Z1 + Z3

#d) zdefiniuj nowy wektor, którego składowe to elementy wektora z1, które są większe niż 1.
B = Z1[Z1 > 1]

#zad_6
#Utwórz macierz A=[(2,3,0),(1,-1,2),(1,1,-1)]
A = rbind(c(2,3,0),c(1,-1,2),c(1,1,-1))

#a) sprawdź wynik działania A^2 oraz A%*%A;
A^2
A%*%A

#b) wyznacz transpozycję, wyznacznik i odwrotność macierzy A;
t(A)
det(A)
solve(A)
#c) zdefiniuj wektor b, będący trzecim wierszem macierzy A.
b=A[3,]

#zad_7
#Utwórz dwa dowolne wektory x i y składające się z 10 składowych, a następnie:
x = seq(6,9, length=10)
y = seq(42,0, length=10)
#a) Narysuj punkty (x, y) na wykresie (wykres punktowy). 
library(arm)
plot(x,y)

#b) Połącz wektory x i y za pomocą polecenia data.frame i narysuj powstały wykres
plot(data.frame(x,y))

#c) Połącz wektory x i y za pomocą poleceń rbind i cbind i narysuj powstałe wykresy.
plot(cbind(x,y))

rbind(x,y)
plot(rbind(x,y))
#Narysuj funkcję f(x) = x^2 + 3x − 5 na przedziale (-3, 4). Spróbuj narysować inne funkcje.
curve(x^2 + 3*x - 5, from=-3, to=4, ylim=c(-10,10), col='42069')
