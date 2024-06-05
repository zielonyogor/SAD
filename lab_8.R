#1
#Poniższa tabela przedstawia wyniki eksperymentu, w którym inżynier chce wyznaczyć 
#relację miedzy końcową wielkością produkcji środków chemicznych Y (w kg) w zależności 
#od ilości zużytego surowca X (w litrach):

dane=read.csv("Reg_chemikalia.csv", sep=";")
X = dane$surowiec
Y = dane$produkt

#a) Narysuj wykres punktowy przedstawiający zależność wielkości produkcji od ilości 
#zużytego surowca (scatter plot).

plot(X, Y)

#b) Wyznacz i zinterpretuj kowariancję próbkową między ilością zużytego surowca 
#a wielkością produkcji.

cov(X, Y) # S_xy = 138.4889
#kowariancja między ilością zużytego surowca X a końcową wielkością produkcji
#środków chemicznych jest niezerowa (istnieje związek) i dodatnia (zależność jest 
#rosnąca - im więcej surowca tym większa wielkość produkcji)

#c) Wyznacz i zinterpretuj współczynnik korelacji.

cor(X, Y) #p_xy = 0.8953468
#istnieje bardzo silny związek liniowy między X a Y

#d) Wyznacz ocenę prostej regresji między wielkością produkcji a ilością zużytego surowca.

# y = b_0 + b_1*x
prosta=lm(Y~X)
prosta # y = 22.405 + 3.619x
# prosta regresji liniowej wielkości produkcji środków 
# chemicznych ze względu na iloścć zużytego surowca

#e) Dodaj do wykresu punktowego prostą regresji

plot(X, Y)
abline(prosta)

#f) W jaki sposób zmieni się wielkość produkcji, jeśli ilość surowca wzrośnie o 1 litr?
# +3.619 * 1 - wielkość produkcji wzrośnie o 3.619 kg

#g) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 20 litrów surowca?

predict(prosta, data.frame(X=20))
#94.78571 - wielkość produkcji [kg]

#h) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 15 litrów surowca?

predict(prosta, data.frame(X=15))
#76.69048  - wielkość produkcji [kg]

#i) Oceń dopasowanie prostej regresji do danych.

#współczynnik determinacji:
cor(X, Y)^2 #0.8016458
#równanie regresji liniowej jest dobrze dopasowane do danych, ponieważ 0.8 jest bliskie 1

#j) Zweryfikuj test o istotności regresji. Przyjmij poziom istotności 5%. Zinterpretuj wynik.

#hipoteza
#H_0: b_1 = 0 - regresja liniowa jest nieistotna
#H_1: b_1 =/= 0 - regresja liniowa jest istotna
alpha = 0.05
anova(prosta)
#alpha = 0.05 > 0.0004617 = pval  ->  odzrzucamy H_0 na rzecz H_1
#na poziomie istotności 5% dane potwierdzają, że regresja liniowa jest istotna


#2
#Żywotność pewnego urządzenia (w miesiącach) zależy od liczby wyprodukowanych 
#przez to urządzenie elementów (efektywność urządzenia). Dla próby 9 urządzeń tego 
#samego typu otrzymano następujące wyniki:

dane=read.csv("Reg_urzadzenie.csv", sep=";")
X = dane$efektywnosc
Y = dane$zywotnosc

#a) Narysuj wykres punktowy przedstawiający zależność żywotności od efektywności 
#(scatter plot).

plot(X, Y)

#b) Oblicz i zinterpretuj kowariancję między żywotnością i efektywnością.

cov(X, Y) #s_xy = -8.652778
#kowariancja między efektywnośćią urządzenia X a jego żywotnością jest niezerowa 
#(istnieje związek) i ujemna (zależność jest 
#malejąca - im większa efektywność tym mniejsza żywotność)

#c) Oblicz i zinterpretuj współczynnik korelacji.

cor(X, Y) #p_xy = -0.9094164
#.|p_xy| > 0.8  ->  istnieje bardzo silny związek liniowy między X a Y

#d) Wyznacz ocenę prostej regresji żywotności urządzenia od jego efektywności.

# y = b_0 + b_1*x
prosta=lm(Y~X)
prosta # y = 18.8823 + -0.8629x
abline(prosta)

#e) Jak zmieni się żywotność urządzenia jeśli efektywność wzrośnie o 1 element?

#żywotność zmaleje o -0.8629

#f) Oszacuj żywotność urządzenia przy efektywności 11 elementów.

predict(prosta, data.frame(X=11))
#żywotność = 9.390582 [msc]

#g) Oszacuj żywotność urządzenia przy efektywności 19 elementów.

predict(prosta, data.frame(X=19))
#żywotność = 2.487535 [msc]

#h) Oceń dopasowanie prostej regresji.

cor(X, Y)^2 #0.8270381
#równanie regresji liniowej jest dobrze dopasowane do danych, ponieważ 0.8 jest bliskie 1

#i) Zweryfikuj test istotności regresji. Przyjmij poziom istotności 1%. 
#Zinterpretuj otrzymany wynik

#hipoteza
#H_0: b_1 = 0 - regresja liniowa jest nieistotna
#H_1: b_1 =/= 0 - regresja liniowa jest istotna
alpha = 0.05
anova(prosta)
#alpha = 0.05 > 0.0006735 = pval  ->  odzrzucamy H_0 na rzecz H_1
#na poziomie istotności 5% dane potwierdzają istotność regresji liniowej


#3
#Przeprowadzono proces usuwania arszeniku z wód gruntowych. Poniższa tabela przedstawia 
#procentowe ilości usuniętego przez proces arszeniku w zależności od zakwaszenia (pH) gleby:

dane=read.csv("Reg_arszenik.csv", sep=";", dec=",")
X = dane$pH
Y = dane$arszenik

#(a) Narysuj diagram punktowy ilości usuniętego arszeniku w zależności od zakwaszenia gleby.

plot(X, Y)

#b) Oblicz i zinterpretuj kowariancję i współczynnik korelacji między zakwaszeniem gleby a ilością
#usuniętego arszeniku.

cov(X, Y) #s_xy = -0.9504953
#kowariancja między zakwaszeniem X a ilością procentową usuniętego arszeniku X jest niezerowa 
#(istnieje związek) i ujemna (zależność jest 
#malejąca - im większe zakwaszenie tym mniejszy procent usuniętego arszeniku)

cor(X, Y) #p_xy = -0.9504953
#.|p_xy| > 0.8  ->  istnieje bardzo silny związek liniowy między X a Y

#c) Wyznacz prostą regresji zależności ilości usuniętego arszeniku i zakwaszenia gleby.

# y = b_0 + b_1*x
prosta=lm(Y~X)
prosta # y = 190.27 + -18.03x
abline(prosta)

#d) W jaki sposób zmieni się ilość usuniętego przez proces arszeniku jeśli pH gleby wzrośnie o 1?

#ilość usuniętego arszeniku zmaleje o 18.03%

#e) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 7,5?

predict(prosta, data.frame(X=7.5))
#usunięty arszenik = 55.01145 %

#f) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 9?

predict(prosta, data.frame(X=9))
#usunięty arszenik = 27.96008 %

#g) Jak dobra jest ocena liniowa regresji?

cor(X, Y)^2 #0.9034413
#równanie regresji liniowej jest dobrze dopasowane do danych, ponieważ 0.9 jest bliskie 1

#h) Zweryfikuj test istotności regresji. Przyjmij poziom istotności 1%. 

#hipoteza
#H_0: b_1 = 0 - regresja liniowa jest nieistotna
#H_1: b_1 =/= 0 - regresja liniowa jest istotna
alpha = 0.01
anova(prosta)
#alpha = 0.01 > 1.552e-09 = pval  ->  odzrzucamy H_0 na rzecz H_1
#na poziomie istotności 1% dane potwierdzają istotność regresji liniowej
