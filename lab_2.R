#1
#Wczytaj plik loty.csv zawierający loty dotyczące liczby pasażerów pewnej 
#linii lotniczej wkolejnych miesiącach i latach, a następnie wykonaj polecenia:
loty = read.csv("loty.csv", sep=";")

#a) sprawdź, jakie wartości zawiera plik i jaki jest typ danych (class(loty));
class(loty)

#b) wyznacz i zinterpretuj podstawowe miary statystyczne (średnia, mediana, pierwszy i
#trzeci kwartyl, odchylenie standardowe, współczynnik zmienności);
mean(loty$X1956)
mean(loty[,2])

#srednia
nazwy=names(loty)
for(i in 1:length(loty)){
  print("srednia dla roku")
  print(nazwy[i])
  print(mean(loty[,i]))
}
#interpretacja dla roku 1956
#srednia liczba pasazerow pewnej linii lotniczej w roku 1956 byla rowna 328

#mediana
for(i in 1:length(loty)){
  print("mediana dla roku")
  print(nazwy[i])
  print(median(loty[,i]))
}
#w 6 miesiacach liczba osob byla mniejsza lub rowna 315, w pozostalych miesiacach wieksza
#lub rowna 315

#kwantyl
for(i in 1:length(loty)){
  print("kwantyl pierwszy i trzeci dla roku")
  print(nazwy[i])
  print(quantile(loty[,i])[2])
  print(quantile(loty[,i])[4])
}
#25% obserwacji (miesiece), czyli w 3 miesiacach liczba pasazerow w 1956 byla
#mniejsza lub rowna 301 osob, w pozostalych miesiacach byla wieksza lub rowna 301 osob

#odchylenie standardowe
for(i in 1:length(loty)){
  print("odchylenie standardowe dla roku")
  print(nazwy[i])
  print(sd(loty[,i]))
}
#przecietnie liczba pasażerow odchyla sie od sredniej o 48 osob

#wspolczynnik zmiennosci
for(i in 1:length(loty)){
  print("wspolczynnik zmiennosci dla roku")
  print(nazwy[i])
  print(sd(loty[,i])/mean(loty[,i]) * 100)
}
#slabe zroznicowanie liczby pasazerow w roku 1956

#c) narysuj histogramy liczebności dla danych z kolejnych lat; zautomatyzuj rysowanie za
#pomocą pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet
#danych; przedstaw wszystkie wykresy w jednym oknie;

#jej wykres
par(mfrow=c(2,3))
przedzialy=seq(200,650,length=10)
kolory=c("red", "pink", "blue", "green", "cyan", "yellow")
for(i in 1:length(loty)){
  hist(loty[,i], main=paste('loty ', nazwy[i]),breaks=przedzialy, col=kolory[i],ylim=c(0,6))
}

par(mfrow=c(2,3))
library(arm)
for(i in 1:length(loty)){
  discrete.histogram(seq(1,12),loty[,i], main=nazwy[i],freq=T)  
}
#w kazdym roku na przedziale 1955-1960 najwieksza ilosc pasazerow pojawia sie
#w miesiacach letnich (tj. 6-9)

#d) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych
par(mfrow=c(1,1))
boxplot(loty)

#2
#Wczytaj plik oceny.csv i wykonaj następujące polecenia:
oceny = read.csv("oceny.csv", sep=";", dec=",")

#a) sprawdź typ danych wczytanych z pliku i zwróć uwagę na długości kolejnych
# zmiennych oraz sposób zapisu
class(oceny)
#rozna ilosc ocen

#c) wyznacz i zinterpretuj podstawowe miary statystyczne; w przypadku niepełnej
#długości danych posłuż się funkcją na.omit;
#srednia
nazwy=names(oceny)
for(i in 1:length(oceny)){
  print("srednia dla roku")
  print(nazwy[i])
  print(mean(na.omit(oceny[,i])))
  print(median(na.omit(oceny[,i])))
  print(quantile(na.omit(oceny[,i]))[2])
  print(quantile(na.omit(oceny[,i]))[4])
  print(sd(na.omit(oceny[,i])))
  print(sd(na.omit(oceny[,i]))/mean(na.omit(oceny[,i])) * 100)
}

#d) narysuj diagramy odcinkowe dla danych z kolejnych grup; zautomatyzuj rysowanie za
#pomocą pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet
#danych; wszystkie wykresy umieść w jednym oknie;
par(mfrow=c(2,2))
for(i in 1:length(oceny)){
  title= paste("histogram", nazwy[i])
  discrete.histogram(oceny[,i], freq=T, main=title, xlab="oceny", ylim=c(0,10), col=kolory[i])
}

#e) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych;
boxplot(oceny)

#f) sporządź szeregi rozdzielcze punktowe ocen w poszczególnych grupach (table)
table(oceny[,1]) #nie dziala w forze

#e) przedstaw dane z szeregów rozdzielczych na wykresach kołowych.
par(mfrow=c(2,2))
for(i in 1:length(oceny)){
  pie(table(oceny[,i]))
}

#3
#Wczytaj plik truskawki.csv i wykonaj następujące polecenia:
truskawki = read.csv("truskawki.csv", sep=";")
#a) sprawdź typ danych wczytanych z pliku i zwrócić uwagę na długości kolejnych
#zmiennych; wyświetl dane zwracając uwagę na brakujące pomiary;

#b) wyznacz i zinterpretuj podstawowe miary statystyczne; w przypadku danych
#„plon2010” wykorzystaj funkcję na.omit;
for(i in 1:length(truskawki)){
  print(nazwy[i])
  print(mean(na.omit(truskawki[,i])))
  print(median(na.omit(truskawki[,i])))
  print(quantile(na.omit(truskawki[,i]))[2])
  print(quantile(na.omit(truskawki[,i]))[4])
  print(sd(na.omit(truskawki[,i])))
  print(sd(na.omit(truskawki[,i]))/mean(na.omit(truskawki[,i])) * 100)
}

#c) sporządź szeregi rozdzielcze przedziałowe plonów w poszczególnych latach (cut);
table(cut(truskawki[,1], breaks=7))
table(cut(truskawki[,2], breaks=4))

#d) przedstaw dane z szeregów rozdzielczych na wykresach kołowych;
pie(table(cut(truskawki[,1], breaks=7)))
pie(table(cut(truskawki[,2], breaks=4)))

#e) narysuj histogramy probabilistyczne (freq=FALSE) dla plonów z kolejnych lat
#wykorzystując szeregi rozdzielcze z punktu (c); zautomatyzuj rysowanie za pomocą
#pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet danych;
#wszystkie wykresy przedstaw w jednym oknie;
par(mfrow=c(1,2))
library(arm)
discrete.histogram(truskawki[,1], breaks=4, freq=F)
