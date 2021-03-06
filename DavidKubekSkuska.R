#D�ta poch�dzaj� zo str�nky nasdaq.com, kde sme sledovali relat�vnu zmenu hodnoty akci� po�as dn� v roku 2018 (250 hodn�t pre ka�d� korpor�ciu)
#Zvolen� korpor�cie Apple a EA sme vybrali kv�li ich technick�mu zameraniu

#Na��tanie d�t cez pr�kaz read.table, ke�e d�ta m�me v tabu�k�ch

Apple<-read.table ("C:/Users/D�vid/Desktop/APP.txt")
EA<-read.table ("C:/Users/D�vid/Desktop/EA.txt")
A<-Apple$V1
E<-EA$V1

#Zaokruhlenie na 6 desatinn�ch miest m��eme dosiahnu� pou�it�m funkcie round, ktorej v argumentoch zad�me ktor� �daje a na ko�ko desatinn�ch miest m� zaokr�hli�

A<- round(A,6)
A
E<- round(E,6)
E

#sum�cia jednotliv�ch hodn�t pomocou funkcie sum

sum(A)
sum(E)

#Vykreslenie d�t - m��eme pri �om niektor� z�kladn� vlastnosti vybra� pod�a "vkusu" (farba at�)

plot(A,col="blue",main="Apple",xlab="De�",ylab="Percentu�lna zmena ceny")
plot(E,col="red",main="EA",xlab="De�",ylab="Percentu�lna zmena ceny")

#Vytvorenie histogramov

hist(A, col="blue", main="Apple", xlab="Denn� relat�vna zmena(%)",
 ylab="Po�etnos�")
hist(E, col="red", main="EA", xlab="Denn� relat�vna zmena(%)",
 ylab="Po�etnos�")

#Priemery jednoducho zist�me cez funkciu mean

mean(A)
mean(E)

#Pre medi�ny podobne

median(E)
median(A)

#Kvantily m��eme z�ska� nasledovne

qA1<-quantile(A, 0.25)
qA3<-quantile(A, 0.75)
qE1<-quantile(E, 0.25)
qE3<-quantile(E, 0.75)
qA1
qA3
qE1
qE3

#M��eme vyh�ada� minim�lne a maxim�lne hodnoty pomocou nasleduj�cich funkci�. Pre z�skanie z�kladn�ch vlastnost� datasetu mo�no pou�i� funkciu summary, teraz sme v�ak pou�ili osobitne funkcie hlavne z d�vodu "pred�enia" k�du :) - pri anal�ze je v�ak snahou presn� opak

min(A)
max(A)
min(E)
max(E)

#Podobne aj pre rozptyl a odch�lku

var(A)
var(E)
sd(A)
sd(E)

#Medzikvartilov� rozp�tie

iqrA<-IQR(A)
iqrE<-IQR(E)

#Hradby boxplotu

dhA<-qA1-1.5*iqrA
hhA<-qA3+1.5*iqrA
dhE<-qE1-1.5*iqrE
hhE<-qE3+1.5*iqrE

#Boxplot - podobne pri jeho tvorbe m��eme zvoli� niektor� vlastnosti vykres�ovan�ho plotu

boxplot(A, col="blue", main="Apple")
boxplot(E, col="red", main="EA")

#�ikmos� a funkcia �ikmosti, na��tanie kni�nice potrebnej pre v�po�et funkcie library(e1071)

library(e1071)
n<-250
citatelA <- (1/n)*sum((A-mean(A))^3)
menovatelA <- ((1/n)*sum((A-mean(A))^2))^(3/2)
gA <- citatelA/menovatelA
gA
skewness(A)
citatelE <- (1/n)*sum((E-mean(E))^3)
menovatelE <- ((1/n)*sum((E-mean(E))^2))^(3/2)
gE <- citatelE/menovatelE
gE
skewness(E)

#�picatos� a funkcia �picatosti - existuj� aj funkcie v bal��koch ktor� dok�u t�to vlastnos� r�ta� ( u� spomenut� library e1071 )

citatelA <- (1/n)*sum((A-mean(A))^4)
menovatelA <- (var(A))^2
bA <- citatelA/menovatelA-3
bA
kurtosis(A)
citatelE <- (1/n)*sum((E-mean(E))^4)
menovatelE <- (var(E))^2
bE<- citatelE/menovatelE-3
bE
kurtosis(E)

#Test normality

shapiro.test(A)

#p-value = 8.872e-09 men�ia ako hladina v�znamnosti alfa=5% �o znamen� zamietnutie hypot�zy, �e d�ta poch�dzaj� z norm�lneho rozdelenia

shapiro.test(E)

#p-value = 1.261e-12 men�ia ako hladina v�znamnosti alfa=5% �o znamen� zamietnutie hypot�zy, �e d�ta poch�dzaj� z norm�lneho rozdelenia

#Ke�e zamietame te�riu o norm�lnom rozdelen� d�t, pou�ijeme test pre medi�ny

wilcox.test(A,E)

#D�ta nie s� norm�lne rozdelen�, tak�e nem��eme pou�i� Pearsonov test, korel�ciu ale m��eme zisti� cez pr�kaz cor

cor(A,E) 

#Monot�nnu z�vislos� m��eme otestova� pomocou Kendallovho korela�n�ho koeficientu, na to n�m posl��i nasledovn� pr�kaz

cor.test(A,E,method = "kendall")

#p-value = 2.599e-14 men�ia ako hladina v�znamnosti 0,05 a cor = 0.3234108,
#�i�e v tomto pr�pade hypot�zu H0, ktor� testovala �i s� d�ta monot�nne nez�visl� zamietame.
