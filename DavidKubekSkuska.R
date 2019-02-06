#Dáta pochádzajú zo stránky nasdaq.com, kde sme sledovali relatívnu zmenu hodnoty akcií poèas dní v roku 2018 (250 hodnôt pre každú korporáciu)
#Zvolené korporácie Apple a EA sme vybrali kvôli ich technickému zameraniu

#Naèítanie dát cez príkaz read.table, keïže dáta máme v tabu¾kách

Apple<-read.table ("C:/Users/Dávid/Desktop/APP.txt")
EA<-read.table ("C:/Users/Dávid/Desktop/EA.txt")
A<-Apple$V1
E<-EA$V1

#Zaokruhlenie na 6 desatinných miest môžeme dosiahnu použitím funkcie round, ktorej v argumentoch zadáme ktoré údaje a na ko¾ko desatinných miest má zaokrúhli

A<- round(A,6)
A
E<- round(E,6)
E

#sumácia jednotlivých hodnôt pomocou funkcie sum

sum(A)
sum(E)

#Vykreslenie dát - môžeme pri òom niektoré základné vlastnosti vybra pod¾a "vkusu" (farba atï)

plot(A,col="blue",main="Apple",xlab="Deò",ylab="Percentuálna zmena ceny")
plot(E,col="red",main="EA",xlab="Deò",ylab="Percentuálna zmena ceny")

#Vytvorenie histogramov

hist(A, col="blue", main="Apple", xlab="Denná relatívna zmena(%)",
 ylab="Poèetnos")
hist(E, col="red", main="EA", xlab="Denná relatívna zmena(%)",
 ylab="Poèetnos")

#Priemery jednoducho zistíme cez funkciu mean

mean(A)
mean(E)

#Pre mediány podobne

median(E)
median(A)

#Kvantily môžeme získa nasledovne

qA1<-quantile(A, 0.25)
qA3<-quantile(A, 0.75)
qE1<-quantile(E, 0.25)
qE3<-quantile(E, 0.75)
qA1
qA3
qE1
qE3

#Môžeme vyh¾ada minimálne a maximálne hodnoty pomocou nasledujúcich funkcií. Pre získanie základných vlastností datasetu možno použi funkciu summary, teraz sme však použili osobitne funkcie hlavne z dôvodu "predåženia" kódu :) - pri analýze je však snahou presný opak

min(A)
max(A)
min(E)
max(E)

#Podobne aj pre rozptyl a odchýlku

var(A)
var(E)
sd(A)
sd(E)

#Medzikvartilové rozpätie

iqrA<-IQR(A)
iqrE<-IQR(E)

#Hradby boxplotu

dhA<-qA1-1.5*iqrA
hhA<-qA3+1.5*iqrA
dhE<-qE1-1.5*iqrE
hhE<-qE3+1.5*iqrE

#Boxplot - podobne pri jeho tvorbe môžeme zvoli niektoré vlastnosti vykres¾ovaného plotu

boxplot(A, col="blue", main="Apple")
boxplot(E, col="red", main="EA")

#Šikmos a funkcia šikmosti, naèítanie knižnice potrebnej pre výpoèet funkcie library(e1071)

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

#Špicatos a funkcia špicatosti - existujú aj funkcie v balíèkoch ktoré dokážu túto vlastnos ráta ( už spomenutá library e1071 )

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

#p-value = 8.872e-09 menšia ako hladina významnosti alfa=5% èo znamená zamietnutie hypotézy, že dáta pochádzajú z normálneho rozdelenia

shapiro.test(E)

#p-value = 1.261e-12 menšia ako hladina významnosti alfa=5% èo znamená zamietnutie hypotézy, že dáta pochádzajú z normálneho rozdelenia

#Keïže zamietame teóriu o normálnom rozdelení dát, použijeme test pre mediány

wilcox.test(A,E)

#Dáta nie sú normálne rozdelené, takže nemôžeme použi Pearsonov test, koreláciu ale môžeme zisti cez príkaz cor

cor(A,E) 

#Monotónnu závislos môžeme otestova pomocou Kendallovho korelaèného koeficientu, na to nám poslúži nasledovný príkaz

cor.test(A,E,method = "kendall")

#p-value = 2.599e-14 menšia ako hladina významnosti 0,05 a cor = 0.3234108,
#èiže v tomto prípade hypotézu H0, ktorá testovala èi sú dáta monotónne nezávislé zamietame.
