#kod projektowy
library(MASS)
library(dplyr)
set.seed(123)
dane=read.csv("wszystkie_dane.csv",header = FALSE, sep=";") #wczytanie danych

stad1 <- subset(dane, select = c(V2,V4)) #podzia³ na stada oryginalnych wartoœci
stad1<-subset(stad1, V4 == 1)
stad2 <- subset(dane, select = c(V2,V5))
stad2<-subset(stad2, V5 == 1)
stad3<- subset(dane, select = c(V2,V6))
stad3<-subset(stad3, V6 == 1)

make_sample=sample(1:nrow(dane),270) #podziaÅ‚ na zbiÃ³r testowy i walidacyjny
zb_t=(dane[make_sample,])
zb_w=(dane[-make_sample,])

h2=0.53 #odziedziczalnoÅ›Ä‡

y=as.matrix(zb_t$V2)  #dane dla zbioru testowego
x=as.matrix(zb_t[,3:6])

stad01 <- subset(zb_t, select = c(V2,V4)) #podzia³ na stada oryginalnych wartoœci dla 270
stad01<-subset(stad01, V4 == 1)
stad02 <- subset(zb_t, select = c(V2,V5))
stad02<-subset(stad02, V5 == 1)
stad03<- subset(zb_t, select = c(V2,V6))
stad03<-subset(stad03, V6 == 1)

z2 = matrix(0,270,30)
z1 = diag(1,270,270)
z = cbind(z1,z2) 

yw=as.matrix(zb_w$V2) #dane dla zbioru walidacyjnego
xw=as.matrix(zb_w[,3:6])
zw=diag(1,30,30)

stad01bw <- subset(zb_w, select = c(V2,V4)) #podzia³ na stada oryginalnych wartoœci
stad01bw<-subset(stad01bw, V4 == 1)
stad02bw <- subset(zb_w, select = c(V2,V5))
stad02bw<-subset(stad02bw, V5 == 1)
stad03bw<- subset(zb_w, select = c(V2,V6))
stad03bw<-subset(stad03bw, V6 == 1)

A=as.matrix(read.table("Macierz.txt",header = FALSE)) #wczytanie macierzy

invA=solve(A) #przeksztaÅ‚cenia
TrX=t(x)
TrZ=t(z)

L11 =TrX%*%x #lewa strona
L12=TrX%*%z
L21=TrZ%*%x
L22 = t(z)%*%z + invA*(1-h2)/h2
L1= cbind(L11,L12)
L2= cbind(L21,L22)
L = rbind(L1,L2)
invL = ginv(L)

P1=TrX%*%y #prawa strona
P2=t(z)%*%y
P=rbind(P1,P2)

result = invL%*%P #wyniki
b=result[1:4]
a=result[5:304]


d=cbind(dane$V1,a) #tworzenie rankingu krów
d <- as.data.frame(d)
d=arrange(d,desc(a))

pred_y = x%*%b + z%*%a #predykcja na zbiorze testowym

errory = pred_y-y

stad01t<-subset(zb_t, select = c(V4)) #podzia³ na stada wartoœci przewidzianych
stad01t<-cbind (pred_y,stad01t)
stad01t<-subset(stad01t, V4 == 1)
error_stad01t=stad01$V2-stad01t$pred_y
cor_stad01=cor(stad01t$pred_y,stad01$V2)
cort_stad01=cor.test(stad01t$pred_y,stad01$V2)

stad02t<-subset(zb_t, select = c(V5))
stad02t<-cbind (pred_y,stad02t)
stad02t<-subset(stad02t, V5 == 1)
error_stad02=stad02$V2-stad02t$pred_y
cor_stad02t=cor(stad02t$pred_y,stad02$V2)
cort_stad02t=cor.test(stad02t$pred_y,stad02$V2)


stad03t<-subset(zb_t, select = c(V6))
stad03t<-cbind (pred_y,stad03t)
stad03t<-subset(stad03t, V6 == 1)
error_stad03=stad03$V2-stad03t$pred_y
cor_stad03=cor(stad03t$pred_y,stad03$V2)
cort_stad03=cor.test(stad03t$pred_y,stad03$V2)

aw=sample((a),30) #predykcja na zbiorze walidacyjnym
pred_yw = xw%*%b + zw%*%aw
cor_yw=cor(pred_yw,yw)
cort_yw=cor.test(pred_yw,yw)
erroryw = pred_yw-yw

stad01w<-subset(zb_w, select = c(V4)) #podzia³ na stada wartoœci przewidzianych
stad01w<-cbind (pred_yw,stad01w)
stad01w<-subset(stad01w, V4 == 1)
error_stad01=stad01bw$V2-stad01w$pred_yw
cor_stad01w=cor(stad01w$pred_yw,stad01bw$V2)
cort_stad01w=cor.test(stad01w$pred_yw,stad01bw$V2)

stad02w<-subset(zb_w, select = c(V5))
stad02w<-cbind (pred_yw,stad02w)
stad02w<-subset(stad02w, V5 == 1)
error_stad02w=stad02bw$V2-stad02w$pred_yw
cor_stad02w=cor(stad02w$pred_yw,stad02bw$V2)
cort_stad02w=cor.test(stad02w$pred_yw,stad02bw$V2)

stad03w<-subset(zb_w, select = c(V6))
stad03w<-cbind (pred_yw,stad03w)
stad03w<-subset(stad03w, V6 == 1)
error_stad03w=stad03bw$V2-stad03w$pred_yw
cor_stad03w=cor(stad03w$pred_yw,stad03bw$V2)
cort_stad03w=cor.test(stad03w$pred_yw,stad03bw$V2)

#zadanie 3
#a) oszacowania efektów sta³ych
print (b)
#b) wykres wartoœci hodowlanych 
plot(a, xlab="", ylab="")
title(main="Wykres wartoœci hodowlanych",xlab="Nr osobnika",
      ylab="Wartoœci wartoœci hodowlanych" )
#c) statystyki wartoœci hodowlanych
summary(a)
sd(a)
#d) ranking krów
d=cbind(dane$V1,a) #tworzenie rankingu krów
d <- as.data.frame(d)
d=arrange(d,desc(a))
#e) porównanie mleka krów dla zbioru testowego
#wszystkie dane
porwt=cbind(y,pred_y)
cor_yt=cor(pred_y,y)
cort_yt=cor.test(pred_y,y)
summary(y)
sd(y)
summary(pred_y)
sd(pred_y)

#stado 1
porwt1=cbind(stad01,stad01t)
cor_st1t=cor(stad01$V2,stad01t$pred_y)
cort_st1t=cor.test(stad01$V2,stad01t$pred_y)
summary(stad01)
sd(stad01$V2)
summary(stad01t)
sd(stad01t$pred_y)

#stado 2
porwt2=cbind(stad02,stad02t)
cor_st2t=cor(stad02$V2,stad02t$pred_y)
cort_st2t=cor.test(stad02$V2,stad02t$pred_y)
summary(stad02)
sd(stad02$V2)
summary(stad02t)
sd(stad02t$pred_y)

#stado 3
porwt3=cbind(stad03,stad03t)
cor_st3t=cor(stad03$V2,stad03t$pred_y)
cort_st3t=cor.test(stad03$V2,stad03t$pred_y)
summary(stad03)
sd(stad03$V2)
summary(stad03t)
sd(stad03t$pred_y)


#f) porównanie mleka krów dla zbioru walidacyjnego
#wszystkie dane
porww=cbind(yw,pred_yw)
cor_yw=cor(pred_yw,yw)
cort_yw=cor.test(pred_yw,yw)
summary(yw)
sd(yw)
summary(pred_yw)
sd(pred_yw)

#stado 1
porww1=cbind(stad01bw,stad01w)
cor_st1w=cor(stad01bw$V2,stad01w$pred_yw)
cort_st1w=cor.test(stad01bw$V2,stad01w$pred_yw)
summary(stad01bw)
sd(stad01bw$V2)
summary(stad01w)
sd(stad01w$pred_yw)

#stado 2
porww2=cbind(stad02bw,stad02w)
cor_st2w=cor(stad02bw$V2,stad02wt$pred_yw)
cort_st2w=cor.test(stad02bw$V2,stad02w$pred_y)
summary(stad02bw)
sd(stad02bw$V2)
summary(stad02w)
sd(stad02w$pred_y)

#stado 3
porww3=cbind(stad03bw,stad03w)
cor_st3w=cor(stad03bw$V2,stad03w$pred_yw)
cort_st3w=cor.test(stad03bw$V2,stad03w$pred_y)
summary(stad03bw)
sd(stad03bw$V2)
summary(stad03w)
sd(stad03w$pred_y)

#h - wszystkie wykresy
plot(stad01$V2~stad01t$pred_y, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 1",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór testowy" )
plot(stad02$V2~stad02t$pred_y, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 2",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór testowy" )
plot(stad03$V2~stad03t$pred_y, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 3",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór testowy" )
plot(pred_yw~yw, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór walidacyjny" )
plot(stad01bw$V2~stad01w$pred_yw, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 1",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór walidacyjny")
plot(stad02bw$V2~stad02w$pred_yw, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 2",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór walidacyjny")
plot(stad03bw$V2~stad03w$pred_yw, xlab="", ylab="")
title(main="Porównanie wartoœci prawdziwych i predykcyjnych dla stada 3",xlab="Wartoœci predykcyjne",
      ylab="Wartoœci prawdziwe", sub = "Zbiór walidacyjny" )


#i 
errory = pred_y-y
erroryw = pred_yw-yw
mean(errory)
mean(erroryw)
plot(errory, xlab="", ylab="")
title(main="Wykres b³êdu dla zbioru testowego",xlab="Numer osobnika",
      ylab="Wielkoœæ b³êdu" )
plot(erroryw, xlab="", ylab="")
title(main="Wykres b³êdu dla zbioru walidacyjnego",xlab="Numer osobnika",
      ylab="Wielkoœæ b³êdu" )

