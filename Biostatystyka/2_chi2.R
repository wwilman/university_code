#dla BSM pvalue nieistotne!
data <- read.csv("2_BSM.csv")
summary(data)
#H0: są takie same
#H1: są inne

#Ogólne liczby
all=nrow(data)
ct=nrow(data[data$BSM == "CT" & data[data$BSM == "TC"],])
cc=nrow(data[data$BSM == "CC",])
tt=nrow(data[data$BSM == "TT",])
canc=nrow(data[data$Grupa == "Cancer",])
cont=nrow(data[data$Grupa == "Control" ,])

#tabela frekwencji (wartości obserwowane)
tab.freq=table(data)
tab.freq[1,2]

#wartości oczekiwane
canc_ct=(canc*ct)/all
canc_cc=(canc*cc)/all
canc_tt=(canc*tt)/all

cont_ct=(cont*ct)/all
cont_cc=(cont*cc)/all
cont_tt=(cont*tt)/all

#chi2

chi2=(((tab.freq[1,1]-canc_cc)^2)/canc_cc)+(((tab.freq[1,2]-canc_ct)^2)/canc_ct)+(((tab.freq[1,3]-canc_tt)^2)/canc_tt)+(((tab.freq[2,1]-cont_cc)^2)/cont_cc)+(((tab.freq[2,2]-cont_ct)^2)/cont_ct)+(((tab.freq[2,3]-cont_tt)^2)/cont_tt)
chisq.test(data$Grupa,data$BSM)
#https://statsandr.com/blog/chi-square-test-of-independence-by-hand/

#dla VDR pvalue istotne!
data <- read.csv("2_VDR.csv")
summary(data)
#H0: są takie same
#H1: są inne

#Ogólne liczby
all=nrow(data)
aa=nrow(data[data$VDR.FokI == "AA",])
ag=nrow(data[data$VDR.FokI == "AG",])
gg=nrow(data[data$VDR.FokI == "GG",])
canc=nrow(data[data$Grupa == "Cancer",])
cont=nrow(data[data$Grupa == "Control" ,])

#tabela frekwencji (wartości obserwowane)
tab.freq=table(data)
tab.freq[1,2]

#wartości oczekiwane
canc_aa=(canc*aa)/all
canc_ag=(canc*ag)/all
canc_gg=(canc*gg)/all

cont_aa=(cont*aa)/all
cont_ag=(cont*ag)/all
cont_gg=(cont*gg)/all


#chi2

chi2=(((tab.freq[1,1]-canc_aa)^2)/canc_aa)+(((tab.freq[1,2]-canc_ag)^2)/canc_ag)+(((tab.freq[1,3]-canc_gg)^2)/canc_gg)+(((tab.freq[2,1]-cont_aa)^2)/cont_aa)+(((tab.freq[2,2]-cont_ag)^2)/cont_ag)+(((tab.freq[2,3]-cont_gg)^2)/cont_gg)
chisq.test(data$Grupa,data$VDR.FokI)
