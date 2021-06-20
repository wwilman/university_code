#Analiza LD
library(dplyr)
library(tidyr)

#Wczytanie danych
all_reads <- read.csv("2_BSM_VDR.csv")
all_reads <- all_reads %>% separate(VDR.FokI, c("FokI_n1","FokI_n2" ),1) %>% separate(BSM, c("BSM_n1","BSM_n2" ),1)

#Wyliczenie frekwencji alleli
# BSM
first_nuc_BSM <- as.data.frame(all_reads[,4])
second_nuc_BSM <- as.data.frame(all_reads[,5])

table(first_nuc_BSM)
freq_1_BSM_sum=table(first_nuc_BSM)[2]+table(first_nuc_BSM)[3]
freq_1_c_BSM=(table(first_nuc_BSM)[2])/freq_1_BSM_sum
freq_1_t_BSM=(table(first_nuc_BSM)[3])/freq_1_BSM_sum


table(second_nuc_BSM)
freq_2_BSM_sum=table(second_nuc_BSM)[2]+table(second_nuc_BSM)[3]
freq_2_c_BSM=(table(second_nuc_BSM)[2])/freq_2_BSM_sum
freq_2_t_BSM=(table(second_nuc_BSM)[3])/freq_2_BSM_sum

# FokI
first_nuc_FokI <- as.data.frame(all_reads[,2])
second_nuc_FokI <- as.data.frame(all_reads[,3])



table(first_nuc_FokI)
freq_1_FokI_sum=table(first_nuc_FokI)[2]+table(first_nuc_FokI)[3]
freq_1_a_FokI=(table(first_nuc_FokI)[2])/freq_1_FokI_sum
freq_1_g_FokI=(table(first_nuc_FokI)[3])/freq_1_FokI_sum


table(second_nuc_FokI)
freq_2_FokI_sum=table(second_nuc_FokI)[2]+table(second_nuc_FokI)[3]
freq_2_a_FokI=(table(second_nuc_FokI)[2])/freq_2_FokI_sum
freq_2_g_FokI=(table(second_nuc_FokI)[3])/freq_2_FokI_sum


#Wyliczenie frekwencji haplotypów
all_reads$pair1 <- paste(all_reads$FokI_n1, all_reads$BSM_n1, sep="")
pair1 <- as.data.frame(all_reads[,6])
tab_pair1 <- table(pair1)
all_reads$pair2 <-paste(all_reads$FokI_n2, all_reads$BSM_n2, sep="")
pair2 <- as.data.frame(all_reads[,7])
tab_pair2 <- table(pair2)

freq_all=sum(tab_pair1[3:4],tab_pair1[6:7],tab_pair2[3:4],tab_pair2[6:7])

freq_AC_sum=(sum(tab_pair1[3],tab_pair2[3]))/freq_all
freq_AT_sum=(sum(tab_pair1[4],tab_pair2[4]))/freq_all
freq_GC_sum=(sum(tab_pair1[6],tab_pair2[6]))/freq_all
freq_GT_sum=(sum(tab_pair1[7],tab_pair2[7]))/freq_all


#Wyliczenie współczynnika D
D=(freq_AC_sum*freq_GT_sum)-(freq_GC_sum*freq_AT_sum)

#Standaryzacja współczynnika D
Dmax=freq_1_t_BSM*freq_2_a_FokI
D_prim=D/Dmax


#Wyliczenie współczynnika korelacji
r=D_prim/(freq_1_t_BSM*freq_2_t_BSM*freq_2_a_FokI*freq_1_a_FokI)^(1/2)
r_kw=r^2

