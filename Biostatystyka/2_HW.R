#Wczytanie danych
data <-read.table("dane.csv", sep = ';', header = TRUE)
data <-data[,1:3]

#______________________________________FokI
# Genotypy FokI
genotype_table <- table(data$Grupa,data$VDR.FokI)

FOKI_cancer <-genotype_table[1,]
FOKI_control <-genotype_table[2,]

#Liczebnosc calokowita N
FOKI_N_cancer <-sum(FOKI_cancer)
FOKI_N_control <-sum(FOKI_control)

# Czestosci wystepowania genotypow
AA_cancer <- FOKI_cancer[1]/FOKI_N_cancer
AG_cancer <- FOKI_cancer[2]/FOKI_N_cancer
GG_cancer <- FOKI_cancer[3]/FOKI_N_cancer

AA_control<- FOKI_control[1]/FOKI_N_control
AG_control <- FOKI_control[2]/FOKI_N_control
GG_control <- FOKI_control[3]/FOKI_N_control

FOKI_cancer_p <- AA_cancer + 1/2*AG_cancer
FOKI_cancer_q <- GG_cancer + 1/2*AG_cancer

FOKI_control_p <- AA_control + 1/2*AG_control
FOKI_control_q <- GG_control + 1/2*AG_control

# rownanie Hardiego Weinberga
FOKI_cancer_p^2 + 2 * FOKI_cancer_p * FOKI_cancer_q + FOKI_cancer_q^2
FOKI_control_p^2 + 2 * FOKI_control_p * FOKI_control_q + FOKI_control_q^2



#____________________________________BSM
# Genotypy BSM, 63 NA, CT i TC to samo
#zamiana Tc na CT
data$BSM <- gsub("TC","CT",data$BSM)


# Genotypy BSM
genotype_table <- table(data$Grupa,data$BSM)

BSM_cancer <-genotype_table[1,2:4] # bez NA
BSM_control <-genotype_table[2,2:4]

#Liczebnosc calokowita N
BSM_N_cancer <-sum(BSM_cancer)
BSM_N_control <-sum(BSM_control)


# Czestosci wystepowania genotypow
CC_cancer <- BSM_cancer[1]/BSM_N_cancer
CT_cancer <- BSM_cancer[2]/BSM_N_cancer
TT_cancer <- BSM_cancer[3]/BSM_N_cancer

CC_control<- BSM_control[1]/BSM_N_control
CT_control <- BSM_control[2]/BSM_N_control
TT_control <- BSM_control[3]/BSM_N_control


BSM_cancer_p <- CC_cancer + 1/2 * CT_cancer
BSM_cancer_q <- TT_cancer + 1/2 * CT_cancer

BSM_control_p <- CC_control + 1/2 * CT_control
BSM_control_q <- TT_control + 1/2 * CT_control

# rownanie Hardiego Weinberga
BSM_cancer_p^2 + 2 * BSM_cancer_p * BSM_cancer_q + BSM_cancer_q^2
BSM_control_p^2 + 2 * BSM_control_p * BSM_control_q + BSM_control_q^2
