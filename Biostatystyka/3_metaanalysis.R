library(meta)
file=read.table("3_BSM.csv", sep=",",header=T)
fnc=metabin(file$Chory,file$N.all_chory.,file$Zdrowy,file$N.all_zdrowy.,data=file,sm="OR",method="I",comb.fixed = F, subset = Population =='Caucasian')
summary(fnc)
fna=metabin(file$Chory,file$N.all_chory.,file$Zdrowy,file$N.all_zdrowy.,data=file,sm="OR",method="I",comb.fixed = F, subset = Population =='Asian')
summary(fna)
fn=metabin(file$Chory,file$N.all_chory.,file$Zdrowy,file$N.all_zdrowy.,data=file,sm="OR",method="I",comb.fixed = F)
summary(fn)
forest(fn)
baujat(fn)
funnel(fn)
