#gene searches for interactions with miRNAs
#!/bin/bash

for miRNA in ENSG00000050820 ENSG00000137869 ENSG00000172500 ENSG00000145860 ENSG00000170820 ENSG00000138039 ENSG00000254087 ENSG00000198400 ENSG00000215021 ENSG00000171497 ENSG00000100401 ENSG00000213445 ENSG00000069702 ENSG00000204257 ENSG00000185033 ENSG00000126583 ENSG00000169598 ENSG00000081320 ENSG00000170775 ENSG00000142627 ENSG00000182580 ENSG00000146904 ENSG00000215021 ENSG00000108342 ENSG00000081041 ENSG00000090339 ENSG00000137070 ENSG00000150782 ENSG00000115008 ENSG00000109471 ENSG00000136244 ENSG00000091409 ENSG00000226979 ENSG00000134352 ENSG00000112658 ENSG00000113140 ENSG00000232810 ENSG00000198400 ENSG00000011422 ENSG00000112715 ENSG00000102786 ENSG00000121989 ENSG00000102245 ENSG00000013725 
do
	wget -O data.csv 'diana.imis.athena-innovation.gr/DianaTools/index.php?r=download/microT_CDS&keywords='$miRNA'&genes='$miRNA'%20&mirnas=&descr=&threshold=0.7'
	cat data.csv | egrep 'hsa-miR-3934|hsa-miR-555|hsa-miR-1272|hsa-miR-519e-3p|hsa-miR-1226-3p|hsa-miR-1245a|hsa-miR-759|hsa-miR-518d-5p|hsa-miR-3680-3p|hsa-miR-106a-5p|hsa-miR-191-5p|hsa-miR-33a-5p|hsa-miR-597|hsa-miR-17-5p|hsa-miR-1233|hsa-miR-3174|hsa-miR-3142|hsa-miR-1322|hsa-miR-190b|hsa-miRPlus-I320a*|hsa-miR-3175|hsa-miR-636|hsa-miR-873-5p|hsa-miR-4292|hsa-miR-194-5p|hsa-miR-125b-2-3p|hsa-miR-125a-3p|hsa-miR-374b-3p|hsa-miR-876-3p|hsa-miR-18a-5p|hsa-miR-491-5p|hsa-miR-20a-3p|hsa-miR-4320|hsa-miR-340-3p|hsa-miR-1247-5p|hsa-miR-500a-5p|hsa-miR-519d|hsa-miR-187-3p|hsa-miR-99a-3p|hsa-miR-149-5p' > $miRNA'_log'
	rm data.csv
done
