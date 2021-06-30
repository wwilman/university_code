# SNP Classificator
### Here you can find all of my university work that I am able to share. 

Data owner: Theta Statistical Genetics Group (http://theta.edu.pl/)
Data contains samples of SNP and description of the authenticity of each genotype. Data comes from 4 "traditional Danish Red Dairy Cattle" bulls, sequenced in Next Generation Sequencing by Illumina. There are 12 variables:
* Genotype - 0 is false SNP, 1 is true SNP,
* QUAL = −10 ∙ 𝑙𝑜𝑔10[𝑃(𝐴𝐿𝑇 is wrong)],
* DP - coverage of the reference genome,
* DP2 - coverage of the reference genome on second level.
* 𝐺𝑄 = −10 ∙ 𝑙𝑜𝑔10[𝑃(genotype is wrong|SNP)],
* BEFORE1,2,3 - Three nucleotides before the SNP,
* BEHIND1,2,3 - Three nucleotides after the SNP.

### Aim: based on triplets before and after SNP and other measurements classify SNP as true or false.

Conclusions:
Such data with bias are realy hard to deal with, unfortunatelly variables do not describe genotype well. My way of completing project was not the best, but best for me at the time (2019/2020) as my first ML usage. If you have any questions or your own conclusions - contact me!

References:
* Hands-On Machine Learning with Scikit-Learn, Keras, and TensorFlow. O'Reilly Media
* https://morioh.com/p/134b77787d2a 
* https://www.scipy.org/ 
* https://scikit-learn.org/stable/ 
