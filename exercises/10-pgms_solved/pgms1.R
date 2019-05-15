library("gRain")

## Para installa dependencias:
## source("http://bioconductor.org/biocLite.R")
## biocLite(c("graph", "RBGL", "Rgraphviz", "igraph"))

## Set of genotype values
geno <- c("AA", "Aa", "aa")

## Prior allele frequencies in (founder) population
PrA <- 2/3
(Pra <- 1 - PrA)

## Prior genotype frequencies in (founder) population - according to Hardy-Weinberg's law
(gP <- c(PrA*PrA, 2*PrA*Pra, Pra*Pra))

## Genotype frequencies in children given the genotype of parents - according to Mendel's law 
gM_AA_AA <- c(  1,   0,   0)
gM_AA_Aa <- c(1/2, 1/2,   0)
gM_AA_aa <- c(  0,   1,   0)

gM_Aa_AA <- c(1/2, 1/2,   0)
gM_Aa_Aa <- c(1/4, 1/2, 1/4)
gM_Aa_aa <- c(  0, 1/2, 1/2)

gM_aa_AA <- c(  0,   1,   0)
gM_aa_Aa <- c(  0, 1/2, 1/2)
gM_aa_aa <- c(  0,   0,   1)

gM <- c(gM_AA_AA, gM_AA_Aa, gM_AA_aa,
        gM_Aa_AA, gM_Aa_Aa, gM_Aa_aa,
        gM_aa_AA, gM_aa_Aa, gM_aa_aa)

## Set of phenotype values
phen <- c("OK", "NOK")

## Error rate (= 1 - penetrance)
e <- 0.01

## Phenotype frequencies in individual given the genotype of individual - penetrance
##      OK  NOK
p <- c(1-e, 0+e, ## AA
       1-e, 0+e, ## Aa
       0+e, 1-e) ## aa

## Graph - joint distribution
g01 <- cptable(~g01,             values=gP, levels=geno)
g02 <- cptable(~g02,             values=gP, levels=geno)
g03 <- cptable(~g03 + g01 + g02, values=gM, levels=geno)
p03 <- cptable(~p03 + g03,       values=p,  levels=phen)

## Compile model
plist <- compileCPT(list(g01, g02, g03, p03))
g <- grain(plist)
summary(g)

## Plot the model
iplot(g)