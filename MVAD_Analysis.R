rm(list=ls());
library("TraMineR")
library("cluster")
data("mvad")
setwd("C:\\bhupi\\Personal\\Cool_Analytics\\Sequence_Analysis");
mvad.alphab <- c("EM", "FE", "HE", "JL","SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, xtstep = 6, alphabet = mvad.alphab)

#Compute pairwise optimal matching (OM) distances between sequences with an insertion/
#deletion cost of 1 and a substitution cost matrix based on observed transition rates.

mvad.om <- seqdist(mvad.seq, method = "OM", indel = 1, sm = "TRATE")

#perform agglomerative hierarchical clustering of data... 
clusterward <- agnes(mvad.om, diss = TRUE, method = "ward")
mvad.cl4 <- cutree(clusterward, k = 4)
cl4.lab <- factor(mvad.cl4, labels = paste("Cluster", 1:4))
seqdplot(mvad.seq, group = cl4.lab, border = NA)
#examine how the diversity of states within each sequence is related to sex, to
#hether the father is unemployed and to whether the qualification grade at end of compulsory school was good.
#We compute the longitudinal entropy and regress it on the covariates

entropies <- seqient(mvad.seq)
lm.ent <- lm(entropies ~ male + funemp + gcse5eq, mvad)

#display these statuses for the first six considered months
mvad[1:2, 17:22]   #This shows STS format.  

#SPS Format....
mvad.lab <- c("Employment", "Further education", "Higher education","Joblessness", "School", "Training")
mvad.scode <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphab, states = mvad.scode,labels = mvad.lab, xtstep = 6)

print(mvad.seq[1:10, ], format = "SPS")



# Sequence Index Plot
seqiplot(mvad.seq, border = NA, withlegend = "right")
#Sequence Frequencies
Y

# plotting mean time spent in each state...
# say respondant's father was unemployed in duration of  survey...
seqmtplot(mvad.seq, group = mvad$funemp, ylim = c(0, 30))

#compute transition rate matrix for mvad sequence object.
mvad.trate <- seqtrate(mvad.seq)
round(mvad.trate, 2)

#time varying transsitION RATES can be obtained as follows: 
mvad.trate <- seqtrate(mvad.seq,time.varying=TRUE)
round(mvad.trate, 2)
# plotting transveral state distributions....
seqstatd(mvad.seq[, 1:8])





setwd("C:\\bhupi\\Personal\\Cool_Analytics\\Sequence_Analysis");
write.table(mvad, "Nmvad.csv", sep=",",row.names=FALSE, quote=FALSE);









