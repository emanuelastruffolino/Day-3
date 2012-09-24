###############################
######## Assignment 3 #########
###############################

# Rendering state sequences

rm(list = ls())
setwd("/Users/emanuelastruffolino/Desktop/SequenceCourse/Lezione 3_18092012")
library (TraMineRextras)

#1. Load the biofam data set that comes with the TraMineR library 
#(look at the online help to get more information)

data(biofam)
?biofam
#2. Create a weighted state sequence object named biofam.seq with variables a15 to a30, 
#using the following state names and labels and the weights respecting the sample size.

biofam.lab<-c("Parent", "Left", "Married","Left+Marr","Child","Left+Child","Left+Marr+Child","Divorced")
biofam.shortlab<-c("P", "L", "M", "LM", "C","LC","LMC", "D")
biofam.seq<-seqdef(biofam[,10:25], states=biofam.shortlab,labels=biofam.lab,biofam$weight, xtstep=5)

plot (biofam.seq, border=NA)
#3. Create a full sequence index plot sorted from the end for each class of the cohort variable created in assignment 2.
biofam$cohort <- cut(biofam$birthyr, c(1900,1930,1940,1950,1960),
                     labels=c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right=FALSE)
seqIplot(biofam.seq, group=biofam$cohort,xtstep=5, sortv = "from.end",title="Seq. by cohort")

#4. Print the frequencies of the first 20 sequences.
print(biofam.seq[1:20,])

#5. Create a sequence frequency plot of the 20 most frequent patterns grouped by values
#of the cohort variable and save it as a ‘jpeg’ file.
seqtab(biofam.seq, tlim = 1:10)
jpeg('fplot.jpg')
seqfplot(biofam.seq, group = biofam$cohort, border = NA)
dev.off()

#6. Compute the transition rate matrix for the biofam data set
tr <- seqtrate(biofam.seq)
## Display only first two digits of the rates
round(tr, digits=2)

#7. What is the transition rate between states ‘Left/Married’ and ‘Left/Married/Child’?
tr[which(biofam.shortlab=="LM"),which(biofam.shortlab=="LMC")]

#8. Display the sequence of transversal state distributions by cohort.
seqdplot(biofam.seq, group=biofam$cohort)

#9. Within each cohort, at what age is the diversity of the transversal state distribution at its highest?

biofam.seqstatd <- seqstatd(biofam.seq)
biofam.entrop <- biofam.seqstatd$Entropy
names(biofam.seq)[which(biofam.entrop==max(biofam.entrop))]

#10. Display side by side in a same plot area the mean times spent in each 
#of the states and the sequence of modal states.
dev.off()
par(mfrow = c(2, 2))
seqmtplot(biofam.seq, withlegend = FALSE)
seqmsplot(biofam.seq, withlegend = FALSE)
seqlegend(biofam.seq)
