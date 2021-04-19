setwd("~/R")

#read in data -- NOTE THAT THE BIBLIOGRAPHIC INFO IS REMOVED BECAUSE IT DOES NOT BEHAVE IN R!
#also, sometimes you have to open the file in a text editor and add one line at the end
data = read.table("CAMVSGEN_METAT1.csv", header=T, sep=",")

#cacluate ratio - number of individuals
data$oddsratio_num = data$camera.num.indv/data$genetic.num.indv
hist(data$oddsratio_num)
#data skewed to the left 

#calculate ratio- with genetic on top but gave me wacky numbers so stuck to camera on top 
data$oddsratio_num2 = data$genetic.num.indv/data$camera.num.indv
hist(data$oddsratio_num2)

#cacluate ratio - using density
#this one was calculated then ignored since it gave the same numbers as individual #
data$oddsratio_den = data$camera.density/data$genetic.density

write.table(data, "CAMVSGEN_METAT1.csv", row.names=F, col.names=T, sep=",")

#test for normality using shapiro due to small sample size
shapiro.test(data$oddsratio_num)
shapiro.test(data$oddsratio_num2)
#not normal not a surprise 

#checking the log response ratio
data$LOG = log(data$camera.num.indv) - log(data$genetic.num.indv)
hist(data$LOG)
#if <0, negative response. if = 1, no response. if >1, positive response

##install metafor package if not already done
library(metafor) ##load metafor package

#random effects mode, unweighted due to absence of variance 
results=rma(yi= data$LOG, vi=vi,data=data, weighted=FALSE) #either way will be unweighted since the variance isnt available
results
forest(results)
#no funnel plot was made since there was no variance recorded in the studies. 
funnel(results)

#Moderator analysis 1: use camera method as a moderator
results3=rma(yi= data$LOG, vi= vi , mods=~ camera.method, data=data,method="DL") #can also do Loci-1 to get rid of the intercept. logically I don't know that we'd need to
#check here for info on intercepts https://www.metafor-project.org/doku.php/tips:models_with_or_without_intercept
#"When the model only includes continuous (i.e., numeric) predictors/moderators, then removing the intercept does just that: it removes the intercept. Hence, the model above forces the intercept to be 0, that is, it assumes that at the equator, the (average) log risk ratio is exactly zero. This seems like a rather strong assumption to make, so I would not recommend doing so. In fact, only in rare cases is it ever appropriate to remove the intercept term from a regression model (that involves only continuous predictors/moderators)."
results3
forest(results3)
#moderator analysis for study area size
results4=rma(yi= data$LOG, vi= vi , mods=~ study.area.size.km2, data=data,method="DL")
results4
forest(results4)
#no influence on effect size 

#moderator analysis for species
results5=rma(yi= data$LOG, vi= vi , mods=~ species , data=data,method="DL")
results5
forest(results5)
data$common.name
#no influence on effect size 

#moderator analysis for genetic method
results6=rma(yi= data$LOG, vi= vi , mods=~ gen.method , data=data,method="DL")
results6
forest(results6)


#to find citation for the paper
citation('metafor')
