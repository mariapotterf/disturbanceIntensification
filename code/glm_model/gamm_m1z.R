
.libPaths(c("/projappl/project_2003211/project_rpackages", .libPaths()))


# Marek Svitok test run 
# GAMM
# 2020/11/05

DAT <-read.table("cum.rate6.txt", header=T, sep="\t", stringsAsFactor = TRUE)
DAT$ID <- as.factor(rep(1:nrow(DAT)))
DAT$INT <-interaction(DAT$DISTANCE, DAT$REGION.SITE)
DAT$AR.START <-DAT$YEAR==2000
DAT2<-DAT
DAT2$DISTANCE <- factor(DAT$DISTANCE, levels = c("PA", 
                                                 "500", 
                                                 "2000",
                                                 "control"))
#DAT.SMALL <- droplevels(DAT2[DAT2$REGION.SITE %in% levels(DAT2$REGION.SITE)[1:20],]) #len prvych par kombinacii site & zone


#libs
#library(boot)
library(mgcv)
#library(gamm4)
#library(itsadug)
#library(ncf)

#GAMM

system.time(M1z.reordered <- bam(STALES~s(YEAR, by=DISTANCE, k=18) + 
                                   DISTANCE+
                                   s(YEAR, INT, bs="fs",m=1) +  
                                   s(ALT, k=3) + s(AGE, k=3)+ 
                                   SMREK + SHANNON,         # + TREE.RICHNESS, 
                                 family=betar, 
                                 data=DAT2,  # DAT.SMALL
                                 AR.start=AR.START, rho=0.15,
                                 method="fREML", 
                                 discrete=TRUE, nthreads = 39))



save.image("vysledok_M1z_pred_shannon.RData")
