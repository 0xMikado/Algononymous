#=====================================================================
# Title		: Statistical disclosure analysis of EVD dataset
# Dataset		: 3rd-June-2020 version
# Data curated by	: Sam Strudwick
# R version		: R version 3.6.3 (2020-02-29)
#=====================================================================
rm(list=ls())
library(sdcMicro)
library(ggplot2)

# Shiny app version
#sdcApp()
inputdata <- readMicrodata(path="D:/_statistical discloure analysis/Ebola/Data/EVD_disclosure_data_set_18_08_2020.csv", type="csv", convertCharToFac=TRUE, drop_all_missings=TRUE, header=TRUE, sep=",")

#-----------------------
# Declare Dates in R
#-----------------------
#inputdata$RFSTDTC <- as.Date(inputdata$RFSTDTC, "%Y-%m-%d")
#inputdata$DTHDTC	<- as.Date(inputdata$DTHDTC, "%Y-%m-%d")

nrow(inputdata )
inputdata <- rbind(inputdata,inputdata,inputdata,inputdata,inputdata,inputdata,inputdata)
nrow(inputdata )

#------------------------------
# Distribution of age
#------------------------------
ggplot(dat=inputdata,  aes(x=AGE_STD, fill=COUNTRY)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    #scale_fill_manual(values=c("#69b3a2", "#404080")) +
    labs(fill="")+
	xlab("Age (years)")+
	ylab("Number of patients")+
    facet_wrap(~STUDYID)

#=========================================================
# Statistical Discloure risk assessment
#=========================================================

# Start the clock!
ptm <- proc.time()


## Set up sdcMicro object
sdcObj <- createSdcObj(dat=inputdata,
        keyVars=c("COUNTRY","SEX","RFSTDTC","DTHDTC","PREFECTURE","DTHFL","DSDECOD","DSSTDY"),
        numVars=c("AGE_STD"),
        weightVar=c("sample_weight"),
        hhId=NULL,
        strataVar=NULL,
        pramVars=NULL,
        excludeVars=NULL,
        seed=0,
        randomizeRecords=FALSE,
        alpha=c(1)
	)

# Stop the clock
proc.time() - ptm


# Print the summary information 
print(sdcObj)

#--------------------
# Global risk
#--------------------
print(sdcObj, "risk")
(sdcObj@risk$global$risk )

#-------------------------------------------------------------------------------
# Expected number of re-identifications: is given by the sum of individal risks
#-------------------------------------------------------------------------------
ind_risk <- as.data.frame(unlist(sdcObj@risk$individual))
sum(ind_risk$risk)

# Max individual risk
max(ind_risk$risk)

# Mean individual risk
mean(ind_risk$risk)

# EMA Policy 0070 advises that max risk be less than 0.09
sum((ind_risk$risk>0.09)) # None of the indvidual risks are greater than 0.09

hist(ind_risk$risk, prob=TRUE, 
	xlim=c(0,0.1),
	ylim=c(0,2000),
	ylab="Density",
	xlab="Indvidual risk",
	main="Distribution of individual re-identification risk",
	las=1
	)

abline(v=0.09, col="red", lwd=2, lty=2)
abline(v=mean(ind_risk$risk), col="seagreen4", lwd=2, lty=2)

legend(0.02, 1500,, legend=c("Average risk in the sample","EMA 0.09 max risk threshold"), 
		lty=2, 
		lwd=2,
		col=c("seagreen4","red"),
		bg='lightblue',
		title="Risks", 
		text.font=4
	)

#-----------------------------------------------------------------
# Estimation of population and sample frequency
#-----------------------------------------------------------------
risk <- slot(sdcObj, "risk")$individual
freq <- data.frame(risk[, c("fk", "Fk")])

inputdata$fk <- freq$fk
inputdata$Fk <- freq$Fk

ggplot(inputdata, aes(x=fk, y=Fk/1000)) + 
		geom_point(pch=1, size=8) +
		xlab("Sample frequency counts") +
		ylab("Estimated population frequency counts (x 1000)"
		)

#------------------------------------------------------------------------------
# Data Intrustion Simulation (DIS) score based on SUDA Algorith: disScore
#------------------------------------------------------------------------------
## calculating suda2 riskmeasure
## This displays the SUDA2 scores only when run on ShinyApp
suda2(obj=sdcObj, DisFraction=0.1, missing=NA)

# End code
