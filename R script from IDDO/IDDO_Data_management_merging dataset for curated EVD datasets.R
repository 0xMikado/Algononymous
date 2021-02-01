#=====================================================================
# Title		: Statistical discloure analysis of EVD dataset
# Dataset		: 3rd-June-2020 version
# Data curated by	: Sam Strudwick
# R version		: R version 3.6.3 (2020-02-29)
#=====================================================================
#rm(list=ls())
library(tidyverse)
library (plyr) # to convert list to data frame

#-----------------------------
# Set the working directory
#-----------------------------
setwd("W:/Data Management/Ebola/StatD")

# locate .csv files and complie it as a list
filelist <- list.files(pattern = ".csv") 
filelist 

# read all the .csv files 
for (i in 1:length(filelist )) 
		assign(filelist [i], read.csv(filelist [i])
	)


# subject table not available for EORKWS and ESYADD
#--------------------
# EJPDEJ
#--------------------
EJPDEJ_demo		 <- read.csv("EJPDEJ_Demographics_DM_2020_04_01.csv")
EJPDEJ_subject	 <- read.csv("EJPDEJ_Subject_Characteristics_SC_2020_04_01.csv")
EJPDEJ_disposition <- read.csv("EJPDEJ_Disposition_DS_2020_04_09.csv")
EJPDEJ_repro 	 <- read.csv("EJPDEJ_Reproductive_Findings_RP_2020_03_31.csv")

#----------
# EOPNOJ
#----------
EOPNOJ_demo		 <- read.csv("EOPNOJ_Demographics_DM_2019-11-08.csv")
EOPNOJ_subject	 <- read.csv("EOPNOJ_Subject Characteristics_SC_2020-01-14.csv")
EOPNOJ_disposition <- read.csv("EOPNOJ_Disposition_DS_2020-01-20.csv")
EOPNOJ_repro 	 <- read.csv("EOPNOJ_Reproductive System_RP_2019-12-11.csv")

#----------
# EORKWS
#----------
# EORKWS sbuject demographics table was not available
EORKWS_demo		 <- read.csv("EORKWS_Demographics_DM_2020-01-07.csv")
EORKWS_disposition <- read.csv("EORKWS_Disposition_DS_2020-01-20.csv")
EORKWS_repro 	 <- read.csv("EORKWS_Reproductive System_RP_2019-12-19.csv")

#----------
# EQJJGF
#----------
EQJJGF_demo		 <- read.csv("EQJJGF_Demographics_DM_2020-03-09.csv")
EQJJGF_subject	 <- read.csv("EQJJGF_Subject Characteristics_SC_2020-03-09.csv")
EQJJGF_disposition <- read.csv("EQJJGF_Disposition_DS_2020-03-09.csv")
EQJJGF_repro 	 <- read.csv("EQJJGF_Reproductive System_RP_2020-03-09.csv")

#----------
# ERFCVU
#----------
ERFCVU_demo		 <- read.csv("ERFCVU_Demographics_DM_2019-12-18.csv")
ERFCVU_subject	 <- read.csv("ERFCVU_Subject Characteristics_SC_2020-01-14.csv")
ERFCVU_disposition <- read.csv("ERFCVU_Disposition_DS_2020-01-23.csv")
ERFCVU_repro 	 <- read.csv("ERFCVU_Reproductive System_RP_2020-02-20.csv")

#----------
# ESYADD
#----------
# ESYADD subject demographics table was not available
ESYADD_demo		 <- read.csv("ESYADD_Demographics_DM_2020_01_15.csv")
ESYADD_disposition <- read.csv("ESYADD_Disposition_DS_2020-03-27.csv")
ESYADD_repro 	 <- read.csv("ESYADD_Reproductive_Finings_RP_2020_01_15.csv")

#----------
# EUZJTB
#----------
EUZJTB_demo		 <- read.csv("EUZJTB_Demographics_DM_2020-05-21.csv")
EUZJTB_subject	 <- read.csv("EUZJTB_Subject Characteristics_SC_2020-05-21.csv")
EUZJTB_disposition <- read.csv("EUZJTB_Disposition_DS_2020-05-21.csv")
EUZJTB_repro 	 <- read.csv("EUZJTB_Reproductive System_RP_2020-05-21.csv")

####################################
# Merge the Demographics table
####################################
demo_merged <- lapply(list(EUZJTB_demo,EORKWS_demo,EOPNOJ_demo,EJPDEJ_demo,EQJJGF_demo,ESYADD_demo,EUZJTB_demo,ERFCVU_demo), 
							function(x) 
						x[, c("USUBJID","STUDYID","RFSTDTC","DTHDTC","AGE","AGEU","SEX","COUNTRY","DTHFL")]
					)
demo_merged <- ldply (demo_merged, data.frame)
demo_merged$AGE_STD <-  ifelse(demo_merged$AGEU=="MONTHS",demo_merged$AGE/12, demo_merged$AGE)
demo_merged$AGE_STD [demo_merged$AGE_STD < 0] <- NA

####################################
# Merge the Pregnancy table
####################################
preg_merged <- lapply(list(EUZJTB_repro,EORKWS_repro,EOPNOJ_repro,EQJJGF_repro,ESYADD_repro,ERFCVU_repro,EJPDEJ_repro), 
							function(x) 
						x[, c("STUDYID","USUBJID", "RPTESTCD", "RPTEST","RPORRES")]
					)
preg_merged <- ldply (preg_merged, data.frame)

#--------------------------------------------------
# Select the rows which indicates pregnancy status
#--------------------------------------------------

preg_merged1 <- preg_merged %>% 
			  filter(RPTESTCD=="PREGIND")

####################################
# Merge the disposition table
####################################
# DSSTDY is missing from ESYADD_disposition file
ESYADD_disposition$DSSTDY <- NA

disposition_merged <- lapply(list(EUZJTB_disposition,EORKWS_disposition,EOPNOJ_disposition,EQJJGF_disposition,ERFCVU_disposition,EJPDEJ_disposition,ESYADD_disposition), 
							function(x) 
						x[, c("STUDYID","USUBJID","DSDECOD","DSSTDY")]
				)
disposition_merged  <- ldply (disposition_merged , data.frame)

####################################
# Merge the subject table
####################################
subject_merged <- lapply(list(EJPDEJ_subject,EOPNOJ_subject,EQJJGF_subject,ERFCVU_subject,EUZJTB_subject), 
							function(x) 
						x[, c("STUDYID","USUBJID","SCTESTCD","SCTEST","SCORRES")]
				)
subject_merged  <- ldply (subject_merged , data.frame)

#-------------------------------------------
# Employment status
#-------------------------------------------
subjet_employment <- subject_merged %>% 
			  filter(SCTESTCD=="EMPJOB")

#-------------------------------------------
# Job position
#-------------------------------------------
subjet_employment_position <- subject_merged %>% 
			  filter(SCTESTCD=="EMPPOS")

#-------------------------------------------
# Prefecture
#-------------------------------------------
subjet_prefecture <- subject_merged %>% 
			  filter(SCTESTCD=="PREFPAD")

#-------------------------------------------
# Sub-Prefecture
#-------------------------------------------
subjet_sub_prefecture <- subject_merged %>% 
			  filter(SCTESTCD=="SPRFPAD")

#-------------------------------------------
# Recode the SCORESS variable
#-------------------------------------------
subjet_employment 		<- subset(subjet_employment, select=c("STUDYID","USUBJID","SCORRES"))
colnames(subjet_employment) <- c("STUDYID","USUBJID","EMPLOYMENT")

subjet_employment_position  	<- subset(subjet_employment_position , select=c("STUDYID","USUBJID","SCORRES"))
colnames(subjet_employment_position) <- c("STUDYID","USUBJID","POSITION")

subjet_prefecture   		<- subset(subjet_prefecture, select=c("STUDYID","USUBJID","SCORRES"))
colnames(subjet_prefecture) <- c("STUDYID","USUBJID","PREFECTURE")

subjet_sub_prefecture 		<- subset(subjet_sub_prefecture , select=c("STUDYID","USUBJID","SCORRES"))
colnames(subjet_sub_prefecture) <- c("STUDYID","USUBJID","SUB_PREFECTURE")

######################################################
# Merge the multiple tables with one row per patient 
######################################################
discloure_table <- merge(demo_merged,preg_merged1, by=c("USUBJID","STUDYID"), all.x=TRUE)
discloure_table <- merge(discloure_table ,disposition_merged  , by=c("USUBJID","STUDYID"), all.x=TRUE)
discloure_table <- merge(discloure_table ,subjet_employment, by=c("USUBJID","STUDYID"), all.x=TRUE)
discloure_table <- merge(discloure_table ,subjet_employment_position, by=c("USUBJID","STUDYID"), all.x=TRUE)
discloure_table <- merge(discloure_table ,subjet_prefecture , by=c("USUBJID","STUDYID"), all.x=TRUE)
discloure_table <- merge(discloure_table ,subjet_sub_prefecture , by=c("USUBJID","STUDYID"), all.x=TRUE)
#fix(discloure_table )

discloure_table  <- subset(discloure_table ,
					select=c("USUBJID","STUDYID","RFSTDTC","DTHDTC","AGE_STD","SEX","COUNTRY","PREFECTURE","SUB_PREFECTURE",
						"DTHFL", "RPORRES",  "DSDECOD","DSSTDY","EMPLOYMENT","POSITION"))

#------------------------------
# Tidy up SEX variable
#------------------------------
discloure_table$SEX[discloure_table$SEX=="U"] <- NA
summary(discloure_table$SEX)
discloure_table <- droplevels(discloure_table)

#------------------------------
# Tidy up PREGNANCY STATUS
#------------------------------

# Trim white spaces
discloure_table$RPORRES <- trimws(discloure_table$RPORRES )

# Convert pregnant males as NO
discloure_table$RPORRES[discloure_table$SEX=="M"] <- "N"

discloure_table$RPORRES[discloure_table$RPORRES=="PREGNANT"] <- "Y"
discloure_table$RPORRES[discloure_table$RPORRES=="pregnant"] <- "Y"
discloure_table$RPORRES[discloure_table$RPORRES=="Yes"] <- "Y"
discloure_table$RPORRES[discloure_table$RPORRES=="No"] <- "N"
discloure_table$RPORRES[discloure_table$RPORRES=="Not Recorded"] <- NA
discloure_table$RPORRES[discloure_table$RPORRES=="Unknown"] <- NA
discloure_table$RPORRES[discloure_table$RPORRES=="U"] <- NA
discloure_table$RPORRES[discloure_table$RPORRES=="NS"] <- NA
discloure_table$RPORRES[discloure_table$RPORRES==""] <- NA

#=========================================================
# Merge population frequencies for each country by gender 
#=========================================================
discloure_table <- droplevels(discloure_table)
table(discloure_table$COUNTRY)

#---------------------------------------------------------------------
# Replace missing age with mean for the purpose of discloure analysis
#---------------------------------------------------------------------
discloure_table$AGE_STD <-ave(discloure_table$AGE_STD,discloure_table$COUNTRY,FUN=function(x) 
				  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

sle <- discloure_table[which(discloure_table$COUNTRY=="SLE"),]
gin <- discloure_table[which(discloure_table$COUNTRY=="GIN"),]
lib <- discloure_table[which(discloure_table$COUNTRY=="LBR"),]

#----------------------------------------------------------
# Age group for SLE to match with extracted pop frequency
#----------------------------------------------------------
sle$AGE_GP <- cut(sle$AGE_STD,
	                     breaks=c(-Inf, 5, 10, 15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,105),
      	               labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49",
				"50_54","55_59","60_64","65_69","70_74","75_79","80_84","85_89","90_94","95+")
		)
table(sle$AGE_GP)

#----------------------------------------------------------
# Age group for GIN to match with extracted pop frequency
#----------------------------------------------------------
gin$AGE_GP <- cut(gin$AGE_STD,
	                     breaks=c(-Inf, 5, 10, 15,20,25,30,35,40,45,50,55,60,65,70,75,80,105),
      	               labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49",
				"50_54","55_59","60_64","65_69","70_74","75_79","80+")
		)
table(gin$AGE_GP)

#----------------------------------------------------------
# Age group for Liberia to match with extracted pop frequency
#----------------------------------------------------------
lib$AGE_GP <- cut(lib$AGE_STD,
	                     breaks=c(-Inf, 5, 10, 15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,105),
      	               labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49",
				"50_54","55_59","60_64","65_69","70_74","75_79","80_84","85+")
		)

discloure_table1 <- rbind(sle,gin,lib)

summary(discloure_table1$AGE_STD)
summary(discloure_table1$SEX)

#=======================================================
# Merge the population frequency dataset 
#=======================================================
pop<- read.csv("D:/_statistical discloure analysis/Ebola/Data/pop_freq_data.csv")
pop$key <- paste(pop$COUNTRY,pop$SEX, pop$AGE_GP, sep=".")
pop <- subset(pop, select=c("key","n_pop"))
discloure_table1$key <- paste(discloure_table1$COUNTRY,discloure_table1$SEX, discloure_table1$AGE_GP, sep=".")
discloure_table2 <- merge(discloure_table1, pop, by="key", all.x=TRUE)
summary(discloure_table1$n_pop)
summary(discloure_table1$SEX)

#--------------------------------------------------------
# Replace the missing population by mean of the country
#--------------------------------------------------------
discloure_table2 %>% 
	dplyr::group_by(COUNTRY) %>%
	dplyr::summarise(
		mean_pop = sum(n_pop, na.rm=TRUE)
	)

discloure_table2$n_pop <-ave(discloure_table2$n_pop,discloure_table2$COUNTRY,FUN=function(x) 
				  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

#----------------------------------------------------------------
# Now work out sample freq for age group by country and age_group
#----------------------------------------------------------------

sample_freq <- discloure_table2 %>% 
	dplyr::group_by(COUNTRY, AGE_GP, SEX) %>%
	dplyr::summarise(
		n_sample = length(unique(USUBJID))
	)

nrow(discloure_table2)
discloure_table2<- merge(discloure_table2, sample_freq, by=c("COUNTRY","AGE_GP", "SEX"), all.x=TRUE)
nrow(discloure_table2)

discloure_table2$selection_prob <- discloure_table2$n_sample/discloure_table2$n_pop
discloure_table2$sample_weight <- 1/(discloure_table2$selection_prob)

#=======================================================
# Correct the names with french characters 
#=======================================================

discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "BOKÉ"="Boka")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "FORÉCARIAH"="FORACARIAH")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "GUÉCKÉDOU"="GUACKADOU")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "KÉROUANE"="KAROUANE")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "LÉLOUMA"="LALOUMA")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "NZÉRÉKORÉ"="NZARAKORA")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "TÉLIMÉLÉ"="TALIMALA")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "BOKÉ"="Boka")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "DUBRÉKA"="DUBRAKA")
discloure_table2$PREFECTURE <- recode(discloure_table2$PREFECTURE, "LABÉ"="LABA")


discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "BOKÉ"="Boka")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "DUBRÉKA"="DUBRAKA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "FORÉCARIAH"="FORACARIAH")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, " FRIGUIAGBÉ"=" FRIGUIAGBA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "GUÉCKÉDOU"="GUACKADOU")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "KA'GBELEN"="KAGBELEN")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "KÉROUANE"="KAROUANE")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "LÉLOUMA"="LALOUMA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "LABÉ"="LABA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "MANÉAH"="MANAAH")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "NZÉRÉKORÉ"="NZARAKORA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "SANGARÉDI"="SANGARADI")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "SARÉKALY"="SARAKALY")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "TÉLIMÉLÉ"="TALIMALA")
discloure_table2$SUB_PREFECTURE<- recode(discloure_table2$SUB_PREFECTURE, "TANÉNÉ"="TANANA")

discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "AIDE DE SANTÉ"="AIDE DE SANTA")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "AUTRE MÉDICALE"="AUTRE MADICALE")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "AUTRES NON-MÉDICAL"="AUTRES NON-MADICAL")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "ETUDIANT MÉDICINE"="ETUDIANT MADICINE")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "INFIRMIÈRE"="INFIRMIARE")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "MÉDECIN"="MADECIN")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "??©L??¨VE"="")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "AGENT DE SANT? COMMUNAUTAIRE"="AGENT DE SANTE COMMUNAUTAIRE")
discloure_table2$EMPLOYMENT<- recode(discloure_table2$EMPLOYMENT, "B?N?VOLE DE LA CROIX-ROUGE"="BENEVOLE DE LA CROIX-ROUGE")

discloure_table2$EMPLOYMENT <- NULL

#write.csv(discloure_table2, "D:/_statistical discloure analysis/Ebola/Data/EVD_disclosure_data_set_18_08_2020.csv")