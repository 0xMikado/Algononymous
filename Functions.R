##################
# Set of functions for Algononymous shiny app
# Author: Nicolas Martinod
##################

#-------------------------------------
# Imports the dataset from OS path
#-------------------------------------
import_dataset <- function(dataset) {
  if(dataset == "Ebola Demographic"){
    inputdata <- readMicrodata(path="/Users/nicolasmartinod/Documents/EPFL/EPFL MA3/Pds/Dataset/Curated dataset/DM_copy.csv", type="csv", convertCharToFac=TRUE, drop_all_missings=TRUE, header=TRUE, sep=",")
  } else if(dataset == "Death Detail") {
    inputdata <- readMicrodata(path="/Users/nicolasmartinod/Documents/EPFL/EPFL MA3/Pds/Dataset/Curated dataset/DD_2020-09-04.csv", type="csv", convertCharToFac=TRUE, drop_all_missings=TRUE, header=TRUE, sep=",")
  }
  return(inputdata)
}

#-------------------------------------
# Set dataset abstract for each dataset
#-------------------------------------
dataset_abstract <- function(dataset){
  if(dataset == "Ebola Demographic"){
    abstract <- "This dataset represent the demographics of this and that. It was gathered by X in the country of Y and Z"
  } else if(dataset == "Death Detail") {
    abstract <- "This dataset represent the death detail of this and that. It was gathered by X in the country of Y and Z"
  }
  return(abstract)
}

#-------------------------------------
# List variable with explainations
#-------------------------------------
variable_overview <- function(){
  RFSTDTC <- "This variable describes the date and time of the start of the Subject Reference Period. The Subject Reference Period is defined by IDDO as starting with the subjects first study encounter and ending with the subjects final study encounter. RFSTDTC corresponds with the time and date of the subject's first study encounter (e.g., screening, enrollment, admission). This date will be used to calculate the relative days in the --DY, --STDY, --ENDY variables.  This date and time will be provided in ISO 8601 format. This variable will be blank for submissions that do not provide this initial date. All of the derived variables will also be blank since they are all calculated based on RFSTDTC."
  DTHL <- "This variable contains information about whether the subject died during the study period. The variable is expected to be null if the choice is not Yes. "
  SITEID <- "This variable contains information about the study site."
  AGE <- "This variable contains the age (expressed in the units described in AGEU) for the subject."
  AGEU <- "This variable contains the unit describing the value in AGE. This is defined by CDISC Controlled Terminology"
  SEX <- "This variable describes the sex of the subject. This is defined by CDISC Controlled Terminology"
  COUNTRY <- "This variable describes the sex of the subject. This is defined by CDISC Controlled Terminology"

  Name <- c("RFSTDTC", "DTHFL", "SITEID", "AGE", "AGEU", "SEX", "COUNTRY" )
  Description <- c(RFSTDTC, DTHL, SITEID, AGE, AGEU, SEX, COUNTRY)
  
  b <- cbind(Name,Description)
}

#-------------------------------------
# Create the SDCmicro object with the dataset and key variables
#-------------------------------------
sdc <- function(db, keyVars){
  sdcObj <- createSdcObj(dat=db,
                         keyVars=c("COUNTRY","SEX","RFSTDTC","DTHFL", "AGE", "AGEU", "SITEID"),
                         numVars=c("AGE"),
                         hhId=NULL,
                         strataVar=NULL,
                         pramVars=NULL,
                         excludeVars=NULL,
                         seed=0,
                         randomizeRecords=FALSE,
                         alpha=c(1)
  )
  
}