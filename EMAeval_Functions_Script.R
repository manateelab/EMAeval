########### Library #####################################

# Below are the package dependencies that will be needed in order for the functions
# listed below to run properly. Also, R version must be later than 3.6.2 
# Some of the base functions needed will be in R version 3.6.2 and later.


# library(plyr)
# library(dplyr)
# library(reshape)
# library(lubridate)
# library(ggplot2)
# library(TSA)
# library(smooth)
# library(qualtRics)
# library(DescTools)




########### DESCRIPTION############

# The following script can be used in lue of the EMAeval Package. 
# This script includes Qualtrics functions to produce flagging plots, and flag any careless responders.
# 
# 
# Every function must use data in a data.frame configuration. The variable "data" is present in every function.
# Fill this function by data = YourData. 
# 
# The functions will be included in the script, all that is needed is to load each function
# and to run the functions as described in each Notes section below the function, with your data.
# 
# Please email nkraus@miami.edu if you have any questions.



########## Flagging Plots function#######################





flagging.plots <- function(data, ttc.colnames, item.colnames, ttc.plotx.max, sd.plotx.max, mode.plotx.max, na.rm){
  if (missing(ttc.colnames)){
    ttc.colnames <- stop("ttc.colnames missing.\nSpecify Start-Date & End-Date / Completion Time with ttc.colnames\n")
  }
  
  if (missing(number.items)){
    number.items <- stop("number.items missing.\nSpecify number of items with number.items\n")
  }
  
  if (missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify number of items with item.colnames\n")
  }
  if (missing(ttc.plotx.max)){
    ttc.plotx.max <- 100
  }
  
  if (missing(sd.plotx.max)){
    sd.plotx.max <- 50
  }
  
  if (missing(mode.plotx.max)){
    mode.plotx.max <- 100
  }
  if (missing(na.rm)){
    na.rm <- FALSE
  }
  #comparison DF for plots
  calc.df <- data.frame(matrix(NA, nrow = nrow(data), ncol = 4))
  data <- as.data.frame(data)
  for (idx in 1:nrow(data)){
    
    #calculate TTC and place in calc.df
    
    if (length(ttc.colnames[!is.na(ttc.colnames)]) > 1){
      starttime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[1])])
      endtime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[2])])
      calc.df[idx,1] <- as.numeric(endtime-starttime, units="secs")
    } else if (length(ttc.colnames[!is.na(ttc.colnames)]) == 1){
      calc.df[idx,1] <- (ttc.colnames[!is.na(ttc.colnames)])
    } else {
      calc.df[idx,1] <- NA
    }
    
    #calculate TPI and place in calc.df
    item_length <- length(which(!is.na(data[idx, item.colnames])))
    calc.df[idx,2] <- (calc.df[idx,1] / item_length)
    
    #calculate SD and place in calc.df
    if (length(item.colnames[!is.na(item.colnames)]) == 0){
      calc.df[idx,3] <- NA
    } else {
      calc.df[idx,3] <- round(sd(data[idx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])], na.rm = TRUE),2)
    }
    
    #calculate mode and place in calc.df
    MODE1 = as.vector(lsr::modeOf(as.numeric(data[idx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])]), na.rm = TRUE))
    if (length(MODE1) == 1){
      calc.df[idx,4] = MODE1
    } else if (length(MODE1) == length(item.colnames[!is.na(item.colnames)])){
      calc.df[idx,4] = NA
    } else{
      calc.df[idx,4] = paste(MODE1, collapse=",")
    }
    
  }
  
  #Mode for plots
  Mode_List <- NULL
  
  for (jdx in 1:nrow(data)){
    new_mode = as.vector(apply(data[jdx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])], 1, DescTools::Mode))
    Mode_List <- c(Mode_List, new_mode)
  }
  
  
  calc.df <- as.data.frame(calc.df)
  colnames(calc.df)[1:4] <- c("TTC", "TPI", "SD", "Longstring")
  
  Mode_List <- as.data.frame(Mode_List)
  
  
  
  #Plots:
  
  
  pTTC <- ggplot(data = calc.df, aes(TTC)) +
    geom_histogram(breaks= seq(0,ttc.plotx.max, by = 1),
                   col="blue",
                   fill="dark blue",
                   alpha = 0.3) +
    labs(subtitle = "Time to Complete Per Assessment") +
    labs(x="Time To Complete (seconds)", y="Count", tag = "A") +
    theme_classic()+
    scale_x_continuous(breaks = seq(0,ttc.plotx.max,10))+
    #scale_y_continuous(breaks = seq(0,800,100))+
    theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title.x = element_text(face = "bold"), axis.title.y =  element_text(face = "bold"))
  
  
  pTPI <- ggplot(data = calc.df, aes(TPI)) +
    geom_histogram(breaks= seq(0,(ttc.plotx.max/number.items), by = 0.5),
                   col="blue",
                   fill="dark blue",
                   alpha = 0.3) +
    labs(subtitle = "Time Per Item") +
    labs(x="Time Per Item(seconds)", y="Count", tag = "B") +
    theme_classic()+
    scale_x_continuous(breaks = seq(0,(ttc.plotx.max/number.items),2))+
    # scale_y_continuous(breaks = seq(0,30,1))+
    theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title.x = element_text(face = "bold"), axis.title.y =  element_text(face = "bold"))
  
  
  pSD <- ggplot(data = calc.df, aes(SD)) +
    geom_histogram(breaks= seq(0,sd.plotx.max, by = 1),
                   col="blue",
                   fill="dark blue",
                   alpha = 0.3) +
    labs(subtitle = "Standard Deviation Per Assessment") +
    labs(x="Standard Deviation", y="Count", tag = "C")+
    theme_classic()+
    scale_x_continuous(breaks = seq(0,sd.plotx.max,5))+
    #scale_y_continuous(breaks = seq(0,800,100))+
    theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title.x = element_text(face = "bold"), axis.title.y =  element_text(face = "bold"))
  
  
  pMODE <- ggplot(data = Mode_List, aes(Mode_List)) +
    geom_histogram(breaks= seq(0,mode.plotx.max, by = 1),
                   col="blue",
                   fill="dark blue",
                   na.rm = TRUE,
                   alpha = 0.3) +
    labs(subtitle = "Longstring Score Per Assessment") +
    labs(x="Modal Score", y="Count", tag = "D")+
    theme_classic()+
    scale_x_continuous(breaks = seq(0,mode.plotx.max,10))+
    #scale_y_continuous(breaks = seq(0,3000,1000))+
    theme(plot.title = element_text(hjust = 0.5, size = 18), axis.title.x = element_text(face = "bold"), axis.title.y =  element_text(face = "bold"))
  
  
  library(gridExtra)
  library(ggpubr)
  
  
  gridExtra::grid.arrange(grid.arrange(pTTC, pTPI, pSD, pMODE, nrow = 2, top = text_grob("Flagging Identification Plots", size = 20, just = "center")))
  
}



# Notes:
# The following variables for the function can be adjusted in order to create a better visualization based on your data.
# For now, the default values for the x axis limits for the following plots are 
# ttc.plotx.max = 100, sd.plotx.max = 50, mode.plotx.max = 100 
# 
#
# The ttc.colnames can have 1 (if your data already has a assessment completion time with the data point),
# or 2 (if a start date and end date are provided in data). 
#
# The order for the ttc.colnames MUST be start date, then end date. 
# 
#
# The commented out code below serves as an example.



# flagging.plots(Data1, ttc.colnames = c("StartDate", "EndDate") , number.items = 11, item.colnames = colnames(Data1[,7:17]))
# flagging.plots(Data2, ttc.colnames = c("CompletionTime") , number.items = 8, item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"))

#########################################################

########## Flagging DF function##########################

# This function is creates the dataframe that is used to create
# the plots for the last function. This can be used for further analyses


flagging.df <- function(data, ttc.colnames, number.items, item.colnames){
  if (missing(ttc.colnames)){
    ttc.colnames <- stop("ttc.colnames missing.\nSpecify Start-Date & End-Date / Completion Time with ttc.colnames\n")
  }
  
  if (missing(number.items)){
    number.items <- stop("number.items missing.\nSpecify number of items with number.items\n")
  }
  
  if (missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify number of items with item.colnames\n")
  }
  
  #comparison DF for plots
  calc.df <- data.frame(matrix(NA, nrow = nrow(data), ncol = 4))
  data <- as.data.frame(data)
  for (idx in 1:nrow(data)){
    
    #calculate TTC and place in calc.df
    
    if (length(ttc.colnames[!is.na(ttc.colnames)]) > 1){
      starttime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[1])])
      endtime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[2])])
      calc.df[idx,1] <- as.numeric(endtime-starttime, units="secs")
    } else if (length(ttc.colnames[!is.na(ttc.colnames)]) == 1){
      calc.df[idx,1] <- (ttc.colnames[!is.na(ttc.colnames)])
    } else {
      calc.df[idx,1] <- NA
    }
    
    #calculate TPI and place in calc.df
    item_length <- length(which(!is.na(data[idx, item.colnames])))
    calc.df[idx,2] <- (calc.df[idx,1] / item_length)
    
    #calculate SD and place in calc.df
    if (length(item.colnames[!is.na(item.colnames)]) == 0){
      calc.df[idx,3] <- NA
    } else {
      calc.df[idx,3] <- round(sd(data[idx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])], na.rm = TRUE),2)
    }
    
    #calculate mode and place in calc.df
    MODE1 = as.vector(lsr::modeOf(as.numeric(data[idx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])]), na.rm = TRUE))
    if (length(MODE1) == 1){
      calc.df[idx,4] = MODE1
    } else if (length(MODE1) == length(item.colnames[!is.na(item.colnames)])){
      calc.df[idx,4] = NA
    } else{
      calc.df[idx,4] = paste(MODE1, collapse=",")
    }
    
  }
  
  calc.df <- as.data.frame(calc.df)
  colnames(calc.df)[1:4] <- c("TTC", "TPI", "SD", "Longstring")
  calc.df
}

# Notes:
#
# The ttc.colnames can have 1 (if your data already has a assessment completion time with the data point),
# or 2 (if a start date and end date are provided in data). 
#
# The order for the ttc.colnames MUST be start date, then end date. 
# 
#
# The commented out code below serves as an example.


# flagging.df(Data1, ttc.colnames = c("StartDate", "EndDate") , number.items = 11, item.colnames = colnames(Data1[,7:17]))
# flagging.df(Data2, ttc.colnames = c("CompletionTime") , number.items = 8, item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"))

#########################################################

########## Lonstringr function##########################

# This function creates a vector of all the longstring values for further analysis if needed.
# It does not associate which assessment or ID the lonstring value came from. This should be used
# for visualization of the longstring and analysis of the values in general.

longstringr <- function(data, item.colnames) {
  
  #Mode
  Mode_List <- NULL
  
  for (jdx in 1:nrow(data)){
    new_mode = as.vector(apply(data[jdx, which(colnames(data) %in% item.colnames[!is.na(item.colnames)])], 1, DescTools::Mode))
    Mode_List <- c(Mode_List, new_mode)
  }
  
  Mode_List
}

# Notes:
#
# All that is needed are the column names of the Items used for the longstring analysis.
# This will provide you with a list of all possible longstring values. 
# There may be more longstring values due to the possiblility of multiple modal values for one participant.
#
# The commented out code below serves as an example.


# longstringr(Data1, item.colnames = colnames(Data1[,7:21]))
# longstringr(Data2, item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"))


#########################################################


########## TPI.cutoff function##########################

# This function will flag all the assessments that have are identified by the cutoff
# specified by the user. This function will generate a vector of indices for the
# dataframe provided by the user. This will NOT provide a list of how many times a subject is flagged for this cutoff.

TPI.cutoff <- function(data, cutoff, ttc.colnames, number.items, mandatory.response, item.colnames, ID.colname){
  if (missing(ttc.colnames)){
    ttc.colnames <- stop("ttc.colnames missing. Specify Start-Date & End-Date / Completion Time with ttc.colnames\n")
  }
  if (isTRUE(mandatory.response) & missing(number.items)){
    number.items <- stop("number.items missing. Specify number of items with number.items\n")
  }
  
  if (missing(mandatory.response)){
    mandatory.response <- stop("mandatory.response missing.\nSpecify if response to all items was mandatory, resulting in with no NAs for item responses.
                               \nIf responses are mandatory, mandatory.response = TRUE \nIf responses are NOT mandatory, mandatory.response = FALSE")
  }
  if (isFALSE(mandatory.response) & missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify column names of items with item.colnames\n")
  }
  
  if (missing(cutoff)){
    warning("cutoff missing.\nCutoff value automatically set to 2 seconds. Specify cutoff value to change from 2 seconds with variable 'cutoff = ' \n")
    cutoff <- 2
  }
  
  
  newDF <- c()
  
  for(idx in 1:nrow(data)){
    # number of items if response to items not mandatory
    if(isFALSE(mandatory.response)){
      item_length <- length(which(!is.na(data[idx, item.colnames])))
    } else {
      item_length <- number.items
    }
    
    
    # calc TTC for the assessment
    data_TTC <- c()
    
    if (length(ttc.colnames[!is.na(ttc.colnames)]) > 1){
      starttime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[1])])
      endtime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[2])])
      data_TTC <- as.numeric(endtime-starttime, units="secs")
    } else if (length(ttc.colnames[!is.na(ttc.colnames)]) == 1){
      data_TTC <- (ttc.colnames[!is.na(ttc.colnames)])
    } else {
      data_TTC <- NA
    }
    
    # TPI
    data_TPI <- (as.numeric(data_TTC) / as.numeric(item_length))
    
    # check to see if TPI is <= cutoff and paste into dataframe if so
    data_point <- c()
    if(!is.na(data_TPI) & data_TPI <= cutoff){
      data_point <- idx
      ID <- data[idx,ID.colname]
      flag_point <- cbind(ID, data_point)
    } else {
      next
    }
    newDF <- rbind(newDF, flag_point)
  }
  newDF <- as.data.frame(newDF)
  colnames(newDF)[2] <- "Index_of_Flagged_Assessment"
  newDF
  
  }

# Notes:
#
# The ttc.colnames can have 1 (if your data already has a assessment completion time with the data point),
# or 2 (if a start date and end date are provided in data). The order for the ttc.colnames MUST be start date, then end date. 
# 
# If response to items was MANDATORY, then mandatory.response = TRUE. Number of items should be provided through variable: number.items. 
# item.colnames does not need to be provided when mandatory.response = TRUE.
#
# If response to items was NOT MANDATORY, then mandatory.response = FALSE. The column names of the items should be provided by variable: item.colnames.
# number.items does not need to be provided when mandatory.response = FALSE
#
# The column name of the Participant ID or assessment identification number should be provided with variable: ID.colname.
#
# The commented out code below serves as an example.


# TPI.cutoff(Data1, cutoff = 2, ttc.colnames = c("StartDate", "EndDate") , mandatory.response = FALSE, item.colnames = colnames(Data1[,7:21]), ID.colname = colnames(Data1[,1]))
# TPI.cutoff(Data2, cutoff = 10, ttc.colnames = c("CompletionTime") , mandatory.response = TRUE, number.items = 8, ID.colname = "ID")




#########################################################

########## SD.cutoff function##########################

# This function will flag all the assessments that have are identified by the cutoff
# specified by the user. This function will generate a vector of indices for the
# dataframe provided by the user. This will NOT provide a list of how many times a subject is flagged for this cutoff.



SD.cutoff <- function(data, cutoff, item.colnames, ID.colname){
  
  if (missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify number of items with item.colnames\n")
  }
  
  
  if (missing(ID.colname)){
    ID.colname <- stop("ID.colname missing.\nSpecify column name of ID with ID.colname\n")
  }
  
  if (missing(cutoff)){
    warning("cutoff missing.\nCutoff value automatically set to 5. Specify cutoff value to change from 5 with variable 'cutoff = ' \n")
    cutoff <- 5
  }
  
  
  newDF <- c()
  suppressWarnings(
    for(idx in 1:nrow(data)){
      
      # calculate SD
      data_SD <- c()
      if (length(item.colnames[!is.na(item.colnames)]) == 0){
        data_SD <- NA
      } else {
        data_SD <- suppressWarnings(sd(data[idx,item.colnames], na.rm = TRUE))
      }
      
      
      # check to see if TPI is <= cutoff and paste into dataframe if so
      data_point <- c()
      flag_point <- c()
      if(data_SD <= cutoff){
        data_point <- idx
        ID <- data[idx,ID.colname]
        flag_point <- cbind(ID, data_point)
      }else{
        next
      }
      newDF <- rbind(newDF, flag_point)
    }
  )
  newDF <- as.data.frame(newDF)
  colnames(newDF)[2] <- "Index_of_Flagged_Assessment"
  newDF
  
}

# Notes:
#
# Column names of all items to be included in the analysis should be provided by the variable: item.colnames. 
#
# The column name of the Participant ID or assessment identification number should be provided with variable: ID.colname.
#
# The commented out code below serves as an example.


# SD.cutoff(Data1, cutoff = 5, item.colnames = colnames(Data1[,7:21]), ID.colname = colnames(Data1[,1]))
# SD.cutoff(Data2, cutoff = 10, item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"), ID.colname = "ID")


#########################################################

########## Combined.cutoff function##########################

# This is going to identify Assessments and IDs that are flagged by both the TPI and SD cutoffs. 
# This will pull all IDs and Indices for the corresponding flagged assessments. This does NOT compute 
# percentages of Assessments. That is the next function.


Combined.cutoff <- function(data, SD.cutoff, TPI.cutoff, ttc.colnames, number.items, mandatory.response, item.colnames, ID.colname){
  if (missing(TPI.cutoff)){
    warning("TPI.cutoff missing.\nTPI cutoff value automatically set to 2 seconds. Specify TPI cutoff value to change from 2 seconds with variable 'TPI.cutoff = ' \n")
    TPI.cutoff <- 2
  }
  if (missing(SD.cutoff)){
    warning("SD.cutoff missing.\nSD cutoff value automatically set to 5. Specify SD cutoff value to change from 5 with variable 'SD.cutoff = ' \n")
    SD.cutoff <- 5
  }
  
  if (missing(ttc.colnames)){
    ttc.colnames <- stop("ttc.colnames missing. Specify Start-Date & End-Date / Completion Time with ttc.colnames\n")
  }
  if (isTRUE(mandatory.response) & missing(number.items)){
    number.items <- stop("number.items missing. Specify number of items with number.items\n")
  }
  
  if (missing(mandatory.response)){
    mandatory.response <- stop("mandatory.response missing.\nSpecify if response to all items was mandatory, resulting in with no NAs for item responses.
                               \nIf responses are mandatory, mandatory.response = TRUE \nIf responses are NOT mandatory, mandatory.response = FALSE")
  }
  if (isFALSE(mandatory.response) & missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify column names of items with item.colnames\n")
  }
  
  newDF <- c()
  
  for(idx in 1:nrow(data)){
    # number of items if response to items not mandatory
    if(isFALSE(mandatory.response)){
      item_length <- length(which(!is.na(data[idx, item.colnames])))
    } else {
      item_length <- number.items
    }
    
    
    # calc TTC for the assessment
    data_TTC <- c()
    
    if (length(ttc.colnames[!is.na(ttc.colnames)]) > 1){
      starttime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[1])])
      endtime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[2])])
      data_TTC <- as.numeric(endtime-starttime, units = "secs")
    } else if (length(ttc.colnames[!is.na(ttc.colnames)]) == 1){
      data_TTC <- (ttc.colnames[!is.na(ttc.colnames)])
    } else {
      data_TTC <- NA
    }
    
    # TPI
    data_TPI <- (as.numeric(data_TTC) / as.numeric(item_length))
    
    # SD
    data_SD <- c()
    if (length(item.colnames[!is.na(item.colnames)]) == 0){
      data_SD <- NA
    } else {
      data_SD <- suppressWarnings(sd(data[idx,item.colnames], na.rm = TRUE))
    }
    
    
    # check to see if TPI is <= cutoff and paste into dataframe if so
    data_point <- c()
    flag_point <- c()
    
    if(!is.na(data_TPI) & !is.na(data_SD) & data_TPI <= TPI.cutoff & data_SD <= SD.cutoff){
      data_point <- idx
      ID <- data[idx,ID.colname]
      flag_point <- cbind(ID, data_point)
    } 
    
    newDF <- rbind(newDF, flag_point)
  }
  newDF <- as.data.frame(newDF)
  colnames(newDF)[2] <- "Index_of_Flagged_Assessment"
  newDF
  
  }


# Notes:
#
# The ttc.colnames can have 1 (if your data already has a assessment completion time with the data point),
# or 2 (if a start date and end date are provided in data). The order for the ttc.colnames MUST be start date, then end date. 
# 
# If response to items was MANDATORY, then mandatory.response = TRUE. Number of items should be provided through variable: number.items. 
# item.colnames does not need to be provided when mandatory.response = TRUE.
#
# If response to items was NOT MANDATORY, then mandatory.response = FALSE. The column names of the items should be provided by variable: item.colnames.
# number.items does not need to be provided when mandatory.response = FALSE
#
# Specify Cutoff values for SD by SD.cutoff. 
# Specify Cutoff values for Time Per Item by TPI.cutoff.
#
# The column name of the Participant ID or assessment identification number should be provided with variable: ID.colname.
#
# The commented out code below serves as an example.



# Combined.cutoff(Data1, SD.cutoff = 5, TPI.cutoff = 2, ttc.colnames = c("StartDate", "EndDate"), mandatory.response = TRUE, 
#                 number.items = 10, ID.colname = colnames(Data1[,1]))
# Combined.cutoff(Data2,  SD.cutoff = 10, TPI.cutoff = 5, ttc.colnames = c("CompletionTime"), mandatory.response = FALSE,
#                 item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"), ID.colname = "ID")




#########################################################

########## Combined.cutoff.percent function##########################

# This is going to produce a dataframe of percentage of assessments that are flagged by the combined cutoffs.



Combined.cutoff.percent <- function(data, SD.cutoff, TPI.cutoff, ttc.colnames, number.items, mandatory.response, item.colnames, ID.colname){
  if (missing(TPI.cutoff)){
    warning("TPI.cutoff missing.\nTPI cutoff value automatically set to 2 seconds. Specify TPI cutoff value to change from 2 seconds with variable 'TPI.cutoff = ' \n")
    TPI.cutoff <- 2
  }
  if (missing(SD.cutoff)){
    warning("SD.cutoff missing.\nSD cutoff value automatically set to 5. Specify SD cutoff value to change from 5 with variable 'SD.cutoff = ' \n")
    SD.cutoff <- 5
  }
  
  if (missing(ttc.colnames)){
    ttc.colnames <- stop("ttc.colnames missing. Specify Start-Date & End-Date / Completion Time with ttc.colnames\n")
  }
  if (isTRUE(mandatory.response) & missing(number.items)){
    number.items <- stop("number.items missing. Specify number of items with number.items\n")
  }
  
  if (missing(mandatory.response)){
    mandatory.response <- stop("mandatory.response missing.\nSpecify if response to all items was mandatory, resulting in with no NAs for item responses.
                               \nIf responses are mandatory, mandatory.response = TRUE \nIf responses are NOT mandatory, mandatory.response = FALSE")
  }
  if (isFALSE(mandatory.response) & missing(item.colnames)){
    item.colnames <- stop("item.colnames missing.\nSpecify column names of items with item.colnames\n")
  }
  
  
  
  newDF <- c()
  
  for(idx in 1:nrow(data)){
    # number of items if response to items not mandatory
    if(isFALSE(mandatory.response)){
      item_length <- length(which(!is.na(data[idx, item.colnames])))
    } else {
      item_length <- number.items
    }
    
    
    # calc TTC for the assessment
    data_TTC <- c()
    
    if (length(ttc.colnames[!is.na(ttc.colnames)]) > 1){
      starttime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[1])])
      endtime <- lubridate::ymd_hms(data[idx, which(colnames(data) %in% ttc.colnames[2])])
      data_TTC <- as.numeric(endtime-starttime, units = "secs")
    } else if (length(ttc.colnames[!is.na(ttc.colnames)]) == 1){
      data_TTC <- (ttc.colnames[!is.na(ttc.colnames)])
    } else {
      data_TTC <- NA
    }
    
    # TPI
    data_TPI <- (as.numeric(data_TTC) / as.numeric(item_length))
    
    # SD
    data_SD <- c()
    if (length(item.colnames[!is.na(item.colnames)]) == 0){
      data_SD <- NA
    } else {
      data_SD <- suppressWarnings(sd(data[idx,item.colnames], na.rm = TRUE))
    }
    
    
    # check to see if TPI is <= cutoff and paste into dataframe if so
    data_point <- c()
    flag_point <- c()
    
    if(!is.na(data_TPI) & !is.na(data_SD) & data_TPI <= TPI.cutoff & data_SD <= SD.cutoff){
      data_point <- idx
      ID <- data[idx,ID.colname]
      flag_point <- cbind(ID, data_point)
    } 
    
    newDF <- rbind(newDF, flag_point)
  }
  newDF <- as.data.frame(newDF)
  colnames(newDF)[1:2] <- c("ID", "Index_of_Flagged_Assessment")
  
  
  newDF2 <- cbind(ID = rep(NA,length(unique(newDF$ID))), Percent_Flagged = rep(NA,length(unique(newDF$ID))))
  
  
  uniqueID1 <- unique(newDF$ID)
  
  for(idx in 1:length(uniqueID1)){
    newDF2[idx,1] <- uniqueID1[idx]
    newDF2[idx,2] <- ((length(which(newDF$ID == uniqueID1[idx])) / length(which(data[,ID.colname] == uniqueID1[idx])) ) * 100)
  }
  newDF2 <- as.data.frame(newDF2)
  newDF2 <- na.omit(newDF2)
  newDF2
  }




# Notes:
#
# The ttc.colnames can have 1 (if your data already has a assessment completion time with the data point),
# or 2 (if a start date and end date are provided in data). The order for the ttc.colnames MUST be start date, then end date. 
# 
# If response to items was MANDATORY, then mandatory.response = TRUE. Number of items should be provided through variable: number.items. 
# item.colnames does not need to be provided when mandatory.response = TRUE.
#
# If response to items was NOT MANDATORY, then mandatory.response = FALSE. The column names of the items should be provided by variable: item.colnames.
# number.items does not need to be provided when mandatory.response = FALSE
#
# Specify Cutoff values for SD by SD.cutoff. 
# Specify Cutoff values for Time Per Item by TPI.cutoff.
#
# The column name of the Participant ID or assessment identification number should be provided with variable: ID.colname.
#
# The commented out code below serves as an example.



# Combined.cutoff.percent(Data1, SD.cutoff = 5, TPI.cutoff = 2, ttc.colnames = c("StartDate", "EndDate"), mandatory.response = TRUE, 
#                         number.items = 10, ID.colname = colnames(Data1[,1]))
# Combined.cutoff.percent(Data2,  SD.cutoff = 10, TPI.cutoff = 5, ttc.colnames = c("CompletionTime"), mandatory.response = FALSE,
#                         item.colnames = c("Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious"), ID.colname = "ID")



