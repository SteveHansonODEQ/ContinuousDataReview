library(plyr)
library(dplyr)
library(reshape2)
library(RODBC)
library(psych)
library(ggplot2)


#######################################

###
#
#
###nut by user

######################################

# Continuous Data Characteristics Information file 
ConCharInfo<- '//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConCharInfo.RData'
# LOAD information about each charcteristic, units, methods
load(ConCharInfo)

### INPUT provide sampling organization from VolWQdb.tlu_Organization.OrgAbrv
ORG <- 'RRWC' 

###  LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

###  LOCATION TO SAVE DATA FILES CREATED IN PROCESS
save_path <- "//deqlead02/Vol_Data/RogueWISE/2016/Continuous/"

###
# #
###pen the ODBC connection to the database

#load("V:/RogueWISE/2016/Continuous/edits/editedData/0027_23062_14350000_DO_20150924_Emmigrant Cr at Gage EMI_NA_.Rdata")




#             
#             
### oad the Files to use in for loop

################################################

#Gather filenames from the shiny folder
in_fnames <- list.files(shiny_path, full.names = TRUE)


########
# Get a vector of the Continuous Data Files, exclude the audit info data
datafiles <- in_fnames[!grepl('AUDIT_INFO', in_fnames)]
datafiles <- datafiles[!grepl('SiteMasterInfo', datafiles)]

#########
# Get vector of audit data files, exclude the logged data files
auditfiles <- in_fnames[grepl('AUDIT_INFO', in_fnames)]

########
# LOAD the site master information dataframe
load(in_fnames[grepl('SiteMasterInfo', in_fnames)])

# make a dataframe of the audit file components
allaudit.fileinfo<- read.table(text = basename(auditfiles) , sep = '_', as.is=TRUE)
allaudit.fileinfo<- cbind(allaudit.fileinfo,auditfiles)
names(allaudit.fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'audit', 'info_extension', 'filepath' )
allaudit.fileinfo$filepath <- as.character(allaudit.fileinfo$filepath)

########



################################################################################################################
################################################################################################################

#####  #
#      #
###    #
#      #  
# or   #### oop through files loading output into Volunteer WQ Database


#
##
###############  START For loop through logger files here
##
#  
for (i in 1:length(datafiles)) { 
  fname <- datafiles[i]#"XXXX_99990_10429625_DO_20150420_Salmon R Hatchery_1_.Rdata"
  load(fname)
  print(paste0(i, ' of ', length(datafiles), ' is ' , fname))
  
  # Get continuous data file information from name
  fileinfo <- read.table(text = basename(fname) , sep = '_', as.is=TRUE, colClasses = "character")
  names(fileinfo) <- c('subid', 'lasar', 'LoggerID', 'charid', 'date', 'desc', 'depth_m','extension' )
  
  # Clean up logged result rows with poor DQL
  tmp_data$r4calc <- as.numeric(ifelse(is.na(tmp_data$rDQL) | tmp_data$rDQL == 'C'| tmp_data$rDQL == 'D',
                                       NA, tmp_data$r))
  
  # If there is not valid data for calculations, move on to tne next file
  if (sum(tmp_data$r4calc, na.rm= TRUE) == 0) next
  
  print(paste0(fileinfo$LoggerID, '-', fileinfo$charid, ' Calculate Daily Stats'))
  ###################
  #   #
  #   #
  #####
  #   #
  #   #ourly Values
  ##################
  
  # Get unique hour values
  tmp_data$hr <- format(tmp_data$DATETIME, "%Y-%j-%H")
  
  # If there is no comment field from running the edit script, then create one
  if(!'cmnt' %in% names(tmp_data)) tmp_data$cmnt<- NA
  
  
  # Simplify to hourly values and Stats
  hrsumna<-ddply(tmp_data,"hr",summarise, # transform for retaining rows
                 date = mean(date),
                 hrDTmin = min(DATETIME),
                 hrDTmax = max(DATETIME),
                 hrN = sum(!is.na(r4calc)),
                 hrMean = mean(r4calc, na.rm=TRUE),
                 hrMin = min(r4calc, na.rm=TRUE),
                 hrMax = max(r4calc, na.rm=TRUE),
                 hrdql = max(rDQL, na.rm=TRUE),
                 cmnt = toString(unique(cmnt[!is.na(cmnt)])))
  # Warnings are OK but need to run NA assignments below
  hrsumna$hrMin[which(is.infinite(hrsumna$hrMin))] <- NA
  hrsumna$hrMax[which(is.infinite(hrsumna$hrMax))] <- NA
  
  # replace blank cmnt cells with NA
  hrsumna$cmnt[which(hrsumna$cmnt == '')] <- NA
  
  
  #########################
  # #
  #  #
  #   # 
  #  #  
  ##  aily stats
  ##########################
  
  # For each date, how many hours have hrN > 0
  # remove rows with zero records in an hour.
  hrdat<- hrsumna[which(hrsumna$hrN >0),]
  
  # Summarise to daily statistics
  daydat<-ddply(hrdat,"date",summarise,
                dDTmin = min(hrDTmin),
                dDTmax = max(hrDTmax),
                hrNday = length(hrN), 
                dyN = sum(hrN),
                dyMean = mean(hrMean, na.rm=TRUE),
                dyMin = min(hrMin, na.rm=TRUE),
                dyMax = max(hrMax, na.rm=TRUE),
                dydql = max(hrdql, na.rm=TRUE),
                cmnt = paste(unique(unlist(strsplit(toString(unique(cmnt[!is.na(cmnt)])), split = ', '))), collapse = '; ') )
  
  # replace blank cmnt cells with NA
  daydat$cmnt[which(daydat$cmnt == '')] <- NA
  
  # assign dayDQL based on hours with data dydat$hrNday
  for (j in 1:length(daydat$date)) {
    daydat$dDQL[j] <-  ifelse(daydat$hrNday[j] > 22, daydat$dydql[j],
                              ifelse(daydat$hrNday[j] < 23 & daydat$hrNday[j] > 20, max(c(daydat$dydql[j],'B'))
                                     ,'C'))
  }
  
  # Add comment regarding dDQL determination
  daydat$cmnt <- ifelse(daydat$hrNday > 22, daydat$cmnt, 
                        (ifelse(is.na(daydat$cmnt),paste0(as.character(daydat$hrNday),' hrs with valid data in day'),
                                paste0(daydat$cmnt,'; ',as.character(daydat$hrNday),' hrs with valid data in day'))))
  
  
  
  # Delta T
  for (j in 1:length(daydat$date)) {
    daydat$delta[j] <- ifelse(daydat$dDQL[j] == 'C' | is.na(daydat$dDQL[j]), NA, 
                              daydat$dyMax[j] - daydat$dyMin[j] )
  }
  
  # Get the daily median values and add them to daily data
  dm <- aggregate(r4calc~date, data=tmp_data, FUN=median, na.rm = TRUE)
  names(dm) <- c("date", "dyMedian")
  daydat <- merge(x= daydat,y= dm, by="date",all.x=TRUE)
  
  
  ##########
  #   #
  ## ##
  # # #
  #   # oving Averages
  #########
  
  # create column for moving average calculations
  daydat$ma <- NA
  daydat$anaStart <- as.POSIXct(NA)# Add analysis start and end dates
  daydat$anaEnd <- as.POSIXct(NA)
  
  ##  DISSOLVED OXYGEN 
  if (fileinfo$charid  %in% c('DO','adjDO','DOs')) {
    # remove data with bad dDQL's and get daily minimum value
    daydat$r4ma <- ifelse(daydat$dDQL == 'C' | is.na(daydat$dDQL), NA, daydat$dyMin ) 
    for (j in 1:length(daydat$date)) {
      if (j < 30) {
        daydat$ma[j]<- NA
      } else if (j >29 && (daydat$dDTmax[j] - daydat$dDTmin[j-29])<= 30) {
        daydat$anaStart[j] <- daydat$dDTmin[j-29] # careful that the local time zone doesn't mess this up
        daydat$anaEnd[j] <- daydat$dDTmax[j] # careful that the timeshift doesn't mess this up
        ifelse(sum(is.na(daydat$r4ma[(j-29):j])) > 3, NA, # if more than 3 missing days than no calculation
               daydat$ma[j] <- mean(daydat$r4ma[(j-29):j], na.rm = TRUE))
      }
    }
  }
  
  ##  TEMPERATURE
  if (fileinfo$charid %in% c('TEMP','adjTEMP')) {
    # remove data with bad dDQL's and get daily minimum value
    daydat$r4ma <- ifelse(daydat$dDQL == 'C' | is.na(daydat$dDQL), NA, daydat$dyMin ) 
    for (j in 1:length(daydat$date)) {
      if (j < 7) {
        daydat$ma[j]<- NA
      } else if (j > 6 && (daydat$dDTmax[j] - daydat$dDTmin[j-6]) <= 7) {
        daydat$anaStart[j] <- daydat$dDTmin[j-6] # careful that the default time zone doesn't mess this up
        daydat$anaEnd[j] <- daydat$dDTmax[j] # careful that the the default time zone doesn't mess this up
        ifelse(sum(is.na(daydat$r4ma[(j-6):j])) > 1, NA, # if more than on missing day then no calculation
               daydat$ma[j] <- mean(daydat$r4ma[(j-6):j], na.rm = TRUE))
      }
    }
  }
  
  ###################################################
  #########################
  
  ###
  #
  ###
    #
  ### ummary Stat Tables (daily and per deployment) for export to csv
  
  # Add deployment metadata
  daydat$LASAR <- fileinfo[,"lasar"]
  daydat$charid <- fileinfo[,"charid"]
  daydat$Depth_m <- fileinfo[,"depth_m"]
  daydat$LoggerID <- fileinfo[,"LoggerID"]
  daydat$SiteDesc <-  fileinfo[,"desc"]
  
  ds <- daydat[,c("LASAR", "SiteDesc", "LoggerID", "Depth_m", "charid", "date", "dDTmin", "dDTmax","dyN", "hrNday", 
                  "dydql", "dDQL", "dyMean", "dyMin", "dyMax", "delta", "dyMedian", "anaStart", "anaEnd", "ma", "cmnt")]
  
  # Stack all the daily summaries together and then save as a csv.
  if (i == 1) {
    tmpDyStat <- ds
  } else if (i > 1 && i < length(datafiles)) {
    tmpDyStat <- rbind(tmpDyStat, ds)
  } else if ( i == length(datafiles)) {
    tmpDyStat <- rbind(tmpDyStat, ds)
    write.csv(tmpDyStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DailyStats.csv'))
  }
  
  ######################################################
  ####################################
  
  ##
  # #
  # #
  ## eployment Statistics
  
  
  DepStat <- fileinfo[,c("lasar", "desc", "LoggerID", "depth_m", "date", "charid")]
  DepStat$StartDate <- min(tmp_data$DATETIME[which(!is.na(tmp_data$r4calc))])
  DepStat$EndDate <- max(tmp_data$DATETIME[which(!is.na(tmp_data$r4calc))])
  DepStat$Resultcount <- length(!is.na(tmp_data$r4calc))
  DepStat$DayCount <- length(unique(tmp_data$date[which(!is.na(tmp_data$r4calc))]))
  DepStat$Mean <- mean(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$GeoMean <- geometric.mean(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$Min <- min(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$Fifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.05)
  DepStat$Tenth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.10)
  DepStat$Twentieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.20)
  DepStat$TwentyFifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.25)
  DepStat$Median <- median(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  DepStat$SeventyFifth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.75)
  DepStat$Eightieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.80)
  DepStat$Ninetieth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.90)
  DepStat$NinetyFifthth <- quantile(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))],probs=.95)
  DepStat$Max <- max(tmp_data$r4calc[which(!is.na(tmp_data$r4calc))])
  
  if (i == 1) {
    tmpDepStat <- DepStat
  } else if (i > 1 && i < length(datafiles)) {
    tmpDepStat <- rbind(tmpDepStat, DepStat)
  } else if ( i == length(datafiles)) {
    tmpDepStat <- rbind(tmpDepStat, DepStat)
    write.csv(tmpDepStat, file = paste0(save_path,'/',fileinfo[,"subid"],'_','DeployStats.csv'))
  }
  #############################################################
  ###############################
  
  ####
  #  #
  ###
  #
  # lots
  
  
  # Build data frame with plotting data
  PltDat <- tmp_data[,c("DATETIME", "date", "charid", "r", "r4calc", "field_audit_grade", "cvDQL", "rDQL", "anomaly") ]
  PltDat <- merge(x = PltDat, y = daydat, by = "date")
  # get audit data
  auditfilepath <- allaudit.fileinfo$filepath[Reduce(intersect, list(which(allaudit.fileinfo$lasar == fileinfo$lasar), 
                                                                     which(allaudit.fileinfo$charid == fileinfo$charid), 
                                                                     which(allaudit.fileinfo$LoggerID == fileinfo$LoggerID)))]
  load(auditfilepath)
  # Get error bar limits for audits...later. B/c of the low level criteria this will be difficult.
  
  # Basic plot data  
  pd <- ggplot(data = PltDat, aes(DATETIME, r)) + geom_point(aes(color = rDQL)) + xlab("Date") + theme_bw() +
    ylim(min(PltDat$r4calc[which(!is.na(PltDat$r4calc))]),max(PltDat$r4calc[which(!is.na(PltDat$r4calc))])) +
    ylab(paste0(fileinfo[,'charid'],' (',ConCharInfo$Unit[which(ConCharInfo$charid == fileinfo[,'charid'])],')')) +
    labs(title = paste0('Continuous ',fileinfo[,'charid'], ' Data at LASAR Station ', fileinfo[,'lasar']) ) +
    geom_point(data = dr_info, aes(x = AUDIT_DATETIME, y = AUDIT_RESULT), color = 'black', size = 3, alpha = 0.5) 
  
  
  if (fileinfo[,'charid'] %in% c('Q', 'TURB', 'adjTURB')){
    # Log Scale
    pd <- pd + scale_y_log10() + annotation_logticks( base = 10, sides = 'l')
  } else if (fileinfo[,'charid'] %in% c('TEMP', 'adjTEMP')) {
    pd <- pd + geom_hline(aes(yintercept = 20), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 20.5, label = 'Migration', color = 'blue') + # Migration corridor
      geom_hline(aes(yintercept = 18), linetype = 6, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 18.5, label = 'Rearing', color = 'blue') +# Rearing and Migration
      geom_hline(aes(yintercept = 16), linetype = 4, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 16.5, label = 'Cold Habitat', color = 'blue') +# Core Cold Habitat
      geom_hline(aes(yintercept = 13), linetype = 3, color = "blue" ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 13.5, label = 'Spawning', color = 'blue') + # Spawning
      geom_line(data = PltDat, aes(DATETIME, ma) , size = 1.25, color = 'blue')
  } else if (fileinfo[,'charid'] %in% c('DO', 'adjDO')) {
    pd <- pd + geom_hline(aes(yintercept = 11.0), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 11.2, label = 'Spawning', color = 'blue') +  # Spawning
      geom_hline(aes(yintercept = 8.0), linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.2, label = 'Cold-water', color = 'blue') + # Coldwater
      geom_hline(aes(yintercept = 6.5), linetype = 4, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Cool-water', color = 'blue')  + # Coolwater
      geom_hline(aes(yintercept = 5.5), linetype = 3, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 5.7, label = 'Warm-water', color = 'blue')  # Warmwater
  } else if (fileinfo[,'charid'] %in% c('PH', 'adjPH')) {
    pd <- pd + geom_hline(aes(yintercept = 8.5), linetype = 5, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 8.7, label = 'Upper Criteria', color = 'blue') +  # Upper
      geom_hline(aes(yintercept = 6.5) , linetype = 6, color = 'blue' ) + 
      annotate('text', x = PltDat$DATETIME[10], y = 6.7, label = 'Lower Criteria', color = 'blue') # Lower
  }
  
  ggsave(filename = gsub('.Rdata','tsPLOT.png', basename(fname)), path = save_path, width = 11, height = 8.5, units = 'in', plot = pd)
  
}