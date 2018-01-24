library(readxl)
library(plyr)
library(lubridate)

#This script expects as input an excel file with worksheets named:
#    SiteMasterInfo
#    PrePostResults
#    FieldAuditResults
#    And a single sheet for each unique logger ID
#
#Expected format for SiteMasterInfo:
#Header info takes up rows 1 through 5 with column names in row 6
#Columns to include in this exact format:
#    Logger_ID
#    LASAR_ID
#    Station Description
#
#Expected format for PrePostResults:
#No header info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER_ID 
#    DATA QUALITY LEVEL
#Column LOGGER ID must at least contain the same logger ids as are in SiteMasterInfo
#
#Expected format for FieldAuditResults:
#No head info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER_ID
#    PARAMETER (DO,TEMP,COND,DOs,adjDO,TURB,Q,PH) 
#         each parameter reported must be listed regardless of the presence of audit info for parameter
#    DATE
#    TIME
#    AUDIT_RESULT
#    COMMENTS
#
#
#Expected format for worksheets with logger ID as their name:
#Header infro on rows 1-4. COlumn names in row 5.  Number of columns depends on the number of continuous parameters
#but there must always be one row with results and a second with data quality levels for each parameter.
#    DATE
#    TIME
#    PARAMETER_r : 'PARAMETER' must exactly match parameter listed in FieldAuditResults 
#    PARAMETER_DQL : 'PARAMETER' must exactly match parameter listed in FieldAuditResults
# repeat 
#Note: the last column name is DQL because TEMP is contained in row 4 which is a skipped row

#Set the data file path and file that you want to process
src_file <- '//deqlab1/Vol_Data/umpqua/2014/ReferenceTemp14/UmpCrRefTemp4r.xlsx'

SubID <- '0089' # Enter the submission ID from VolWQDB



#Set the output location where the shiny app can use it
save_dir <- '//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data'



# When OLD TEMPERATURE AUDIT FORMAT is used, you will need to upload these files output from CnTmpOldAudit.R 
#load('//deqlead02/Vol_Data/UpperDeschutes/2016/audits.RData')
#audits$COMMENTS <- as.character(NA) # the old audit didn't grab comments.
#load('//deqlead02/Vol_Data/UpperDeschutes/2016/smi.RData')
#load('//deqlead02/Vol_Data/UpperDeschutes/2016/logchar.RData')
#load('//deqlead02/Vol_Data/UpperDeschutes/2016/ppcheck.RData')
# If you load the files above, then skip the steps below down to '# Load the QC criteria for continuous data.'

# for comparison to valid charid values load
load('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConCharInfo.RData')

#Grab the master info sheet that has the logger ids
capture.output(smi <- read_excel(src_file, sheet = 'SiteMasterInfo', skip = 5), file = "nul")  ###  NOTE skip rows seem inconsistent 4 or 5
smi <- smi[!is.na(smi$Logger_ID),]
smi <- plyr::rename(smi, replace = c("Deploy_Depth(meters)" = "Depth_m"))
smi <- plyr::rename(smi, replace = c('Station Description' = 'Station_Description'))
smi$Logger_ID <- gsub("\\..*","",smi$Logger_ID) # get rid of extraneous .000000
smi$Depth_m <- gsub("\\..*","",smi$Depth_m)
save(smi, file = paste(save_dir, paste0(SubID,"_SiteMasterInfo.RData"), sep = "/"))


#Grab the PrePostResults for getting the bath_dql
capture.output(ppcheck <- read_excel(src_file, sheet = 'PrePostResults'), file = "nul")
ppcheck <- ppcheck[!is.na(ppcheck$LOGGER_ID),]
ppcheck <- ppcheck[!is.na(ppcheck$DATE_TIME),] 
ppcheck <- ppcheck[!is.na(ppcheck$EXPECTED_RESULT),]
ppcheck$DATE_TIME <- as.character(ppcheck$DATE_TIME)
ppcheck$DATE_TIME <- as.POSIXct(ppcheck$DATE_TIME, format = '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')
ppcheck$LOGGER_ID <- gsub("\\..*","",ppcheck$LOGGER_ID)# get rid of extraneous .000000

#  Fix incorrect cases in PARAMETER
for (i in seq_along(ppcheck$PARAMETER)) { 
  ppcheck$PARAMETER[i] <- as.character(ConCharInfo$charid[which(toupper(ConCharInfo$charid) == toupper(ppcheck$PARAMETER[i]))])   
}


#Get the audit sheet which has the deploy and retrieval times as well
capture.output(audits <- read_excel(src_file, sheet = 'FieldAuditResults'), file = "nul")
audits <- audits[!is.na(audits$LOGGER_ID),]
audits <- audits[!is.na(audits$DATE),]
audits <- audits[!is.na(audits$TIME),]
audits$LOGGER_ID <- gsub("\\..*","",audits$LOGGER_ID)
audits$date_char <- strftime(audits$DATE, format = '%Y-%m-%d', origin = "1970-01-01", tz = 'UTC')
audits$time_char <- strftime(audits$TIME, format = '%H:%M:%S', tz ='UTC')
audits$datetime <- paste(audits$date_char, audits$time_char)
audits$AUDIT_DATETIME <- as.POSIXct(strptime(audits$datetime, format = "%Y-%m-%d %H:%M:%S", tz = 'America/Los_Angeles'))

#  Fix incorrect cases in PARAMETER
for (i in seq_along(audits$PARAMETER)) { 
  audits$PARAMETER[i] <- as.character(ConCharInfo$charid[which(toupper(ConCharInfo$charid) == toupper(audits$PARAMETER[i]))])
}

logchar<-unique(audits[,c('LOGGER_ID','PARAMETER')])
names(logchar)<-c('log','char')


############################################################################################################
############################################################################################################

# Load the QC criteria for continuous data
load('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConQC.RData')
#ConQC <- read.csv('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ConQC.csv')


#####
###
#   CHECK TO MAKE SURE THE DATA TABS MATCH THE LOGGER ID'S
logchar$log[!which(logchar$log %in% excel_sheets(src_file))]


###
#
##
#
# or loop through each logger/characteristic combination

for (i in seq_along(logchar$log)) {
  start.time <- Sys.time()
  print(paste("Starting file", i, "of", nrow(logchar), "=", logchar[i,1], logchar[i,2], start.time))
  capture.output(tmp_data <- read_excel(src_file, sheet = logchar[[i,1]], skip = 4), file = "nul")
  tmp_data <- tmp_data[,c(1,2,grep(paste0("^",logchar[i,2],"_"),names(tmp_data), ignore.case = TRUE))]
  tmp_data$charid <- logchar$char[i]
  
  names(tmp_data) <- c("DATE","TIME","r","dql", "charid")
  tmp_data <- tmp_data[!is.na(tmp_data$r),]
  tmp_data <- tmp_data[!is.na(tmp_data$DATE),]
  tmp_data$date_char <- strftime(tmp_data$DATE, format = "%Y-%m-%d", tz = 'UTC')
  tmp_data$time_char <- strftime(tmp_data$TIME, format = '%H:%M:%S', tz ='UTC')
  tmp_data$datetime <- paste(tmp_data$date_char, tmp_data$time_char)
  tmp_data$DATETIME <- as.POSIXct(strptime(tmp_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = 'America/Los_Angeles'))
  
  #  Correct times for changes between PDT and PST 
  ifelse(length(unique(dst(tmp_data$DATETIME))) == 1, print("No Time Change"), print("Time Change Correction Needed"))
         dttz <- tmp_data[,c(1:3,6:9)]
         dttz$dst <- dst(dttz$DATETIME)
         #ifelse(dttz$dst == TRUE, print('PDT'), print ('PST'))
         dttz$UTCoffset <- ifelse(dttz$dst == TRUE, 7, 8) # determine the offset from UTC 
         dttz$DTutc <- force_tz(dttz$DATETIME, 'UTC') + dhours(dttz$UTCoffset[1]) # set all the times to UTC based on the timezone at deployment
         dttz$CorrectDateTime <- with_tz(dttz$DTutc, tzone = 'America/Los_Angeles') # convert the times back to Pacific
  tmp_data$DATETIME <- dttz$CorrectDateTime # Copy the corrected datetimes back into the tmp_data dataframe.
  
  
  # pull out audits based on logger id and characteristic/parameter
  print('starting audits')
  dr_info <- audits[which(audits$LOGGER_ID %in% logchar$log[i]),c('AUDIT_DATETIME','PARAMETER', 'AUDIT_RESULT', 'LOGGER_RESULT','COMMENTS')]
  dr_info <- dr_info[which(dr_info$PARAMETER %in% logchar$char[i]),]
  dr_info <- as.data.frame(dr_info)
  dr_info <- dr_info[order(dr_info$AUDIT_DATETIME), ]
  # Set TRUE FALSE for before and after deployment
  tmp_data$dbf <- ifelse(tmp_data$DATETIME < dr_info[1, 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  tmp_data$raf <- ifelse(tmp_data$DATETIME > dr_info[nrow(dr_info), 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  
  # get the probe values at the time of the audits  
  deploy_ind <- min(which(tmp_data$dbf))
  obs.dt <- tmp_data[deploy_ind, c("DATETIME","r")]
  
  retrieve_ind <- max(which(tmp_data$raf))
  obs.rt <- tmp_data[retrieve_ind, c("DATETIME","r")]
  
  # Grading
  tmp_data$field_audit_grade <- NA
  
  # Quality control calculation type
  qcad <- as.vector(ConQC$charid[which(ConQC$Qccal == 'AbsDiff')])
  qcpd <- as.vector(ConQC$charid[which(ConQC$Qccal == 'PerDiff')])
  
  # calc differences
  diff.d <- NA
  diff.r <- NA
  
  # Assumption that calculations based on values determined in obs.dt and obs.rt. If there are observed results then
  # could replace obs.dt with dr_info[1, 'OBS_RESULT'] and obs.rt with dr_info[nrow(dr_info), 'OBS_RESULT']
  
  # Select logger value for comparison to audit.  If Logger_Result reported us it, otherwise select based on closest in time.
  dlv <- ifelse(is.na(dr_info[1,'LOGGER_RESULT']), obs.dt$r , dr_info[1,'LOGGER_RESULT'])
  rlv <- ifelse(is.na(dr_info[nrow(dr_info), 'LOGGER_RESULT']),obs.rt$r,dr_info[nrow(dr_info), 'LOGGER_RESULT'])
  if (logchar[i,"char"] %in% "TURB") {
    diff.d <- ifelse (mean(c(dr_info[1, 'AUDIT_RESULT'], dlv)) > 20,
                      abs(dr_info[1, 'AUDIT_RESULT'] - dlv)/mean(c(dr_info[1, 'AUDIT_RESULT'], dlv)), #calc RPD based on logged result not dr_info[1, 'OBS_RESULT']
                      abs(dr_info[1, 'AUDIT_RESULT'] - dlv) )# calc AbsDiff
    diff.r <- ifelse (mean(c(dr_info[nrow(dr_info), 'AUDIT_RESULT'], rlv)) > 20,
                      abs(dr_info[nrow(dr_info), 'AUDIT_RESULT'] - rlv)/mean(c(dr_info[nrow(dr_info), 'AUDIT_RESULT'], rlv)), #calc RPD
                      abs(dr_info[nrow(dr_info), 'AUDIT_RESULT'] - rlv) )# calc AbsDiff
  } else if (logchar[i,"char"] %in% qcad){
    diff.d <- abs(dr_info[1, 'AUDIT_RESULT'] - dlv)
    diff.r <- abs(dr_info[nrow(dr_info), 'AUDIT_RESULT'] - rlv)
  } else if (logchar[i,"char"] %in% qcpd){
    diff.d <- abs((dr_info[1, 'AUDIT_RESULT'] - dlv)/dlv)
    diff.r <- abs((dr_info[nrow(dr_info), 'AUDIT_RESULT'] - rlv)/rlv)
  }
  
  
  # deploy grade
  if (logchar[i,"char"] %in% "TURB"){
    pA <- ifelse (mean(c(dr_info[1, 'AUDIT_RESULT'], dlv)) > 20,
                  ConQC$prec_A[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                  ConQC$prec_A[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "AbsDiff")] )# AbsDiff criteria
    pB <- ifelse (mean(c(dr_info[1, 'AUDIT_RESULT'], dlv)) > 20,
                  ConQC$prec_B[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                  ConQC$prec_B[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "AbsDiff")] )# AbsDiff criteria
  } else {
    pA <- ConQC$prec_A[which(ConQC$charid == logchar$char[i])]
    pB <- ConQC$prec_B[which(ConQC$charid == logchar$char[i])]
  }
  obs.dt$AUDIT_GRADE <- ifelse(is.na(diff.d),"E",
                               ifelse(diff.d < pA,"A",
                                      ifelse(diff.d < pB,"B", "C")))
  
  # retrieval grade
  if (logchar[i,"char"] %in% "TURB"){
    pA <- ifelse (mean(c(dr_info[nrow(dr_info), 'AUDIT_RESULT'], rlv)) > 20,
                  ConQC$prec_A[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                  ConQC$prec_A[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "AbsDiff")] )# AbsDiff criteria
    pB <- ifelse (mean(c(dr_info[nrow(dr_info), 'AUDIT_RESULT'], rlv)) > 20,
                  ConQC$prec_B[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                  ConQC$prec_B[which(ConQC$charid == logchar$char[i] & ConQC$Qccalc == "AbsDiff")] )# AbsDiff criteria
  } else {
    pA <- ConQC$prec_A[which(ConQC$charid == logchar$char[i])]
    pB <- ConQC$prec_B[which(ConQC$charid == logchar$char[i])]
  }
  obs.rt$AUDIT_GRADE <- ifelse(is.na(diff.r),"E",
                               ifelse(diff.r < pA,"A",
                                      ifelse(diff.r < pB,"B", "C")))
  
  obs.dt$IND <- deploy_ind
  obs.rt$IND <- retrieve_ind
  dr_obs <- rbind(obs.dt[,c('DATETIME','r', 'AUDIT_GRADE', 'IND')], 
                  obs.rt[,c('DATETIME','r', 'AUDIT_GRADE', 'IND')])
  print("finished deploy and retrieve audits, starting mid deployment audits")
  
  
  #Handling of additional audits
  if (nrow(dr_info) > 2) {
    dr_info_sub <- dr_info[2:(nrow(dr_info) - 1),] # subset dr_info excluding first and last
    for (j in 1:nrow(dr_info_sub)) { # for each of the middle audits
      if (grepl("before cleaning",dr_info_sub[j, 'COMMENTS'], ignore.case = T)){
        audit_ind <- max(which(tmp_data$DATETIME < dr_info_sub[j, 'AUDIT_DATETIME'])) # closest time before audit
      } else if (grepl("final audit",dr_info_sub[j, 'COMMENTS'], ignore.case = T)) {
        audit_ind <- min(which(tmp_data$DATETIME > dr_info_sub[j, 'AUDIT_DATETIME'])) # closest time after audit
      } else {
        audit_ind <- which.min(abs(tmp_data$DATETIME - dr_info_sub[j, 'AUDIT_DATETIME']))
      }
      
      tmp.obs <- tmp_data[audit_ind, c('DATETIME', 'r')]
      #tmp.obs$AUDIT_DateTime <- dr_info_sub[j, 'AUDIT_DATETIME']
      #tmp.obs$AUDIT_r <- dr_info_sub[j,'AUDIT_RESULT']
      
      # Need specify difference cacluation
      
      # Assign logger value to use, provided in submission or selected from logged data (tmp.obs)
      tmplv <- ifelse(is.na(dr_info_sub[j,'LOGGER_RESULT']), tmp.obs$r,dr_info_sub[j,'LOGGER_RESULT'])
      if (dr_info_sub[j,'PARAMETER'] %in% "TURB") {
        diff.a <- ifelse(mean(c(dr_info_sub[j,'AUDIT_RESULT'],tmplv)) > 20,
                         abs(dr_info_sub[j,'AUDIT_RESULT'] - tmplv)/mean(c(dr_info_sub[j,'AUDIT_RESULT'], tmplv)),
                         abs(dr_info_sub[j,'AUDIT_RESULT'] - tmplv))
        pA <- ifelse (mean(c(dr_info_sub[j,'AUDIT_RESULT'], tmplv)) > 20,
                      ConQC$prec_A[which(ConQC$charid == "TURB" & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                      ConQC$prec_A[which(ConQC$charid == "TURB" & ConQC$Qccalc == "AbsDiff")])
        pB <- ifelse (mean(c(dr_info_sub[j,'AUDIT_RESULT'], tmplv)) > 20,
                      ConQC$prec_B[which(ConQC$charid == "TURB" & ConQC$Qccalc == "PerDiff")], # perDiff criteria
                      ConQC$prec_B[which(ConQC$charid == "TURB" & ConQC$Qccalc == "AbsDiff")] )# AbsDiff criteria)
        
      } else if (dr_info_sub$PARAMETER[j] %in% qcad){
        diff.a <- abs(dr_info_sub[j , 'AUDIT_RESULT'] - tmplv)
      } else if (dr_info_sub$PARAMETER[j] %in% qcpd) {
        diff.a <- abs((dr_info_sub[j , 'AUDIT_RESULT'] - tmplv)/dr_info_sub[j , 'AUDIT_RESULT'])
      } else diff.a <- NA
      
      tmp.obs$AUDIT_GRADE <- ifelse(is.na(diff.a),"E",
                                    ifelse(diff.a < pA,"A",
                                           ifelse(diff.a < pB,"B", "C")))
      tmp.obs$IND <- audit_ind
      dr_obs <- rbind(dr_obs, tmp.obs)
    }
  }
  
  
  dr_obs <- dr_obs[order(dr_obs$DATETIME), ]
  dr_obs <- rename(dr_obs, c('DATETIME' = "OBS_DATETIME",
                             'r' = 'LOGGED_RESULT'))
  dr_info <- cbind(dr_info, dr_obs) # the two results will not be the same b/c former is hand selected and later is assigned via 
  # code above. OBS_RESULT is not a value reported in logged data, but read directly from unit at time of audit. 
  
  
  # apply the grades   
  for (k in 2:nrow(dr_info)) {
    if (dr_info$IND[k-1]+1 != dr_info$IND[k]){ # prevent operation from overwriting calculated audit grades
      start_ind <- ifelse(k == 1, dr_info$IND[1], dr_info$IND[k - 1] + 1) # unless at the start, use first logged result after previous audit
      grade <- max(c(dr_info$AUDIT_GRADE[k-1],dr_info$AUDIT_GRADE[k]))# choose lower grade  
      tmp_data[start_ind:dr_info$IND[k]-1, 
               'field_audit_grade'] <- grade
      tmp_data[dr_info$IND[k], 'field_audit_grade'] <- dr_info[k, 'AUDIT_GRADE']
    }
  }
  
  
  
  ####  ####  #   #
  #     #     #  #
  #     #      ##
  ####  ####   #    's
  print("starting CCV processing")
  #Determine what the DQL from the continuing calibration verifications 

  cvdata <- ppcheck[grep(logchar[i,'log'], ppcheck$LOGGER_ID),] 
  cvdata <- cvdata[which(cvdata$PARAMETER == logchar$char[i]),
                   c('LOGGER_ID', 'PARAMETER', 'DATE_TIME', 'EXPECTED_RESULT', 'LOGGER_RESULT', 'DQL', 'COMMENTS' )]
  # When no CCV for the parameter
  if (length(cvdata$LOGGER_ID) == 0) {
    tmp_data$cvDQL <- as.character('E') # grade all tmp_data$aDQL 'E'
  } else {
  cvdata <- cvdata[order(cvdata$DATE_TIME),]
  cvdata$aDQL <- as.character(NA) # this will fail if there are 0 obs in cvdata.
  
  # Calculate difference based on appropriate method
  cvdata$diff <- NA # this should also fail if there are 0 obs cvdata.
  for (l in 1:length(cvdata$LOGGER_ID)) {
    # Determine the closest logged result
    cvdata$LgrInd[l] <- which.min(abs(tmp_data$DATETIME - cvdata$DATE_TIME[l]))
    if (cvdata$PARAMETER[l] %in% c("TURB", "COND", "Q") & cvdata$EXPECTED_RESULT[l] != 0) {
      cvdata$diff[l] <- round(abs(cvdata$LOGGER_RESULT[l] - cvdata$EXPECTED_RESULT[l])/cvdata$EXPECTED_RESULT[l],5)
    } else {
      cvdata$diff[l] <- abs(cvdata$LOGGER_RESULT[l] - cvdata$EXPECTED_RESULT[l])
    } 
  }
  
  # Get the accuracy criteria, if tb an sc expected values are 0 then 10 x criteria
  aA <- as.numeric(ConQC$acc_A[which(ConQC$charid == logchar$char[i] & !is.na(ConQC$acc_A))])
  aB <- as.numeric(ConQC$acc_B[which(ConQC$charid == logchar$char[i] & !is.na(ConQC$acc_B))])
  
  # Assign grades
  for (m in 1:length(cvdata$LOGGER_ID)){ 
    if (cvdata$PARAMETER[m] %in% c("TURB", "COND", "Q") & cvdata$EXPECTED_RESULT[m] == 0) {
      cvdata$aDQL[m] <- ifelse(is.na(cvdata$diff[m]), "E", 
                               ifelse(cvdata$diff[m] < (aA * 10), "A", 
                                      ifelse(cvdata$diff[m] < (aB * 10), "B", "C")))
    } else {
      cvdata$aDQL[m] <- ifelse(is.na(cvdata$diff[m]), "E", 
                               ifelse(cvdata$diff[m] < aA, "A", 
                                      ifelse(cvdata$diff[m] < aB, "B", "C")))
    }
  }
  
  # Create Dataframe of tmp_data index and acc DQL value
  cvDQL <- ddply(cvdata, .(LgrInd), summarize, accDQL = max(aDQL))
  
  # Assign Accuracy Data Quality Levels to logged data
  tmp_data$cvDQL <- as.character(NA)
  for (n in seq_along(cvDQL$LgrInd)){
    tmp_data$cvDQL[cvDQL$LgrInd[n]] <- cvDQL$accDQL[n]
  }
  
  # Spread the cvDQL between ccv measurements
  # if there is only one ccv measure then the associated index gets the cvDQL all others get B at best, or if cvDQL < B then cvDQL 
  if (nrow(cvDQL) == 1) { #when there is only one ccv
    tmp_data$cvDQL <- ifelse(cvDQL$accDQL == 'A', 'B', cvDQL$accDQL )
    tmp_data$cvDQL[cvDQL$LgrInd] <- cvDQL$accDQL
  } else { # when there is more than one ccv
    for (k in 2:nrow(cvDQL)) {
      if (cvDQL$LgrInd[k-1] +1 != cvDQL$LgrInd[k]){ # Prevent this operation from overwriting calculated cvDQL's
        start_ind <- cvDQL$LgrInd[k - 1] + 1 # unless at the start, use first logged result after previous audit
        grade <- max(c(cvDQL$accDQL[k-1],cvDQL$accDQL[k]))# choose lower grade  
        tmp_data[start_ind:cvDQL$LgrInd[k]-1, 
                 'cvDQL'] <- grade
        tmp_data[cvDQL$LgrInd[k], 'cvDQL'] <- cvDQL$accDQL[k]
      }
    }  
  }
  
  } # end of if statement testing there is ccv data
  
  
  
  
  ##
  # #
  # #
  ## aily Statistics
  print('starting daily stats and anomaly checks')
  # Create a vector of daily dates for grouping
  tmp_data$date <- as.Date(tmp_data$DATETIME, format="%m/%d/%Y", tz="America/Los_Angeles")
  
  
  
  # Calculate the daily min and maximums
    # Final daily statistics should be calculated after the data has been graded...
      #these are preliminary used for anomaly considerations
  max <- aggregate(r~date, data=tmp_data, FUN= 'max')
  colnames(max)[2] <- "daily_max"
  min <- aggregate(r~date, data=tmp_data, FUN= 'min')
  colnames(min)[2] <- "daily_min"
  mean <- aggregate(r~date, data=tmp_data, FUN= 'mean')
  colnames(mean)[2] <- "daily_mean"
  
  day <- merge(x=min, y=mean,by="date",all.x=TRUE, all.y=TRUE)
  day <- merge(x=day, y=max, by="date", all.x=TRUE, all.y=TRUE)
  day$daily_diel <- day$daily_max - day$daily_min
  date <- seq(min(tmp_data$date), max(tmp_data$date), by=1)
  date_seq <- as.data.frame(date)
  day <- merge(x=date_seq,y=day, by="date",all.x=TRUE)
  
  # Find anamolies
  aDD <- ConQC$AnomDiel[which(ConQC$charid == logchar$char[i])][1]# [1] to select the first if needed
  aDMax <- ConQC$AnomMax[which(ConQC$charid == logchar$char[i])][1]
  aDMin <- ConQC$AnomMin[which(ConQC$charid == logchar$char[i])][1]
  aDMnL <- ConQC$AnomMnL[which(ConQC$charid == logchar$char[i])][1]
  aDMnH <- ConQC$AnomMnH[which(ConQC$charid == logchar$char[i])][1]
  # if any of the anomolies are present report FALSE
  day$anomaly  <- (day$daily_diel > aDD | 
                     day$daily_max > aDMax | 
                     day$daily_min < aDMin | 
                     day$daily_mean < aDMnL | 
                     day$daily_mean > aDMnH)
  
  tmp_data <- merge(tmp_data,day,by="date",all.x=TRUE)
  
  # flag observations before deployment and after retreival date
  tmp_data[which(tmp_data$DATETIME < obs.dt$DATETIME | 
                   tmp_data$DATETIME > obs.rt$DATETIME),"anomaly"] <- NA
  
  tmp_data$field_audit_grade[is.na(tmp_data$field_audit_grade)] <- "E"
  tmp_data$cvDQL[is.na(tmp_data$cvDQL)] <- "E"
  print('assign final DQL')
  
  
  #Set up final grade column to be verified using shiny app and further review
  tmp_data$rDQL <- ifelse(tmp_data$field_audit_grade == 'C' | tmp_data$cvDQL == 'C',
                          'C',
                          ifelse(tmp_data$field_audit_grade == 'B' | tmp_data$cvDQL == 'B',
                                 'B',
                                 ifelse(tmp_data$field_audit_grade == 'A' & tmp_data$cvDQL == 'A',
                                        'A',
                                        ifelse(tmp_data$field_audit_grade == 'E' & tmp_data$cvDQL == 'E',
                                               'E',
                                               'B'))))
  
  #Set pre and post deployment as NA for future clipping
  tmp_data$rDQL <- ifelse(tmp_data$dbf & tmp_data$raf, tmp_data$rDQL, NA) # Not sure I want this as may not clip before and after reported deploy
  
  #Update the rDQL when the original file suggests a lower grade is appropriate
  tmp_data[which(tmp_data$dql %in% c('B','C','D') & 
             tmp_data$dql > tmp_data$rDQL), 
           'rDQL'] <- tmp_data[which(tmp_data$dql %in% c('B','C','D') & 
                                        tmp_data$dql > tmp_data$rDQL), 'dql']
  
  # anomaly check for provided dql not matching the calculated rDQL
  tmp_data$anomaly <- ifelse(is.na(tmp_data$dql) | is.na(tmp_data$rDQL), tmp_data$anomaly, 
                             ifelse(tmp_data$dql == tmp_data$rDQL, tmp_data$anomaly, FALSE))
  
  
  ###
  #
  ##
  #
  # ile Managment
  print ('DQL assignment done, start file management')
  #Just keep the fields we want to persits
  tmp_data <- tmp_data[,c('DATETIME', 'charid','r', 'dql','rDQL', 'anomaly', 
                          'field_audit_grade', 'cvDQL','date', 'daily_min', 
                          'daily_max', 'daily_mean', 'daily_diel')]
  
  dploy <- strftime(dr_info[1, 'AUDIT_DATETIME'], "%Y%m%d")
  lasar <- smi[which(smi$Logger_ID == logchar$log[i]), c('LASAR_ID', 'Station_Description', 'Depth_m' )]
  fname <- paste(SubID, lasar$LASAR_ID, logchar[i,1], logchar[i,2], dploy, lasar$Station_Description, 
                 lasar$Depth_m, ".Rdata", sep = "_")
  fname <- gsub("/","_",fname) # not sure what this is for
  
  fname_audit <- paste(SubID, lasar$"LASAR_ID", logchar[i,1], logchar[i,2], dploy,  
                       lasar$Station_Description, "AUDIT_INFO.Rdata", sep = "_")
  fname_audit <- gsub("/","_",fname_audit)
  
  print(fname)
  print(fname_audit)
  cat('\n\n')
  
  save(tmp_data, file = paste(save_dir, fname, sep = "/"))
  save(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
  
  rm("aDD","aDMax","aDMin","aDMnH", "aDMnL", "audit_ind", "cvDQL", "date", "date_seq", "day", 
     "deploy_ind", "diff.a", "diff.d", "diff.r", "dploy", "dr_info", "dr_info_sub", "dr_obs", "fname", "fname_audit",
     "lasar", "max",  "mean", "min", "obs.dt", "obs.rt",  "pA", "pB", "qcad", "qcpd", "retrieve_ind", "start_ind",
     "tmp.obs", "tmp_data", "grade")
  #   write.csv(tmp_data, file = paste(save_dir, fname, sep = "/"))
  #   write.csv(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
}
