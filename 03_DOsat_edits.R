###   ####   ###   ##   #####
#  #  #  #  #     #  #    #
#  #  #  #  ###   ####    #
#  #  #  #     #  #  #    #
###   ####  ###   #  #    #  EDITS

# Data quality level for logged dissolved oxygen satuation data is assigned from the DQL for DO data

####
#
# ##
#  #
### RADE THE DO DATA FIRST!!

################################################################
################################################################

# Retrieve the graded DO data
# filename for the graded DO data
dofile <- "TMDL17_11241_Taco_DO_20170710_Salmon R at Old Hwy 101_NA_.Rdata"

#Filename for saving this script should be equivalent to fname_edits.R
# Set the filename - This should be what you see in the select station box in the shiny app
fname <- "TMDL17_11241_Taco_DOs_20170710_Salmon R at Old Hwy 101_NA_.Rdata"

#This won't need to change
path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

# This is the directory of the original Excel files and is where this script is saved at the end
EditPath <- '//deqlab1/wqm/TMDL/aWestern Region/MidCoast/Salmon River DO TMDL/2017SalmonData/201707CnData/Rfiles/'


# Load DO data
load(paste0(path, dofile))
# Rename
dodata <- tmp_data

# Load the DOsat data
load(paste0(path, fname))

# Assign DQL's 
tmp_data$rDQL <- dodata$rDQL

# Assign Comments
tmp_data$cmnt <- dodata$cmnt

# If needed use standard edits scripts to adjust DO sat if necessary before saving


#When you have made all the edits run this line to save it back to the shiny app data folder
rm(dodata)
save(tmp_data, file = paste0(path, fname))




# Save this script with the original data ....this doesn't work yet so you'll need to use save as
#save(03_edits_examples.R*, file = (paste0(EditPath,unlist(strsplit(fname,split = '.',fixed = TRUE))[1],'EDITS.R')))

