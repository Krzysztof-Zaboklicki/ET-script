####################################################################
#######################  INTRODUCTION  #############################
####################################################################

# This script was created to analyze data from an eye-tracking study.
# 100 participants took part in the study, each one was presented
# either with 4 or 85 

# Data from each participant is stored in 3 different files.
# MessageEvent files contain information about starting and ending timestamps of each trial for each participant.
# Condition_variables files contain information about the types of trial which were presented to each participant.
# BinocularEyeSampleEvent files contain all the data gathered during each trial for each participant.

####################################################################
#####################  LOADING LIBRARIES  ##########################
####################################################################

library(rhdf5)
library(tidyverse)
library(data.table)
library(car)
library(ez)
library(knitr)
library(ggplot2)
library(plotrix)
library(signal)
library(lme4)
library(parameters)
library(extrafont)
loadfonts(device = "win")

####################################################################
#########################  STEP #1  ################################
####################################################################
################  IMPORTING AND MERGING THE DATA  ##################
####################################################################



# My first task is to import all the data and merge it into data.frames which
# can be easily analyzed. Unfortunately, I cannot merge it into one elegant data.frame,
# because it would be too big for R to analyze it quickly. What I need to do is to divide
# all the data into 10 different data.frames 10 participants each.



# I need to find names of the files I import in the folder
files = dir("E:\\data\\sopid", "*hdf5", recursive = T) 

# I declare lists for the datasets I import
df_all         = list()
conditions_all = list()
messages_all   = list()


for (k in 1:10)
{
  # For each tranche of the participants I declare data.frames
  # containing MessageEvent, Condition_variables and BinocularEyeSampleEvent files
  df_all[[k]]       = data.frame()
  conditions_all[[k]]     = data.frame()
  messages_all[[k]] = data.frame()
  
  
  for (j in 1:10)
  {
    expp = tryCatch(
    {
      # I use h5read() function to import the data; I also drop the redundant columns.
      messages  = h5read(paste("E:\\data\\sopid", files[10*(k-1)+j],sep="\\"), "/data_collection/events/experiment/MessageEvent")
      cond      = h5read(paste("E:\\data\\sopid", files[10*(k-1)+j],sep="\\"), "/data_collection/condition_variables/EXP_CV_1")
      data_temp = h5read(paste("E:\\data\\sopid", files[10*(k-1)+j],sep="\\"), "/data_collection/events/eyetracker/BinocularEyeSampleEvent")
      data_temp = data_temp %>% select(-one_of('experiment_id', 'session_id', 'device_id', 'type', 'device_time', 'logged_time', 'filter_id',
                                      'left_gaze_z',  'left_eye_cam_x',  'left_eye_cam_y',  'left_eye_cam_z',  'left_angle_x',  'left_angle_y',
                                      'left_ppd_x',  'left_ppd_y',  'left_velocity_x',  'left_velocity_y',  'left_velocity_xy',
                                      'right_gaze_z', 'right_eye_cam_x', 'right_eye_cam_y', 'right_eye_cam_z', 'right_angle_x', 'right_angle_y',
                                      'right_ppd_x', 'right_ppd_y', 'right_velocity_x', 'right_velocity_y', 'right_velocity_xy',
                                      'left_pupil_measure1_type', 'left_pupil_measure2_type', 'right_pupil_measure1_type', 'right_pupil_measure2_type', 'status'))
      
      # I create columns containing information about the participant name and trial number, where necessary
      cond$ID     = files[10*(k-1)+j]
      cond$trial  = 1:84
      
      messages$ID = files[10*(k-1)+j]
      if   (k == 3 && j == 8){messages = messages[-c(1,2,3,4),]} # One of the datasets has different
      else                   {messages = messages[-c(1,2),]}     # amount of columns, I deal with it here.
      messages$trial_n = sort(rep(1:(nrow(messages)/2),2))       # I also sort trial number column to fit my needs.
      
      data_temp$trial = NA
      for (i in 1:(nrow(messages)/2))
      {data_temp$trial = ifelse(data_temp$time < messages[messages$trial_n == i & messages$text == "trial stop", "time"]  &
                                data_temp$time > messages[messages$trial_n == i & messages$text == "trial start", "time"], i, data_temp$trial)}
      data_temp$ID     = files[10*(k-1)+j]
      
      
      # Finally, I bind the data from the participant with the rest of the data.frame I declared.
      df_all[[k]]         = rbind(df_all[[k]], data_temp)
      conditions_all[[k]] = rbind(conditions_all[[k]], cond)
      messages_all[[k]]   = rbind(messages_all[[k]], messages)
      h5closeAll()
    }, error = function(e){})
  }
  
  
  # After I imported the data from 10 participants and merged it together,
  # I do some tidying up and changing variable types.
  # I also create columns that will help me to clean the data later.
  
  df_all[[k]][is.na(df_all[[k]])] = ""
  df_all[[k]]$trial = as.factor(as.character(df_all[[k]]$trial))
  df_all[[k]] = df_all[[k]][which(df_all[[k]]$trial != "85"),]
  for(i in 2:16){df_all[[k]][,k] = as.numeric(df_all[[k]][,i])}
  df_all[[k]]$ID    = as.factor(df_all[[k]]$ID)
  df_all[[k]]$trialstartend = ""
  df_all[[k]]$clue          = ""
  df_all[[k]]$rejected = F
  df_all[[k]]$AOI      = F
  df_all[[k]][which(df_all[[k]]$left_gaze_x  > -480 & df_all[[k]]$left_gaze_y  > -270 &
                    df_all[[k]]$left_gaze_x  <  480 & df_all[[k]]$left_gaze_y  <  270 &
                    df_all[[k]]$right_gaze_x > -480 & df_all[[k]]$right_gaze_y > -270 &
                    df_all[[k]]$right_gaze_x <  480 & df_all[[k]]$right_gaze_y <  270 ),]$AOI = T
  
  rownames(df_all[[k]])  = seq(length=nrow(df_all[[k]]))
  
  conditions_all[[k]]$ID         = as.factor(conditions_all[[k]]$ID)
  conditions_all[[k]]$trial      = as.factor(conditions_all[[k]]$trial)
  conditions_all[[k]]$code_photo = as.factor(conditions_all[[k]]$code_photo)
  
  # I repeat this for each participants tranche, 10 of the tranches in total. 
}
rm(data_temp, cond, expp, messages, files) # After the loop ends I drop the redundant variables.


# The most time-consuming part is merging the messages, the conditions, and the main datasets together.
# I need to insert rows with trial types as well as staring and ending timestamps into the main data.frames.
# Also, I need to insert them in the correct places, in order feed the data to an analysis algorithm properly.

for(i in 1:length(df_all))
{
  # For each participant I insert "trial start" and "trial end" rows for each trial.
  for(curr_id in unique(df_all[[i]]$ID))
  {
    
    cat(paste0(curr_id, "\n")) # It is time-consuming so I added cat() function with participants' ID to track loop's progress.
    for(curr_trial in as.character(1:84))
    {
      # First
      rows         = as.numeric(rownames(df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trial == curr_trial),]))
      starting_row = rows[1]
      ending_row   = rows[length(rows)]
      df_all[[i]][starting_row:ending_row,]$clue = as.character(conditions_all[[i]][which(conditions_all[[i]]$ID == curr_id & conditions_all[[i]]$trial == curr_trial),]$code_photo)
      
      new_starting_row     = df_all[[i]][starting_row,]
      new_starting_row[19] = "trial start"
      new_starting_row[2]  = messages_all[[i]][which(messages_all[[i]]$ID == curr_id & messages_all[[i]]$trial_n == curr_trial & messages_all[[i]]$text == "trial start"),]$time
      new_starting_row[1]  = "trial start"
      
      new_ending_row       = new_starting_row
      new_ending_row[19]   = "trial end"
      new_ending_row[2]    = messages_all[[i]][which(messages_all[[i]]$ID == curr_id & messages_all[[i]]$trial_n == curr_trial & messages_all[[i]]$text == "trial stop"),]$time
      new_ending_row[1]    = "trial end"
      
      if (starting_row == 1)
      {
        df_all[[i]] = rbind(new_starting_row,
                            df_all[[i]][starting_row:ending_row,],
                            new_ending_row,
                            df_all[[i]][ending_row+1:nrow(df_all[[i]]),],
                            make.row.names=FALSE)
      }
      else if (ending_row == nrow(df_all[[i]]))
      {
        df_all[[i]] = rbind(df_all[[i]][1:(starting_row-1),],
                            new_starting_row,
                            df_all[[i]][starting_row:ending_row,],
                            new_ending_row,
                            make.row.names=FALSE)
      }
      else
      {
        df_all[[i]] = rbind(df_all[[i]][1:(starting_row-1),],
                            new_starting_row,
                            df_all[[i]][starting_row:ending_row,],
                            new_ending_row,
                            df_all[[i]][(ending_row+1):nrow(df_all[[i]]),],
                            make.row.names=FALSE)
      }
      
    }
  }
}
# After the loop ends I drop the redundant variables.
rm(rows, starting_row, ending_row, new_starting_row, new_ending_row, messages_all)


####################################################################
###########################  STEP #2  ##############################
####################################################################
##################  MARKING THE INVALID DATA   #####################
####################################################################



rejected_id = list()
for(i in 1:length(df_all))
{
  rejected_id = append(rejected_id, as.character(i))
  df_all[[i]]$rejected = FALSE
  
  for(curr_id in levels(droplevels(df_all[[i]]$ID)))
  {
    for(curr_trial in as.factor(1:84))
    {
      AOI = df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$AOI
      if(length(AOI[which(AOI == F)])/length(AOI)>0.5)
      {
        df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$rejected = T
        #cat(paste0(curr_id, " ", curr_trial, "\n"))
      }
    }
  }
  for(curr_id in levels(droplevels(df_all[[i]]$ID)))
  {
    AOI = df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trialstartend == "trial start"),]$rejected
    if(length(AOI[which(AOI == T)])/length(AOI)>0.5)
    {
      df_all[[i]][which(df_all[[i]]$ID==curr_id),]$rejected = T
      rejected_id = append(rejected_id, curr_id)
      cat(paste0(curr_id, "\n"))
    }
  }
  rm(AOI)
  for(curr_id in levels(droplevels(df_all[[i]]$ID)))
  {
    #cat(paste0(curr_id, "\n"))
    for(curr_trial in as.factor(1:84))
    {
      
      df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trial == curr_trial),]$time =
        df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trial == curr_trial),]$time -
        df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trial == curr_trial),]$time[1]
    }
  }
}
