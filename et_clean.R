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
files        = dir("E:\\data\\sopid", "*hdf5", recursive = T) 

# I declare lists for the datasets I import
df_all       = list()
conditions_all     = list()
messages_all = list()


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
      {data_temp$trial = ifelse(data_temp$time < messages[messages$trial_n == i & messages$text == "trial stop", "time"]  & data_temp$time > messages[messages$trial_n == i & messages$text == "trial start", "time"], i, data_temp$trial)}
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
View(df_all[[9]][which(df_all[[9]]$AOI != T & df_all[[9]]$AOI != F),])
AOI = df_all[[i]][which(df_all[[i]]$ID == curr_id & df_all[[i]]$trialstartend == "trial start"),]$rejected
length(AOI[which(AOI == T)])
length(AOI)
df_all[[9]][which(df_all[[i]]$trial!=""),] %>% group_by(AOI) %>% summarise(n = length(AOI)) %>% kable()


#for(i in 1:length(df_all)){fwrite(df_all[[i]], paste0("E:\\data\\data_all", i, "0.csv", sep=""), col.names = T)}



########################################################################
#fwrite(df, "E:\\data\\data_all.csv", col.names = T)

#df_all = list()
for(i in 1:10)
{
  df_all[[i]] = read.csv2(paste0("E:\\data\\data_all", i, "0.csv", sep=""), header = T, sep = ",")
  df_all[[i]] = df_all[[i]][which(df_all[[i]]$rejected == F),]
  #df_all[[i]]$trial = as.factor(df_all[[i]]$trial)
  #df_all[[i]][is.na(df_all[[i]])] = ""
  df_all[[i]]$ID   = as.factor(df_all[[i]]$ID)
  df_all[[i]]$clue = as.factor(df_all[[i]]$clue)
  for(k in 2:16){df_all[[i]][,k] = as.numeric(df_all[[i]][,k])}
  
  for (k in 1:150){df_all[[i]][which( df_all[[i]]$time > (k-1)/50 & df_all[[i]]$time <= k/50 & df_all[[i]]$trial!=""),]$time = k*20}
  
  rows = as.numeric(rownames(df_all[[i]][which(df_all[[i]]$trialstartend == "trial start"),]))
  for(x in rows)
  {
    #cat(paste0(df[x,]$ID, "  ", df[x,]$trial, "\n"))
    df_all[[i]][x,]$left_pupil_measure1  = mean(df_all[[i]][(x-34):(x-1),]$left_pupil_measure1)
    df_all[[i]][x,]$left_pupil_measure2  = mean(df_all[[i]][(x-34):(x-1),]$left_pupil_measure2)
    df_all[[i]][x,]$right_pupil_measure1 = mean(df_all[[i]][(x-34):(x-1),]$right_pupil_measure1)
    df_all[[i]][x,]$right_pupil_measure2 = mean(df_all[[i]][(x-34):(x-1),]$right_pupil_measure2)
  }
  
  for(curr_id in levels(droplevels(df_all[[i]]$ID)))
  {
    cat(paste0(curr_id, "\n"))
    for(curr_trial in as.character(1:84))
    {
      df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure1  = df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure1  - df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure1[1]
      df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure2  = df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure2  - df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$left_pupil_measure2[1]
      df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure1 = df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure1 - df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure1[1]
      df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure2 = df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure2 - df_all[[i]][which(df_all[[i]]$ID==curr_id & df_all[[i]]$trial==curr_trial),]$right_pupil_measure2[1]
    }
  }
}
#for(i in 1:length(df_all)){fwrite(df_all[[i]], paste0("E:\\data\\data_all_200ms_baseline", i, "0.csv", sep=""), col.names = T)}

#mean(df$left_pupil_measure2)

##################################################################################################################
df_all = list()
for(i in 1:10)
{
  df_all[[i]] = read.csv2(paste0("E:\\data\\data_all_200ms_baseline", i, "0.csv", sep=""), header = T, sep = ",")
  df_all[[i]] = df_all[[i]][which(df_all[[i]]$rejected == F),]
  df_all[[i]][is.na(df_all[[i]])] = ""
  df_all[[i]]$ID    = as.factor(df_all[[i]]$ID)
  df_all[[i]]$clue  = as.factor(df_all[[i]]$clue)
  df_all[[i]]$trial = as.factor(df_all[[i]]$trial)
  for(k in 2:16) {df_all[[i]][,k] = as.numeric(df_all[[i]][,k])}
}

#View(df_all[[1]])
df = rbind(df_all[[1]], df_all[[2]], df_all[[3]], df_all[[4]], df_all[[5]],
           df_all[[6]], df_all[[7]], df_all[[8]], df_all[[9]], df_all[[10]])
rm(df_all)
#df = df[which(df$ID != "Sopid_OPG052.hdf5"),]
#df$ID = as.factor(as.character(df$ID))
#df = df[which(df$ID != "Sopid_OPG025.hdf5"),]
#df$ID = as.factor(as.character(df$ID))
#df = df[which(df$ID != "sopit_OSM022.hdf5"),]
#df$ID = as.factor(as.character(df$ID))

prere = read.csv2("E:\\data\\dane_prere2.csv", header = T, sep = ",")
prere[which(prere$participant == "O SM022"),]$participant = "OSM022"
prere$included = T
for(i in 1:nrow(prere))
{
  if(all(grepl(prere$participant[i], unique(df$ID))==F)){prere$included[i] = F}
  #else{}
}

prere = prere[which(prere$included == T),]
#prere = prere[which(prere$participant != "OPG025"),]
prere = prere[order(prere$participant),]
rownames(prere) = seq(length=nrow(prere))
substring(unique(df$ID), 7, 12) == prere$participant
prere = prere[,-c(1, 7, 8)]

#substring(unique(df$ID), 7, 12)

#dane_OA  = df[which(df$clue == "21"),]
#dane_ZZA = df[which(df$clue == "22"),]
#dane_ON  = df[which(df$clue == "23"),]

#wyniki     = data.frame("time" = seq(20, 3000, 20), "right_pupil_measure1" = 0)
wyniki_OA  = data.frame("time" = seq(20, 3000, 20), "right_pupil_measure1" = 0, "clue" = "watch/negative")
wyniki_ZZA = data.frame("time" = seq(20, 3000, 20), "right_pupil_measure1" = 0, "clue" = "change the meaning/negative")
wyniki_ON  = data.frame("time" = seq(20, 3000, 20), "right_pupil_measure1" = 0, "clue" = "watch/neutral")

#for (i in 1:150)
#{
#  wyniki_OA[ which(wyniki_OA$time  == i*20),]$right_pupil_measure1 = mean(c(mean(dane_OA[ which(dane_OA$time  == i*20),]$right_pupil_measure1), mean(dane_OA[ which(dane_OA$time  == i*20),]$left_pupil_measure1)))
#  wyniki_ZZA[which(wyniki_ZZA$time == i*20),]$right_pupil_measure1 = mean(c(mean(dane_ZZA[which(dane_ZZA$time == i*20),]$right_pupil_measure1), mean(dane_ZZA[which(dane_ZZA$time == i*20),]$left_pupil_measure1)))
#  wyniki_ON[ which(wyniki_ON$time  == i*20),]$right_pupil_measure1 = mean(c(mean(dane_ON[ which(dane_ON$time  == i*20),]$right_pupil_measure1), mean(dane_ON[ which(dane_ON$time  == i*20),]$left_pupil_measure1)))
#}
for (i in 1:150)
{
  wyniki_OA[ which(wyniki_OA$time  == i*20),]$right_pupil_measure1 = mean(c(mean(df[which(df$time == i*20 & (df$clue == "21" | df$clue == "24")),]$right_pupil_measure2), mean(df[which(df$time == i*20 & (df$clue == "21" | df$clue == "24")),]$left_pupil_measure2)))
  wyniki_ZZA[which(wyniki_ZZA$time == i*20),]$right_pupil_measure1 = mean(c(mean(df[which(df$time == i*20 & (df$clue == "22" | df$clue == "25")),]$right_pupil_measure2), mean(df[which(df$time == i*20 & (df$clue == "22" | df$clue == "25")),]$left_pupil_measure2)))
  wyniki_ON[ which(wyniki_ON$time  == i*20),]$right_pupil_measure1 = mean(c(mean(df[which(df$time == i*20 & (df$clue == "23" | df$clue == "26")),]$right_pupil_measure2), mean(df[which(df$time == i*20 & (df$clue == "23" | df$clue == "26")),]$left_pupil_measure2)))
}

wyniki = rbind(wyniki_OA, wyniki_ZZA, wyniki_ON, make.row.names=FALSE)
wyniki$time = wyniki$time/1000

ggplot(data = wyniki, aes(x = time, y = right_pupil_measure1, color = clue )) +
  geom_line(size = 1.2) + #, aes(linetype = clue)) +
  #xlab("time [s]") + ylab("pupil diameter change [mm]") +
  labs(x = "\nTime (s)", y = "Pupil Dilation (mm)\n", color = "\nTrial Type")+
  scale_x_continuous(breaks = c(0, 1, 2, 3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(-0.02, 1)) +
  scale_linetype_manual(values=c("solid", "solid", "solid")) +
  scale_color_manual(values=c("#000000", "#FF2828", "#2828FF")) +
  #scale_fill_continuous(limits = c('watch/negative', 'change the meaning/negative', 'watch/neutral')) +
  theme(text = element_text(size = 16, family = "serif"),
        axis.title.x  = element_text(size = 15),
        axis.title.y  = element_text(size = 15),
        axis.text.x   = element_text(size = 15, colour = "#000000"),
        axis.text.y   = element_text(size = 15, colour = "#000000"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.grid.major = element_line(size = 0.6, linetype = 'solid', colour = "#bcbcbc"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#ffffff"))


dane_test = data.frame(
  "id"    = as.factor(rep((levels(droplevels(df$ID))), each = 3)),
  "rucla" = rep(prere$RUCLA, each = 3),
  "lsas"  = rep(prere$LSAS,  each = 3),
  "age"   = rep(prere$AGE,   each = 3),
  "sex"   = rep(prere$SEX,   each = 3),
  "clue"  = as.factor(rep(c(21, 22, 23), length(levels(droplevels(df$ID))))),
  "l_r_pm1" = 0,"rpm1" = 0, "lpm1" = 0, "l_r_pm1_sd" = 0,"rpm1_sd" = 0, "lpm1_sd" = 0)
for(curr_id in levels(droplevels(df$ID)))
{
  cat(paste0(curr_id, "\n"))
  for(curr_clue in as.factor(c(21, 22, 23)))
  {
    if      (curr_clue == "21")
    {clue_number1 = "21"
    clue_number2 = "24"}
    else if (curr_clue == "22")
    {clue_number1 = "22"
    clue_number2 = "25"}
    else if (curr_clue == "23")
    {clue_number1 = "23"
    clue_number2 = "26"}
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1    = mean(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$right_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1    = mean(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$left_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1_sd =   sd(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$right_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1_sd =   sd(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$left_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$l_r_pm1 = mean(c(dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1,
                                                                                              dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1))
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$l_r_pm1_sd = sd(c(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$right_pupil_measure2,
                                                                                               df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time >= 1000),]$left_pupil_measure2))
  }
}
for(curr_id in levels(droplevels(df$ID)))
{
  cat(paste0(curr_id, "\n"))
  for(curr_clue in as.factor(c(21, 22, 23)))
  {
    if      (curr_clue == "21")
    {clue_number1 = "21"
    clue_number2 = "24"}
    else if (curr_clue == "22")
    {clue_number1 = "22"
    clue_number2 = "25"}
    else if (curr_clue == "23")
    {clue_number1 = "23"
    clue_number2 = "26"}
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1    = mean(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$right_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1    = mean(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$left_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1_sd =   sd(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$right_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1_sd =   sd(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$left_pupil_measure2)
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$l_r_pm1 = mean(c(dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$rpm1,
                                                                                              dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$lpm1))
    dane_test[which(dane_test$id == curr_id & dane_test$clue == curr_clue),]$l_r_pm1_sd = sd(c(df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$right_pupil_measure2,
                                                                                               df[which(df$ID == curr_id & (df$clue == clue_number1 | df$clue == clue_number2) & df$time < 1000),]$left_pupil_measure2))
  }
}
#fwrite(dane_test, "E:\\data\\results_pm2_200ms_baseline_before1000ms.csv", col.names = T)
dane_test = read.csv2("E:\\data\\results_pm2_200ms_baseline.csv", header = T, sep = ",")
dane_test$id   = as.factor(dane_test$id)
dane_test$clue = as.character(dane_test$clue)
#dane_test[which(dane_test$clue == "21"),]$clue = "watch/negative"
#dane_test[which(dane_test$clue == "22"),]$clue = "change the meaning/negative"
#dane_test[which(dane_test$clue == "23"),]$clue = "watch/neutral"
dane_test$clue = as.factor(dane_test$clue)
for(k in 7:12){dane_test[,k] = as.numeric(dane_test[,k])}

#ezDesign(data = dane_test, x = rucla, y = l_r_pm1)
#chisq.test(dane_test$rucla, dane_test$sex)
shapiro.test(prere$LSAS)
shapiro.test(prere$RUCLA)

cor.test(prere$LSAS, prere$RUCLA)

ezANOVA(data = dane_test, dv = l_r_pm1, wid = id, within = clue)

model = lmer(l_r_pm1~ clue * rucla + (1|id), data = dane_test)
model = lmer(l_r_pm1~ clue *lsas + (1|id), data = dane_test)
model_parameters(model)
lmr = model_parameters(model)
#fwrite(model_parameters(model), "C:\\Users\\User\\Desktop\\magisterka\\lmr.csv", col.names = T)
mean(prere$RUCLA)
sd(prere$RUCLA)
min(prere$RUCLA)
max(prere$RUCLA)
table(prere$SEX)

t.test(dane_test$l_r_pm1[dane_test$clue=="21"], dane_test$l_r_pm1[dane_test$clue=="22"], paired = T)
t.test(dane_test$l_r_pm1[dane_test$clue=="21"], dane_test$l_r_pm1[dane_test$clue=="23"], paired = T)
t.test(dane_test$l_r_pm1[dane_test$clue=="22"], dane_test$l_r_pm1[dane_test$clue=="23"], paired = T)

pairwise.t.test(dane_test$rpm1, dane_test$clue, p.adjust.method = "holm", paired = T)

pairwise.wilcox.test(dane_test$l_r_pm1, dane_test$clue, p.adjust.method = "holm", paired = T)
?pairwise.wilcox.test()

clue_name = c("21" = " watch\n/negative", "22" = "change the meaning\n/negative", "23" = "  watch\n/neutral")
#dane_test_2 = data.frame("clue" = c(21, 22, 23), "pm" = 0, "psd" = 0, "clue_order" = c(2, 3, 1), "clue_name" = c("watch\n/neutral", "watch\n/negative", "change the meaning\n/negative"))
dane_test_2 = data.frame("clue" = c(" watch\n/negative", "change the meaning\n/negative", "  watch\n/neutral"), "pm" = 0, "psd" = 0)
for(curr_clue in as.factor(c(21, 22, 23)))
{
  dane_test_2[which(dane_test_2$clue == clue_name[as.character(curr_clue)]),]$pm  =      mean(dane_test[which(dane_test$clue == curr_clue),]$l_r_pm1)
  dane_test_2[which(dane_test_2$clue == clue_name[as.character(curr_clue)]),]$psd = std.error(dane_test[which(dane_test$clue == curr_clue),]$l_r_pm1)
}
ggplot(data = dane_test_2) +
  geom_bar(aes(x = clue_name, y = pm), stat="identity", fill = c("#FF4646", "#787878", "#468CFF")) +
  #scale_x_discrete(breaks = c(1,2,3), labels = c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) + xlab("\nTrial Type") + ylab("Pupil Dilation (mm)\n") + 
  geom_pointrange( aes(x = clue_name, y = pm, ymin = pm-psd, ymax=pm+psd), colour="#000000", alpha = 1, size=0.2) +
  geom_errorbar  ( aes(x = clue_name,         ymin = pm-psd, ymax=pm+psd), width=0.2, colour="#000000", alpha = 1, size=1.5) +
  theme(text = element_text(size = 16, family = "serif"),
        axis.title.x  = element_text(size = 16),
        axis.title.y  = element_text(size = 16),
        axis.text.x   = element_text(size = 15, colour = "#000000"),
        axis.text.y   = element_text(size = 15, colour = "#000000"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.grid.major = element_line(size = 0.6, linetype = 'solid', colour = "#bcbcbc"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#ffffff"))
#geom_text(aes(x = dane_wykres_zmiana900$TrialType, y = dane_wykres_zmiana900$SredniaZmiana-dane_wykres_zmiana900$SredniaZmianaSD-0.03, label = c("-0,0614\nSD = 0,1079", "-0,0843\nSD = 0,1042", "-0,0727\nSD = 0,0906", "-0,1037\nSD = 0,1099")), size = 3.5)

ggplot(data = prere) + geom_point(aes(x = RUCLA, y = LSAS))

#chisq.test()
#x = c(5.8, 6.42, 5.67, 8.25, 5, 6.42)
#y = c(6.42, 7.75, 7, 7.75, 5.91, 6.83)
#t.test(x,y)
#############################################################################
#############################################################################
#############                     GAZE TYPE                      ############
#############################################################################
#############################################################################


df_all = list()
for(i in 1:10)
{
  df_all[[i]] = read.csv2(paste0("E:\\data\\data_all", i, "0.csv", sep=""), header = T, sep = ",")
  df_all[[i]] = df_all[[i]][which(df_all[[i]]$rejected == F),]
  df_all[[i]][is.na(df_all[[i]])] = ""
  df_all[[i]]$ID    = as.factor(df_all[[i]]$ID)
  df_all[[i]]$clue  = as.factor(df_all[[i]]$clue)
  df_all[[i]]$trial = as.factor(df_all[[i]]$trial)
  for(k in 2:16) {df_all[[i]][,k] = as.numeric(df_all[[i]][,k])}
}


#View(df_all[[1]])

#fwrite(df, "E:\\data\\data_mov.csv", col.names = T)
df = rbind(df_all[[1]], df_all[[2]], df_all[[3]], df_all[[4]], df_all[[5]],
           df_all[[6]], df_all[[7]], df_all[[8]], df_all[[9]], df_all[[10]])
rm(df_all)
df = df[which(df$trial != ""),]
rownames(df) = seq(length=nrow(df))
df$time_resampled = 0
df$left_gaze_x_resampled  = 0
df$left_gaze_y_resampled  = 0
df$right_gaze_x_resampled = 0
df$right_gaze_y_resampled = 0

for (k in 1:150){df[which( df$time > (k-1)/50 & df$time <= k/50 & df$trial!=""),]$time_resampled = k*20}

for(curr_id in unique(df$ID))
{
  for(curr_trial in as.factor(c(1:84)))
  {
    #cat(paste0(curr_id," ", curr_trial, "\n"))
    df_tmp = df[which(df$ID==curr_id & df$trial==curr_trial),]
    if(length(df_tmp$time)==0){next}
    cat(paste0(curr_id," ", curr_trial, "\n"))
    for (k in seq(20, 3000, 20))
    {
      if(length(df_tmp[which(df_tmp$time_resampled==k),]$time)==0){next}
      x = df_tmp[which(df_tmp$time_resampled==k),]
      if(length(x[which(x$AOI == T),])>=(length(x[which(x$AOI == F),]))){x$AOI = T}
      else{x$AOI = F}
      x$left_gaze_x_resampled  = mean(x$left_gaze_x)
      x$left_gaze_y_resampled  = mean(x$left_gaze_y)
      x$right_gaze_x_resampled = mean(x$right_gaze_x)
      x$right_gaze_y_resampled = mean(x$right_gaze_y)
      
      #x = x[1,]
      df_tmp[which(df_tmp$time_resampled==k),] = x
    }
  }
  df[which(df$ID==curr_id & df$trial==curr_trial),] = df_tmp
}
df[which(df$ID==unique(df$ID)[1] & df$trial== "1" & df$time_resampled==40),] = x

#fwrite(df, "E:\\data\\data_resampled_geaze_type.csv", col.names = T)

#df = df[which(df$rejected == F),]
#for (k in seq(20, 3000, 20)){cat(paste0(k, "\n"))}

df$AOI_resampled = F
for(curr_id in unique(df$ID))
{
  for(curr_trial in as.factor(c(1:84)))
  {
    #cat(paste0(curr_id," ", curr_trial, "\n"))
    df_tmp = df[which(df$ID==curr_id & df$trial==curr_trial),]
    if(length(df_tmp$time)==0){next}
    cat(paste0(curr_id," ", curr_trial, "\n"))
    for (k in seq(20, 3000, 20))
    {
      if(length(df_tmp[which(df_tmp$time_resampled==k),]$time)==0){next}
      x = df_tmp[which(df_tmp$time_resampled==k),]
      if      (length(x[which(x$AOI == T),][,22])>=(length(x[which(x$AOI == F),][,22]))){x$AOI_resampled = T}
      else if (length(x[which(x$AOI == T),][,22]) <(length(x[which(x$AOI == F),][,22]))){x$AOI_resampled = F}
      df_tmp[which(df_tmp$time_resampled==k),]$AOI_resampled = x$AOI_resampled
      
    }
    df[which(df$ID==curr_id & df$trial==curr_trial),]$AOI_resampled = df_tmp$AOI_resampled
  }
  
}



df = read.csv2("E:\\data\\data_resampled_geaze_type.csv", header = T, sep = ",")
for(k in c(2:16, 23:27)) {df[,k] = as.numeric(df[,k])}


df$time_interval_resampled = df$time_resampled - c(0, df[1:(nrow(df)-1),]$time_resampled) #29
df = df[which(df$time_interval_resampled == 20),]


df$left_x_change_resampled  = (180/pi)*atan((df$left_gaze_x_resampled  - c(0, df[1:(nrow(df)-1),]$left_gaze_x_resampled))*(543/1920)/650)  #30
df$left_y_change_resampled  = (180/pi)*atan((df$left_gaze_y_resampled  - c(0, df[1:(nrow(df)-1),]$left_gaze_y_resampled))*(300/1080)/650)  #31
df$right_x_change_resampled = (180/pi)*atan((df$right_gaze_x_resampled - c(0, df[1:(nrow(df)-1),]$right_gaze_x_resampled))*(543/1920)/650) #32
df$right_y_change_resampled = (180/pi)*atan((df$right_gaze_y_resampled - c(0, df[1:(nrow(df)-1),]$right_gaze_y_resampled))*(300/1080)/650) #33
df$left_x_velocity_resampled  = 1000*df$left_x_change_resampled/df$time_interval_resampled  #34
df$left_y_velocity_resampled  = 1000*df$left_y_change_resampled/df$time_interval_resampled  #35
df$right_x_velocity_resampled = 1000*df$right_x_change_resampled/df$time_interval_resampled #36
df$right_y_velocity_resampled = 1000*df$right_y_change_resampled/df$time_interval_resampled #37
df$left_x_velocity_change_resampled  = df$left_x_velocity_resampled  - c(0, df[1:(nrow(df)-1),]$left_x_velocity_resampled)  #38
df$left_y_velocity_change_resampled  = df$left_y_velocity_resampled  - c(0, df[1:(nrow(df)-1),]$left_y_velocity_resampled)  #39
df$right_x_velocity_change_resampled = df$right_x_velocity_resampled - c(0, df[1:(nrow(df)-1),]$right_x_velocity_resampled) #40
df$right_y_velocity_change_resampled = df$right_y_velocity_resampled - c(0, df[1:(nrow(df)-1),]$right_y_velocity_resampled) #41
df$left_x_acceleration_resampled  = 1000*df$left_x_velocity_change_resampled/df$time_interval_resampled  #42
df$left_y_acceleration_resampled  = 1000*df$left_y_velocity_change_resampled/df$time_interval_resampled  #43
df$right_x_acceleration_resampled = 1000*df$right_x_velocity_change_resampled/df$time_interval_resampled #44
df$right_y_acceleration_resampled = 1000*df$right_y_velocity_change_resampled/df$time_interval_resampled #45
df$left_change_resampled        = sqrt(df$left_x_change_resampled**2  + df$left_y_change_resampled**2)  #46
df$right_change_resampled       = sqrt(df$right_x_change_resampled**2 + df$right_y_change_resampled**2) #47
df$left_velocity_resampled      = sqrt(df$left_x_velocity_resampled**2  + df$left_y_velocity_resampled**2)  #48
df$right_velocity_resampled     = sqrt(df$right_x_velocity_resampled**2 + df$right_y_velocity_resampled**2) #49
df$left_acceleration_resampled  = sqrt(df$left_x_acceleration_resampled**2  + df$left_y_acceleration_resampled**2)  #50
df$right_acceleration_resampled = sqrt(df$right_x_acceleration_resampled**2 + df$right_y_acceleration_resampled**2) #51

df$change_resampled       = (df$left_change_resampled       + df$right_change_resampled)/2
df$velocity_resampled     = (df$left_velocity_resampled     + df$right_velocity_resampled)/2
df$acceleration_resampled = (df$left_acceleration_resampled + df$right_acceleration_resampled)/2
df = df[,-c(30:51)]


ggplot(data = df[which(df$ID == unique(df$ID)[15] & df$trial == "44"),], aes(x = time_resampled, y = velocity_resampled, color = ID )) +
  geom_line() + #xlab("time [ms]") + ylab("pupil diameter change [px]") +
  #scale_color_manual(values=c("red", "#FF9A15", "black"))+
  theme(plot.background = element_rect(fill = "#ffffff", color = "#ffffff"), panel.background = element_rect(fill = "#DCECF9", color = "#DCECF9"))

ggplot(data = df[which(df$ID == unique(df$ID)[15] & df$trial == "44"),], aes(x = time, y = velocity_resampled, color = AOI_resampled )) +
  geom_line() + #xlab("time [ms]") + ylab("pupil diameter change [px]") +
  #scale_color_manual(values=c("red", "#FF9A15", "black"))+
  theme(plot.background = element_rect(fill = "#ffffff", color = "#ffffff"), panel.background = element_rect(fill = "#DCECF9", color = "#DCECF9"))

View(df[which(df$ID == unique(df$ID)[15] & df$trial == "44"),])
#df = df_resampled
#rm(df_resampled)
#df = df[,-c(5:16, 21, 22)]
df$gaze_type = ""
#df[which(df$velocity_resampled <  30),]$gaze_type = "fixation"
#df[which(df$velocity_resampled >= 300) ,]$gaze_type = "saccade"
df[which(df$velocity >= 1000),]$gaze_type       = "noise"
df[which(df$acceleration >= 100000),]$gaze_type = "noise"
df[which(df$AOI_resampled == F),]$gaze_type     = "noise"


#par(bg = 'white')
#hist(df[which(df$gaze_type == "end fixation" & df$gaze_type_length>0.025 & df$clue == "ZMIEÅ\u0083 ZNACZENIE"),]$gaze_type_length,
#     breaks = seq(0.025, 3.1, 0.025), col = "gray", border = "black")
#ggplot(data = wyniki2, aes(x = time, y = right_pupil_measure1, color = clue )) +
#  geom_line() +
#  scale_color_manual(values=c("black", "red"))

PTx = 100
PTy = 0
while (abs(PTy - PTx)>1)
{
  PTy = PTx
  #mean(df[which(df$gaze_type != "noise" & df$velocity <= PTx),]$velocity)
  #mean(df[which(df$gaze_type != "noise" & df$velocity <= PTx),]$velocity) + 6*sd(df[which(df$gaze_type != "noise" & df$velocity <= PTx),]$velocity)
  PTx = mean(df[which(df$gaze_type != "noise" & df$velocity <= PTx),]$velocity) + 6*sd(df[which(df$gaze_type != "noise" & df$velocity <= PTx),]$velocity)
  
}
df[which(df$gaze_type != "noise" & df$velocity_resampled >= 30 & df$acceleration_resampled >= 8000) ,]$gaze_type = "saccade"
length(df[which(df$gaze_type == "saccade"),]$gaze_type)


df = df[which(df$trialstartend != "trial start"),]
for(curr_id in unique(df$ID))
{
  print(table(as.factor(df[which(df$ID==curr_id),]$clue)))
}


stimuli = dir("E:/data/Paradygmaty/cognitive_reappraisal_SOPIT", "*mp4")
stimuli = data.frame("aggresive" = c('r', 'l', 'b', 'b', 'r', 'l', 'b', 'b', 'b', 'b', 'r', 'l',
                                     'r', 'l', 'n', 'n', 'n', 'n', 'n', 'n', 'n', 'n', 'n', 'n',
                                     'n', 'n', 'n', 'n', 'b', 'b', 'b', 'b', 'r', 'l', 'b', 'b',
                                     'b', 'b', 'b', 'b', 'r', 'l', 'n', 'n', 'n', 'n', 'n', 'n',
                                     'n', 'n', 'n', 'n', 'n', 'n', 'n', 'n'),"name" = stimuli)

