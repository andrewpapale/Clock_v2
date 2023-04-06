# 2023-03-10 AndyP
# Clock_2_Pilot_Post_Checks.R
# This code loads in a csv from Clock 2.0 and checks for good data and outputs the bonus payment
clock_2_pilot_post_checks <- function(clock_dir='~/Inquisit Code/EEG_clock/Clock_v2/data/pilot_data'){
  
  files <- list.files(path=clock_dir,pattern=glob2rx('*clock_raw*.csv$'))
  nF <- length(files)
  
  if (nF==0){
    message('no clock_raw csv files found in directory')
  } else {
    
    summary <- NULL
    for (iF in 1:nF){
      temp_str <- str_split(files[iF],'_')
      subj_id <- temp_str[[1]][4]
      if (length(strfind(subj_id,'.csv'))==1){
        temp_str <- str_split(subj_id,'[.]')
        subj_id <- temp_str[[1]][1]
      }
      
      
      df <- read_csv(paste0(clock_dir,'/',files[iF]))
      
      check_latency_df <- df %>% filter(blockcode=='experiment' & (trialcode=='experiment_noU' | trialcode=='experiment_U'))
      n_timeout <- sum(check_latency_df$latency == 7500) # timeout is 7.5s
      n_too_fast <- sum(check_latency_df$latency  < 500) # 500ms is pretty fast and often doesn't lead to the best choice
      n_blocks <- max(df$blocknum)
      if (n_blocks==11){
        complete = TRUE
      }
      
      bonus <- max(df$totalEarnings)/100
      
      temp_sum <- data.frame(subj_id=subj_id,n_timeout=n_timeout,n_too_fast=n_too_fast,complete=complete,bonus=bonus)
      if (length(summary)==0){
        summary <- temp_sum
      } else {
        summary <- rbind(summary,temp_sum)
      }
    }
    
  }
  return(summary)
}

##############################
#Data stored in sharepoint under /skinner/data/prolific/clock_v2_pilot/no_contingencies_March_2023/

## Read in raw data and preprocess subjects, create summary file to check basic stats and performance, identify participants to approve on Prolific, and calculate bonus payments

library(dplyr)
library(stringr)

#path to skinner
#change onedrive_path for local skinner location
onedrive_path='/Users/laurataglioni/University of Pittsburgh/DNPLskinner - Documents/skinner/'
data_path=paste0(onedrive_path,'data/prolific/clock_v2_pilot/no_contingencies_04-05-2023/')

#read in prolific export file
export_file=read.csv(paste0(data_path, 'additional/prolific_export/prolific_export_642dc8c1ffd57036424e0ed2.csv')) #add demographic file from prolific once data is collected
prolific_IDs=export_file$Participant.id

#clean names and create variables to identify participants who were particularly slow (over 40 min) or did not get completion code to check on later
prolific_data <- export_file %>% 
  select(Participant.id, Submission.id, Status, Started.at, Completed.at, 
         Time.taken, Completion.code, Total.approvals)
colnames(prolific_data) <- c("prolific_id", "submission_id", "status", 
                             "date_time_started", "date_time_completed", "time_taken", 
                             "completion_code", "total_approvals")
prolific_data$time_taken_min <- prolific_data$time_taken/60
prolific_data$code <- ifelse(prolific_data$completion_code=="NOCODE", "NO CODE", "ok")
prolific_data$slow <- ifelse(prolific_data$time_taken_min>40, ">40 MIN", "ok")
prolific_data <- prolific_data %>% select(prolific_id, code, slow, everything())

#preprocess new raw behavioral data
subjs_raw_done=c(dir(paste0(data_path,'processed')))
raw_todo_files=dir(paste0(data_path,'raw/'))
for (b in 1:length(raw_todo_files)) {
  in_behav=read.csv(paste0(data_path,'raw/', raw_todo_files[b]), na.strings=c("","NA"))
  in_subjs=unique(in_behav$subject)
  new_subjs=in_subjs[!(in_subjs %in% subjs_raw_done)]
  if (length(new_subjs)>0) {
    for (n in 1:length(new_subjs)) {
      subjID=new_subjs[n]
      all_subj_data=in_behav[in_behav$subject==subjID,]
      dir.create(paste0(data_path,'processed/',subjID))
      subj_prac=all_subj_data[all_subj_data$blockcode=='instructions',]
      write.csv(subj_prac,file=paste0(data_path,'processed/',subjID,
                                      '/practice_',subjID,'.csv'),row.names=FALSE, na="")
      subj_task=all_subj_data[all_subj_data$blockcode=='experiment',]
      write.csv(subj_task,file=paste0(data_path,'processed/',subjID,
                                      '/clock_pilot_',subjID,'.csv'), row.names=FALSE, na="")
      subj_endscreen=all_subj_data[all_subj_data$blockcode=='endscreen',]
      write.csv(subj_endscreen,file=paste0(data_path,'processed/',subjID,
                                          '/end_trial_',subjID,'.csv'), row.names=FALSE, na="")
    }
  }
}

#merge data for later analysis/more preproc + extract and calculate needed variables for prolific approvals and bonus payments
processed_subjects=dir(paste0(data_path,'processed'))
processed_subjects=processed_subjects[!(grepl('all_processed',processed_subjects,
                                              fixed=TRUE))]
rm(all_data)
for (f in 1:length(processed_subjects)) {
  task_file=dir(paste0(data_path,'processed/',processed_subjects[f]),fixed('clock_pilot'))
  main_data_all=read.csv(paste0(data_path,'processed/',processed_subjects[f],'/',task_file),
                         header=T)
  if (dim(main_data_all)[1]>0) {
    main_data=dplyr::select(main_data_all,c("subject","date", "blocknum","trialcode",
                                            "latency","scrfunc","mag","freq","ev",
                                            "inc","rng","fogPos1","fogPos2",
                                            "chooseUncertainty1","chooseUncertainty2", 
                                            "uncertaintyCount1","uncertaintyCount2", 
                                            "blockCount","ntrials","trialCount", 
                                            "uncertaintyStart1", "uncertaintyStart2", 
                                            "uncertainty_block", "rt_shifted", 
                                            "totalPoints", "totalEarnings", "Earnings"))
    if (exists("all_data")) {
      all_data=rbind(all_data,main_data)
    } else {
      all_data=main_data
    }
  }
  main_data$timeout <- ifelse(main_data$latency == 6500, 1, 0) #changed this from 7500 to 6500 because it looks like the trial start time is set to 1000 in task code so latency of 6500 would indicate that the subject reached the 7500 timeout before submitting a response on that trial
  main_data$n_timeout=sum(main_data$timeout==1)
  main_data$too_fast <- ifelse(main_data$latency < 500, 1, 0) #not sure how well this one would work out
  main_data$n_too_fast=sum(main_data$too_fast==1)
  tails <- main_data %>% tail(1) %>% select(subject, date, blocknum, totalPoints, 
                                            totalEarnings, n_timeout, n_too_fast)
  tails$approve <- ifelse(tails$blocknum==9, "Yes", "NO: DID NOT COMPLETE 9 BLOCKS")
  tails$bonus <- ifelse(tails$approve=="Yes", max(tails$totalEarnings)/200, 0) #100 seems pretty high for bonus payments...? - change to 1000? or 500? pts already receive an hourly rate so bonus payments are typically $1-$5
  tails$bonus <- round(tails$bonus, digits=2)
  colnames(tails) <- c("subject", "date", "blocks_completed", "points", 
                       "earnings", "total_timeouts", "fast_trials", 
                       "approve", "bonus")
  if (exists("processed_sub_info")) {
    processed_sub_info=full_join(processed_sub_info, tails)
  } else {
    processed_sub_info=tails
  }
}

names(processed_sub_info )[names(processed_sub_info ) == 'subject'] <- 'prolific_id'
processed_sub_info <- full_join(processed_sub_info, prolific_data, by='prolific_id')
processed_sub_info <- processed_sub_info %>% select(-c(date, status, date_time_started, date_time_completed, completion_code, total_approvals, time_taken))
processed_sub_info <- processed_sub_info %>% select(prolific_id, blocks_completed, points, earnings, time_taken_min, total_timeouts, fast_trials, code, slow, submission_id, approve, bonus)

write.csv(all_data,file=paste0(data_path,'processed/all_processed_data_',
                               Sys.Date(),'.csv'), row.names = F)
write.csv(processed_sub_info,file=paste0(data_path,'additional/processed_subject_info_',
                                Sys.Date(),'.csv'), row.names = F)


#get bonus formatting for bulk payment on prolific
approve_subjects <- processed_sub_info %>% filter(approve=="Yes")
approve_subjects$bonuses <- paste(approve_subjects$prolific_id, approve_subjects$bonus,
                                    sep = ", ")
bonuses <- approve_subjects$bonuses
write.table(bonuses, file = paste0(data_path, 'additional/bonuses_0.25cents.txt'), sep = "\t", row.names = F, 
            col.names = F, quote = FALSE)

