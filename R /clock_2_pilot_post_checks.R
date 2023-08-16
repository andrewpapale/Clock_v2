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
#Data stored in sharepoint under /skinner/data/prolific/clock_v2_pilot/

## Read in raw data and preprocess subjects, create summary file to check basic stats and performance, identify participants to approve on Prolific, and calculate bonus payments

library(dplyr)
library(stringr)
library(psych)
library(chemometrics)

#path to skinner
#change onedrive_path for local skinner location
#onedrive_path='/Users/andypapale/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Documents - DNPLskinner/skinner/'
onedrive_path='/Users/laurataglioni/University of Pittsburgh/DNPLskinner - Documents/skinner/'
data_path=paste0(onedrive_path,'data/prolific/clock_v2_pilot/pilot_v3_03_07-31-23/')

#read in prolific export file
export_file=read.csv(paste0(data_path, 'additional/prolific_export/prolific_export_64c538de486f9bccff1386f2.csv')) #add demographic file from prolific once data is collected
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
prolific_data$slow <- ifelse(prolific_data$time_taken_min>55, ">55 MIN", "ok")
prolific_data <- prolific_data %>% select(prolific_id, code, slow, everything())
prolific_data <- prolific_data %>% select(-c(code))

#preprocess new raw behavioral data
subjs_raw_done=c(dir(paste0(data_path,'processed')))
completed_prolific=export_file$Participant.id
raw_todo_files=dir(paste0(data_path,'raw/'))
for (b in 1:length(raw_todo_files)) {
  in_behav=read.csv(paste0(data_path,'raw/', raw_todo_files[b]), na.strings=c("","NA"))
  in_subjs=unique(in_behav$subject)
  new_subjs=in_subjs[!(in_subjs %in% subjs_raw_done) & (in_subjs %in% completed_prolific)]
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
    main_data=dplyr::select(main_data_all,c("subject","date", "group", "sessionid", "computer.platform", "build", "blockcode","blocknum","trialcode","latency","startPos","scrfunc","rtvspos","randStart", "mag","freq","ev","inc","rng","fogPos1","fogPos_out","windPos2", "windPos_out", "chooseFog1", "chooseWind2", "choose_unc_att1", "choose_unc_att_out1", "choose_unc_att2", "choose_unc_att_out2", "unc_att_count1", "unc_att_count_out1", "unc_att_count2", "unc_att_count_out2", "chooseAttentionalControl", "chooseUncertainty", "blockCount","ntrials","trialCount", "meta_trialCount", "attentional_control", "local_uncertainty", "unc_att_start1", "unc_att_start2", "unc_att_block", "list.rt_0.currentindex", "rt_shifted", "pos_shifted", "totalPoints", "totalEarnings", "Earnings"))
    if (exists("all_data")) {
      all_data=rbind(all_data,main_data)
    } else {
      all_data=main_data
    }
  }
}

#calculate "cutoff" point for fast responses and for bonuses
#latency first
fast_latency <- mean(all_data$latency, trim = 0.1)-(sd_trim(all_data$latency, trim = 0.1, const=F)*1.75)
mean(all_data$latency, trim = 0.1)
sd_trim(all_data$latency, trim = 0.1, const=F)
sd_trim(all_data$latency, trim = 0.1, const=F)*1.5
#closer look at latency
dev.off()
ggplot(all_data, aes(x=latency)) + geom_histogram()
describe(all_data$latency)
median(all_data$latency, trim = 0.2)
mean(all_data$latency)-sd(all_data$latency)
mean(all_data$latency, trim = 0.1)
mean(all_data$latency)
sd(all_data$latency)

#only assign bonuses to participants who met basic check of earnings greater than mean minus 2.5 standard deviations of total earnings in sample
all_tails <- all_data %>% group_by(subject) %>% slice(tail(row_number(), 1))
bonus_cutoff <- mean(all_tails$totalEarnings)-(2.5*sd(all_tails$totalEarnings))
#for checking (also look at fast trials and slow runs in processed sub_info for these subjects)
describe(all_tails$totalEarnings)
ggplot(all_tails, aes(x=totalEarnings)) + geom_histogram()
sum(all_tails$totalEarnings<bonus_cutoff)
no_bonus_subs <- all_tails %>% filter(totalEarnings<bonus_cutoff)

for (f in 1:length(processed_subjects)) {
  task_file=dir(paste0(data_path,'processed/',processed_subjects[f]),fixed('clock_pilot'))
  main_data_all=read.csv(paste0(data_path,'processed/',processed_subjects[f],'/',task_file),
                         header=T)
  if (dim(main_data_all)[1]>0) {
    main_data=dplyr::select(main_data_all,c("subject","date", "group", "sessionid", "computer.platform", "build", "blockcode","blocknum","trialcode","latency","startPos","scrfunc","rtvspos","randStart", "mag","freq","ev","inc","rng","fogPos1","fogPos_out","windPos2", "windPos_out", "chooseFog1", "chooseWind2", "choose_unc_att1", "choose_unc_att_out1", "choose_unc_att2", "choose_unc_att_out2", "unc_att_count1", "unc_att_count_out1", "unc_att_count2", "unc_att_count_out2", "chooseAttentionalControl", "chooseUncertainty", "blockCount","ntrials","trialCount", "meta_trialCount", "attentional_control", "local_uncertainty", "unc_att_start1", "unc_att_start2", "unc_att_block", "list.rt_0.currentindex", "rt_shifted", "pos_shifted", "totalPoints", "totalEarnings", "Earnings"))
  main_data$timeout <- ifelse(main_data$latency == 6500, 1, 0) #changed this from 7500 to 6500 because it looks like the trial start time is set to 1000 in task code so latency of 6500 would indicate that the subject reached the 7500 timeout before submitting a response on that trial
  main_data$n_timeout=sum(main_data$timeout==1)
  main_data$too_fast <- ifelse(main_data$latency <= fast_latency, 1, 0) 
  main_data$n_too_fast=sum(main_data$too_fast==1)
  main_data$totalEarnings=as.numeric(main_data$totalEarnings)
  tails <- main_data %>% tail(1) %>% select(subject, date, group, sessionid, blocknum, attentional_control, local_uncertainty, list.rt_0.currentindex, totalPoints, ntrials, totalEarnings, n_timeout, n_too_fast)
  tails$task <- ifelse(tails$blocknum==9, "Yes", "NO: DID NOT COMPLETE 8 BLOCKS")
  tails$bonus <- ifelse(tails$task=="Yes" & tails$totalEarnings > bonus_cutoff, max(tails$totalEarnings)/200, NA) #100 seems pretty high for bonus payments...? - change to 1000? or 500? pts already receive an hourly rate so bonus payments are typically $1-$5
  tails$bonus <- round(tails$bonus, digits=2)
  colnames(tails) <- c("subject", "date", "group", "sessionid", "blocks_n", "attentional_control", "local_uncertainty", "list.rt_0", "points", "ntrials", "tot_earnings", "n_timeouts", "n_fast_trials", "task", "bonus")
  if (exists("processed_sub_info")) {
    processed_sub_info=full_join(processed_sub_info, tails)
  } else {
    processed_sub_info=tails
  }
  }
}

names(processed_sub_info )[names(processed_sub_info ) == 'subject'] <- 'prolific_id'
prolific_data <- prolific_data %>% filter(prolific_id %in% processed_subjects)
prolific_summary <- left_join(processed_sub_info, prolific_data, by='prolific_id')
prolific_summary <- prolific_summary %>% select(prolific_id, date, blocks_n, tot_earnings, time_taken_min, n_timeouts, n_fast_trials, slow, task, bonus) 

#check if questionnaires were completed
pid_raw=read.csv(paste0(data_path, 'other_data/questionnaires/pid5/raw_data/papalea_prosper_pid5_survey_pid5_2308022125.csv'), row.names = NULL)
ipip_raw=read.csv(paste0(data_path, 'other_data/questionnaires/ipip_120/raw_data/papalea_prosper_ipip_120_survey_ipip_neo_120_2307282140.csv'))
pid_id <- pid_raw$subject
ipip_id <- ipip_raw$subject

setdiff(prolific_summary$prolific_id, pid_raw$subject)

prolific_summary$pid <- ifelse(prolific_summary$prolific_id %in% pid_id, "Yes", "MISSING")
prolific_summary$ipip <- ifelse(prolific_summary$prolific_id %in% ipip_id, "Yes", "MISSING")
prolific_summary$tot_earnings <- as.numeric(prolific_summary$tot_earnings)
prolific_summary$earnings_compared <- ifelse(prolific_summary$tot_earnings<(mean(prolific_summary$tot_earnings) - sd(prolific_summary$tot_earnings)), paste0("less than tot_earnings mean - 2 SD (mean =", mean(prolific_summary$tot_earnings), "1 SD = ", sd(prolific_summary$tot_earnings), ")"), "> mean - 2sd")

pid_scored=read.csv(paste0(data_path, 'other_data/questionnaires/pid5/scored/papalea_prosper_pid5_summary_2307282145.csv'))
ipip_scored=read.csv(paste0(data_path, 'other_data/questionnaires/ipip_120/scored/papalea_prosper_ipip_120_summary_2307282140.csv'))

pid_domains <- pid_scored %>% select(subjectid, pid5_neg_aff, pid5_detach, pid5_antag, pid5_disinhib, pid5_psycho)
ipip_domains <- ipip_scored %>% select(subjectid, ipipneo120_a, ipipneo120_c, ipipneo120_e, ipipneo120_o, ipipneo120_n)
domains_combined <- full_join(pid_domains, ipip_domains, by="subjectid") %>% select(subjectid, ipipneo120_a, pid5_antag, ipipneo120_c, pid5_disinhib, ipipneo120_e, pid5_detach, ipipneo120_o, pid5_psycho, ipipneo120_n, pid5_neg_aff)

prolific_summary$approve <- ifelse(prolific_summary$task=="Yes", "Yes", "No")
prolific_summary$bonus <- ifelse(prolific_summary$pid=="MISSING" & prolific_summary$ipip=="MISSING", NA, prolific_summary$bonus)

exclude_bonus <- c("63627698dc63a02b1783c8e9")

#upload to sharepoint
write.csv(all_data,file=paste0(data_path,'processed/all_processed_data_',
                               Sys.Date(),'.csv'), row.names = F)
write.csv(prolific_summary,file=paste0(data_path,'additional/processed_subject_info_',
                                Sys.Date(),'.csv'), row.names = F)

#get bonus formatting for bulk payment on prolific
approve_subjects <- prolific_summary %>% filter(approve=="Yes")
bonus_subjects <- prolific_summary %>% filter(!is.na(bonus)) %>% filter(approve=="Yes") %>% filter(!prolific_id %in% exclude_bonus)
bonus_subjects$bonuses <- paste(bonus_subjects$prolific_id, bonus_subjects$bonus,
                                    sep = ", ")
bonuses <- bonus_subjects$bonuses
approvals <- approve_subjects$prolific_id

write.table(bonuses, file = paste0(data_path, 'admin/bonuses.txt'), sep = "\t", row.names = F, col.names = F, quote = FALSE)
write.table(approvals, file = paste0(data_path, 'admin/approvals.txt'), sep = ", ", row.names = F, col.names = F, quote = FALSE)


consent <- read.csv(paste0(data_path, 'other_data/consent/papalea_prosper_instruct_consent_raw_2307032055.csv'))
consent[duplicated(consent$subject), ]
consent$subject[duplicated(consent$subject) | duplicated(consent$subject, fromLast=TRUE)]
n_occur[n_occur$Freq > 1,]
