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