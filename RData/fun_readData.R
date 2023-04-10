###################################################
###################################################
#####          Functions for loading the data
###################################################
###########################   by Adriana F. Chavez

library(tidyverse)
library(readr)
library(data.table)
library(osfr)

###################################################
##### Load data from Rey-Mermet et al. (2018)
###################################################

loadData.ReyMermet <- function(existingFolderForData){
        # Look for existing file
        test <- ("reymermet_mergeddata.txt" %in% dir(existingFolderForData))
        # Download data if the file does not exist
            if(!test){
                OSF.project <- osf_retrieve_node("https://osf.io/fjytw/")
                OSF.files <- osf_ls_files(OSF.project)
                mergedData <- which(OSF.files=="mergeddata.txt")
                osf_download(OSF.files[mergedData,],path=existingFolderForData)
            }
        # Load data from file
        dataLocation <- paste(existingFolderForData,"reymermet_mergeddata.txt",sep="")
        data <- read.table(dataLocation,header = TRUE)
        
        #####################
        # Data cleaning
        
        # Remove uninformative columns
        data <- data[,-c(2,4:9,14)]
        
        # Remove "Antisaccade" and "Stop-signal" tasks
        remove.Tasks <- c("antisaccade","stopsignal")
        remove.Tasks <- which(data$task %in% remove.Tasks)
        data <- data[-remove.Tasks,]
        # Remove any record of these tasks in data set
        data$task <- as.character(data$task)
        
        # Relabel the two conditions on the n2repetition cost task
        data$congruency[which(data$congruency=="n2switch")] <- "congruent"  #Expected to be faster
        data$congruency[which(data$congruency=="n2repetition")] <- "incongruent"  #Expected to be slower
        # Completely remove any record of these tasks ever existing
        data$congruency <- as.character(data$congruency)
        
        # Remove participants identified as "bad observations" by authors
        bad.part <- c(132,164,368,402,429)
        bad.rows <- which(data$subj %in% bad.part)
            if(length(bad.rows)>0){
               data <- data[-bad.rows,]
            }
        
        # Remove trials where a "correct" value of 99 was recorded
        good.corr <- data$corr %in% c(0,1)
        bad.corr <- !good.corr
            if(sum(bad.corr)>0){
               data <- data[-which(bad.corr),]
            }
        
        # Express RT in seconds
        data$rt <- data$rt*0.001
        
  return(data)
}


###################################################
#####     Load data from Enkavi & Poldrack (2019)
###################################################

loadData.EnkaviPoldrack <- function(existingFolderForData){
        # Look for existing file
        folderRawData <- paste(existingFolderForData,"enkaviPoldrack_rawFiles", sep="")
        datosFile <- paste(existingFolderForData,"enkavi_mergeddata.csv",sep="")
        test <- file.exists(datosFile)
        
        # If file does not exist, generate it
        if(!test){
              # 1. Create folder for saving raw data files
          
              cmd = paste("mkdir ", folderRawData, sep="")   
              system(cmd)
              setwd(folderRawData)
              
              # 2. Download each task dataset from Github
              # Github repo
              gitLink <- "https://github.com/IanEisenberg/Self_Regulation_Ontology/raw/master/Data/Complete_02-16-2019/Individual_Measures/"
              # Files to download
              tasks <- c("adaptive_n_back.csv.gz","attention_network_task.csv.gz","simon.csv.gz",
                          "choice_reaction_time.csv.gz","directed_forgetting.csv.gz","stroop.csv.gz",
                          "dot_pattern_expectancy.csv.gz","go_nogo.csv.gz","shift_task.csv.gz",
                          "local_global_letter.csv.gz","psychological_refractory_period_two_choices.csv.gz",
                          "probabilistic_selection.csv.gz","recent_probes.csv.gz","threebytwo.csv.gz",
                          "shape_matching.csv.gz","simple_reaction_time.csv.gz","two_stage_decision.csv.gz")
              # Download
                    for(t in tasks){
                          taskLink <- paste(gitLink,t,sep="")
                          cmd = paste("wget -c ", taskLink, sep="")
                          system(cmd)
                    }
              
              # 3. We merge together the tasks with only simple RT (no difference score across conditions)
        
                  # 3.a. We start with the tasks where accuracy was measured
                      tasks <- c("adaptive_n_back.csv.gz","choice_reaction_time.csv.gz","shift_task.csv.gz",
                                 "dot_pattern_expectancy.csv.gz","probabilistic_selection.csv.gz","threebytwo.csv.gz")
                      dat_RT_Corr <- NULL
                      keep.RTcols <- c("correct","exp_stage","experiment_exp_id","rt", "worker_id")
                            for(t in tasks){
                                  data = NULL
                                  data <- gzfile(t,'rt') 
                                  data = read.csv(data,header=TRUE)
                                  data <- data[,keep.RTcols]
                                  dat_RT_Corr=rbind(dat_RT_Corr,data)
                            }

                 # 3.b Then, we take the tasks where accuracy was NOT measured (i.e. don't have "correct" column)
                      simpleRT <- gzfile('simple_reaction_time.csv.gz','rt')
                      simpleRT = read.csv(simpleRT,header=TRUE)
                      simpleRT <- simpleRT[,keep.RTcols[-1]]
                      simpleRT$correct <- 1
                      
                      #We exclude the Two Step Task as RTs are measured at two different points in time
                      #twostepTsk <- gzfile('two_stage_decision.csv.gz','rt')
                      #twostepTsk=read.csv(twostepTsk,header=TRUE)
                      #twostepTsk$rt <- twostepTsk$rt_first+twostepTsk$rt_second
                      #twostepTsk <- twostepTsk[,keep.RTcols[-1]]
                      #twostepTsk$correct <- 1
                      
                      #dat_RT=rbind(simpleRT,twostepTsk)
                      dat_RT = simpleRT
                      
              
              dat_onlyRT <- rbind(dat_RT_Corr,dat_RT)
              dat_onlyRT$cond <- NA
                        
              # 4. We read individually tasks with different conditions.
              
              attNtwk <- gzfile('attention_network_task.csv.gz','rt')      # Read data
              attNtwk=read.csv(attNtwk,header=TRUE) 
              attNtwk <- attNtwk[,c(keep.RTcols,"flanker_type")]           # Keep also condition column
              names(attNtwk)[names(attNtwk) == 'flanker_type'] <- 'cond'   # Rename condition column
              
              dirFrgt <- gzfile('directed_forgetting.csv.gz','rt')  
              dirFrgt=read.csv(dirFrgt,header=TRUE)
              dirFrgt <- dirFrgt[,c(keep.RTcols,"probe_type")]
              names(dirFrgt)[names(dirFrgt) == 'probe_type'] <- 'cond'   # Rename condition column
              
              goNogo <- gzfile('go_nogo.csv.gz','rt')  
              goNogo=read.csv(goNogo,header=TRUE)
              goNogo <- goNogo[,c(keep.RTcols,"condition")]
              names(goNogo)[names(goNogo) == 'condition'] <- 'cond'   # Rename condition column
              
              recProbes <- gzfile('recent_probes.csv.gz','rt')
              recProbes=read.csv(recProbes,header=TRUE)
              recProbes <- recProbes[,c(keep.RTcols,"probeType")]
              names(recProbes)[names(recProbes) == 'probeType'] <- 'cond'   # Rename condition column
              
              simon <- gzfile('simon.csv.gz','rt')
              simon=read.csv(simon,header=TRUE)
              simon <- simon[,c(keep.RTcols,"condition")]
              names(simon)[names(simon) == 'condition'] <- 'cond'   # Rename condition column
              
              stroop <- gzfile('stroop.csv.gz','rt')
              stroop=read.csv(stroop,header=TRUE)
              stroop <- stroop[,c(keep.RTcols,"condition")]
              names(stroop)[names(stroop) == 'condition'] <- 'cond'   # Rename condition column
              
              dat_cond <- rbind(attNtwk,dirFrgt,goNogo,recProbes,simon,stroop)
              
              
              # A few of the tasks included in the study, require special treatment/reading
              
              #### Local global tasks
              lclGlb <- gzfile('local_global_letter.csv.gz','rt')  
              lclGlb <- read.csv(lclGlb,header=TRUE)
              lclGlb <- lclGlb[,c(keep.RTcols,"conflict_condition","condition")]
              lclGlb$experiment_exp_id <- lclGlb$condition  # Separate Local / Global task
              lclGlb$condition <- NULL    # Remove Globa / Local task
              names(lclGlb)[names(lclGlb) == 'conflict_condition'] <- 'cond'   # Rename condition column
              
              #### Psychological Refractory Period
              psyRefPer <- gzfile('psychological_refractory_period_two_choices.csv.gz','rt')
              psyRefPer <- read.csv(psyRefPer,header=TRUE)
              names(psyRefPer)[names(psyRefPer) == 'choice1_rt'] <- 'rt'  
              names(psyRefPer)[names(psyRefPer) == 'choice1_correct'] <- 'correct'  
              psyRefPer <- psyRefPer[,c(keep.RTcols,"ISI")]
              ignoreRows <- which(psyRefPer$ISI==150|psyRefPer$ISI==300)
              psyRefPer <- psyRefPer[-ignoreRows,]
              names(psyRefPer)[names(psyRefPer) == 'ISI'] <- 'cond'   # Rename condition column
              
              #### Shape Matching task
              shapeMtch <- gzfile('shape_matching.csv.gz','rt')
              shapeMtch <- read.csv(shapeMtch,header=TRUE)
              noDistractorRows <- which(shapeMtch$condition=="SNN"|shapeMtch$condition=="DNN") # Trials without distracter
              shapeMtch$distractor_id[noDistractorRows] <- "X" # Change Distracter label
              N <- nrow(shapeMtch)  # No. observations
              targets <- shapeMtch$target_id[2:N]   # Current target
              distractors <- shapeMtch$distractor_id[1:(N-1)]  # Previous distracter
              shapeMtch$same <- 0    # Target WAS NOT distractor on previous trial
              shapeMtch$same[2:N] <- targets==distractors  # Target WAS distractor on previous trial
              shapeMtch <- shapeMtch[,c(keep.RTcols,"same")]
              names(shapeMtch)[names(shapeMtch) == 'same'] <- 'cond'   # Rename condition column

              dat_special <- rbind(lclGlb,psyRefPer,shapeMtch)
              
              datos <- rbind(dat_special,dat_onlyRT,dat_cond)
              write.csv(datos,file="../enkavi_mergeddata.csv",row.names = FALSE)
        }else{
              datos <- read.csv(datosFile)    
        }
        
        ########################
        # Cleaning data set
        
        # Change column names
        names(datos)[names(datos) == 'worker_id'] <- 'sub'  
        names(datos)[names(datos) == 'experiment_exp_id'] <- 'task'
        names(datos)[names(datos) == 'correct'] <- 'corr'  
        
        # We standardize the "correct" column
        bad.correct <- which(datos$corr=="-1")
            if(length(bad.correct)>0){
              datos <- datos[-bad.correct,]
            }
        
        # Merge "True" and "False" as part of 1/0 binary code
        datos$corr[datos$corr=="True"] <- 1
        datos$corr[datos$corr=="False"] <- 0
        datos$corr <- as.character(datos$corr)
        datos$corr <- as.numeric(datos$corr)
        
        # We remove participants who had an overall accuracy rate lower than 70%
        acc <- aggregate(datos$corr, by=list(datos$sub), FUN=mean, na.rm=TRUE)
        colnames(acc) <- c("sub", "prop")
        badPerf <- acc$prop<0.7
        badPerf <- acc[badPerf,]
        bad.part <- unique(badPerf$sub)
        badParticipants.rows <- which(datos$sub %in% bad.part)
        datos <- datos[-badParticipants.rows,]
        
        # We remove slow participants (RT > 2000 in more than 3% trials)
        #high.RT <- datos$rt > 2000
        #propHighRT <-aggregate(high.RT, by=list(datos$sub,datos$task), FUN=mean)
        #colnames(propHighRT) <- c("subj", "task" ,"prop")
        #manyHighRT <- propHighRT$prop >= 0.3
        #higRTpart <- propHighRT[manyHighRT,]
        #bad.part <- unique(higRTpart$subj)
        #slowParticipants.row <- which(datos$sub %in% bad.part)
        #datos <- datos[-slowParticipants.row,]
        
        # We remove fast participants (RT < 200 in more than 3% trials)
        #low.RT <- datos$rt < 200
        #propLowRT <-aggregate(low.RT, by=list(datos$sub,datos$task), FUN=mean)
        #colnames(propLowRT) <- c("subj", "task" ,"prop")
        #manyLowRt <- propLowRT$prop >= 0.3
        #lowRTpart <- propLowRT[manyLowRt,]
        #bad.part <- unique(lowRTpart$subj)
        #fastParticipants.row<- which(datos$sub %in% bad.part)
        #datos <- datos[-fastParticipants.row,]
        
        # We remove NA RT trials
        NA.RT.trials <- which(is.na(datos$rt))
        if(sum(NA.RT.trials)>0){
           datos <- datos[-NA.RT.trials,]
           }
        
        # We remove extreme RTs
        extremeRT.trials <- which(datos$rt <= 0)
        if(length(extremeRT.trials)>0){
           datos <- datos[-extremeRT.trials,]
        }
        
        datos <- as.data.frame(datos)
        datos$sub <- as.character(datos$sub)

        # We make sure that we have observations forr all tasks across participants:
        a <- tapply(datos$rt,list(datos$task,datos$sub),length)
        # sum(is.na(a))              # Identified number of missing points
        x <- a["shift_task",]        # We identify all 4 missing points on "Shift_task"
        bad.subs <- names(which(is.na(x)))   # We identify their IDS
        bad.subs <- which(datos$sub %in% bad.subs)
        datos <- datos[-bad.subs,]
        
        datos$rt <- datos$rt*0.001
                
    return(datos)
  }

###################################################
#####       Load data from Hedge et al. (2018)
###################################################

# loadData.Hedge <- function(existingFolderForData){
#   datosFile <- paste(existingFolderForData,"hedge_mergeddata.csv",sep="")
#   test <- file.exists(datosFile)
#   if(!test){
#     project <- osf_retrieve_node("https://osf.io/cwzds/")
#     files <- osf_ls_files(project)
#     data <- which(files=="Data")
#     osf_download(files[data,],path=existingFolderForData)
#   }
#   data <- paste(existingFolderForData,"mergeddata.txt",sep="")
#   data <- read.table(data,header = TRUE)
#   
#   #Remove uninformative columns
#   data <- data[,-c(2,4:9,14)]
#   
#   #Remove "Antisaccade" and "Stop-signal" tasks
#   remove.Tasks <- c("antisaccade","stopsignal")
#   remove.Tasks <- which(data$task %in% remove.Tasks)
#   data <- data[-remove.Tasks,]
#   #Completely remove any record of these tasks ever existing
#   data$task <- as.character(data$task)
#   
#   #Relabel the two conditions on the n2repetition cost task
#   data$congruency[which(data$congruency=="n2switch")] <- "congruent"  #Expected to be slower
#   data$congruency[which(data$congruency=="n2repetition")] <- "incongruent"  #Expected to be slower
#   #Completely remove any record of these tasks ever existing
#   data$congruency <- as.character(data$congruency)
#   
#   #Remove participants identified as "bad observations" by authors
#   bad.part <- c(132,164,368,402,429)
#   bad.rows <- which(data$subj %in% bad.part)
#   if(length(bad.rows)>0){
#     data <- data[-bad.rows,]
#   }
#   
#   #Remove trials where a "correct" value of 99 was recorded
#   good.corr <- data$corr %in% c(0,1)
#   bad.corr <- !good.corr
#   if(sum(bad.corr)>0){
#     data <- data[-which(bad.corr),]
#   }
#   
#   return(data)
# }



#####################################################
#### Specific to Von Bastian's data
loadData.vonBastian <- function(existingFolderForData){
            filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_stroop.csv")
            stroop <- read.csv2(filename, header=TRUE, dec=".")
            stroop$cond <- as.numeric(stroop$congruency)  #congruent -> 1, incongruent -> 2, neutral -> 3
            ntrial <- length(stroop[stroop$ID == stroop$ID[1], 1])
            nsub <- length(unique(stroop$ID))
            stroop$trial <- rep(1:ntrial, nsub)
            stroop$rt <- stroop$RT/1000 #rt data in seconds
            stroop <- stroop[stroop$rt > .2 & stroop$rt < 2, ]
            stroop <- subset(stroop, accuracy == 1 & cond != 3)
            stroop=data.frame(stroop$ID,stroop$cond,stroop$rt)
            colnames(stroop)=c('sub','cond','rt')
          
            filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_simon.csv")
            simon <- read.csv2(filename, header=TRUE, dec=".")
            simon$cond <- as.numeric(simon$congruency)  #congruent -> 1, incongruent -> 2, neutral -> 3
            ntrial <- length(simon[simon$ID == simon$ID[1], 1])
            nsub <- length(unique(simon$ID))
            simon$trial <- rep(1:ntrial, nsub)
            simon$rt <- simon$RT/1000
            simon <- simon[simon$rt > .2 & simon$rt < 2, ]
            simon <- subset(simon, accuracy == 1)
            simon=data.frame(simon$ID,simon$cond,simon$rt)
            colnames(simon)=c('sub','cond','rt')
            
            filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_flanker.csv")
            flanker <- read.csv2(filename, header=TRUE, dec=".")
            flanker$cond <- as.numeric(flanker$congruency)  #congruent -> 1, incongruent -> 2, neutral -> 3
            ntrial <- length(flanker[flanker$ID == flanker$ID[1], 1])
            nsub <- length(unique(flanker$ID))
            flanker$trial <- rep(1:ntrial, nsub)
            flanker$rt <- flanker$RT/1000
            flanker <- flanker[flanker$rt > .2 & flanker$rt < 2, ]
            flanker <- subset(flanker, accuracy == 1 & cond != 3)
            flanker=data.frame(flanker$ID,flanker$cond,flanker$rt)
            colnames(flanker)=c('sub','cond','rt')
            
            stroop$task <- "Stroop"
            simon$task <- "Simon"
            flankr$task <- "Flanker"
            
            vonBastian <- rbind(stroop,flankr,simon)
  return(vonBastian)}
  