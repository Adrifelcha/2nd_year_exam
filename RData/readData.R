library(curl)

###  Von Bastian's Data

readVbStroop=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_stroop.csv")
  stroop <- read.csv2(filename, header=TRUE, dec=".")
  stroop$cond <- as.numeric(factor(stroop$congruency))  #congruent -> 1, incongruent -> 2, neutral -> 3
  ntrial <- length(stroop[stroop$ID == stroop$ID[1], 1])
  nsub <- length(unique(stroop$ID))
  stroop$trial <- rep(1:ntrial, nsub)
  stroop$rt <- stroop$RT/1000 #rt data in seconds
  stroop <- stroop[stroop$rt > .2 & stroop$rt < 2, ]
  stroop <- subset(stroop, accuracy == 1 & cond != 3)
  dat=data.frame(stroop$ID,stroop$cond,stroop$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

readVbSimon=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_simon.csv")
  simon <- read.csv2(filename, header=TRUE, dec=".")
  simon$cond <- as.numeric(factor(simon$congruency))  #congruent -> 1, incongruent -> 2, neutral -> 3
  ntrial <- length(simon[simon$ID == simon$ID[1], 1])
  nsub <- length(unique(simon$ID))
  simon$trial <- rep(1:ntrial, nsub)
  simon$rt <- simon$RT/1000
  simon <- simon[simon$rt > .2 & simon$rt < 2, ]
  simon <- subset(simon, accuracy == 1)
  dat=data.frame(simon$ID,simon$cond,simon$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

readVbFlanker=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/vonBastianJEPG2015/LEF_flanker.csv")
  flanker <- read.csv2(filename, header=TRUE, dec=".")
  flanker$cond <- as.numeric(factor(flanker$congruency))  #congruent -> 1, incongruent -> 2, neutral -> 3
  ntrial <- length(flanker[flanker$ID == flanker$ID[1], 1])
  nsub <- length(unique(flanker$ID))
  flanker$trial <- rep(1:ntrial, nsub)
  flanker$rt <- flanker$RT/1000
  flanker <- flanker[flanker$rt > .2 & flanker$rt < 2, ]
  flanker <- subset(flanker, accuracy == 1 & cond != 3)
  dat=data.frame(flanker$ID,flanker$cond,flanker$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

###  Pratte's Data

readPratteStroopI=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/PratteAPP2010/allsi2.dat")
  clnames <- c('exp'
             , 'sub'
             , 'blk'
             , 'trial'
             , 'color'
             , 'distract'
             , 'cond'
             , 'resp'
             , 'acc'
             , 'rt'
             , 'errorTotal')
  dat <- read.table(filename)
  colnames(dat) <- clnames
  #clean rt data as proposed in Pratte et al., 2010
  dat <- dat[dat$rt > .2 & dat$rt < 2, ] #Delete very slow and very fast responses
  dat <- subset(dat, acc == 1 & cond != 2 & exp == 1) #accurate data, deleting neutral condition, only stroop task data
  tmp <- dat[!(dat$trial %in% 0:4), ] #Delete first 5 trials in each block

  dat=data.frame(tmp$sub,2-tmp$cond,tmp$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

readPratteStroopII=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/PratteAPP2010/allsi7.dat")
  clnames <- c('sub'
             ,'blk'
             ,'blktype'
             ,'trial'
             ,'word'
             ,'location'
             ,'cond'
             ,'resp'
             ,'acc'
             ,'rt'
             ,'errorTotal')
  dat <- read.table(filename)
  colnames(dat) <- clnames
  #clean rt data as proposed in Pratte et al., 2010
  #only congruent & incongruent condition and only stroop data
  dat <- dat[dat$rt > .2 & dat$rt < 2, ]
  dat <- subset(dat, acc == 1 & blktype == 1) #Only accurate data, only data from Stroop task
  tmp <- dat[!(dat$trial %in% 0:4), ]
  dat=data.frame(tmp$sub,2-tmp$cond,tmp$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

readPratteSimonI=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/PratteAPP2010/allsi2.dat")
  clnames <- c('exp'
             , 'sub'
             , 'blk'
             , 'trial'
             , 'color'
             , 'distract'
             , 'cond'
             , 'resp'
             , 'acc'
             , 'rt'
             , 'errorTotal')
  dat <- read.table(filename)
  colnames(dat) <- clnames
  #clean rt data as proposed in Pratte et al., 2010
  #only congruent & incongruent condition and only stroop data
  dat <- dat[dat$rt > .2 & dat$rt < 2, ]
  dat <- subset(dat, acc == 1 & exp == 0) #accurate data, only simon task data
  tmp <- dat[!(dat$trial %in% 0:4), ]
  dat=data.frame(tmp$sub,2-tmp$cond,tmp$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}
  
readPratteSimonII=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/PratteAPP2010/allsi7.dat")
  clnames <- c('sub'
             ,'blk'
             ,'blktype'
             ,'trial'
             ,'word'
             ,'location'
             ,'cond'
             ,'resp'
             ,'acc'
             ,'rt'
             ,'errorTotal')
  dat <- read.table(filename)
  colnames(dat) <- clnames
  #clean rt data as proposed in Pratte et al., 2010
  #only congruent & incongruent condition and only stroop data
  dat <- dat[dat$rt > .2 & dat$rt < 2, ]
  dat <- subset(dat, acc == 1 & blktype == 0) #accurate data, only simon task data
  tmp <- dat[!(dat$trial %in% 0:4), ]
  dat=data.frame(tmp$sub,2-tmp$cond,tmp$rt)
  colnames(dat)=c('sub','cond','rt')
  return(dat)}

### Rey-Mermet's Data

rmCleanData=function(dat,
                     top=1.995,                 
                     hiTrlRT=1.995,
                     loTrlRT=.275,
                     loTop=.98,
                     loAcc=.90,
                     loRT=.99){
    excludeSub=c(132,164,368,402,429)  #origial author exclusions
    #exclusions
    bad0=dat$sub %in% excludeSub
    bad1=dat$block == 'practice'
    bad2=dat$trialType != 'exp'
    bad3=!(dat$acc %in% c(0,1)) #there are some strange things
    dat=dat[!(bad0 | bad1 | bad2 | bad3),]	
    sub=tapply(dat$sub,dat$sub,mean)
    topBySub=tapply(dat$rt<top,dat$sub,mean)
    accBySub=(tapply(dat$acc,dat$sub,mean))
    loBySub=tapply(dat$rt>loTrlRT,dat$sub,mean)	
    bad4=dat$sub %in% sub[
      accBySub<loAcc |
        topBySub<loTop |
        loBySub<loRT]
    bad5 = dat$rt<loTrlRT | dat$rt>min(hiTrlRT,top) | dat$acc==0 
    dat=dat[!(bad4 | bad5),]	
    return(dat)
}

readRMStroopI=function(){
  numStroop=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/numStroop.dat",head=T)
  a3=rmCleanData(numStroop)
  numStroop=a3[a3$acc==1,]
  numStroop$cond=as.integer(as.factor(numStroop$cond))
  dat=numStroop[numStroop$cond %in% 1:2,]
  return(dat)}

readRMStroopII=function(){
  colStroop=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/colStroop.dat",head=T)
  a3=rmCleanData(colStroop)
  colStroop=a3[a3$acc==1,]
  colStroop$cond=as.integer(as.factor(colStroop$cond))
  dat=colStroop[colStroop$cond %in% 1:2,]
  return(dat)}

readRMFlankI=function(){
  letFlanker=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/letFlanker.dat",head=T)
  a3=rmCleanData(letFlanker)
  letFlanker=a3[a3$acc==1,]
  letFlanker$cond=as.integer(as.factor(letFlanker$cond))
  dat=letFlanker[letFlanker$cond %in% 1:2,]
  return(dat)}

readRMFlankII=function(){
  arrowFlanker=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/arrowFlanker.dat",head=T)
  a3=rmCleanData(arrowFlanker)
  arrowFlanker=a3[a3$acc==1,]
  arrowFlanker$cond=as.integer(as.factor(arrowFlanker$cond))
  dat=arrowFlanker[arrowFlanker$cond %in% 1:2,]
  return(dat)}


### Hedge's Data

readHedgeFlanker=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Hedge2018/clean/flankerClean.dat")
  dat=read.table(filename,head=T)
  return(dat)}

readHedgeStroop=function(){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Hedge2018/clean/stroopClean.dat")
  dat=read.table(filename,head=T)
  return(dat)}

## Rouder's Data

readRouderOtherI=function(){
  indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
  colnames(indat)=c('sub','block','trial','stim','resp','rt','error')
  bad1=indat$sub%in%c(34,43)
  bad2=indat$rt<250 | indat$rt>2000
  bad3=indat$err==1
  bad4=indat$block==0 & indat$trial<20
  bad5=indat$trial==0
  bad=bad1 | bad2 | bad3 |bad4 |bad5
  tmp=indat[!bad,]
  cond=rep(0,length(tmp$stim))
  cond[tmp$stim==0 | tmp$stim==5]=1
  cond[tmp$stim==2 | tmp$stim==3]=2
  dat=data.frame(tmp$sub[cond>0],cond[cond>0],tmp$rt[cond>0])
  colnames(dat)=c('sub','cond','rt')
  dat$rt=dat$rt/1000
  return(dat)}


readRouderOtherII=function(upper=5,lower=.2){
  filename <- curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/rt-shiftScaleShape/st3.dat")
  dat=read.table(filename)
  colnames(dat)=c('sub','block','trial','trialB','cond','resp','rt','acc')
#codes
#resp: 0=left, 1=right
#acc: 0=error, 1=correct
#cond: 0 easiest left, 2 is hardest left, 3 hardest right, 5 easiest right
#dat$mycond is ordered from easiest to hardest condition
#mycond=easy(r,g), med(r,g), hard(r,g)
  dat$mycond=NA
  dat$mycond[dat$cond==5]=1
  dat$mycond[dat$cond==0]=2
  dat$mycond[dat$cond==4]=3
  dat$mycond[dat$cond==1]=4
  dat$mycond[dat$cond==3]=5
  dat$mycond[dat$cond==2]=6
  dat$sub=as.integer(as.factor(dat$sub))
  I=length(levels(as.factor(dat$sub))) #Number of Subs.
  #Wrong-button responses
  wrong.button=dat$resp>1
  error=dat$acc==0
  stop=wrong.button | dat$trialB==0 |error
  after.stop=c(0,stop)[1:length(stop)]
  a=dat$rt<lower
  b=dat$rt>upper
  dont.use=stop | after.stop |a |b
  clean=dat[!dont.use,]
  ct=clean[clean$block>2,]
  cond=rep(0,length(ct$cond))
  cond[ct$cond==0 | ct$cond==5]=1
  cond[ct$cond==2 | ct$cond==3]=2
  dat=data.frame(ct$sub[cond>0],cond[cond>0],ct$rt[cond>0])
  colnames(dat)=c('sub','cond','rt')
  return(dat)}



cleanWhitehead1=function(){
  filename <- "https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/Experiment1.csv"
  data=read.csv(url(filename),header=T,na.strings=c(""))  
    includesubs <- c(507,508,513,514,517,518,519,520,521,506,515,523,524,525,526,527,528,529,530,532,
                     533,534,535,537,539,540,541,542,544,545,546,547,548,549,551,553,555,559,560,561,
                     562,567,568,569,571,573,576,577,578,579,583,584,585,586,587,588,591,592,593,594,
                     595,596,597,599,601,602,603,605,606,607,608,609,610,611,613,614,615,616,617,619,
                     620,621,622,623,624,625,626,629,630,631,633,634,635,637,639,640,641,642,643,645,
                     647,648,649,650,651,652,654,655,656,657,658,659,660,661,662,663,664,665,666,667,
                     668,669,670,671,672,673,674,675,676,680,678,679,681,683,685,687,689,690,692,693,
                     694,696,697,698,699,700,701,702,704,705,706,708,710,711,712,713,715,716,718,719,
                     720,721,723,724,725,726,728,729,730,732,733,734,736,737,738,739,740,741)
    
    #data$StimSlideSimon.RT <- as.numeric(as.character(data$StimSlideSimon.RT))
    
    #Filter and clean data
    df.simon <- data %>% mutate(prevcon = lag(Congruency)) %>%
      mutate(StimSlideSimon.RT = as.integer(as.character(StimSlideSimon.RT)),
             StimSlideSimon.ACC = as.integer(as.character(StimSlideSimon.ACC)),
             BlockNum = as.integer(as.character(BlockNum))) %>%
      filter(Subject %in% includesubs & StimSlideSimon.RT != "" &
               (StimSlideSimon.RT > 200 & StimSlideSimon.RT < 3000) &
               StimSlideSimon.ACC == 1 & prevcon != 'NA' &
               BlockNum > 2) %>%
      mutate(RT = StimSlideSimon.RT, task = factor(1), 
             Congruency = abs(as.numeric(as.character(Congruency))-1))
    
    df.flanker <- data %>% mutate(prevcon = lag(Congruency)) %>%
      mutate(StimSlideFlanker.RT = as.integer(as.character(StimSlideFlanker.RT)),
             StimSlideFlanker.ACC = as.integer(as.character(StimSlideFlanker.ACC)),
             BlockNum = as.integer(as.character(BlockNum))) %>%
      filter(Subject %in% includesubs & StimSlideFlanker.RT != "" &
               (StimSlideFlanker.RT > 200 & StimSlideFlanker.RT < 3000) &
               StimSlideFlanker.ACC == 1 & prevcon != 'NA' &
               BlockNum > 2) %>%
      mutate(RT = StimSlideFlanker.RT, task = factor(2), 
             Congruency = abs(as.numeric(as.character(Congruency))-1))
    
    df.stroop <- data %>% mutate(prevcon = lag(Congruency)) %>%
      mutate(StimSlideStroop.RT = as.integer(as.character(StimSlideStroop.RT)),
             StimSlideStroop.RT = as.integer(as.character(StimSlideStroop.RT)),
             BlockNum = as.integer(as.character(BlockNum))) %>%
      filter(Subject %in% includesubs &  StimSlideStroop.RT != "" &
               (StimSlideStroop.RT > 200 & StimSlideStroop.RT < 3000) &
               StimSlideStroop.ACC == 1 & prevcon != 'NA' &
               BlockNum > 2) %>%
      mutate(RT = StimSlideStroop.RT, task = factor(3), 
             Congruency = abs(as.numeric(as.character(Congruency))-1))
    
    #simon is task 1
    #flanker is task 2
    #stroop is task 3
    
    datLarge <- rbind(df.simon,df.flanker,df.stroop)
    dat <- data.frame(datLarge$Subject,datLarge$task,datLarge$Congruency,datLarge$RT)
    colnames(dat)=c("sub","task","cond","rt")
    dat$sub=as.factor(as.integer(dat$sub))
    levels(dat$sub)=1:length(unique(dat$sub))
    dat$cond=dat$cond+1
    dat$rt=dat$rt/1000
    return(dat)
  }
  
readWhiteheadI=function(task){
  dat=cleanWhitehead1()
  return(dat[dat$task==task,])
}


cleanWhitehead2=function(){
  urlSimon="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp2.csv"
  urlFlank="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp2.csv"
  urlStroop="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp2.csv"
  
  data.simon<- read.csv(urlSimon,header=T,na.strings=c(""))
  data.stroop<- read.csv(urlStroop,header=T,na.strings=c(""))
  data.flanker<- read.csv(urlFlank,header=T,na.strings=c(""))
  excludesubs <- c(115,116,126,140,148,153,160,175,188,189,194,195,203,210,212,220,
                   229,233,237,239,243,250,253,297,901,913,918,145,217,258,280,222)
  
  #Filter and clean data
  df.simon <- data.simon %>% mutate(StimSlideSimon.RT = as.integer(as.character(StimSlideSimon.RT))) %>%
    filter(!Subject %in% excludesubs &
             (StimSlideSimon.RT > 200 & StimSlideSimon.RT < 3000) &
             StimSlideSimon.ACC == 1 &
             BlockNum > 2) %>%
    mutate(RT = StimSlideSimon.RT, task = factor(1), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  df.flanker <- data.flanker %>% mutate(StimSlideFlanker.RT = as.integer(as.character(StimSlideFlanker.RT))) %>%
    filter(!Subject %in% excludesubs &
             (StimSlideFlanker.RT > 200 & StimSlideFlanker.RT < 3000) &
             StimSlideFlanker.ACC == 1 &
             BlockNum > 2) %>%
    mutate(RT = StimSlideFlanker.RT, task = factor(2), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  df.stroop <- data.stroop %>% mutate(StimSlideStroop.RT = as.integer(as.character(StimSlideStroop.RT)),
                                      BlockNum = as.integer(as.character(BlockNum)),
                                      Subject = as.integer(as.character(Subject))) %>%
    filter(!Subject %in% excludesubs & 
             (StimSlideStroop.RT > 200 & StimSlideStroop.RT < 3000) &
             StimSlideStroop.ACC == 1 &
             BlockNum > 2) %>%
    mutate(RT = StimSlideStroop.RT, task = factor(3), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  #simon is task 1
  #flanker is task 2
  #stroop is task 3
  simon=df.simon[,c(2,8,4,7)]
  flanker=df.flanker[,c(2,8,4,7)]
  stroop=df.stroop[,c(2,8,4,7)]  
  dat <- rbind(simon,flanker,stroop)
  colnames(dat)=c("sub","task","cond","rt")
  dat$sub=as.factor(as.integer(dat$sub))
  levels(dat$sub)=1:length(unique(dat$sub))
  dat$cond=dat$cond+1
  dat$rt=dat$rt/1000
  return(dat)
}

readWhiteheadII=function(task){
  dat=cleanWhitehead2()
  return(dat[dat$task==task,])
}


cleanWhitehead3=function(){
  urlSimon="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp3.csv"
  urlFlank="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp3.csv"
  urlStroop="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp3.csv"
  
  data.simon<- read.csv(urlSimon,header=T,na.strings=c(""))
  data.stroop<- read.csv(urlStroop,header=T,na.strings=c(""))
  data.flanker<- read.csv(urlFlank,header=T,na.strings=c(""))
  
  #subjects to exclude
  includesubs <- c(105,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,
                   131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,
                   155,156,157,158,159,160,161,162,163,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,
                   180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,
                   205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,228,229,
                   231,232,233,234,235,236,237,238,240,241,242,243,244,246,247,248,249,250,251,253,254,255,256,257,
                   259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,277,278,279,280,281,282,283,284,
                   285,286,287,288,289,290,291,293,294,295,296,297,299,301,303,305,306,307,308,309,311,312,313,314,
                   316,317,318,319,320,321,322,323,324,325,326,327,328,330,331,332,333,334)
  
  #Filter and clean data
  df.simon <- data.simon %>% mutate(StimSlideSimon.RT = as.integer(as.character(StimSlideSimon.RT))) %>%
    filter(Subject %in% includesubs &
             (StimSlideSimon.RT > 200 & StimSlideSimon.RT < 3000) &
             StimSlideSimon.ACC == 1 &
             PracExp == "Exp")%>%
    mutate(RT = StimSlideSimon.RT, task = factor(1), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  df.flanker <- data.flanker %>% mutate(StimSlideFlanker.RT = as.integer(as.character(StimSlideFlanker.RT))) %>%
    filter(Subject %in% includesubs &
             (StimSlideFlanker.RT > 200 & StimSlideFlanker.RT < 3000) &
             StimSlideFlanker.ACC == 1 &
             PracExp == "Exp")%>%
    mutate(RT = StimSlideFlanker.RT, task = factor(2), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  df.stroop <- data.stroop %>% mutate(StimSlideStroop.RT = as.integer(as.character(StimSlideStroop.RT))) %>%
    filter(Subject %in% includesubs & 
             (StimSlideStroop.RT > 200 & StimSlideStroop.RT < 3000) &
             StimSlideStroop.ACC == 1 &
             PracExp == "Exp")%>%
    mutate(RT = StimSlideStroop.RT, task = factor(3), Congruency = abs(as.numeric(as.character(Congruency))-1))
  
  #simon is task 1
  #flanker is task 2
  #stroop is task 3
  simon=df.simon[,c(2,8,4,7)]
  flanker=df.flanker[,c(2,8,4,7)]
  stroop=df.stroop[,c(2,8,4,7)]  
  dat <- rbind(simon,flanker,stroop)
  colnames(dat)=c("sub","task","cond","rt")
  dat$sub=as.factor(as.integer(dat$sub))
  levels(dat$sub)=1:length(unique(dat$sub))
  dat$cond=dat$cond+1
  dat$rt=dat$rt/1000
  return(dat)
}


readWhiteheadIII=function(task){
  dat=cleanWhitehead3()
  return(dat[dat$task==task,])
}