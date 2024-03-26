data = read.csv('~/icu_data/rwean_data_24hr_v1_3.csv')
names(data)
grep('hour',names(data),value=T)
data = data[data$hour_baseline>=1,]
data$control[!data$ventilation_status %in% c(2,5)]=0
data$wean = 1-data$control
data = data[order(data$stay_id,data$hour),]
data$past_wean = unlist(sapply(split(data$wean,data$stay_id),function(x)cumsum(cumsum(x))>1))
data = data[data$past_wean==0,]
data = data[order(data$stay_id,data$hour),]
get_last = function(x){
  if(length(x)>1){
    x=c(NA,x[1:(length(x)-1)])
  }else{
    x=NA
  }
  x
}

data$last_opioid = unlist(sapply(split(data$opioid,data$stay_id),get_last))
data$last_benzo = unlist(sapply(split(data$benzo,data$stay_id),get_last))
data$last_propofol = unlist(sapply(split(data$propofol,data$stay_id),get_last))
data$last_dex = unlist(sapply(split(data$dex,data$stay_id),get_last))
data$last_driving_pressure = unlist(sapply(split(data$driving_pressure,data$stay_id),get_last))
data$urine_output[is.na(data$urine_output)]=0
data$last_urine_output[is.na(data$last_urine_output)]=0
data$rate_std[is.na(data$rate_std)] = 0
data$last_rate_std[is.na(data$rate_std)] = 0
data$any_vaso = data$rate_std > 0
data$last_any_vaso = unlist(sapply(split(data$any_vaso,data$stay_id),get_last))


cohort = read.csv("~/icu_data/mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("~/icu_data/mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
admissions = read.csv("~/icu_data/mimic4_v1/admissions.csv",header=T)
cohort = merge(cohort,admissions)

#get rid of patients with misentered deathtimes before baseline
data = merge(data,cohort[,c('stay_id','deathtime','intime')])
data$baseline_time = 3600*data$baseline_hour + strptime(as.character(data$intime), "%Y-%m-%d %H:%M:%S")
omit = unique(data$stay_id[data$baseline_time > strptime(as.character(data$deathtime), "%Y-%m-%d %H:%M:%S")])
data = data[!data$stay_id %in% omit,]
data = data[order(data$stay_id,data$hour),]

riker = read.csv("~/icu_data/mimic4_v1/riker_sas.csv")
riker = merge(riker,cohort[,c("stay_id","intime")])
names(riker)[8] = 'riker_score'
riker$hour = ceiling(as.numeric(difftime(strptime(as.character(riker$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(riker$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))

riker_max = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
names(riker_max)[3] = 'riker_max'

riker_min = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(min,na.rm=T)
names(riker_min)[3] = 'riker_min'

riker_mean = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(mean,na.rm=T)
names(riker_mean)[3] = 'riker_mean'

data = merge(data,riker_max,all.x=T)
data = merge(data,riker_min,all.x=T)
data = merge(data,riker_mean,all.x=T)
data$riker_meas = !is.na(data$riker_max)

carry_forward = function(x){
  if(length(x)>1){
    for(j in 2:length(x)){
      if(is.na(x[j])|x[j]==''){
        x[j] = x[j-1]
      }
    }
  }
  x
}

data$riker_max = unlist(lapply(split(data$riker_max,data$stay_id),carry_forward))
data$riker_min = unlist(lapply(split(data$riker_min,data$stay_id),carry_forward))
data$riker_mean = unlist(lapply(split(data$riker_mean,data$stay_id),carry_forward))

data$last_riker_max=unlist(lapply(split(data$riker_max,data$stay_id),get_last))
data$last_riker_min=unlist(lapply(split(data$riker_min,data$stay_id),get_last))
data$last_riker_mean=unlist(lapply(split(data$riker_mean,data$stay_id),get_last))
data$last_riker_meas=unlist(lapply(split(data$riker_meas,data$stay_id),get_last))

data$last_rass_max=unlist(lapply(split(data$rass_max,data$stay_id),get_last))
data$last_rass_min=unlist(lapply(split(data$rass_min,data$stay_id),get_last))
data$last_rass_mean=unlist(lapply(split(data$rass_mean,data$stay_id),get_last))
data$last_rass_meas=unlist(lapply(split(data$rass_meas,data$stay_id),get_last))

data$sedation_meas = pmax(data$riker_meas,data$rass_meas)
data$unarousable = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min==1,ifelse(data$rass_meas,data$rass_min==-5,NA)),NA)
data$unarousable = unlist(lapply(split(data$unarousable,data$stay_id),carry_forward))
data$unarousable[is.na(data$unarousable)]=0
data$last_unarousable = unlist(sapply(split(data$unarousable,data$stay_id),get_last))

data$deep = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min==2,ifelse(data$rass_meas,data$rass_min==-4,NA)),NA)
data$deep = unlist(lapply(split(data$deep,data$stay_id),carry_forward))
data$deep[is.na(data$deep)]=0
data$last_deep = unlist(sapply(split(data$deep,data$stay_id),get_last))

data$moderate = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min==3,ifelse(data$rass_meas,data$rass_min%in%c(-3,-2),NA)),NA)
data$moderate = unlist(lapply(split(data$moderate,data$stay_id),carry_forward))
data$moderate[is.na(data$moderate)]=0
data$last_moderate = unlist(sapply(split(data$moderate,data$stay_id),get_last))

data$light = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min==4,ifelse(data$rass_meas,data$rass_min%in%c(-1,0),NA)),NA)
data$light = unlist(lapply(split(data$light,data$stay_id),carry_forward))
data$light[is.na(data$light)]=0
data$last_light = unlist(sapply(split(data$light,data$stay_id),get_last))

data$restless = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min==5,ifelse(data$rass_meas,data$rass_min%in%c(1,2),NA)),NA)
data$restless = unlist(lapply(split(data$restless,data$stay_id),carry_forward))
data$restless[is.na(data$restless)]=0
data$last_restless = unlist(sapply(split(data$restless,data$stay_id),get_last))

data$agitated = ifelse(data$sedation_meas,ifelse(data$riker_meas,data$riker_min>5,ifelse(data$rass_meas,data$rass_min>2,NA)),NA)
data$agitated = unlist(lapply(split(data$agitated,data$stay_id),carry_forward))
data$agitated[is.na(data$agitated)]=0
data$last_agitated = unlist(sapply(split(data$agitated,data$stay_id),get_last))

data$total_unarousable = unlist(sapply(split(data$unarousable,data$stay_id),cumsum))
data$mean_unarousable = data$total_unarousable/data$hour_baseline
data$last_total_unarousable = unlist(sapply(split(data$total_unarousable,data$stay_id),get_last))
data$last_mean_unarousable = unlist(sapply(split(data$mean_unarousable,data$stay_id),get_last))

data$total_deep = unlist(sapply(split(data$deep,data$stay_id),cumsum))
data$mean_deep = data$total_deep/data$hour_baseline
data$last_total_deep = unlist(sapply(split(data$total_deep,data$stay_id),get_last))
data$last_mean_deep = unlist(sapply(split(data$mean_deep,data$stay_id),get_last))

data$total_moderate = unlist(sapply(split(data$moderate,data$stay_id),cumsum))
data$mean_moderate = data$total_moderate/data$hour_baseline
data$last_total_moderate = unlist(sapply(split(data$total_moderate,data$stay_id),get_last))
data$last_mean_moderate = unlist(sapply(split(data$mean_moderate,data$stay_id),get_last))

data$total_light = unlist(sapply(split(data$light,data$stay_id),cumsum))
data$mean_light = data$total_light/data$hour_baseline
data$last_total_light = unlist(sapply(split(data$total_light,data$stay_id),get_last))
data$last_mean_light = unlist(sapply(split(data$mean_light,data$stay_id),get_last))

data$total_restless = unlist(sapply(split(data$restless,data$stay_id),cumsum))
data$mean_restless = data$total_restless/data$hour_baseline
data$last_total_restless = unlist(sapply(split(data$total_restless,data$stay_id),get_last))
data$last_mean_restless = unlist(sapply(split(data$mean_restless,data$stay_id),get_last))

data$total_agitated = unlist(sapply(split(data$agitated,data$stay_id),cumsum))
data$mean_agitated = data$total_agitated/data$hour_baseline
data$last_total_agitated = unlist(sapply(split(data$total_agitated,data$stay_id),get_last))
data$last_mean_agitated = unlist(sapply(split(data$mean_agitated,data$stay_id),get_last))

#only use first icu stays of same patient
subjects = cohort[cohort$stay_id %in% data$stay_id,c("subject_id","stay_id","intime")]
subjects = subjects[order(subjects$subject_id,strptime(as.character(subjects$intime),"%Y-%m-%d %H:%M:%S")),]
dups = duplicated(subjects$subject_id)
keepers = subjects$stay_id[!dups]
data = data[data$stay_id%in%keepers,]
data = data[order(data$stay_id,data$hour),]
subjects = subjects[subjects$stay_id %in% keepers,]
L = c('hour','PARALYSIS','CHRONIC_PULMONARY','OBESITY','elixhauser_score','age','gender','admission_type','admission_location','insurance',
      'marital_status','ethnicity','language','first_careunit','imputed_IBW','hour_baseline','imputed_height',
      paste0('last_',c('peep_set','tidal_volume_set','resp_rate','peak_insp_pressure','plateau_pressure',
                       'mean_airway_pressure','minutes_vol','pao2fio2ratio','driving_pressure', 'po2','pco2','aado2_calc',
                       'ph','baseexcess','totalco2','temperature','fio2_chartevents', 'urine_output',
                       'lactate','glucose','gcs','gcs_verbal','gcs_motor','gcs_eyes','gcs_unable',
                       'CREATININE','PLATELET','PTT','BUN','WBC','sbp','dbp','mbp','spo2','heart_rate',
                       'respiration_24hours','coagulation_24hours','liver_24hours','cardiovascular_24hours',
                       'cns_24hours','renal_24hours','any_vaso','amount','weight','imputed_TV_standardized',
                       'opioid','benzo','propofol','dex','unarousable','deep','moderate','light','restless','agitated')))
apply(data[,L],2,function(x)mean(is.na(x)))>.1
for(col in L){
  if(is.numeric(data[,col])|is.integer(data[,col])|is.logical(data[,col])){
    data[is.na(data[,col]),col] = median(data[,col],na.rm=T)
  }
  if(is.factor(data[,col])){
    val <- unique(data[!is.na(data[,col]),col]) 
    mode <- val[which.max(tabulate(match(data[,col], val)))]
    data[is.na(data[,col]),col] = mode
  }
}

for(col in grep('last',L,value=T)){
  m = mean(data[,col],na.rm=T)
  s = sd(data[,col],na.rm=T)
  data[,col] = (data[,col] - m)/s
}

for(col in grep('last',L,value=T)){
  data[,col] = pmax(-5,pmin(data[,col],5)) 
}
library(arm)



low_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio<=150])
low_pfratio_pats = low_pfratio_pats[!is.na(low_pfratio_pats)]

high_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio>=300])
high_pfratio_pats = high_pfratio_pats[!is.na(high_pfratio_pats)]

high_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours>=12])
high_sofa_pats = high_sofa_pats[!is.na(high_sofa_pats)]

low_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours<=5])
low_sofa_pats = low_sofa_pats[!is.na(low_sofa_pats)]

pulmonary_edema = read.csv("~/icu_data/mimic4_v1/mimic_iv_cxr_reports_labeled_notext.csv")
pulmonary_edema = pulmonary_edema[(pulmonary_edema$Edema==1|pulmonary_edema$Edema==-1) & pulmonary_edema$Cardiomegaly!=1,]
pulmonary_edema = pulmonary_edema[!is.na(pulmonary_edema$Edema),]
pulmonary_edema = merge(pulmonary_edema,cohort[,c("subject_id","hadm_id","stay_id","intime","outtime")])
pulmonary_edema$charthour = as.numeric(difftime(strptime(as.character(pulmonary_edema$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(pulmonary_edema$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
pulmonary_edema$outhour = as.numeric(difftime(strptime(as.character(pulmonary_edema$outtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(pulmonary_edema$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
pulmonary_edema = pulmonary_edema[pulmonary_edema$charthour<pulmonary_edema$outhour,]
pulmonary_edema = pulmonary_edema[,c("stay_id","charthour","Edema")]
pulmonary_edema = pulmonary_edema %>%
  group_by(stay_id,Edema) %>%
  summarise_all(min,na.rm=T)
pulmonary_edema$hour = pmax(1,ceiling(pulmonary_edema$charthour))
names(pulmonary_edema)[2] = 'pulmonary_edema'
pulmonary_edema$pulmonary_edema=1
data = merge(data,pulmonary_edema[,c("stay_id","hour","pulmonary_edema")],all.x=T)
data$pulmonary_edema[is.na(data$pulmonary_edema)]=0
data$pulmonary_edema = unlist(lapply(split(data$pulmonary_edema,data$stay_id),cumsum))
ards_pats = unique(data$stay_id[data$pao2fio2ratio<=300 & data$pulmonary_edema==1])
ards_pats = ards_pats[!is.na(ards_pats)]


data_pf = data[data$stay_id %in% low_pfratio_pats,]
data_pf_high = data[data$stay_id %in% high_pfratio_pats,]
data_sofa = data[data$stay_id %in% high_sofa_pats,]
data_sofa_low = data[data$stay_id %in% low_sofa_pats,]
data_ards = data[data$stay_id %in% ards_pats,]

L = c(L,'pulmonary_edema')
wean_mod2 = bayesglm(wean~.,data = data[,c(L,'wean')],family='binomial')
wean_mod = glm(wean~.,data = data[,c(L,'wean')],family='binomial')

data$phat = predict(wean_mod,data,type='response')
bins = rep(NA,10)
for(i in 1:10){
  bins[i] = mean(data$wean[data$phat>(.01*(i-1)) & data$phat<(.01*i)])
}
plot(seq(.005,.095,by=.01),bins,xlim=c(0,.1),ylim=c(0,.1))
curve(1*x,from=0,to=1,add=T)

cal_mod = isoreg(y=data$wean,x=data$phat)
yf = cal_mod$yf
yf[order(data$phat)] = cal_mod$yf
data$phat_cal = yf
bins2 = rep(NA,10)
for(i in 1:10){
  bins2[i] = mean(data$wean[data$phat_cal>(.01*(i-1)) & data$phat_cal<(.01*i)])
}
plot(seq(.005,.095,by=.01),bins2,xlim=c(0,.1),ylim=c(0,.1))
curve(1*x,from=0,to=1,add=T)

wean_mod_pf = glm(wean~.,data = data_pf[,c(L,'wean')],family='binomial')
data_pf$phat = predict(wean_mod_pf,data_pf,type='response')
cal_mod_pf = isoreg(y=data_pf$wean,x=data_pf$phat)
yf = cal_mod_pf$yf
yf[order(data_pf$phat)] = cal_mod_pf$yf
data_pf$phat_cal = yf

wean_mod_pf_high = glm(wean~.,data = data_pf_high[,c(L,'wean')],family='binomial')
data_pf_high$phat = predict(wean_mod_pf_high,data_pf_high,type='response')
cal_mod_pf_high = isoreg(y=data_pf_high$wean,x=data_pf_high$phat)
yf = cal_mod_pf_high$yf
yf[order(data_pf_high$phat)] = cal_mod_pf_high$yf
data_pf_high$phat_cal = yf

wean_mod_sofa = glm(wean~.,data = data_sofa[,c(L,'wean')],family='binomial')
data_sofa$phat = predict(wean_mod_sofa,data_sofa,type='response')
cal_mod_sofa = isoreg(y=data_sofa$wean,x=data_sofa$phat)
yf = cal_mod_sofa$yf
yf[order(data_sofa$phat)] = cal_mod_sofa$yf
data_sofa$phat_cal = yf

wean_mod_sofa_low = glm(wean~.,data = data_sofa_low[,c(L,'wean')],family='binomial')
data_sofa_low$phat = predict(wean_mod_sofa_low,data_sofa_low,type='response')
cal_mod_sofa_low = isoreg(y=data_sofa_low$wean,x=data_sofa_low$phat)
yf = cal_mod_sofa_low$yf
yf[order(data_sofa_low$phat)] = cal_mod_sofa_low$yf
data_sofa_low$phat_cal = yf

wean_mod_ards = glm(wean~.,data = data_ards[,c(L,'wean')],family='binomial')
data_ards$phat = predict(wean_mod_ards,data_ards,type='response')
cal_mod_ards = isoreg(y=data_ards$wean,x=data_ards$phat)
yf = cal_mod_ards$yf
yf[order(data_ards$phat)] = cal_mod_ards$yf
data_ards$phat_cal = yf

#make weights
deltas = seq(.5,4,by=.1)
for(delta in deltas){
  data[,paste0("weights",delta)] = (data$wean*delta + 1 - data$wean)/(delta*data$phat_cal + 1 - data$phat_cal)
}

for(delta in deltas){
  data_pf[,paste0("weights",delta)] = (data_pf$wean*delta + 1 - data_pf$wean)/(delta*data_pf$phat_cal + 1 - data_pf$phat_cal)
}

for(delta in deltas){
  data_pf_high[,paste0("weights",delta)] = (data_pf_high$wean*delta + 1 - data_pf_high$wean)/(delta*data_pf_high$phat_cal + 1 - data_pf_high$phat_cal)
}

for(delta in deltas){
  data_sofa[,paste0("weights",delta)] = (data_sofa$wean*delta + 1 - data_sofa$wean)/(delta*data_sofa$phat_cal + 1 - data_sofa$phat_cal)
}

for(delta in deltas){
  data_sofa_low[,paste0("weights",delta)] = (data_sofa_low$wean*delta + 1 - data_sofa_low$wean)/(delta*data_sofa_low$phat_cal + 1 - data_sofa_low$phat_cal)
}

for(delta in deltas){
  data_ards[,paste0("weights",delta)] = (data_ards$wean*delta + 1 - data_ards$wean)/(delta*data_ards$phat_cal + 1 - data_ards$phat_cal)
}

Weights = data.frame(matrix(NA,nrow = length(unique(data$stay_id)),ncol=length(deltas)))
names(Weights) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data[,paste0('weights',deltas[i])],by=list(data$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights[,i] = unlist(temp[,2])
  Weights[,i] = pmin(Weights[,i],quantile(Weights[,i],.99))
}

Weights_pf = data.frame(matrix(NA,nrow = length(unique(data_pf$stay_id)),ncol=length(deltas)))
names(Weights_pf) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data_pf[,paste0('weights',deltas[i])],by=list(data_pf$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights_pf[,i] = unlist(temp[,2])
  Weights_pf[,i] = pmin(Weights_pf[,i],quantile(Weights_pf[,i],.99))
}

Weights_pf_high = data.frame(matrix(NA,nrow = length(unique(data_pf_high$stay_id)),ncol=length(deltas)))
names(Weights_pf_high) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data_pf_high[,paste0('weights',deltas[i])],by=list(data_pf_high$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights_pf_high[,i] = unlist(temp[,2])
  Weights_pf_high[,i] = pmin(Weights_pf_high[,i],quantile(Weights_pf_high[,i],.99))
}

Weights_sofa = data.frame(matrix(NA,nrow = length(unique(data_sofa$stay_id)),ncol=length(deltas)))
names(Weights_sofa) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data_sofa[,paste0('weights',deltas[i])],by=list(data_sofa$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights_sofa[,i] = unlist(temp[,2])
  Weights_sofa[,i] = pmin(Weights_sofa[,i],quantile(Weights_sofa[,i],.99))
}

Weights_sofa_low = data.frame(matrix(NA,nrow = length(unique(data_sofa_low$stay_id)),ncol=length(deltas)))
names(Weights_sofa_low) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data_sofa_low[,paste0('weights',deltas[i])],by=list(data_sofa_low$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights_sofa_low[,i] = unlist(temp[,2])
  Weights_sofa_low[,i] = pmin(Weights_sofa_low[,i],quantile(Weights_sofa_low[,i],.99))
}

Weights_ards = data.frame(matrix(NA,nrow = length(unique(data_ards$stay_id)),ncol=length(deltas)))
names(Weights_ards) = paste0("weights",deltas)

for(i in 1:length(deltas)){
  temp = aggregate(data_ards[,paste0('weights',deltas[i])],by=list(data_ards$stay_id),prod)
  temp = temp[order(temp[,1]),]
  Weights_ards[,i] = unlist(temp[,2])
  Weights_ards[,i] = pmin(Weights_ards[,i],quantile(Weights_ards[,i],.99))
}


apply(Weights,2,max)
apply(Weights_pf,2,max)
apply(Weights_pf_high,2,max)
apply(Weights_sofa,2,max)
apply(Weights_sofa_low,2,max)
apply(Weights_ards,2,max)
#get outcomes
#time to weaning
wean_time = cbind(stay_id=unique(data$stay_id),wean_hour=NA)
wean_table = data[data$wean==1,c("stay_id","hour_baseline")]
wean_time = merge(wean_time,wean_table,all.x=T)
wean_time = wean_time[,c(1,3)]
names(wean_time)[2] = 'wean_hour'
wean_time$wean_hour[is.na(wean_time$wean_hour)] = 720
hist(wean_time$wean_hour)

wean_time = wean_time[order(wean_time$stay_id),]

counterfactual_wean_times1 = rep(NA,length(deltas))
counterfactual_wean_times1_pf = rep(NA,length(deltas))
counterfactual_wean_times1_pf_high = rep(NA,length(deltas))
counterfactual_wean_times1_sofa = rep(NA,length(deltas))
counterfactual_wean_times1_sofa_low = rep(NA,length(deltas))
counterfactual_wean_times1_ards = rep(NA,length(deltas))

wean_time_pf = wean_time[wean_time$stay_id%in%low_pfratio_pats,]
wean_time_pf_high = wean_time[wean_time$stay_id%in%high_pfratio_pats,]
wean_time_sofa = wean_time[wean_time$stay_id%in%high_sofa_pats,]
wean_time_sofa_low = wean_time[wean_time$stay_id%in%low_sofa_pats,]
wean_time_ards = wean_time[wean_time$stay_id%in%ards_pats,]

for(i in 1:length(deltas)){
  counterfactual_wean_times1[i] = mean(Weights[,i]*wean_time$wean_hour) 
  counterfactual_wean_times1_pf[i] = mean(Weights_pf[,i]*wean_time_pf$wean_hour) 
  counterfactual_wean_times1_pf_high[i] = mean(Weights_pf_high[,i]*wean_time_pf_high$wean_hour) 
  counterfactual_wean_times1_sofa[i] = mean(Weights_sofa[,i]*wean_time_sofa$wean_hour) 
  counterfactual_wean_times1_sofa_low[i] = mean(Weights_sofa_low[,i]*wean_time_sofa_low$wean_hour) 
  # counterfactual_wean_times1_ards[i] = mean(Weights_ards[,i]*wean_time_ards$wean_hour) 
}
pdf('temp.pdf')
plot(deltas,counterfactual_wean_times1)
points(deltas,counterfactual_wean_times1_pf,col=2)
points(deltas,counterfactual_wean_times1_pf_high,col=3)
points(deltas,counterfactual_wean_times1_sofa,col=4)
points(deltas,counterfactual_wean_times1_sofa_low,col=5)
points(deltas,counterfactual_wean_times1_ards,col=4)
dev.off()

#mortality
mortality_table = data[data$hour_baseline==1,c("stay_id","hospital_expire_flag")]
mortality_table = mortality_table[order(mortality_table$stay_id),]
mortality_table_pf = mortality_table[mortality_table$stay_id%in%low_pfratio_pats,]
mortality_table_pf_high = mortality_table[mortality_table$stay_id%in%high_pfratio_pats,]
mortality_table_sofa = mortality_table[mortality_table$stay_id%in%high_sofa_pats,]
mortality_table_sofa_low = mortality_table[mortality_table$stay_id%in%low_sofa_pats,]

mortality_table_ards = mortality_table[mortality_table$stay_id%in%ards_pats,]

counterfactual_mortality_rates1 = rep(NA,length(deltas))
counterfactual_mortality_rates1_pf = rep(NA,length(deltas))
counterfactual_mortality_rates1_pf_high = rep(NA,length(deltas))
counterfactual_mortality_rates1_sofa = rep(NA,length(deltas))
counterfactual_mortality_rates1_sofa_low = rep(NA,length(deltas))
counterfactual_mortality_rates1_ards = rep(NA,length(deltas))

for(i in 1:length(deltas)){
  counterfactual_mortality_rates1[i] = mean(Weights[,i]*mortality_table$hospital_expire_flag) 
  counterfactual_mortality_rates1_pf[i] = mean(Weights_pf[,i]*mortality_table_pf$hospital_expire_flag) 
  counterfactual_mortality_rates1_pf_high[i] = mean(Weights_pf_high[,i]*mortality_table_pf_high$hospital_expire_flag) 
  counterfactual_mortality_rates1_sofa[i] = mean(Weights_sofa[,i]*mortality_table_sofa$hospital_expire_flag) 
  counterfactual_mortality_rates1_sofa_low[i] = mean(Weights_sofa_low[,i]*mortality_table_sofa_low$hospital_expire_flag) 
  # counterfactual_mortality_rates1_ards[i] = mean(Weights_ards[,i]*mortality_table_ards$hospital_expire_flag) 
}
pdf('temp.pdf')
plot(deltas,counterfactual_mortality_rates1,ylim=c(.2,.6))
points(deltas[which(deltas==1)],counterfactual_mortality_rates1[which(deltas==1)],col=2)
points(deltas,counterfactual_mortality_rates1_pf,col=3)
points(deltas,counterfactual_mortality_rates1_pf_high,col=4)
points(deltas,counterfactual_mortality_rates1_sofa,col=5)
points(deltas,counterfactual_mortality_rates1_sofa_low,col=6)
dev.off()

#icu free days
cohort = read.csv("~/icu_data/mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("~/icu_data/mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
admissions = read.csv("~/icu_data/mimic4_v1/admissions.csv",header=T)
cohort = merge(cohort,admissions)

baseline_hour_table = merge(cohort[,c("stay_id","intime","outtime","subject_id")],data[data$hour_baseline==1,c("stay_id","baseline_hour")])
baseline_hour_table$baseline_time = 3600*baseline_hour_table$baseline_hour + strptime(as.character(baseline_hour_table$intime), "%Y-%m-%d %H:%M:%S")
icu_duration_table = merge(cohort,unique(baseline_hour_table[,c("subject_id","baseline_time")]))
icu_duration_table$max_time = icu_duration_table$baseline_time + 3600*24*30
icu_duration_table = icu_duration_table[as.numeric(difftime(strptime(as.character(icu_duration_table$outtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(icu_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]
icu_duration_table = icu_duration_table[as.numeric(difftime(strptime(as.character(icu_duration_table$max_time), "%Y-%m-%d %H:%M:%S"), strptime(as.character(icu_duration_table$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]

icu_duration_table$icu_stay_durations = as.numeric(difftime(pmin(strptime(as.character(icu_duration_table$outtime), "%Y-%m-%d %H:%M:%S"),icu_duration_table$max_time), pmax(strptime(as.character(icu_duration_table$intime), "%Y-%m-%d %H:%M:%S"),icu_duration_table$baseline_time),units="days"))
icu_duration_table$death_day = pmax(0,as.numeric(difftime(strptime(as.character(icu_duration_table$deathtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(icu_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days")))
icu_duration_table$death_day[is.na(icu_duration_table$death_day)] = 100000
icu_duration_table$max = pmin(30,icu_duration_table$death_day)
max_table = unique(icu_duration_table[,c("subject_id","max")])
max_table = max_table %>%
  group_by(subject_id) %>%
  summarise_all(min,na.rm=T)

duration_table = aggregate(icu_duration_table$icu_stay_durations,by=list(icu_duration_table$subject_id),sum)
names(duration_table) = c('subject_id','icu_duration')
icu_outcome_table = merge(duration_table,max_table)
icu_outcome_table$outcome = ceiling(icu_outcome_table$max - icu_outcome_table$icu_duration)
icu_outcome_table = merge(icu_outcome_table,subjects[,c('subject_id','stay_id')])
icu_outcome_table= icu_outcome_table[order(icu_outcome_table$stay_id),]

icu_outcome_table_pf= icu_outcome_table[icu_outcome_table$stay_id %in% low_pfratio_pats,]
icu_outcome_table_pf_high= icu_outcome_table[icu_outcome_table$stay_id %in% high_pfratio_pats,]
icu_outcome_table_sofa= icu_outcome_table[icu_outcome_table$stay_id %in% high_sofa_pats,]
icu_outcome_table_sofa_low= icu_outcome_table[icu_outcome_table$stay_id %in% low_sofa_pats,]
icu_outcome_table_ards= icu_outcome_table[icu_outcome_table$stay_id %in% ards_pats,]

counterfactual_icu_free_days1 = rep(NA,length(deltas))
counterfactual_icu_free_days1_pf = rep(NA,length(deltas))
counterfactual_icu_free_days1_pf_high = rep(NA,length(deltas))
counterfactual_icu_free_days1_sofa = rep(NA,length(deltas))
counterfactual_icu_free_days1_sofa_low = rep(NA,length(deltas))
counterfactual_icu_free_days1_ards = rep(NA,length(deltas))

for(i in 1:length(deltas)){
  counterfactual_icu_free_days1[i] = mean(Weights[,i]*icu_outcome_table$outcome) 
  counterfactual_icu_free_days1_pf[i] = mean(Weights_pf[,i]*icu_outcome_table_pf$outcome) 
  counterfactual_icu_free_days1_pf_high[i] = mean(Weights_pf_high[,i]*icu_outcome_table_pf_high$outcome) 
  counterfactual_icu_free_days1_sofa[i] = mean(Weights_sofa[,i]*icu_outcome_table_sofa$outcome)
  counterfactual_icu_free_days1_sofa_low[i] = mean(Weights_sofa_low[,i]*icu_outcome_table_sofa_low$outcome)
  # counterfactual_icu_free_days1_ards[i] = mean(Weights_ards[,i]*icu_outcome_table_ards$outcome) 
}
pdf('temp.pdf')
plot(deltas,counterfactual_icu_free_days1,ylim=c(6,20))
points(deltas[which(deltas==1)],counterfactual_icu_free_days1[which(deltas==1)],col=2)
points(deltas,counterfactual_icu_free_days1_pf,col=3)
points(deltas,counterfactual_icu_free_days1_pf_high,col=4)

points(deltas,counterfactual_icu_free_days1_sofa,col=5)
points(deltas,counterfactual_icu_free_days1_sofa_low,col=6)

points(deltas,counterfactual_icu_free_days1_ards,col=5)
dev.off()

#ventilator free days
cohort = read.csv("~/icu_data/mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("~/icu_data/mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
admissions = read.csv("~/icu_data/mimic4_v1/admissions.csv",header=T)
cohort = merge(cohort,admissions)
vent = read.csv("~/icu_data/mimic4_v1/ventilator.csv")

vent_duration_table = merge(vent,baseline_hour_table[,c("stay_id","subject_id","baseline_time")])
vent_duration_table = vent_duration_table[vent_duration_table$ventilation_status %in% c("InvasiveVent","Trach"),]
vent_duration_table$max_time = vent_duration_table$baseline_time + 3600*24*30

vent_duration_table = vent_duration_table[as.numeric(difftime(strptime(as.character(vent_duration_table$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]
vent_duration_table = vent_duration_table[as.numeric(difftime(strptime(as.character(vent_duration_table$max_time), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$starttime), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]
vent_duration_table$startday = as.numeric(difftime(strptime(as.character(vent_duration_table$starttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))
vent_duration_table$endday = as.numeric(difftime(strptime(as.character(vent_duration_table$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))
vent_duration_table$startday_ceiling = pmax(1,ceiling(vent_duration_table$startday))
vent_duration_table$endday_ceiling = pmin(30,ceiling(vent_duration_table$endday))
from_to = function(a,b){
  a:b
}
vent_duration_table$vent_days = mapply(from_to,vent_duration_table$startday_ceiling,vent_duration_table$endday_ceiling)
death_cohort = cohort[cohort$deathtime!='',]

baseline_hour_table = merge(baseline_hour_table,unique(death_cohort[,c("subject_id","deathtime")]),all.x=T)
baseline_hour_table$death_day = ceiling(pmax(1,as.numeric(difftime(strptime(as.character(baseline_hour_table$deathtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(baseline_hour_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))))
baseline_hour_table$death_day[is.na(baseline_hour_table$death_day)] = 10000000


pats = unique(vent_duration_table$stay_id)

vent_days = vector(mode='list',length=length(pats))
for(i in 1:length(pats)){
  vent_days[[i]] = unique(unlist(c(vent_duration_table$vent_days[vent_duration_table$stay_id==pats[i]])))
}

vent_days_table = cbind(stay_id = pats,vent_days=vent_days)
baseline_hour_table = merge(baseline_hour_table,vent_days_table,all.x=T)
from_to_death = function(a){
  if(a<=30){
   a:30
  }else{
    NA
  }
}
baseline_hour_table$death_days = lapply(baseline_hour_table$death_day,from_to_death)
get_vent_free_days = function(i){
  30-ifelse(baseline_hour_table$death_day[i]<=30,length(unique(c(baseline_hour_table$vent_days[[i]],baseline_hour_table$death_days[[i]]))),length(unique(baseline_hour_table$vent_days[[i]])))
}
baseline_hour_table$vent_free_days = sapply(1:nrow(baseline_hour_table),get_vent_free_days)
baseline_hour_table = baseline_hour_table[order(baseline_hour_table$stay_id),]
baseline_hour_table_pf = baseline_hour_table[baseline_hour_table$stay_id%in%low_pfratio_pats,]
baseline_hour_table_pf_high = baseline_hour_table[baseline_hour_table$stay_id%in%high_pfratio_pats,]
baseline_hour_table_sofa = baseline_hour_table[baseline_hour_table$stay_id%in%high_sofa_pats,]
baseline_hour_table_sofa_low = baseline_hour_table[baseline_hour_table$stay_id%in%low_sofa_pats,]
baseline_hour_table_ards = baseline_hour_table[baseline_hour_table$stay_id%in%ards_pats,]

counterfactual_vent_free_days1 = rep(NA,length(deltas))
counterfactual_vent_free_days1_pf = rep(NA,length(deltas))
counterfactual_vent_free_days1_pf_high = rep(NA,length(deltas))

counterfactual_vent_free_days1_sofa = rep(NA,length(deltas))
counterfactual_vent_free_days1_sofa_low = rep(NA,length(deltas))
counterfactual_vent_free_days1_ards = rep(NA,length(deltas))

for(i in 1:length(deltas)){
  counterfactual_vent_free_days1[i] = mean(Weights[,i]*baseline_hour_table$vent_free_days) 
  counterfactual_vent_free_days1_pf[i] = mean(Weights_pf[,i]*baseline_hour_table_pf$vent_free_days) 
  counterfactual_vent_free_days1_pf_high[i] = mean(Weights_pf_high[,i]*baseline_hour_table_pf_high$vent_free_days) 
  counterfactual_vent_free_days1_sofa[i] = mean(Weights_sofa[,i]*baseline_hour_table_sofa$vent_free_days) 
  counterfactual_vent_free_days1_sofa_low[i] = mean(Weights_sofa_low[,i]*baseline_hour_table_sofa_low$vent_free_days) 
  # counterfactual_vent_free_days1_ards[i] = mean(Weights_ards[,i]*baseline_hour_table_ards$vent_free_days) 
}
plot(deltas,counterfactual_vent_free_days1,ylim=c(12,20))
points(deltas[which(deltas==1)],counterfactual_vent_free_days1[which(deltas==1)],col=2)
plot(deltas,counterfactual_vent_free_days1_pf,col=3)
plot(deltas,counterfactual_vent_free_days1_pf_high,col=3)
plot(deltas,counterfactual_vent_free_days1_sofa,col=4)
plot(deltas,counterfactual_vent_free_days1_sofa_low,col=4)
plot(deltas,counterfactual_vent_free_days1_ards,col=5)


nboot = 500
data_orig = data
pats = unique(data$stay_id)
lengths = aggregate(data$stay_id,by=list(data$stay_id),length)[,2]
ind_cuts = c(0,cumsum(lengths))
inds = lapply(1:length(lengths),function(i) (ind_cuts[i]+1):ind_cuts[i+1])
counterfactual_icu_free_days_list = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list = vector(mode='list',length=nboot)
counterfactual_wean_times_list = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list = vector(mode='list',length=nboot)

counterfactual_icu_free_days_list_pf = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list_pf = vector(mode='list',length=nboot)
counterfactual_wean_times_list_pf = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list_pf = vector(mode='list',length=nboot)

counterfactual_icu_free_days_list_pf_high = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list_pf_high = vector(mode='list',length=nboot)
counterfactual_wean_times_list_pf_high = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list_pf_high = vector(mode='list',length=nboot)

counterfactual_icu_free_days_list_sofa = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list_sofa = vector(mode='list',length=nboot)
counterfactual_wean_times_list_sofa = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list_sofa = vector(mode='list',length=nboot)

counterfactual_icu_free_days_list_sofa_low = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list_sofa_low = vector(mode='list',length=nboot)
counterfactual_wean_times_list_sofa_low = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list_sofa_low = vector(mode='list',length=nboot)

counterfactual_icu_free_days_list_ards = vector(mode='list',length=nboot)
counterfactual_mortality_rates_list_ards = vector(mode='list',length=nboot)
counterfactual_wean_times_list_ards = vector(mode='list',length=nboot)
counterfactual_vent_free_days_list_ards = vector(mode='list',length=nboot)

low_pfratio_pat_index = which(pats %in% low_pfratio_pats)
high_pfratio_pat_index = which(pats %in% high_pfratio_pats)
high_sofa_pat_index = which(pats %in% high_sofa_pats)
low_sofa_pat_index = which(pats %in% low_sofa_pats)

ards_pat_index = which(pats %in% ards_pats)

for(b in 1:nboot){
  set.seed(b)
  samp_pats = sample(1:length(pats),length(pats),replace=T)
  pat_inds = unlist(inds[samp_pats])
  data = data_orig[pat_inds,]
  data$stay_id = factor(rep(1:length(samp_pats),lengths[samp_pats]))
  # wean_mod2 = bayesglm(wean~.,data = data[,c(L,'wean')],family='binomial')
  wean_mod = glm(wean~.,data = data[,c(L,'wean')],family='binomial')

  data$phat = predict(wean_mod,data,type='response')
  bins = rep(NA,10)
  # for(i in 1:10){
  #   bins[i] = mean(data$wean[data$phat>(.01*(i-1)) & data$phat<(.01*i)])
  # }
  # plot(seq(.005,.095,by=.01),bins,xlim=c(0,.1),ylim=c(0,.1))
  # curve(1*x,from=0,to=1,add=T)
  
  cal_mod = isoreg(y=data$wean,x=data$phat)
  yf = cal_mod$yf
  yf[order(data$phat)] = cal_mod$yf
  data$phat_cal = yf
  
  low_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio<=150])
  low_pfratio_pats = low_pfratio_pats[!is.na(low_pfratio_pats)]
  
  high_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio>=300])
  high_pfratio_pats = high_pfratio_pats[!is.na(high_pfratio_pats)]
  
  high_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours>=12])
  high_sofa_pats = high_sofa_pats[!is.na(high_sofa_pats)]
  
  low_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours<=5])
  low_sofa_pats = low_sofa_pats[!is.na(low_sofa_pats)]
  
  ards_pats = unique(data$stay_id[data$pao2fio2ratio<=300 & data$pulmonary_edema==1])
  ards_pats = ards_pats[!is.na(ards_pats)]
  
  data_pf = data[data$stay_id %in% low_pfratio_pats,]
  data_pf_high = data[data$stay_id %in% high_pfratio_pats,]
  data_sofa = data[data$stay_id %in% high_sofa_pats,]
  data_sofa_low = data[data$stay_id %in% low_sofa_pats,]
  data_ards = data[data$stay_id %in% ards_pats,]
  
  wean_mod_pf = glm(wean~.,data = data_pf[,c(L,'wean')],family='binomial')
  data_pf$phat = predict(wean_mod_pf,data_pf,type='response')
  cal_mod_pf = isoreg(y=data_pf$wean,x=data_pf$phat)
  yf = cal_mod_pf$yf
  yf[order(data_pf$phat)] = cal_mod_pf$yf
  data_pf$phat_cal = yf
  
  wean_mod_pf_high = glm(wean~.,data = data_pf_high[,c(L,'wean')],family='binomial')
  data_pf_high$phat = predict(wean_mod_pf_high,data_pf_high,type='response')
  cal_mod_pf_high = isoreg(y=data_pf_high$wean,x=data_pf_high$phat)
  yf = cal_mod_pf_high$yf
  yf[order(data_pf_high$phat)] = cal_mod_pf_high$yf
  data_pf_high$phat_cal = yf
  
  wean_mod_sofa = glm(wean~.,data = data_sofa[,c(L,'wean')],family='binomial')
  data_sofa$phat = predict(wean_mod_sofa,data_sofa,type='response')
  cal_mod_sofa = isoreg(y=data_sofa$wean,x=data_sofa$phat)
  yf = cal_mod_sofa$yf
  yf[order(data_sofa$phat)] = cal_mod_sofa$yf
  data_sofa$phat_cal = yf
  
  wean_mod_sofa_low = glm(wean~.,data = data_sofa_low[,c(L,'wean')],family='binomial')
  data_sofa_low$phat = predict(wean_mod_sofa_low,data_sofa_low,type='response')
  cal_mod_sofa_low = isoreg(y=data_sofa_low$wean,x=data_sofa_low$phat)
  yf = cal_mod_sofa_low$yf
  yf[order(data_sofa_low$phat)] = cal_mod_sofa_low$yf
  data_sofa_low$phat_cal = yf
  
  wean_mod_ards = glm(wean~.,data = data_ards[,c(L,'wean')],family='binomial')
  data_ards$phat = predict(wean_mod_ards,data_ards,type='response')
  cal_mod_ards = isoreg(y=data_ards$wean,x=data_ards$phat)
  yf = cal_mod_ards$yf
  yf[order(data_ards$phat)] = cal_mod_ards$yf
  data_ards$phat_cal = yf
  
  #make weights
  deltas = seq(.5,4,by=.1)
  for(delta in deltas){
    data[,paste0("weights",delta)] = (data$wean*delta + 1 - data$wean)/(delta*data$phat_cal + 1 - data$phat_cal)
    data_pf[,paste0("weights",delta)] = (data_pf$wean*delta + 1 - data_pf$wean)/(delta*data_pf$phat_cal + 1 - data_pf$phat_cal)
    data_pf_high[,paste0("weights",delta)] = (data_pf_high$wean*delta + 1 - data_pf_high$wean)/(delta*data_pf_high$phat_cal + 1 - data_pf_high$phat_cal)
    data_sofa[,paste0("weights",delta)] = (data_sofa$wean*delta + 1 - data_sofa$wean)/(delta*data_sofa$phat_cal + 1 - data_sofa$phat_cal)
    data_sofa_low[,paste0("weights",delta)] = (data_sofa_low$wean*delta + 1 - data_sofa_low$wean)/(delta*data_sofa_low$phat_cal + 1 - data_sofa_low$phat_cal)
  }
  
  Weights = data.frame(matrix(NA,nrow = length(unique(data$stay_id)),ncol=length(deltas)))
  names(Weights) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data[,paste0('weights',deltas[i])],by=list(data$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights[,i] = unlist(temp[,2])
    Weights[,i] = pmin(Weights[,i],quantile(Weights[,i],.99))
  }
  
  Weights_pf = data.frame(matrix(NA,nrow = length(unique(data_pf$stay_id)),ncol=length(deltas)))
  names(Weights_pf) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data_pf[,paste0('weights',deltas[i])],by=list(data_pf$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights_pf[,i] = unlist(temp[,2])
    Weights_pf[,i] = pmin(Weights_pf[,i],quantile(Weights_pf[,i],.99))
  }
  
  Weights_pf_high = data.frame(matrix(NA,nrow = length(unique(data_pf_high$stay_id)),ncol=length(deltas)))
  names(Weights_pf_high) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data_pf_high[,paste0('weights',deltas[i])],by=list(data_pf_high$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights_pf_high[,i] = unlist(temp[,2])
    Weights_pf_high[,i] = pmin(Weights_pf_high[,i],quantile(Weights_pf_high[,i],.99))
  }
  
  Weights_sofa = data.frame(matrix(NA,nrow = length(unique(data_sofa$stay_id)),ncol=length(deltas)))
  names(Weights_sofa) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data_sofa[,paste0('weights',deltas[i])],by=list(data_sofa$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights_sofa[,i] = unlist(temp[,2])
    Weights_sofa[,i] = pmin(Weights_sofa[,i],quantile(Weights_sofa[,i],.99))
  }
  
  Weights_sofa_low = data.frame(matrix(NA,nrow = length(unique(data_sofa_low$stay_id)),ncol=length(deltas)))
  names(Weights_sofa_low) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data_sofa_low[,paste0('weights',deltas[i])],by=list(data_sofa_low$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights_sofa_low[,i] = unlist(temp[,2])
    Weights_sofa_low[,i] = pmin(Weights_sofa_low[,i],quantile(Weights_sofa_low[,i],.99))
  }
  
  Weights_ards = data.frame(matrix(NA,nrow = length(unique(data_ards$stay_id)),ncol=length(deltas)))
  names(Weights_ards) = paste0("weights",deltas)
  
  for(i in 1:length(deltas)){
    temp = aggregate(data_ards[,paste0('weights',deltas[i])],by=list(data_ards$stay_id),prod)
    temp = temp[order(temp[,1]),]
    Weights_ards[,i] = unlist(temp[,2])
    Weights_ards[,i] = pmin(Weights_ards[,i],quantile(Weights_ards[,i],.99))
  }
  
  
  #get outcomes
  #time to weaning
  wean_time = cbind(stay_id=unique(data$stay_id),wean_hour=NA)
  wean_table = data[data$wean==1,c("stay_id","hour_baseline")]
  wean_time = merge(wean_time,wean_table,all.x=T)
  wean_time = wean_time[,c(1,3)]
  names(wean_time)[2] = 'wean_hour'
  wean_time$wean_hour[is.na(wean_time$wean_hour)] = 720
  wean_time = wean_time[order(wean_time$stay_id),]
  # hist(wean_time$wean_hour)
  
  counterfactual_wean_times = rep(NA,length(deltas))
  counterfactual_wean_times_pf = rep(NA,length(deltas))
  counterfactual_wean_times_pf_high = rep(NA,length(deltas))
  counterfactual_wean_times_sofa = rep(NA,length(deltas))
  counterfactual_wean_times_sofa_low = rep(NA,length(deltas))
  counterfactual_wean_times_ards = rep(NA,length(deltas))
  
  wean_time_pf = wean_time[wean_time$stay_id%in%low_pfratio_pats,]
  wean_time_pf_high = wean_time[wean_time$stay_id%in%high_pfratio_pats,]
  wean_time_sofa = wean_time[wean_time$stay_id%in%high_sofa_pats,]
  wean_time_sofa_low = wean_time[wean_time$stay_id%in%low_sofa_pats,]
  wean_time_ards = wean_time[wean_time$stay_id%in%ards_pats,]
  
  for(i in 1:length(deltas)){
    counterfactual_wean_times[i] = mean(Weights[,i]*wean_time$wean_hour) 
    counterfactual_wean_times_pf[i] = mean(Weights_pf[,i]*wean_time_pf$wean_hour) 
    counterfactual_wean_times_pf_high[i] = mean(Weights_pf_high[,i]*wean_time_pf_high$wean_hour) 
    counterfactual_wean_times_sofa[i] = mean(Weights_sofa[,i]*wean_time_sofa$wean_hour) 
    counterfactual_wean_times_sofa_low[i] = mean(Weights_sofa_low[,i]*wean_time_sofa_low$wean_hour) 
    counterfactual_wean_times_ards[i] = mean(Weights_ards[,i]*wean_time_ards$wean_hour) 
  }
  counterfactual_wean_times_list[[b]] = counterfactual_wean_times
  counterfactual_wean_times_list_pf[[b]] = counterfactual_wean_times_pf
  counterfactual_wean_times_list_pf_high[[b]] = counterfactual_wean_times_pf_high
  counterfactual_wean_times_list_sofa[[b]] = counterfactual_wean_times_sofa
  counterfactual_wean_times_list_sofa_low[[b]] = counterfactual_wean_times_sofa_low
  counterfactual_wean_times_list_ards[[b]] = counterfactual_wean_times_ards
  
  # plot(deltas,counterfactual_wean_times)
  
  #mortality
  mortality_table = data[data$hour_baseline==1,c("stay_id","hospital_expire_flag")]
  mortality_table = mortality_table[order(mortality_table$stay_id),]
  mortality_table_pf = mortality_table[mortality_table$stay_id%in%low_pfratio_pats,]
  mortality_table_pf_high = mortality_table[mortality_table$stay_id%in%high_pfratio_pats,]
  mortality_table_sofa = mortality_table[mortality_table$stay_id%in%high_sofa_pats,]
  mortality_table_sofa_low = mortality_table[mortality_table$stay_id%in%low_sofa_pats,]
  mortality_table_ards = mortality_table[mortality_table$stay_id%in%ards_pats,]
  
  counterfactual_mortality_rates = rep(NA,length(deltas))
  counterfactual_mortality_rates_pf = rep(NA,length(deltas))
  counterfactual_mortality_rates_pf_high = rep(NA,length(deltas))
  counterfactual_mortality_rates_sofa = rep(NA,length(deltas))
  counterfactual_mortality_rates_sofa_low = rep(NA,length(deltas))
  counterfactual_mortality_rates_ards = rep(NA,length(deltas))
  
  for(i in 1:length(deltas)){
    counterfactual_mortality_rates[i] = mean(Weights[,i]*mortality_table$hospital_expire_flag) 
    counterfactual_mortality_rates_pf[i] = mean(Weights_pf[,i]*mortality_table_pf$hospital_expire_flag) 
    counterfactual_mortality_rates_pf_high[i] = mean(Weights_pf_high[,i]*mortality_table_pf_high$hospital_expire_flag) 
    counterfactual_mortality_rates_sofa[i] = mean(Weights_sofa[,i]*mortality_table_sofa$hospital_expire_flag) 
    counterfactual_mortality_rates_sofa_low[i] = mean(Weights_sofa_low[,i]*mortality_table_sofa_low$hospital_expire_flag) 
    counterfactual_mortality_rates_ards[i] = mean(Weights_ards[,i]*mortality_table_ards$hospital_expire_flag) 
  }
  counterfactual_mortality_rates_list[[b]] = counterfactual_mortality_rates
  counterfactual_mortality_rates_list_pf[[b]] = counterfactual_mortality_rates_pf
  counterfactual_mortality_rates_list_pf_high[[b]] = counterfactual_mortality_rates_pf_high
  counterfactual_mortality_rates_list_sofa[[b]] = counterfactual_mortality_rates_sofa
  counterfactual_mortality_rates_list_sofa_low[[b]] = counterfactual_mortality_rates_sofa_low
  counterfactual_mortality_rates_list_ards[[b]] = counterfactual_mortality_rates_ards
  
  # plot(deltas,counterfactual_mortality_rates)
  # points(deltas[which(deltas==1)],counterfactual_mortality_rates[which(deltas==1)],col=2)
  icu_outcome_table_boot = icu_outcome_table[samp_pats,]
  icu_outcome_table_boot_pf = icu_outcome_table_boot[which(samp_pats %in% low_pfratio_pat_index),]
  icu_outcome_table_boot_pf_high = icu_outcome_table_boot[which(samp_pats %in% high_pfratio_pat_index),]
  icu_outcome_table_boot_sofa = icu_outcome_table_boot[which(samp_pats %in% high_sofa_pat_index),]
  icu_outcome_table_boot_sofa_low = icu_outcome_table_boot[which(samp_pats %in% low_sofa_pat_index),]
  icu_outcome_table_boot_ards = icu_outcome_table_boot[which(samp_pats %in% ards_pat_index),]
  
  counterfactual_icu_free_days = rep(NA,length(deltas))
  counterfactual_icu_free_days_pf = rep(NA,length(deltas))
  counterfactual_icu_free_days_pf_high = rep(NA,length(deltas))
  counterfactual_icu_free_days_sofa = rep(NA,length(deltas))
  counterfactual_icu_free_days_sofa_low = rep(NA,length(deltas))
  counterfactual_icu_free_days_ards = rep(NA,length(deltas))
  
  for(i in 1:length(deltas)){
    counterfactual_icu_free_days[i] = mean(Weights[,i]*icu_outcome_table_boot$outcome) 
    counterfactual_icu_free_days_pf[i] = mean(Weights_pf[,i]*icu_outcome_table_boot_pf$outcome) 
    counterfactual_icu_free_days_pf_high[i] = mean(Weights_pf_high[,i]*icu_outcome_table_boot_pf_high$outcome) 
    counterfactual_icu_free_days_sofa[i] = mean(Weights_sofa[,i]*icu_outcome_table_boot_sofa$outcome) 
    counterfactual_icu_free_days_sofa_low[i] = mean(Weights_sofa_low[,i]*icu_outcome_table_boot_sofa_low$outcome) 
    counterfactual_icu_free_days_ards[i] = mean(Weights_ards[,i]*icu_outcome_table_boot_ards$outcome) 
  }
  counterfactual_icu_free_days_list[[b]] = counterfactual_icu_free_days
  counterfactual_icu_free_days_list_pf[[b]] = counterfactual_icu_free_days_pf
  counterfactual_icu_free_days_list_pf_high[[b]] = counterfactual_icu_free_days_pf_high
  counterfactual_icu_free_days_list_sofa[[b]] = counterfactual_icu_free_days_sofa
  counterfactual_icu_free_days_list_sofa_low[[b]] = counterfactual_icu_free_days_sofa_low
  counterfactual_icu_free_days_list_ards[[b]] = counterfactual_icu_free_days_ards
  
  baseline_hour_table_boot = baseline_hour_table[samp_pats,]
  baseline_hour_table_boot_pf = baseline_hour_table_boot[which(samp_pats %in% low_pfratio_pat_index),]
  baseline_hour_table_boot_pf_high = baseline_hour_table_boot[which(samp_pats %in% high_pfratio_pat_index),]
  baseline_hour_table_boot_sofa = baseline_hour_table_boot[which(samp_pats %in% high_sofa_pat_index),]
  baseline_hour_table_boot_sofa_low = baseline_hour_table_boot[which(samp_pats %in% low_sofa_pat_index),]
  baseline_hour_table_boot_ards = baseline_hour_table_boot[which(samp_pats %in% ards_pat_index),]
  
  counterfactual_vent_free_days = rep(NA,length(deltas))
  counterfactual_vent_free_days_pf = rep(NA,length(deltas))
  counterfactual_vent_free_days_pf_high = rep(NA,length(deltas))
  counterfactual_vent_free_days_sofa = rep(NA,length(deltas))
  counterfactual_vent_free_days_sofa_low = rep(NA,length(deltas))
  counterfactual_vent_free_days_ards = rep(NA,length(deltas))
  
  for(i in 1:length(deltas)){
    counterfactual_vent_free_days[i] = mean(Weights[,i]*baseline_hour_table_boot$vent_free_days) 
    counterfactual_vent_free_days_pf[i] = mean(Weights_pf[,i]*baseline_hour_table_boot_pf$vent_free_days) 
    counterfactual_vent_free_days_pf_high[i] = mean(Weights_pf_high[,i]*baseline_hour_table_boot_pf_high$vent_free_days) 
    counterfactual_vent_free_days_sofa[i] = mean(Weights_sofa[,i]*baseline_hour_table_boot_sofa$vent_free_days) 
    counterfactual_vent_free_days_sofa_low[i] = mean(Weights_sofa_low[,i]*baseline_hour_table_boot_sofa_low$vent_free_days) 
    counterfactual_vent_free_days_ards[i] = mean(Weights_ards[,i]*baseline_hour_table_boot_ards$vent_free_days) 
  }
  counterfactual_vent_free_days_list[[b]] = counterfactual_vent_free_days
  counterfactual_vent_free_days_list_pf[[b]] = counterfactual_vent_free_days_pf
  counterfactual_vent_free_days_list_pf_high[[b]] = counterfactual_vent_free_days_pf_high
  counterfactual_vent_free_days_list_sofa[[b]] = counterfactual_vent_free_days_sofa
  counterfactual_vent_free_days_list_sofa_low[[b]] = counterfactual_vent_free_days_sofa_low
  counterfactual_vent_free_days_list_ards[[b]] = counterfactual_vent_free_days_ards
}  

#remake original tables to save
data = data_orig
ards_pats = unique(data$stay_id[data$pao2fio2ratio<=300 & data$pulmonary_edema==1])
ards_pats = ards_pats[!is.na(ards_pats)]
low_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio<=150])
low_pfratio_pats = low_pfratio_pats[!is.na(low_pfratio_pats)]
high_pfratio_pats = unique(data$stay_id[data$hour_baseline==1 & data$pao2fio2ratio>=300])
high_pfratio_pats = high_pfratio_pats[!is.na(high_pfratio_pats)]
high_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours>=12])
high_sofa_pats = high_sofa_pats[!is.na(high_sofa_pats)]
low_sofa_pats = unique(data$stay_id[data$hour_baseline==1 & data$sofa_24hours<=5])
low_sofa_pats = low_sofa_pats[!is.na(low_sofa_pats)]
mortality_table = data[data$hour_baseline==1,c("stay_id","hospital_expire_flag")]
mortality_table = mortality_table[order(mortality_table$stay_id),]
mortality_table_pf = mortality_table[mortality_table$stay_id%in%low_pfratio_pats,]
mortality_table_pf_high = mortality_table[mortality_table$stay_id%in%high_pfratio_pats,]
mortality_table_sofa = mortality_table[mortality_table$stay_id%in%high_sofa_pats,]
mortality_table_sofa_low = mortality_table[mortality_table$stay_id%in%low_sofa_pats,]
mortality_table_ards = mortality_table[mortality_table$stay_id%in%ards_pats,]

save(data,counterfactual_vent_free_days_list,counterfactual_vent_free_days_list_pf,counterfactual_vent_free_days_list_pf_high,counterfactual_vent_free_days_list_sofa,counterfactual_vent_free_days_list_sofa_low, counterfactual_vent_free_days_list_ards,
     counterfactual_icu_free_days_list,counterfactual_icu_free_days_list_pf,counterfactual_icu_free_days_list_pf_high,counterfactual_icu_free_days_list_sofa,counterfactual_icu_free_days_list_sofa_low,counterfactual_icu_free_days_list_ards,
     counterfactual_mortality_rates_list,counterfactual_mortality_rates_list_pf,counterfactual_mortality_rates_list_pf_high,counterfactual_mortality_rates_list_sofa,counterfactual_mortality_rates_list_sofa_low,counterfactual_mortality_rates_list_ards,
     mortality_table,mortality_table_pf,mortality_table_sofa,icu_outcome_table,icu_outcome_table_pf,icu_outcome_table_sofa,icu_outcome_table_ards,
     baseline_hour_table,baseline_hour_table_pf,baseline_hour_table_pf_high,baseline_hour_table_sofa,baseline_hour_table_sofa_low,baseline_hour_table_ards,
     counterfactual_wean_times_list,counterfactual_wean_times_list_pf,counterfactual_wean_times_list_pf_high,counterfactual_wean_times_list_sofa,counterfactual_wean_times_list_sofa_low,counterfactual_wean_times_list_ards,
     counterfactual_vent_free_days1,counterfactual_vent_free_days1_pf,counterfactual_vent_free_days1_pf_high,counterfactual_vent_free_days1_sofa,counterfactual_vent_free_days1_sofa_low,counterfactual_vent_free_days1_ards,
     counterfactual_icu_free_days1,counterfactual_icu_free_days1_pf,counterfactual_icu_free_days1_pf_high,counterfactual_icu_free_days1_sofa,counterfactual_icu_free_days1_sofa_low,counterfactual_icu_free_days1_ards,
     counterfactual_mortality_rates1,counterfactual_mortality_rates1_pf,counterfactual_mortality_rates1_pf_high,counterfactual_mortality_rates1_sofa,counterfactual_mortality_rates1_sofa_low,counterfactual_mortality_rates1_ards,
     counterfactual_wean_times1,counterfactual_wean_times1_pf,counterfactual_wean_times1_pf_high,counterfactual_wean_times1_sofa,counterfactual_wean_times1_sofa_low,counterfactual_wean_times1_ards,
     file='rwean_results.RData')

plot(deltas,counterfactual_mortality_rates1,ylim=c(min(unlist(counterfactual_mortality_rates_list)),max(unlist(counterfactual_mortality_rates_list))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1)

plot(deltas,counterfactual_mortality_rates1_pf,ylim=c(min(unlist(counterfactual_mortality_rates_list_pf)),max(unlist(counterfactual_mortality_rates_list_pf))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions (PF-Ratio<150)",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list_pf[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1_pf)

plot(deltas,counterfactual_mortality_rates1_pf_high,ylim=c(min(unlist(counterfactual_mortality_rates_list_pf_high)),max(unlist(counterfactual_mortality_rates_list_pf_high))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions (PF-Ratio>300)",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list_pf_high[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1_pf_high)

plot(deltas[1:16],counterfactual_mortality_rates1_sofa[1:16],ylim=c(min(unlist(counterfactual_mortality_rates_list_sofa)),max(unlist(counterfactual_mortality_rates_list_sofa))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions (SOFA>=12)",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list_sofa[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1_sofa)

plot(deltas[1:16],counterfactual_mortality_rates1_sofa_low[1:16],ylim=c(min(unlist(counterfactual_mortality_rates_list_sofa_low)),max(unlist(counterfactual_mortality_rates_list_sofa_low))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions (SOFA>=12)",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list_sofa_low[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1_sofa_low)

plot(deltas[1:16],counterfactual_mortality_rates1_ards[1:16],ylim=c(min(unlist(counterfactual_mortality_rates_list_ards)),max(unlist(counterfactual_mortality_rates_list_ards))),main="Counterfactual Mortality Rates Under Weaning Propensity Interventions (ards>=12)",ylab='Mortality Rate',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_mortality_rates_list_ards[[i]],type='l',col=2)
}
points(deltas,counterfactual_mortality_rates1_ards)

mort2 = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][16]))
mort1 = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][6]))
mort.5 = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][1]))
hist(mort2-mort.5)

mort2_sofa = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_sofa[[i]][16]))
mort1_sofa = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_sofa[[i]][6]))
mort.5_sofa = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_sofa[[i]][1]))
hist(mort2_sofa-mort.5_sofa)

mean(mort2_sofa-mort.5_sofa)
counterfactual_mortality_rates1_sofa[6] - counterfactual_mortality_rates1_sofa[1] + 1.96*sd(mort1_sofa-mort.5_sofa)
counterfactual_mortality_rates1_sofa[6] - counterfactual_mortality_rates1_sofa[1] - 1.96*sd(mort1_sofa-mort.5_sofa)


mort2_pf = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_pf[[i]][16]))
mort.5_pf = unlist(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_pf[[i]][1]))
hist(mort2_pf-mort.5_pf)

min_mort = which.min(counterfactual_mortality_rates1[1:16])
max_mort = which.max(counterfactual_mortality_rates1[1:16])
mort_min = sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][min_mort])
mort_max = sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][max_mort])
hist(mort_max-mort_min)

plot(deltas[1:16],counterfactual_mortality_rates1[1:16],ylim=c(min(.2,min(unlist(counterfactual_mortality_rates_list_pf_high))),max(unlist(counterfactual_mortality_rates_list_pf))),main="Counterfactual Mortality Rates Under Incremental Interventions",ylab='In-Hospital Mortality Rate',xlab=expression(delta),type='l')
# points(deltas[1:16],counterfactual_mortality_rates1[1:16],type='l')
mortality_sds = rep(NA,length(deltas))
for(j in 1:length(mortality_sds)){
  mortality_sds[j] = sd(sapply(1:nboot,function(i) counterfactual_mortality_rates_list[[i]][j]))
}
mortality_uppers = counterfactual_mortality_rates1 + 1.96*mortality_sds
mortality_lowers = counterfactual_mortality_rates1 - 1.96*mortality_sds
points(deltas[1:16],mortality_uppers[1:16],type='l',lty=2)
points(deltas[1:16],mortality_lowers[1:16],type='l',lty=2)

# points(deltas[1:16],counterfactual_mortality_rates1_sofa[1:16],col=2)
points(deltas[1:16],counterfactual_mortality_rates1_pf[1:16],col=2,type='l')
mortality_sds_pf = rep(NA,length(deltas))
for(j in 1:length(mortality_sds_pf)){
  mortality_sds_pf[j] = sd(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_pf[[i]][j]))
}
mortality_uppers_pf = counterfactual_mortality_rates1_pf + 1.96*mortality_sds_pf
mortality_lowers_pf = counterfactual_mortality_rates1_pf - 1.96*mortality_sds_pf
points(deltas[1:16],mortality_uppers_pf[1:16],type='l',col=2,lty=2)
points(deltas[1:16],mortality_lowers_pf[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_mortality_rates1_pf_high[1:16],col=3,type='l')
mortality_sds_pf_high = rep(NA,length(deltas))
for(j in 1:length(mortality_sds_pf_high)){
  mortality_sds_pf_high[j] = sd(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_pf_high[[i]][j]))
}
mortality_uppers_pf_high = counterfactual_mortality_rates1_pf_high + 1.96*mortality_sds_pf_high
mortality_lowers_pf_high = counterfactual_mortality_rates1_pf_high - 1.96*mortality_sds_pf_high
points(deltas[1:16],mortality_uppers_pf_high[1:16],type='l',col=3,lty=2)
points(deltas[1:16],mortality_lowers_pf_high[1:16],type='l',col=3,lty=2)

legend(1.3,.27,fill = c(2,1,3),legend = c("PaO2/FiO2 Ratio<=150","Full cohort","PaO2/FiO2 Ratio>=300"))
abline(v=1,col=4)

points(deltas[1:16],counterfactual_mortality_rates1_sofa[1:16],col=2,type='l')
mortality_sds_sofa = rep(NA,length(deltas))
for(j in 1:length(mortality_sds_sofa)){
  mortality_sds_sofa[j] = sd(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_sofa[[i]][j]))
}
mortality_uppers_sofa = counterfactual_mortality_rates1_sofa + 1.96*mortality_sds_sofa
mortality_lowers_sofa = counterfactual_mortality_rates1_sofa - 1.96*mortality_sds_sofa
points(deltas[1:16],mortality_uppers_sofa[1:16],type='l',col=2,lty=2)
points(deltas[1:16],mortality_lowers_sofa[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_mortality_rates1_sofa_low[1:16],col=3,type='l')
mortality_sds_sofa_low = rep(NA,length(deltas))
for(j in 1:length(mortality_sds_sofa_low)){
  mortality_sds_sofa_low[j] = sd(sapply(1:nboot,function(i) counterfactual_mortality_rates_list_sofa_low[[i]][j]))
}
mortality_uppers_sofa_low = counterfactual_mortality_rates1_sofa_low + 1.96*mortality_sds_sofa_low
mortality_lowers_sofa_low = counterfactual_mortality_rates1_sofa_low - 1.96*mortality_sds_sofa_low
points(deltas[1:16],mortality_uppers_sofa_low[1:16],type='l',col=3,lty=2)
points(deltas[1:16],mortality_lowers_sofa_low[1:16],type='l',col=3,lty=2)

legend(1.5,.5,fill = c(2,1,3),legend = c("SOFA>=12","Full cohort","SOFA<=5"))
abline(v=1,col=4)

plot(deltas[1:16],counterfactual_wean_times1[1:16],ylim=c(min(c(unlist(counterfactual_wean_times_list),unlist(counterfactual_wean_times_list_pf))),max(unlist(counterfactual_wean_times_list_sofa),unlist(counterfactual_wean_times_list_pf))),main="Counterfactual Time To First Attempt Under Incremental Interventions",ylab='Hours to First Liberation Attempt',xlab=expression(delta),type='l')
# points(deltas[1:16],counterfactual_wean_times1[1:16],type='l')
wean_sds = rep(NA,length(deltas))
for(j in 1:length(wean_sds)){
  wean_sds[j] = sd(sapply(1:nboot,function(i) counterfactual_wean_times_list[[i]][j]))
}
wean_uppers = counterfactual_wean_times1 + 1.96*wean_sds
wean_lowers = counterfactual_wean_times1 - 1.96*wean_sds
points(deltas[1:16],wean_uppers[1:16],type='l',lty=2)
points(deltas[1:16],wean_lowers[1:16],type='l',lty=2)

# points(deltas[1:16],counterfactual_wean_times1_sofa[1:16],col=2)
points(deltas[1:16],counterfactual_wean_times1_sofa[1:16],col=2,type='l')
wean_sds_sofa = rep(NA,length(deltas))
for(j in 1:length(wean_sds)){
  wean_sds_sofa[j] = sd(sapply(1:nboot,function(i) counterfactual_wean_times_list_sofa[[i]][j]))
}
wean_uppers_sofa = counterfactual_wean_times1_sofa + 1.96*wean_sds_sofa
wean_lowers_sofa = counterfactual_wean_times1_sofa - 1.96*wean_sds_sofa
points(deltas[1:16],wean_uppers_sofa[1:16],type='l',col=2,lty=2)
points(deltas[1:16],wean_lowers_sofa[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_wean_times1_sofa_low[1:16],col=3,type='l')
wean_sds_sofa_low = rep(NA,length(deltas))
for(j in 1:length(wean_sds)){
  wean_sds_sofa_low[j] = sd(sapply(1:nboot,function(i) counterfactual_wean_times_list_sofa_low[[i]][j]))
}
wean_uppers_sofa_low = counterfactual_wean_times1_sofa_low + 1.96*wean_sds_sofa_low
wean_lowers_sofa_low = counterfactual_wean_times1_sofa_low - 1.96*wean_sds_sofa_low
points(deltas[1:16],wean_uppers_sofa_low[1:16],type='l',col=3,lty=2)
points(deltas[1:16],wean_lowers_sofa_low[1:16],type='l',col=3,lty=2)

legend("topright",fill = c(2,1,3),legend = c("SOFA>=12","Full cohort","SOFA<=5"))
abline(v=1,col=4)

points(deltas[1:16],counterfactual_wean_times1_pf[1:16],col=2,type='l')
wean_sds_pf = rep(NA,length(deltas))
for(j in 1:length(wean_sds)){
  wean_sds_pf[j] = sd(sapply(1:nboot,function(i) counterfactual_wean_times_list_pf[[i]][j]))
}
wean_uppers_pf = counterfactual_wean_times1_pf + 1.96*wean_sds_pf
wean_lowers_pf = counterfactual_wean_times1_pf - 1.96*wean_sds_pf
points(deltas[1:16],wean_uppers_pf[1:16],type='l',col=2,lty=2)
points(deltas[1:16],wean_lowers_pf[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_wean_times1_pf_high[1:16],col=3,type='l')
wean_sds_pf_high = rep(NA,length(deltas))
for(j in 1:length(wean_sds)){
  wean_sds_pf_high[j] = sd(sapply(1:nboot,function(i) counterfactual_wean_times_list_pf_high[[i]][j]))
}
wean_uppers_pf_high = counterfactual_wean_times1_pf_high + 1.96*wean_sds_pf_high
wean_lowers_pf_high = counterfactual_wean_times1_pf_high - 1.96*wean_sds_pf_high
points(deltas[1:16],wean_uppers_pf_high[1:16],type='l',col=3,lty=2)
points(deltas[1:16],wean_lowers_pf_high[1:16],type='l',col=3,lty=2)

legend("topright",fill = c(2,1,3),legend = c("PaO2/FiO2 Ratio<=150","Full cohort","PaO2/FiO2 Ratio>=300"))
abline(v=1,col=4)

# for(i in 1:100){
#   points(deltas,counterfactual_wean_times_list[[i]],type='l',col=2)
# }
# points(deltas,counterfactual_wean_times1[1:16])

plot(deltas,counterfactual_wean_times1_pf,ylim=c(min(unlist(counterfactual_wean_times_list_pf)),max(unlist(counterfactual_wean_times_list_pf))),main="Counterfactual Time-To-First Attempt Under Weaning Propensity Interventions (PF-Ratio<200)",ylab='Hours to First Weaning Attempt',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_wean_times_list_pf[[i]],type='l',col=2)
}
points(deltas,counterfactual_wean_times1_pf[1:16])

plot(deltas[1:16],counterfactual_wean_times1_sofa[1:16],ylim=c(min(unlist(counterfactual_wean_times_list_pf)),max(unlist(counterfactual_wean_times_list_pf))),main="Counterfactual Time-To-First Attempt Under Weaning Propensity Interventions (PF-Ratio<200)",ylab='Hours to First Weaning Attempt',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_wean_times_list_pf[[i]],type='l',col=2)
}
points(deltas,counterfactual_wean_times1_pf[1:16])

wean2 = sapply(1:nboot,function(i) counterfactual_wean_times_list[[i]][16])
wean.5 = sapply(1:nboot,function(i) counterfactual_wean_times_list[[i]][1])
wean1 = sapply(1:nboot,function(i) counterfactual_wean_times_list[[i]][6])
counterfactual_wean_times1[1]
counterfactual_wean_times1[1] + 1.96*sd(wean.5)
counterfactual_wean_times1[1] - 1.96*sd(wean.5)

counterfactual_wean_times1[16]
counterfactual_wean_times1[16] + 1.96*sd(wean2)
counterfactual_wean_times1[16] - 1.96*sd(wean2)

mean(icu2-icu1)
counterfactual_icu_free_days1[16] - counterfactual_icu_free_days1[6] + 1.96*sd(icu2-icu1)
counterfactual_icu_free_days1[16] - counterfactual_icu_free_days1[6] - 1.96*sd(icu2-icu1)

mean(icu1-icu.5)
counterfactual_icu_free_days1[6] - counterfactual_icu_free_days1[1] + 1.96*sd(icu1-icu.5)
counterfactual_icu_free_days1[6] - counterfactual_icu_free_days1[1] - 1.96*sd(icu1-icu.5)


plot(deltas,counterfactual_icu_free_days1,ylim=c(min(unlist(counterfactual_icu_free_days_list)),max(unlist(counterfactual_icu_free_days_list))),main="Counterfactual ICU Free Days Under Weaning Propensity Interventions",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_icu_free_days_list[[i]],type='l',col=2)
}
points(deltas,counterfactual_icu_free_days1[1:16])

plot(deltas,counterfactual_icu_free_days1_pf,ylim=c(min(unlist(counterfactual_icu_free_days_list_pf)),max(unlist(counterfactual_icu_free_days_list_pf))),main="Counterfactual ICU Free Days Under Weaning Propensity Interventions (PF-Ratio<200)",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_icu_free_days_list_pf[[i]],type='l',col=2)
}
points(deltas,counterfactual_icu_free_days1_pf)

plot(deltas,counterfactual_icu_free_days1_sofa,ylim=c(min(unlist(counterfactual_icu_free_days_list_sofa)),max(unlist(counterfactual_icu_free_days_list_sofa))),main="Counterfactual ICU Free Days Under Weaning Propensity Interventions (SOFA>=12)",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_icu_free_days_list_sofa[[i]],type='l',col=2)
}
points(deltas,counterfactual_icu_free_days1_sofa)

plot(deltas,counterfactual_icu_free_days1_ards,ylim=c(min(unlist(counterfactual_icu_free_days_list_ards)),max(unlist(counterfactual_icu_free_days_list_ards))),main="Counterfactual ICU Free Days Under Weaning Propensity Interventions (ards>=12)",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_icu_free_days_list_ards[[i]],type='l',col=2)
}
points(deltas,counterfactual_icu_free_days1_ards)

icu2 = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list[[i]][16]))
icu.5 = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list[[i]][1]))
icu1 = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list[[i]][6]))

mean(icu2-icu1)
counterfactual_icu_free_days1[16] - counterfactual_icu_free_days1[6] + 1.96*sd(icu2-icu1)
counterfactual_icu_free_days1[16] - counterfactual_icu_free_days1[6] - 1.96*sd(icu2-icu1)

icu2_sofa = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_sofa[[i]][16]))
icu.5_sofa = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_sofa[[i]][1]))
icu1_sofa = unlist(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_sofa[[i]][6]))

mean(icu2_sofa-icu1_sofa)
counterfactual_icu_free_days1_sofa[16] - counterfactual_icu_free_days1_sofa[6] + 1.96*sd(icu2_sofa-icu1_sofa)
counterfactual_icu_free_days1_sofa[16] - counterfactual_icu_free_days1_sofa[6] - 1.96*sd(icu2_sofa-icu1_sofa)

mean(icu1-icu.5)
counterfactual_icu_free_days1[6] - counterfactual_icu_free_days1[1] + 1.96*sd(icu1-icu.5)
counterfactual_icu_free_days1[6] - counterfactual_icu_free_days1[1] - 1.96*sd(icu1-icu.5)

plot(deltas[1:16],counterfactual_icu_free_days1[1:16],ylim=c(min(unlist(counterfactual_icu_free_days_list_sofa)),max(unlist(counterfactual_icu_free_days_list_sofa_low))),main="Counterfactual ICU Free Days Under Incremental Interventions",ylab='ICU Free Days',xlab=expression(delta),type='l')
# points(deltas[1:16],counterfactual_icu_free_days1[1:16],type='l')
icu_sds = rep(NA,length(deltas))
for(j in 1:length(icu_sds)){
  icu_sds[j] = sd(sapply(1:nboot,function(i) counterfactual_icu_free_days_list[[i]][j]))
}
icu_uppers = counterfactual_icu_free_days1 + 1.96*icu_sds
icu_lowers = counterfactual_icu_free_days1 - 1.96*icu_sds
points(deltas[1:16],icu_uppers[1:16],type='l',lty=2)
points(deltas[1:16],icu_lowers[1:16],type='l',lty=2)

# points(deltas[1:16],counterfactual_icu_free_days1_sofa[1:16],col=2)
points(deltas[1:16],counterfactual_icu_free_days1_sofa[1:16],col=2,type='l')
icu_sds_sofa = rep(NA,length(deltas))
for(j in 1:length(icu_sds)){
  icu_sds_sofa[j] = sd(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_sofa[[i]][j]))
}
icu_uppers_sofa = counterfactual_icu_free_days1_sofa + 1.96*icu_sds_sofa
icu_lowers_sofa = counterfactual_icu_free_days1_sofa - 1.96*icu_sds_sofa
points(deltas[1:16],icu_uppers_sofa[1:16],type='l',col=2,lty=2)
points(deltas[1:16],icu_lowers_sofa[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_icu_free_days1_sofa_low[1:16],col=3,type='l')
icu_sds_sofa_low = rep(NA,length(deltas))
for(j in 1:length(icu_sds)){
  icu_sds_sofa_low[j] = sd(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_sofa_low[[i]][j]))
}
icu_uppers_sofa_low = counterfactual_icu_free_days1_sofa_low + 1.96*icu_sds_sofa_low
icu_lowers_sofa_low = counterfactual_icu_free_days1_sofa_low - 1.96*icu_sds_sofa_low
points(deltas[1:16],icu_uppers_sofa_low[1:16],type='l',col=3,lty=2)
points(deltas[1:16],icu_lowers_sofa_low[1:16],type='l',col=3,lty=2)

legend(1.5,13,fill = c(3,1,2),legend = c("SOFA<=5","Full cohort","SOFA>=12"))
abline(v=1,col=4)

points(deltas[1:16],counterfactual_icu_free_days1_pf[1:16],col=2,type='l')
icu_sds_pf = rep(NA,length(deltas))
for(j in 1:length(icu_sds)){
  icu_sds_pf[j] = sd(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_pf[[i]][j]))
}
icu_uppers_pf = counterfactual_icu_free_days1_pf + 1.96*icu_sds_pf
icu_lowers_pf = counterfactual_icu_free_days1_pf - 1.96*icu_sds_pf
points(deltas[1:16],icu_uppers_pf[1:16],type='l',col=2,lty=2)
points(deltas[1:16],icu_lowers_pf[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_icu_free_days1_pf_high[1:16],col=3,type='l')
icu_sds_pf_high = rep(NA,length(deltas))
for(j in 1:length(icu_sds)){
  icu_sds_pf_high[j] = sd(sapply(1:nboot,function(i) counterfactual_icu_free_days_list_pf_high[[i]][j]))
}
icu_uppers_pf_high = counterfactual_icu_free_days1_pf_high + 1.96*icu_sds_pf_high
icu_lowers_pf_high = counterfactual_icu_free_days1_pf_high - 1.96*icu_sds_pf_high
points(deltas[1:16],icu_uppers_pf_high[1:16],type='l',col=3,lty=2)
points(deltas[1:16],icu_lowers_pf_high[1:16],type='l',col=3,lty=2)

legend('bottomright',fill = c(3,1,2),legend = c("PaO2/FiO2 Ratio>=300","Full cohort","PaO2/FiO2 Ratio<=150"))
abline(v=1,col=4)


plot(deltas,counterfactual_vent_free_days1,ylim=c(min(unlist(counterfactual_vent_free_days_list)),max(unlist(counterfactual_vent_free_days_list))),main="Counterfactual Ventilator Free Days Under Weaning Propensity Interventions",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_vent_free_days_list[[i]],type='l',col=2)
}
points(deltas,counterfactual_vent_free_days1[1:16])

plot(deltas,counterfactual_vent_free_days1_pf,ylim=c(min(unlist(counterfactual_vent_free_days_list_pf)),max(unlist(counterfactual_vent_free_days_list_pf))),main="Counterfactual Ventilator Free Days Under Weaning Propensity Interventions (SOFA>=12)",ylab='ICU Free Days',xlab=expression(delta))
for(i in 1:100){
  points(deltas,counterfactual_vent_free_days_list_pf[[i]],type='l',col=2)
}
points(deltas,counterfactual_vent_free_days1_sofa[1:16])

vent2 = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list[[i]][16]))
vent.5 = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list[[i]][1]))
vent1 = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list[[i]][6]))

counterfactual_vent_free_days1[16] - counterfactual_vent_free_days1[6] 
counterfactual_vent_free_days1[16] - counterfactual_vent_free_days1[6] + 1.96*sd(vent2-vent1)
counterfactual_vent_free_days1[16] - counterfactual_vent_free_days1[6] - 1.96*sd(vent2-vent1)

counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1]
counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1] + 1.96*sd(vent1-vent.5)
counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1] - 1.96*sd(vent1-vent.5)

vent2_sofa = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_sofa[[i]][10]))
vent.5 = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_sofa[[i]][1]))
vent1 = unlist(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_sofa[[i]][6]))

counterfactual_vent_free_days1_sofa[10] - counterfactual_vent_free_days1_sofa[6] 
counterfactual_vent_free_days1_sofa[10] - counterfactual_vent_free_days1_sofa[6] + 1.96*sd(vent2-vent1)
counterfactual_vent_free_days1_sofa[10] - counterfactual_vent_free_days1_sofa[6] - 1.96*sd(vent2-vent1)

counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1]
counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1] + 1.96*sd(vent1-vent.5)
counterfactual_vent_free_days1[6] - counterfactual_vent_free_days1[1] - 1.96*sd(vent1-vent.5)

plot(deltas[1:16],counterfactual_vent_free_days1[1:16],ylim=c(min(unlist(counterfactual_vent_free_days_list_sofa)),max(unlist(counterfactual_vent_free_days_list_sofa_low))),main="Counterfactual Ventilator Free Days Under Incremental Interventions",ylab='Ventilator Free Days',xlab=expression(delta),type='l')
# points(deltas[1:16],counterfactual_vent_free_days1[1:16],type='l')
vent_sds = rep(NA,length(deltas))
for(j in 1:length(vent_sds)){
  vent_sds[j] = sd(sapply(1:nboot,function(i) counterfactual_vent_free_days_list[[i]][j]))
}
vent_uppers = counterfactual_vent_free_days1 + 1.96*vent_sds
vent_lowers = counterfactual_vent_free_days1 - 1.96*vent_sds
points(deltas[1:16],vent_uppers[1:16],type='l',lty=2)
points(deltas[1:16],vent_lowers[1:16],type='l',lty=2)

# points(deltas[1:16],counterfactual_vent_free_days1_sofa[1:16],col=2)
points(deltas[1:16],counterfactual_vent_free_days1_sofa[1:16],col=2,type='l')
vent_sds_sofa = rep(NA,length(deltas))
for(j in 1:length(vent_sds)){
  vent_sds_sofa[j] = sd(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_sofa[[i]][j]))
}
vent_uppers_sofa = counterfactual_vent_free_days1_sofa + 1.96*vent_sds_sofa
vent_lowers_sofa = counterfactual_vent_free_days1_sofa - 1.96*vent_sds_sofa
points(deltas[1:16],vent_uppers_sofa[1:16],type='l',col=2,lty=2)
points(deltas[1:16],vent_lowers_sofa[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_vent_free_days1_sofa_low[1:16],col=3,type='l')
vent_sds_sofa_low = rep(NA,length(deltas))
for(j in 1:length(vent_sds)){
  vent_sds_sofa_low[j] = sd(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_sofa_low[[i]][j]))
}
vent_uppers_sofa_low = counterfactual_vent_free_days1_sofa_low + 1.96*vent_sds_sofa_low
vent_lowers_sofa_low = counterfactual_vent_free_days1_sofa_low - 1.96*vent_sds_sofa_low
points(deltas[1:16],vent_uppers_sofa_low[1:16],type='l',col=3,lty=2)
points(deltas[1:16],vent_lowers_sofa_low[1:16],type='l',col=3,lty=2)

legend(1.3,16.5,fill = c(3,1,2),legend = c("SOFA<=5","Full cohort","SOFA>=12"))
abline(v=1,col=4)

points(deltas[1:16],counterfactual_vent_free_days1_pf[1:16],col=2,type='l')
vent_sds_pf = rep(NA,length(deltas))
for(j in 1:length(vent_sds)){
  vent_sds_pf[j] = sd(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_pf[[i]][j]))
}
vent_uppers_pf = counterfactual_vent_free_days1_pf + 1.96*vent_sds_pf
vent_lowers_pf = counterfactual_vent_free_days1_pf - 1.96*vent_sds_pf
points(deltas[1:16],vent_uppers_pf[1:16],type='l',col=2,lty=2)
points(deltas[1:16],vent_lowers_pf[1:16],type='l',col=2,lty=2)

points(deltas[1:16],counterfactual_vent_free_days1_pf_high[1:16],col=3,type='l')
vent_sds_pf_high = rep(NA,length(deltas))
for(j in 1:length(vent_sds)){
  vent_sds_pf_high[j] = sd(sapply(1:nboot,function(i) counterfactual_vent_free_days_list_pf_high[[i]][j]))
}
vent_uppers_pf_high = counterfactual_vent_free_days1_pf_high + 1.96*vent_sds_pf_high
vent_lowers_pf_high = counterfactual_vent_free_days1_pf_high - 1.96*vent_sds_pf_high
points(deltas[1:16],vent_uppers_pf_high[1:16],type='l',col=3,lty=2)
points(deltas[1:16],vent_lowers_pf_high[1:16],type='l',col=3,lty=2)

legend('bottomright',fill = c(3,1,2),legend = c("PaO2/FiO2 Ratio>=300","Full cohort","PaO2/FiO2 Ratio<=150"))
abline(v=1,col=4)
###################
###TABLE 1
###################

baseline_data = data[data$hour_baseline==1,c(L)]
table1_means = c()
table1_rows = c()
table1_sds = c()
for(name in names(baseline_data)){
  if(class(baseline_data[,name])=='factor'){
    for(lev in levels(baseline_data[,name])){
      table1_means = c(table1_means,mean(baseline_data[,name]==lev,na.rm=T))
      table1_rows = c(table1_rows,lev)
      table1_sds = c(table1_sds,NA)
    }
  }else{
    table1_means = c(table1_means,mean(baseline_data[,name],na.rm=T))
    table1_rows = c(table1_rows,name)
    table1_sds = c(table1_sds,sd(baseline_data[,name],na.rm=T))
  }
}
baseline_summary = cbind(table1_rows,table1_means,table1_sds)

write.csv(baseline_summary,row.names=F,file="weaning_table1_baseline.csv")

