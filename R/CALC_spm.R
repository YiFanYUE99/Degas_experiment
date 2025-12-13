library(readxl)
library(dplyr)
spm<-read_excel("D:/Degas_Experiment/Data_SPM/capillary_standardprotein.xlsx")
spm<-spm%>%
  filter(capillary!="50_unstirred")

spm_MT <- spm %>%
  group_by(capillary, Peak) %>%
  summarise(
    mean_migration_time = mean(`migration_time(min)`, na.rm = TRUE),
    sd_migration_time = sd(`migration_time(min)`, na.rm = TRUE),
    rsd_migration_time  = (sd_migration_time / mean_migration_time) * 100,
    .groups = 'drop'
  )


spm_NOTP <- spm %>%
  group_by(capillary, Peak) %>%
  summarise(
    meanNOTP=mean(`Number_of_theoretical plates`,na.rm = TRUE),
    sdNOTP=sd(`Number_of_theoretical plates`,na.rm = TRUE),
    rsdNOTP=round((sdNOTP/meanNOTP)*100,2),
    .groups = 'drop'
  )%>%
  select(-sdNOTP)

#Res
Ubq<-spm%>%
  filter(Peak=="Ubiquitin")
Myo<-spm%>%
  filter(Peak=="Myoglobin")

Res<-Ubq%>%
  left_join(Myo,by = c("capillary", "run"),suffix= c("_Ubq", "_Myo"))
Res<-Res%>%
  mutate(Resolution=-1.18*(`migration_time(min)_Ubq`-`migration_time(min)_Myo`)/(FWHM_Ubq+FWHM_Myo))
Res_summary<-Res%>%
  group_by(capillary) %>%
  summarise(
    meanRes=mean(Resolution,na.rm = TRUE),
    sdRes=sd(Resolution,na.rm = TRUE),
    rsdNOTP=round((sdRes/meanRes)*100,2),
    .groups = 'drop'
  )%>%
  select(-sdRes)
##t test
# 75_unstirred 的 Res 值
res_75 <- Res %>% filter(capillary == "75_unstirred") %>% pull(Resolution)
# 其他组的 Res 值
res_other <- Res %>% filter(capillary == "75_stirred") %>% pull(Resolution)
# 执行 t 检验
t.test(res_75, res_other)


#Res
CA<-spm%>%
  filter(Peak=="Carbonic anhydrase")
Myo<-spm%>%
  filter(Peak=="Myoglobin")

Res<-CA%>%
  left_join(Myo,by = c("capillary", "run"),suffix= c("_CA", "_Myo"))
Res<-Res%>%
  mutate(Resolution=1.18*(`migration_time(min)_CA`-`migration_time(min)_Myo`)/(FWHM_CA+FWHM_Myo))
Res_summary<-Res%>%
  group_by(capillary) %>%
  summarise(
    meanRes=mean(Resolution,na.rm = TRUE),
    sdRes=sd(Resolution,na.rm = TRUE),
    rsdNOTP=round((sdRes/meanRes)*100,2),
    .groups = 'drop'
  )%>%
  select(-sdRes)
##t test
# 75_unstirred 的 Res 值
res_75 <- Res %>% filter(capillary == "75_unstirred") %>% pull(Resolution)
# 其他组的 Res 值
res_other <- Res %>% filter(capillary == "50_stirred") %>% pull(Resolution)
# 执行 t 检验
t.test(res_75, res_other)

##MT
ubq75<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "75_unstirred") %>% pull(`migration_time(min)`)
ubq75_s<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "75_stirred") %>% pull(`migration_time(min)`)
ubq50_u<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "50_stirred") %>% pull(`migration_time(min)`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)
#MYo
ubq75<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "75_unstirred") %>% pull(`migration_time(min)`)
ubq75_s<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "75_stirred") %>% pull(`migration_time(min)`)
ubq50_u<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "50_stirred") %>% pull(`migration_time(min)`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)
#CA
ubq75<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "75_unstirred") %>% pull(`migration_time(min)`)
ubq75_s<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "75_stirred") %>% pull(`migration_time(min)`)
ubq50_u<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "50_stirred") %>% pull(`migration_time(min)`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)



##NOTP
ubq75<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "75_unstirred") %>% pull(`Number_of_theoretical plates`)
ubq75_s<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "75_stirred") %>% pull(`Number_of_theoretical plates`)
ubq50_u<-spm%>% 
  filter(Peak=="Ubiquitin")%>%
  filter(capillary == "50_stirred") %>% pull(`Number_of_theoretical plates`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)
#MYo
ubq75<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "75_unstirred") %>% pull(`Number_of_theoretical plates`)
ubq75_s<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "75_stirred") %>% pull(`Number_of_theoretical plates`)
ubq50_u<-spm%>% 
  filter(Peak=="Myoglobin")%>%
  filter(capillary == "50_stirred") %>% pull(`Number_of_theoretical plates`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)
#CA
ubq75<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "75_unstirred") %>% pull(`Number_of_theoretical plates`)
ubq75_s<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "75_stirred") %>% pull(`Number_of_theoretical plates`)
ubq50_u<-spm%>% 
  filter(Peak=="Carbonic anhydrase")%>%
  filter(capillary == "50_stirred") %>% pull(`Number_of_theoretical plates`)
t.test(ubq75, ubq75_s)
t.test(ubq75, ubq50_u)
