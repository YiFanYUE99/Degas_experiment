library(readxl)
library(dplyr)
P1<-read_excel("D:/Degas_Experiment/Data_Ecoli/P1.xlsx")
P2<-read_excel("D:/Degas_Experiment/Data_Ecoli/P2.xlsx")
P1$Peak<-rep("P1",80)
P2$Peak<-rep("P2",80)
P1<-P1%>%mutate(NOTP=round(5.54*(RT/FWHM)^2))
P2<-P2%>%mutate(NOTP=round(5.54*(RT/FWHM)^2))
Pall<-rbind(P1,P2)


#Res
Res<-P1%>%
  left_join(P2,by = c("Group", "Run"),suffix= c("_1", "_2"))
Res<-Res%>%
  mutate(Resolution=-1.18*(`RT_1`-`RT_2`)/(FWHM_1+FWHM_2))
Res_summary<-Res%>%
  group_by(Group) %>%
  summarise(
    meanRes=mean(Resolution,na.rm = TRUE),
    sdRes=sd(Resolution,na.rm = TRUE),
    rsdNOTP=round((sdRes/meanRes)*100,2),
    .groups = 'drop'
  )%>%
  select(-sdRes)
##t test
# 75_unstirred 的 Res 值
res_75 <- Res %>% filter(Group == "75_unstirred") %>% pull(Resolution)
# 其他组的 Res 值
res_other <- Res %>% filter(Group == "50_unstirred") %>% pull(Resolution)
# 执行 t 检验
t.test(res_75, res_other)







#MT
P_MT <- Pall %>%
  group_by(Group, Peak) %>%
  summarise(
    mean_migration_time = mean(`RT`, na.rm = TRUE),
    sd_migration_time = sd(`RT`, na.rm = TRUE),
    rsd_migration_time  = (sd_migration_time / mean_migration_time) * 100,
    .groups = 'drop'
  )%>%
  select(-sd_migration_time)


#NOTP
P_NOTP <- Pall %>%
  group_by(Group, Peak) %>%
  summarise(
    mean_migration_time = mean(`NOTP`, na.rm = TRUE),
    sd_migration_time = sd(`NOTP`, na.rm = TRUE),
    rsd_migration_time  = (sd_migration_time / mean_migration_time) * 100,
    .groups = 'drop'
  )%>%
  select(-sd_migration_time)
#t test 
# 75_unstirred 的 Res 值
P1_75 <- P1 %>% filter(Group == "75_unstirred") %>% pull(NOTP)
# 其他组的 Res 值
P1_other <- P1 %>% filter(Group == "50_stirred") %>% pull(NOTP)
# 执行 t 检验
t.test(P1_75, P1_other)

# 75_unstirred 的 Res 值
P2_75 <- P2 %>% filter(Group == "75_unstirred") %>% pull(NOTP)
# 其他组的 Res 值
P2_other <- P2 %>% filter(Group == "50_unstirred") %>% pull(NOTP)
# 执行 t 检验
t.test(P2_75, P2_other)
