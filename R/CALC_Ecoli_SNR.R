library(dplyr)
library(readxl)

# 读取 Excel 文件
P1 <- read_excel("Data_Ecoli/P1.xlsx")
P1 %>%
  group_by(Group) %>%
  summarise(
    mean_value = mean(SNR),
    RSD = (sd(SNR) / mean_value) * 100,
    n  = dplyr::n()
  )

ref_group <- "75_unstirred"

p_50_stirred = t.test(P1$SNR[P1$Group == ref_group], P1$SNR[P1$Group == "50_stirred"])$p.value
p_50_unstirred = t.test(P1$SNR[P1$Group == ref_group], P1$SNR[P1$Group == "50_unstirred"])$p.value
p_75_stirred = t.test(P1$SNR[P1$Group == ref_group], P1$SNR[P1$Group == "75_stirred"])$p.value


P2 <- read_excel("Data_Ecoli/P2.xlsx")
P2 %>%
  group_by(Group) %>%
  summarise(
    mean_value = mean(SNR),
    RSD = (sd(SNR) / mean_value) * 100
  )

p_50_stirred = t.test(P2$SNR[P2$Group == ref_group], P2$SNR[P2$Group == "50_stirred"])$p.value
p_50_unstirred = t.test(P2$SNR[P2$Group == ref_group], P2$SNR[P2$Group == "50_unstirred"])$p.value
p_75_stirred = t.test(P2$SNR[P2$Group == ref_group], P2$SNR[P2$Group == "75_stirred"])$p.value
