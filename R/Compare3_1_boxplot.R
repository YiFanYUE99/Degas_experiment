library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)#用于标出显著性
library(ggdist)#小提琴(右半边)
library(ggbreak)#断轴
library(tidyr)
library(patchwork)
source("R/parameter.R")
P1<-read_excel("Data_Ecoli/P1_2.xlsx")
P2<-read_excel("Data_Ecoli/P2_2.xlsx")

#1.根据Width和FWHM计算Number of theoretical plates
P1<-P1%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

P2<-P2%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

#2.画1)Migration time 2)Intensity 3) 4)NOTP的boxplot
G<-c("75_unstirred_1","75_unstirred_2","75_unstirred_ZHU")
#1) MT P1
plot11<-ggplot(data = P1,aes(x=Group,y=RT))+
  stat_halfeye(
    aes(fill=Group),
    color=P1_Ecoli,
    adjust = 0.5,
    width = 1,
    .width = 0,#置信区间
    justification = -0.1,
    point_colour = NA
  ) +
  geom_boxplot(fill="white",
               width=boxsize,
               outlier.shape = NA,#隐藏outlier
               outlier.size = point_size,
               color=P1_Ecoli,
               linewidth=boxline_width,
               position = position_nudge(x = 0)#向you移动0.1
  )+
  geom_point(position = position_nudge(x = -0.2),
             size=point_size,
             aes(color=as.factor(Run)))+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  scale_fill_manual(values = capillarycolor2)+
  guides(fill="none")+
  labs(title = "",x="",y="Migration Time")+
  xlim(G)+
  scale_y_continuous(limits = c(20.5,27.5),breaks = c(20,22,24,26))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x= element_text(size=axis_text,angle=45,hjust=1,color = P1_Ecoli,family = "sans",face="bold"),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P1_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P1_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P1_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot11
ggsave(plot = plot11,"pic/Compare3/Migration_P1.pdf",width = 89,height=70,units = "mm",dpi = 600)

plot12<-ggplot(data = P2,aes(x=Group,y=RT))+
  stat_halfeye(
    aes(fill=Group),
    color=P2_Ecoli,
    adjust = 0.5,
    width = 1,
    .width = 0,#置信区间
    justification = -0.2,
    point_colour = NA
  ) +
  geom_boxplot(fill="white",
               width=boxsize,
               outlier.shape = NA,#隐藏outlier
               outlier.size = point_size,
               color=P2_Ecoli,
               linewidth=boxline_width,
               position = position_nudge(x = 0)#向you移动0.1
  )+
  geom_point(position = position_nudge(x = -0.2),
             size=point_size,
             aes(color=as.factor(Run)))+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  scale_fill_manual(values = capillarycolor2)+
  guides(fill="none")+
  labs(title = "",x="",y="")+
  xlim(G)+
  scale_y_continuous(limits = c(20.5,27.5),breaks = c(20,22,24,26))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.y = element_blank(),
        #axis.text.x= element_text(size=axis_text,angle=90,color = P2_Ecoli,family = "sans",face="bold"),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P2_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = P2_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P2_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot12
ggsave(plot = plot12,"pic/Compare3/Migration_P2.pdf",width = 89,height=70,units = "mm",dpi = 600)








#2) Intensity
plot21<-ggplot(data = P1,aes(x=Group,y=Height))+
  stat_halfeye(
    aes(fill=Group),
    color=P1_Ecoli,
    adjust = 0.5,
    width = 0.8,
    .width = 0,#置信区间
    justification = -0.2,
    point_colour = NA
  ) +
  geom_boxplot(fill="white",
               width=boxsize,
               outlier.shape = NA,#隐藏outlier
               outlier.size = point_size,
               color=P1_Ecoli,
               linewidth=boxline_width,
               position = position_nudge(x = 0)#向you移动0.1
  )+
  geom_point(position = position_nudge(x = -0.2),
             size=point_size,
             aes(color=as.factor(Run)))+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  scale_fill_manual(values = capillarycolor2,
                    name="Capillaries")+
  guides(color="none")+
  labs(title = "",x="",y="Intensity")+
  #stat_compare_means(method = "anova", label.y = 8) +
  stat_compare_means(comparisons = list(c(G[1],G[2]), c(G[1], G[3]),
                                        c(G[2],G[3])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=P1_Ecoli,
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.04) +     # 每组标注的高度递增值
  xlim(G)+
  scale_y_break(c(5e5, 3e6), scales = 0.3) +  # 断开区间
  scale_y_continuous(limits = c(0,3.5e+06),
                     breaks=c(0,1e+05,2e+05,3e+05,4e+05,5e+05,3e+06,3.1e+06),
                     labels = function(x) format(x, scientific = TRUE))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x= element_text(size=axis_text,angle=45,color = P1_Ecoli,family = "sans",face="bold"),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P1_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none",              
        legend.title = element_text(size = legend_title,color = P1_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P1_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        ggbreak.axis = element_blank(),  # 👈 隐藏断轴区域的重复横轴
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot21
ggsave(plot = plot21,"pic/Compare3/Intensity_P1.pdf",width = 89,height=70,units = "mm",dpi = 600)








plot22<-ggplot(data = P2,aes(x=Group,y=Height))+
  stat_halfeye(
    aes(fill=Group),
    color=P2_Ecoli,
    adjust = 0.5,
    width = 0.8,
    .width = 0,#置信区间
    justification = -0.2,
    point_colour = NA
  ) +
  geom_boxplot(fill="white",
               width=boxsize,
               outlier.shape = NA,#隐藏outlier
               outlier.size = point_size,
               color=P2_Ecoli,
               linewidth=boxline_width,
               position = position_nudge(x =0)#向you移动0.1
  )+
  geom_point(position = position_nudge(x = -0.2),
             size=point_size,
             aes(color=as.factor(Run)))+
  scale_color_manual(values=Run20,
                     name = "Run")+#legend表题
  scale_fill_manual(values = capillarycolor2,
                    name="Capillaries")+
  guides(color="none")+
  labs(title = "",x="",y="")+
  #stat_compare_means(method = "anova", label.y = 8) +
  stat_compare_means(comparisons = list(c(G[1],G[2]), c(G[1], G[3]),
                                        c(G[2],G[3])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=P2_Ecoli,
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.04) +     # 每组标注的高度递增值
  xlim(G)+
  scale_y_continuous(limits = c(0,1.4e5),
                     breaks = c(0,2e4,4e4,6e4,8e4,1e5,1.2e5),
                     labels = function(x) format(x, scientific = TRUE))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.x= element_text(size=axis_text,angle=45,color = P2_Ecoli,family = "sans",face="bold"),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P2_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = P2_Ecoli,family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = P2_Ecoli,family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        ggbreak.axis = element_blank(),  # 👈 隐藏断轴区域的重复横轴
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot22
ggsave(plot = plot22,"pic/Compare3/Intensity_P2.pdf",width = 89,height=70,units = "mm",dpi = 600)
