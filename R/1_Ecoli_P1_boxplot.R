library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)#用于标出显著性
library(ggdist)#小提琴(右半边)
library(ggbreak)#断轴
library(tidyr)
library(patchwork)
source("R/parameter.R")
P1<-read_excel("Data_Ecoli/P1.xlsx")
P2<-read_excel("Data_Ecoli/P2.xlsx")

#1.根据Width和FWHM计算Number of theoretical plates
P1<-P1%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

P2<-P2%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

#2.画1)Migration time 2)Intensity 3) 4)NOTP的boxplot
G<-c("50_stirred","50_unstirred","75_stirred","75_unstirred")
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
  scale_fill_manual(values = capillarycolor)+
  guides(fill="none")+
  labs(title = "",x="",y="Migration Time")+
  xlim(G)+
  scale_y_continuous(limits = c(22.5,30.5),breaks = c(22.5,25,27.5,30))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x= element_text(size=axis_text,angle=90,color = P1_Ecoli,family = "sans",face="bold"),
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
ggsave(plot = plot11,"pic/test/Migration_P1.pdf",width = 105,height=70,units = "mm",dpi = 600)

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
  scale_fill_manual(values = capillarycolor)+
  guides(fill="none")+
  labs(title = "",x="",y="")+
  xlim(G)+
  scale_y_continuous(limits = c(22.5,30.5),breaks = c(22.5,25,27.5,30))+
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
ggsave(plot = plot12,"pic/test/Migration_P2.pdf",width = 105,height=70,units = "mm",dpi = 600)








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
  scale_fill_manual(values = capillarycolor,
                    name="Capillaries")+
  guides(color="none")+
  labs(title = "",x="",y="Intensity")+
  #stat_compare_means(method = "anova", label.y = 8) +
  stat_compare_means(comparisons = list(c(G[1],G[2]), c(G[1], G[3]), c(G[1], G[4]),
                                        c(G[2],G[3]),c(G[2],G[4]),
                                        c(G[3],G[4])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=P1_Ecoli,
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.05) +     # 每组标注的高度递增值
  xlim(G)+
  scale_y_break(c(5e5, 3e6), scales = 0.3) +  # 断开区间
  scale_y_continuous(limits = c(0,4e+06),
                     breaks=c(0,1e+05,2e+05,3e+05,4e+05,5e+05,3e+06,3.2e+06),
                     labels = function(x) format(x, scientific = TRUE))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x= element_text(size=axis_text,angle=90,color = P1_Ecoli,family = "sans",face="bold"),
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
ggsave(plot = plot21,"pic/test/Intensity_P1.pdf",width = 105,height=70,units = "mm",dpi = 600)








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
  scale_fill_manual(values = capillarycolor,
                    name="Capillaries")+
  guides(color="none")+
  labs(title = "",x="",y="")+
  #stat_compare_means(method = "anova", label.y = 8) +
  stat_compare_means(comparisons = list(c(G[1],G[2]), c(G[1], G[3]), c(G[1], G[4]),
                                        c(G[2],G[3]),c(G[2],G[4]),
                                        c(G[3],G[4])),
                     method = "t.test",
                     label = "p.signif",#标注显著性
                     hide.ns = F,#标非显著
                     color=P2_Ecoli,
                     size=sigsize,
                     tip.length = 0.003,      # 线条末端小短线的长度
                     step.increase = 0.05) +     # 每组标注的高度递增值
  xlim(G)+
  scale_y_continuous(limits = c(0,1.3e5),
                     breaks = c(0,2e4,4e4,6e4,8e4,1e5),
                     labels = function(x) format(x, scientific = TRUE))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
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
        ggbreak.axis = element_blank(),  # 👈 隐藏断轴区域的重复横轴
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot22
ggsave(plot = plot22,"pic/test/Intensity_P2.pdf",width = 105,height=70,units = "mm",dpi = 600)







#3 NOTP(Width)
#需要先整合P1和P2
P1$Peak=rep("Peak 1",80)
P2$Peak=rep("Peak 2",80)
P<-rbind(P1,P2)
P<-P%>%select(Group,Run,`NOTP(width)`,`NOTP(FWHM)`,Peak)
Plong<-pivot_longer(P,
                    cols = starts_with("NOTP"),
                    names_to = "Calculated by",
                    values_to = "NOTP")
Plong1<-Plong%>%filter(Peak=="Peak 1")
Plong2<-Plong%>%filter(Peak=="Peak 2")

plot31<-ggplot(data = Plong1,aes(x=Group,y=NOTP))+
  geom_boxplot(aes(color=`Calculated by`,
                   fill=Group),
               outlier.size = outlier_size,
               outlier.shape = 4,
               linewidth=boxline_width,)+
  scale_color_manual(values=NOTPcolor,labels=c(`NOTP(width)`="Peak Width",`NOTP(FWHM)`="FWHM"))+
  scale_fill_manual(values = capillarycolor)+
  guides(fill="none")+
  labs(title="",x="",y="Number of theoretical plates")+
  xlim(G)+
  scale_y_break(c(1.2e5,3.5e5), scales = 0.4) +  # 断开区间
  scale_y_continuous(limits = c(0,1.2e6),
                     breaks = c(0,3e4,6e4,9e4,1.2e5,3.6e5,
                                7.8e5,1.2e6),
                     labels = function(x) format(x, scientific = TRUE))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=25,hjust=1,color = P1_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
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
plot31
ggsave(plot = plot31,"pic/test/NOTP_P1.pdf",width = 105,height=75,units = "mm",dpi = 600)

plot32<-ggplot(data = Plong2,aes(x=Group,y=NOTP))+
  geom_boxplot(aes(color=`Calculated by`,
                   fill=Group),
               outlier.size = outlier_size,
               outlier.shape = 4,
               linewidth=boxline_width,)+
  scale_color_manual(values=NOTPcolor,labels=c(`NOTP(width)`="Peak Width",`NOTP(FWHM)`="FWHM"))+
  scale_fill_manual(values = capillarycolor)+
  guides(fill="none")+
  labs(title="",x="",y="")+
  xlim(G)+
  scale_y_break(c(3.2e5,7.5e5), scales = 0.2) +  # 断开区间
  scale_y_continuous(limits = c(0,3e6),
                     breaks = c(0,1e5,2e5,3e5,8e5,1.9e6,3e6),
                     labels = function(x) format(x, scientific = TRUE)
                     )+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y.left = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.y = element_blank(),
        axis.text.x= element_text(size=axis_text,angle=25,hjust=1,color = P2_Ecoli,family = "sans",face="bold"),
        #axis.text.x=element_blank(),
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
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
        ggbreak.axis = element_blank(),  # 👈 隐藏断轴区域的重复横轴
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()
  )
plot32
ggsave(plot = plot32,"pic/test/NOTP_P2.pdf",width = 105,height=75,units = "mm",dpi = 600)



