#SNR
library(readxl)
library(dplyr)
library(ggplot2)
source("Rcode/parameter.R")
P1<-read_excel("Data_Ecoli/P1.xlsx")
P2<-read_excel("Data_Ecoli/P2.xlsx")

#1.根据Width和FWHM计算Number of theoretical plates
P1<-P1%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

P2<-P2%>%
  mutate(`NOTP(width)`=round(16*(RT/Width)^2),
         `NOTP(FWHM)`=round(5.54*(RT/FWHM)^2))

str(P1$Run)
xlim<-unique(P1$Run)

#PEAK1
SNR<-ggplot(data=P1,aes(x=Run,y=SNR))+
  geom_line(aes(group=Group,colour = Group),linetype="solid",linewidth=line_width)+
  geom_point(aes(fill=as.factor(Run),colour = Group),shape=21,size=point_size+2)+
  scale_color_manual(values = capillarycolor)+
  scale_fill_manual(values = Run20)+
  labs(title="",x="Run",y="")+
  scale_x_reverse(limits = c(20,1),
                  breaks = rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
                  position = "bottom",
                  expand=c(0,0))+
  scale_y_reverse(limits=c(1325,25),
                  breaks = rev(c(25,125,425,725,1025,1325))
                  )+
  coord_flip()+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P1_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P1_Ecoli,size=axis_ticks),
        axis.text.y = element_text(size=axis_text,color = P1_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle = 90,color = P1_Ecoli,family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P1_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none", 
        plot.margin = margin(0, 0, 0, 0)
  )
SNR
ggsave(plot = SNR,"pic/test1/SNR1.pdf",width = 210/6,height=290,units = "mm",dpi = 600)


#PEAK2
SNR<-ggplot(data=P2,aes(x=Run,y=SNR))+
  geom_line(aes(group=Group,colour = Group),linetype="solid",linewidth=line_width)+
  geom_point(aes(fill=as.factor(Run),colour = Group),shape=21,size=point_size+2)+
  scale_color_manual(values = capillarycolor)+
  scale_fill_manual(values = Run20)+
  labs(title="",x="Run",y="")+
  scale_x_reverse(limits = c(20,1),
                     breaks = rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
                     position = "top",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,380),
                     breaks = c(0,76,152,228,304,380)
  )+
  coord_flip()+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = P2_Ecoli, face = "bold"),
        axis.ticks = element_line(color = P2_Ecoli,size=axis_ticks),
        axis.text.y = element_text(size=axis_text,color = P2_Ecoli,family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle = 90,color = P2_Ecoli,family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "gray90"),  # 保留主网格线
        panel.grid.minor = element_line(color = "gray90"),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = P2_Ecoli,fill=NA,linewidth = 1),  # 无边框
        legend.position = "none", 
        plot.margin = margin(0, 0, 0, 0)
  )
SNR
ggsave(plot = SNR,"pic/test1/SNR2.pdf",width = 210/6,height=290,units = "mm",dpi = 600)
