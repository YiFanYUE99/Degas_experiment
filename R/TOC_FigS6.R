library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggridges)
library(tidyr)
source("R/parameter.R")

#Runs
Run10 <-  c(
  "#5EB8FF",  # 1 明亮蓝
  "#4A9DFF",  # 2 中蓝
  "#3782FA",  # 3 稍深蓝
  "#2E6CE8",  # 4 蓝中带紫
  "#3857D8",  # 5 蓝紫过渡
  "#5043C4",  # 6 正紫蓝
  "#6A35B4",  # 7 紫蓝中段
  "#832EA7",  # 8 纯紫
  "#9A29A0",  # 9 偏红紫
  "#B02597"   # 10 深洋紫
)

# 设置目录路径
dir_path <- "D:/github/Degas_experiment/Data_Hela/hela"
# 获取带完整路径的所有 .csv 文件
file_list <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# 读取所有 CSV，跳过第一行，添加文件名（去除RUN）
df <- map_df(file_list, function(f) {
  df <- read_csv(f, skip = 3)
  df$Run <- sub("\\.csv$", "", basename(f))
  df
})
peak_points <- df %>%
  group_by(Run) %>%
  filter(Intensity == max(Intensity, na.rm = TRUE)) %>%
  slice(1) %>%              # 若多个最大值，取第一个
  ungroup()
peak_summary <- peak_points %>%
  summarise(
    mean_intensity = mean(Intensity, na.rm = TRUE),
    sd_intensity = sd(Intensity, na.rm = TRUE),
    RSD_percent = (sd_intensity / mean_intensity) * 100,
    lowersd=mean_intensity - sd_intensity,
    uppersd=mean_intensity + sd_intensity,
    lower2sd=mean_intensity - 2*sd_intensity,
    upper2sd=mean_intensity + 2*sd_intensity

  )

##峰值强度统计
peak_intensity<-ggplot(peak_points,aes(x=factor(Run),y=Intensity))+
  geom_line(color="gray",group=1, linetype="solid", linewidth=1)+
  geom_hline(yintercept = peak_summary$mean_intensity, linetype = "dashed", color = "red", linewidth = 0.7)+
  geom_hline(yintercept = peak_summary$lowersd, linetype = "dashed", color = "pink", linewidth = 0.5)+
  geom_hline(yintercept = peak_summary$uppersd, linetype = "dashed", color = "pink", linewidth = 0.5)+
  geom_hline(yintercept = peak_summary$lower2sd, linetype = "dashed", color = "lightblue", linewidth = 0.5)+
  geom_hline(yintercept = peak_summary$upper2sd, linetype = "dashed", color = "lightblue", linewidth = 0.5)+
  annotate("text",
           x = "Run01", y = peak_summary$mean_intensity,
           label = "Mean",
           color = "red", size = 2.5,vjust=-0.2,hjust=0.1,fontface = "bold")+
  annotate("text",
           x = "Run02", y = peak_summary$uppersd,
           label = "+SD",
           color = "pink", size = 2.5,vjust=-0.2,hjust=0.1,fontface = "bold")+
  annotate("text",
           x = "Run02", y = peak_summary$lowersd,
           label = "-SD",
           color = "pink", size = 2.5,vjust=-0.2,hjust=0.1,fontface = "bold")+
  annotate("text",
           x = "Run01", y = peak_summary$lower2sd,
           label = "-2SD",
           color = "lightblue", size = 2.5,vjust=-0.2,hjust=0.1,fontface = "bold")+
  annotate("text",
           x = "Run01", y = peak_summary$upper2sd,
           label = "+2SD",
           color = "lightblue", size = 2.5,vjust=-0.2,hjust=0.1,fontface = "bold")+
  geom_point(size=3,aes(color=Run10))+
  scale_color_manual(values=Run10,
                     name = "Run")+#legend表题
  labs(title = "",x="",y=expression(Intensity~"(×10"^8*")"))+
  scale_y_continuous(
    labels = function(x) format(x / 1e8, digits = 2, nsmall = 2)
  ) +
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y = element_text(size=axis_text-2,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text-2,angle=60,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "grey90"),  # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "none"
  )
peak_intensity
ggsave(plot =peak_intensity,"pic/Hela/peak_intensity.pdf",width = 89,height=70,units = "mm",dpi = 600)




##一维
p1<-ggplot(df,aes(x=Time,y=factor(Run), group = factor(Run),
                      height= Intensity/max(Intensity),fill=as.factor(Run), alpha = 0.5))+
  geom_ridgeline(aes(color=Run), scale = 4,linewidth=0.2) +
  geom_text(
    data = peak_points,
    aes(
      x = 35,
      y = factor(Run),
      label = Run      # 显示数值（可改为其他标签）
    ),
    vjust = -1.5,                       # 向上偏移一点
    size = 3,
    color = Run10,
    fontface = "bold"
  ) +
  scale_fill_manual(values=Run10,
                    name = "Run")+#legend表题
  scale_color_manual(values=Run10,
                   name = "Run")+#legend表题
  labs(title = "",x="",y="")+
  scale_x_continuous(limits = c(20,40),
                     breaks=c(20,30,40))+
  theme(plot.title =  element_text(size = plot_title+3,family = "sans",hjust=0.5,color = "black", face = "bold"),
        axis.title =  element_text(size = axis_title+2,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_blank(),
        axis.text.y.left = element_blank(),
        #axis.text.x= element_text(size=axis_text+2,angle=0,hjust=0,color = "black",family = "sans",face="bold"),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "grey", size=0.5,linetype = "dashed"),  # 保留主网格线
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        #标签位置
        legend.position = "none",
        panel.border = element_blank()
  )
p1
ggsave(plot=p1,"pic/Hela/BPC_Hela.pdf",width = 50,height=100,units = "mm",dpi = 600)


df$Run_factor<-as.numeric(as.factor(df$Run))
df_filtered <- df[df$Time >= 10 & df$Time <= 50, ]
df01<-df_filtered[df_filtered$Run=="Run01",]
df02<-df_filtered[df_filtered$Run=="Run02",]
df03<-df_filtered[df_filtered$Run=="Run03",]
df04<-df_filtered[df_filtered$Run=="Run04",]
df05<-df_filtered[df_filtered$Run=="Run05",]
df06<-df_filtered[df_filtered$Run=="Run06",]
df07<-df_filtered[df_filtered$Run=="Run07",]
df08<-df_filtered[df_filtered$Run=="Run08",]
df09<-df_filtered[df_filtered$Run=="Run09",]
df10<-df_filtered[df_filtered$Run=="Run10",]
library(rgl)
library(viridisLite)

rgl::par3d(scale = c(2,12, 9e-8))#缩放坐标轴
# 自定义刻度


bg3d("white") #设置背景颜色

x_range <- c(10, 50)
y_range <- c(1, 10)
z_range <- c(0, 4.2e8)
scale_z <- 9e-8 

x_ticks <- seq(x_range[1], x_range[2], 10)
y_ticks <- seq(y_range[1], y_range[2], 1)
z_ticks <- seq(z_range[1], z_range[2], 1e8)

#网格线
for (x in x_ticks){
  #画z方向线
  for (y in y_ticks) {
    segments3d(
      c(x, x),                 # x 固定
      c(y, y),                   # y 固定
      c(min(z_ticks), max(z_ticks)),  # z 变化
      col = "gray", lwd = 0.5, alpha = 0.9
    )
  }
}

for (y in y_ticks){
  for (z in z_ticks){
    segments3d(
      c(min(x_ticks),max(x_ticks)),                 # x 固定
      c(y, y),  # y 变化
      c(z, z),                   # z 固定
      col = "gray", lwd = 0.5, alpha = 0.9
    )
  }
}

for (x in x_ticks){
  for (z in z_ticks){
    segments3d(
      c(x, x),                 # x 固定
      c(min(y_ticks),max(y_ticks)),  # y 变化
      c(z, z),                   # z 固定
      col = "gray", lwd = 0.5, alpha = 0.9
    )
  }
  
}


lines3d(as.numeric(df01$Time), 
        df01$Run_factor, 
        as.numeric(df01$Intensity), col = Run10[1], alpha = 0.9,lwd = 2)


lines3d(as.numeric(df02$Time), 
        df02$Run_factor, 
        as.matrix(df02$Intensity), col = Run10[2], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df03$Time), 
        df03$Run_factor, 
        as.matrix(df03$Intensity), col = Run10[3], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df04$Time),
        df04$Run_factor, 
        as.matrix(df04$Intensity), col = Run10[4], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df05$Time), 
        df05$Run_factor, 
        as.matrix(df05$Intensity), col = Run10[5], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df06$Time), 
        df06$Run_factor, 
        as.matrix(df06$Intensity), col = Run10[6], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df07$Time), 
        df07$Run_factor, 
        as.matrix(df07$Intensity), col = Run10[7], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df08$Time), 
        df08$Run_factor, 
        as.matrix(df08$Intensity), col = Run10[8], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df09$Time), 
        df09$Run_factor,
        as.matrix(df09$Intensity), col = Run10[9], alpha = 0.9,lwd = 2)

lines3d(as.numeric(df10$Time), 
        df10$Run_factor, 
        as.matrix(df10$Intensity), col = Run10[10], alpha = 0.9,lwd = 2)

text3d(x = peak_points$Time,  # x 都相同
       y = sort(unique(df$Run_factor)),                   # 不同的 y 层
       z = peak_points$Intensity+5e7,  # z 都相同
       texts =  format(peak_points$Intensity / 1e8, digits = 2, scientific = FALSE),
       cex = 1.2, family = "sans", font = 2, col = Run10)



#画几条线作为坐标轴
# X 轴线（Time）
lines3d(c(x_range[1], x_range[2]), c(y_range[1], y_range[1]), c(0, 0),
        col = "grey75", lwd = 2)
# Y 轴线
# lines3d(c(x_range[1],x_range[1]), c(y_range[1], y_range[2]),
#         c(0,0), col = "grey75", lwd = 2)
## Z 轴线（Intensity）
# lines3d(rep(x_range[2], 2), rep(y_range[1], 2),
#         z_range, col = "grey75", lwd = 2)

# ====== 轴标签文字 ======
text3d(x = mean(x_range), y = y_range[1] - 1.5, z = -0.5*0.9e8,
       texts = "Time (min)", cex = 1.4, family = "sans", font = 2, col = "black")

text3d(x = x_range[2], y = y_range[1],
       z = max(z_range)+5e7,
       texts = as.expression(bquote(Intensity~(1 %*% 10^8))), #优雅得显示10^8
       cex = 1.4, family = "sans", font = 2, col = "black")

#坐标轴刻度
# X 轴刻度和标签

text3d(x = x_ticks, y = rep(y_range[1], length(x_ticks)), z = rep(0, length(x_ticks)),
       texts = x_ticks, cex = 1.4, family = "sans", font = 2,col = "gray20", adj = c(0.5, 1.5))
# Y 轴刻度和标签

text3d(x = rep(x_range[1]+2, length(y_ticks)), y = y_ticks+0.5, z = rep(0, length(y_ticks)),
       texts =paste0("Run",y_ticks),family = "sans", font = 2, cex = 1, col = Run10, adj = c(1.5, 0.5))
# Z 轴刻度和标签

# 生成标签：如果为 0 显示 "0"，否则科学计数法
z_labels <- format(z_ticks / 1e8, digits = 2, scientific = FALSE)
text3d(x = rep(x_range[2]+1, length(z_ticks)), y = rep(y_range[1], length(z_ticks)), z = z_ticks,
       texts = z_labels, family = "sans", font = 2, cex = 1.4, col = "gray20", adj = c(1.5, 0.5))

lines3d(as.numeric(peak_points$Time), 
        sort(unique(df$Run_factor)), 
        peak_points$Intensity, col = "#FFA500", alpha = 0.9,lwd = 1.5)

#点
material3d(lit = TRUE, specular = "white")  # 开启光照，高光更自然
spheres3d(as.numeric(peak_points$Time), 
         sort(unique(df$Run_factor)), 
         peak_points$Intensity, col = "#FFE4B5", alpha = 0.8, radius = 0.3)

#rgl::rgl.clear("all")#清空画布
snapshot3d("pic/Hela_BPC.png",
           fmt = "png",
           width = 2400, height = 1800,
           webshot = FALSE)
rgl.postscript("pic/plot3d_Hela.pdf", fmt = "pdf")


