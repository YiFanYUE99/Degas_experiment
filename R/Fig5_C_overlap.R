library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

source("R/parameter.R")
# 设置文件夹路径
folder_path <- "Data_Hela"  # ← 替换为你的路径
tsv_files <- list.files(path = folder_path, pattern = "\\.tsv$", full.names = TRUE)

# 批量读取并加上新列
data_list <- lapply(tsv_files, function(file) {
  # 提取文件名（不含路径）
  file_name <- basename(file)
  
  # 提取 Hela10k 到第一个下划线前的部分
  group_id <- str_extract(file_name, "(?<=Hela10k)[^_]+")
  
  # 读取数据并添加新列
  read_tsv(file, skip = 26) %>%
    mutate(run = group_id)
})

# 合并为一个大表
combined_data <- bind_rows(data_list)

simpledata<-combined_data%>%
  select(`Feature intensity`,`Protein accession`,Proteoform,`Proteoform mass`,run)%>%
  mutate(Proteoform_clean = Proteoform %>%
           str_remove_all("\\[[^\\[\\]]*\\]") %>%  # 去除 [内容]
           str_replace_all("\\(([^()]*)\\)", "\\1")%>%  # 去掉括号但保留内容
           str_replace("^.*?\\.", "") %>%            # 去除最左边的点及前面
           str_replace("\\..*$", "") %>%# 去除最右边的点及后面
           str_remove_all("-"),
         run=as.numeric(run),
         run=ifelse(run>7, run-1, run))  
Proteoform_new<-simpledata%>%
  select(`Protein accession`,Proteoform,Proteoform_clean, `Proteoform mass`)
  
####
####
####作为barcode的library
Proteoform_filtered <- Proteoform_new %>%
  arrange(`Protein accession`, Proteoform_clean, `Proteoform mass`) %>%
  group_by(`Protein accession`,Proteoform_clean) %>%
  filter(
    row_number() == 1 | 
      `Proteoform mass` - lag(`Proteoform mass`, default = -Inf) > 3
  ) %>%
  ungroup()  
write.csv(Proteoform_filtered,file = "data/Hela_library.csv",row.names = FALSE)
length(unique(Proteoform_filtered$`Protein accession`))#280个protein
dim(Proteoform_filtered)[1]#1054个proteoform
data_filtered <- simpledata %>%
  arrange(`Protein accession`, Proteoform_clean) %>%
  inner_join(Proteoform_filtered, by = c("Protein accession", "Proteoform_clean"), suffix = c("_df1", "_df2"),relationship = "many-to-many")%>%
  filter(abs(`Proteoform mass_df1` - `Proteoform mass_df2`) < 3)%>%
  select(`Feature intensity`,`Protein accession`,Proteoform_df1,Proteoform_clean,run, `Proteoform mass_df1`)
colnames(data_filtered)<-c("Feature intensity","Protein accession","Proteoform","Proteoform_clean" , "run" , "Proteoform mass")
####
#计算十个run能检测到的protein accession和proteoform
df_filtered_list<-list()
ppnumber <- data.frame(
  RUN = c(1,2,3,4,5,6,7,8,9,10),
  protein_number = rep(NA_real_, 10),
  proteoform_number = rep(NA_real_, 10)
)
for (i in 1:10) {
  df_filtered_list[[i]] <-data_filtered %>%
    filter(run == i) %>%
    group_by(`Protein accession`,Proteoform_clean) %>%
    filter(
      row_number() == 1 | 
        `Proteoform mass` - lag(`Proteoform mass`, default = -Inf) > 3
    ) %>%
    ungroup()%>%  
    select(`Protein accession`, Proteoform, Proteoform_clean,`Proteoform mass`,`Feature intensity`) %>%
    distinct()%>%#用于从数据框中去除重复行，返回唯一（distinct）组合
    mutate(run=i)
  n_proteins <- df_filtered_list[[i]] %>%
    pull(`Protein accession`) %>%
    unique() %>%
    length()
  ppnumber[i,2]=n_proteins
  ppnumber[i,3]=dim(df_filtered_list[[i]])[1]
}
proteoform_mean=mean(ppnumber$proteoform_number)
proteoform_sd=sd(ppnumber$proteoform_number)
rsd_proteoform<-sd(ppnumber$proteoform_number)/mean(ppnumber$proteoform_number)*100
protein_mean=mean(ppnumber$protein_number)
protein_sd=sd(ppnumber$protein_number)
rsd_protein<-sd(ppnumber$protein_number)/mean(ppnumber$protein_number)*100
#作图
numberplot<-ggplot(data = ppnumber,aes(x=RUN,y=proteoform_number))+
  geom_col(fill="#998EC3")+
  geom_col(aes(x=RUN,y=protein_number),fill="#FEE391")+
  labs(title = "",x="RUN",y="Count")+
  scale_x_discrete(position = "bottom",limits=factor(c(1,2,3,4,5,6,7,8,9,10)))+
  geom_text(data = ppnumber,aes(x=RUN,y=proteoform_number,label = proteoform_number),vjust=1.5,color="white",size=geom_text_size)+
  geom_text(data = ppnumber,aes(x=RUN,y=protein_number,label = protein_number),vjust=1.5,color="black",size=geom_text_size)+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_line(color = "grey90"),  # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
numberplot
ggsave(plot = numberplot,"pic/Hela/number.pdf",width = 80,height=70,units = "mm",dpi = 600)
#都能检出的proteoform
a<-Proteoform_filtered
a <- a %>%
  mutate(
    RUN1 = NA_real_,
    RUN2 = NA_real_,
    RUN3 = NA_real_,
    RUN4 = NA_real_,
    RUN5 = NA_real_,
    RUN6 = NA_real_,
    RUN7 = NA_real_,
    RUN8 = NA_real_,
    RUN9 = NA_real_,
    RUN10 = NA_real_
  )
for (i in 1:10) {
  for (j in 1:nrow(a)) {
    match_row <- df_filtered_list[[i]][
      df_filtered_list[[i]]$`Protein accession` == a$`Protein accession`[j] &
        df_filtered_list[[i]]$Proteoform_clean == a$Proteoform_clean[j] &
        abs(df_filtered_list[[i]]$`Proteoform mass` - a$`Proteoform mass`[j]) < 3,
    ]
    
    if (nrow(match_row) > 0) {
      # 取第一个匹配（如有多个）
      a[j, i+4] <- match_row$`Feature intensity`[1]
    }
  }
  
}
a <- a %>%
  filter(if_all(5:14, ~ !is.na(.)))
#log10
a <- a %>%
  mutate(across(5:14, ~ log10(.)))
a <- a %>%
  mutate(Proteoform_name = paste(.[[1]], .[[2]], .[[3]], sep = "_"))%>%
  mutate(Proteoform_index = as.factor(row_number()))
write.csv(a,file = "data/shared_proteoforms.csv",row.names = FALSE)
long_a <- a %>%
  pivot_longer(
    cols = starts_with("RUN"),
    names_to = "RUN",
    values_to = "Intensity"
  )%>%
  mutate(RUN = str_remove(RUN, "RUN"))
heatmapintensity<-ggplot(long_a,aes(x=RUN,y=Proteoform_index,fill=Intensity))+
  geom_tile(color = "grey") +
  scale_x_discrete(limits = as.character(1:10))+
  scale_fill_gradientn( colors = c("#1F78B4", "white", "#F68013"),
                        limits = c(6.2, 9.2),
                        breaks= c(6.2,7.7,9.2)) +
  labs(title = "",x="RUN",y="Proteoforms")+
  guides(fill = guide_colorbar(title = "log10 (Intensity)",keywidth = 0.5, keyheight = 5))+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "white", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_blank(),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_blank(),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
heatmapintensity  
ggsave(plot = heatmapintensity,"pic/Hela/heatmap.pdf",width = 89,height=70,units = "mm",dpi = 600)




####

####
####
#protein overlap
protein_overlap<-data.frame(
  RUN1 = numeric(),
  RUN2 = numeric(),
  overlap= numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:9) {
  for (j in (i+1):10) {
    df1<-data_filtered%>%
      filter(run == i)
    df2<-data_filtered%>%
      filter(run == j)
    # 计算交集元素个数
    n_intersection <- length(intersect(df1$`Protein accession`, df2$`Protein accession`))
    # 计算并集元素个数
    n_union <- length(union(df1$`Protein accession`, df2$`Protein accession`))
    n<-round(n_intersection/n_union*100,2)
    df3<-data.frame(
      RUN1=i,
      RUN2=j,
      overlap=n
    )
    protein_overlap<-rbind(protein_overlap,df3)
  }
}
#proteoform overlap
proteoform_overlap<-data.frame(
  RUN1 = numeric(),
  RUN2 = numeric(),
  overlap= numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:9) {
  for (j in (i+1):10) {
    df1<-data_filtered%>%
      filter(run == i)
    df2<-data_filtered%>%
      filter(run == j)
    # inner_join 找出 protein + proteoform 匹配的组合
    matched <- inner_join(df1, df2, by = c("Protein accession", "Proteoform"))
    
    # 过滤质谱质量差在3以内的组合（即交集）
    intersection <- matched %>% filter(abs(`Proteoform mass_df1.x` - `Proteoform mass_df1.y`) <= 3)
    
    # 交集个数（按 protein + proteoform 唯一组合）
    n_intersection <- n_distinct(intersection %>% select("Protein accession", "Proteoform"))
    
    # 并集个数（所有唯一组合，不考虑 mass）
    all_comb <- bind_rows(
      df1 %>% select("Protein accession", "Proteoform"),
      df2 %>% select("Protein accession", "Proteoform")
    )
    n_union <- n_distinct(all_comb)
    n<-round(n_intersection/n_union*100,2)
    df3<-data.frame(
      RUN1=i,
      RUN2=j,
      overlap=n
    )
    proteoform_overlap<-rbind(proteoform_overlap,df3)
  }
}
#作图ovelap，一半protein，一半proteoform
lim<-unique(simpledata$run)
ol<-ggplot(data = protein_overlap,aes(x=RUN1,y=RUN2))+
  geom_point(shape=22,size=5,color="grey90",aes(fill=overlap))+
  scale_y_discrete(position = "left",limits=factor(lim))+# 将 y 轴及其文本移到右边
  scale_x_discrete(position = "bottom",limits=factor(lim))+
  scale_fill_gradientn(colors = c("white","#FDBE02","#FF0000"),
                       limits=c(0,100),
                       breaks = c(0,50,100), # 设置颜色渐变的刻度值
                       labels = c("0","50%","100%"))+ # 设置刻度标签) +  # 设置颜色渐变
  geom_point(data = proteoform_overlap,aes(x=RUN2,y=RUN1,colour = overlap),size=4)+
  annotate("text", x = 9, y = 10, label = "72.29%")+
  annotate("text", x = 1, y = 3, label = "52.81%")+
  annotate("text", x = 8, y = 1, label = "33.11%")+
  annotate("text", x = 7, y = 2, label = "44.00%")+
  scale_color_gradientn(colors = c("white","#D1A3C9","#A34FA3","#3F007D"),
                        limits=c(0,100),
                        breaks = c(0,25,50,100), # 设置颜色渐变的刻度值
                        labels = c("0","25%","50%","100%"))+ # 设置刻度标签) +  # 设置颜色渐变
  guides(fill = guide_colorbar(title = "Protein Ovelap",keywidth = 1, keyheight = 5),
         color= guide_colorbar(title = "Proteoform Ovelap",keywidth = 1, keyheight = 5))+
  labs(title="",x="RUN",y="RUN")+
  theme(axis.title =  element_text(size = axis_title,family = "sans",color = "black", face = "bold"),
        axis.ticks = element_line(color = "black",size=axis_ticks),
        axis.text.y = element_text(size=axis_text,color = "black",family = "sans",face="bold"),
        axis.text.x= element_text(size=axis_text,angle=0,hjust=1,color = "black",family = "sans",face="bold"),
        panel.background = element_rect(fill = "grey90", color = NA),  # 透明背景
        plot.background = element_rect(fill = "white", color = NA),   # 透明背景
        panel.grid.major = element_blank(),   # 保留次网格线
        axis.line = element_blank(),  # 保留横纵轴的黑色竖线
        #标签位置
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),  # 无边框
        legend.position = "right",              
        legend.title = element_text(size = legend_title,color = "black",family = "sans",face="bold"),
        legend.text = element_text(size = legend_text,color = "black",family = "sans",face="bold"),
        legend.key.height = unit(2, "mm"),   # 控制每项高度
        legend.spacing.y = unit(0.3, "mm"),     # 控制项与项之间的垂直间隔
        legend.key.width = unit(3, "mm"),     # 控制每项的块宽度
        legend.spacing.x = unit(0.3, "cm"),         # 控制图例项之间的间距
  )
ol
ggsave(plot = ol,"pic/Hela/overlap.pdf",width = 89,height=70,units = "mm",dpi = 600)


