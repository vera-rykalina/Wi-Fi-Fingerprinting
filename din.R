library(readxl)
library (dplyr)
library (ggplot2)
library (wesanderson)
library(tidyr)
library(ggsci)
library(gg.gap)
library(ggpubr)

# images  562 x 519 (460 x 434)
# boxplot 766 x 443
getwd()
din_table <- read_excel("/Users/vera/Desktop/2020_08_07_biobank_din.xlsx",sheet = "din_labs_r")
head(din_table) 
nrow(din_table)
din_table <- mutate(din_table, dif_abs=abs(Difference))

din_table <- din_table %>%
  mutate(din_levels=ifelse(dif_abs==0,"no difference", ifelse(dif_abs>0 & dif_abs<=0.5, "less or equal to 0.5", ifelse(dif_abs>0.5 & dif_abs<=1, "less or equal to 1.0", ifelse(dif_abs>1.0 & dif_abs<=1.5, "less or equal to 1.5", "greater than 1.5")))))

pal_sc <- wes_palette(5, name = "Cavalcanti1", type = "continuous")
din_scatter1 <- din_table %>%
  select(DIN_BBP, DIN_CNR, Difference, din_levels) %>%
  ggplot() +
  geom_point(aes(y = DIN_BBP, x = DIN_CNR, color=factor(din_levels, levels=c("no difference", "less or equal to 0.5","less or equal to 1.0", "less or equal to 1.5","greater than 1.5")))) +
  labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)') +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_color_manual(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"),legend.text=element_text(size=11))+
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter1

pal_sc <- wes_palette(2, name = "Cavalcanti1", type = "continuous")
din_scatter2 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot() +
  geom_point(aes(y = DIN_BBP, x = DIN_CNR, color=dif_abs)) +
  labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)', color="DIN difference") +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  scale_color_gradient(colors=pal_sc)+
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter2

pal_sc <- wes_palette(n=2, name = "Cavalcanti1", type = "continuous")
din_scatter3 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot() +
  geom_point(aes(y = DIN_BBP, x = DIN_CNR, color=dif_abs)) +
  labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)') +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_color_gradient(name="DIN difference", low="#C4961A" , high= "#293352", breaks=c(0.5,1.0,1.5, 2.0),labels=c("0.5", "1.0","1.5","2.0"),limits=c(0,2.5)) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter3


din_scatter4 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot() +
  geom_point(aes(y = DIN_BBP, x = DIN_CNR, color=dif_abs)) +
  labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)', color="DIN difference") +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter4

# fig 4a (1 to 10 scale)
din_scatter5 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot(aes(y = DIN_BBP, x = DIN_CNR)) +
  #geom_abline(slope=1, size=0.6, color="#3A5894") + #linetype="dashed"
  geom_abline(slope=1, size=1, color="darkblue") + #linetype="dashed"
  geom_point(color="#4682B4", fill="white", shape=21) +
  #labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)', color="DIN difference") +
  labs(y='DIN (BBP)', x='DIN (CNRGH)', color="DIN difference") +
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1)) +
  scale_x_continuous(limits = c(1, 10), breaks = seq(1,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter5

# fig 4b (5 to 10 scale)
din_scatter6 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot(aes(y = DIN_BBP, x = DIN_CNR)) +
  geom_abline(slope=1, size=1, color="darkblue") + #linetype="dashed"
  #geom_point(color="steelblue", fill="white", shape=21) +
  geom_point(color="steelblue", fill="white", shape=21) +
  #labs(y='DIN (Biobanque de Picardie)', x="DIN (Centre National de Recherche en Génomique Humaine)", color="DIN difference") +
  labs(y='DIN (BBP)', x="DIN (CNRGH)", color="DIN difference") +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter6

# fig 4c (5 to 10 scale)
din_scatter6b <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot(aes(y = DIN_BBP, x = DIN_CNR)) +
  geom_abline(slope=1, size=1, color="steelblue") + #linetype="dashed"
  #geom_point(color="steelblue", fill="white", shape=21) +
  geom_point(color="darkblue", alpha=0.5, shape=21) +
  #labs(y='DIN (Biobanque de Picardie)', x="DIN (Centre National de Recherche en Génomique Humaine)", color="DIN difference") +
  labs(y='DIN (BBP)', x="DIN (CNRGH)", color="DIN difference") +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter6b



# fig 4d (5 to 10 scale)
din_scatter6a <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot(aes(y = DIN_BBP, x = DIN_CNR)) +
  geom_abline(slope=1, size=1, color="darkblue") + #linetype="dashed"
  #geom_point(color="steelblue", fill="white", shape=21) +
  geom_point(color="steelblue", alpha=0.5) +
  #labs(y='DIN (Biobanque de Picardie)', x="DIN (Centre National de Recherche en Génomique Humaine)", color="DIN difference") +
  labs(y='DIN (BBP)', x="DIN (CNRGH)", color="DIN difference") +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  scale_x_continuous(limits = c(5, 10), breaks = seq(5,10,1)) +
  #scale_color_gradient(name ="DIN difference", values=pal_sc) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter6d
######################## Regression ##########################
din_scatter7 <- din_table %>%
  select(DIN_BBP, DIN_CNR, dif_abs) %>%
  ggplot(aes(y = DIN_BBP, x = DIN_CNR)) +
  geom_smooth(method='lm', formula= y~x, se=F, color="darkblue")+
  stat_cor(label.y = 10)+ #this means at 10th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 9.5)+ #this means at 9.5th unit regresion line equation will be shown
  geom_abline(slope=1, size=1, color="red") + #linetype="dashed"
  geom_point(color="steelblue") +
  labs(y='DIN (Biobanque de Picardie)', x='DIN (Centre National de Recherche en Génomique Humaine)', color="DIN difference") +
  scale_y_continuous(limits = c(6, 10), breaks = seq(6,10,1)) +
  scale_x_continuous(limits = c(6, 10), breaks = seq(6,10,1)) +
  #geom_abline(lm(DIN_BBP ~ DIN_CNR), size=1, color="darkblue") + 
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11)) +
  theme(legend.position=c(0.8, 0.2), legend.key=element_rect(color="grey15"))
din_scatter7



raw_table <- read_excel("/Users/vera/Desktop/2020_08_07_biobank_din.xlsx",sheet = "raw_data")
head(raw_table) 
colnames(raw_table)
t.test(raw_table$delai_ec, raw_table$DIN_BBP1)
cor_buff = lm(DIN_BBP1 ~ delai_ec, data=raw_table)
summary(cor_buff)

crb <- raw_table %>%
  select(code_CRB, DIN_BBP1) %>%
  drop_na() %>%
  group_by(code_CRB) %>%
  count()

sum(crb$n)

tail(crb)
head(crb)
str(crb)


pal <- wes_palette(13, name = "Darjeeling1", type = "continuous")
crb_boxplot1 <- raw_table %>%
  select(code_CRB, DIN_BBP1) %>%
  group_by(code_CRB) %>%
  ggplot(aes(x = factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")), y = DIN_BBP1)) +
  stat_boxplot(geom ='errorbar', aes(color=factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")))) +
  geom_boxplot(aes(color=factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")))) +
  labs(x='Anonymized ID CRB', y='DIN')+
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1))+
  scale_color_manual(label=c(CRB1="444",CRB2="237",CRB3="177",CRB4="70",CRB5="80",CRB6="84",CRB7="154",CRB8="266",CRB9="213",CRB10="99",CRB11="147",CRB12="236",CRB13="429"), name ="Sample size", values = pal) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11))+
  theme(legend.position="right")
crb_boxplot1


colourCount = length(unique(raw_table$code_CRB))
coul <- brewer.pal(9, "Blues")
coul <- colorRampPalette(coul)(13)

crb_boxplot2 <- raw_table %>%
  select(code_CRB, DIN_BBP1) %>%
  group_by(code_CRB) %>%
  ggplot(aes(x = factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")), y = DIN_BBP1)) +
  stat_boxplot(geom ='errorbar', aes(color=factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")))) +
  geom_boxplot(aes(color=factor(code_CRB, levels = c("CRB1","CRB2","CRB3","CRB4","CRB5","CRB6","CRB7","CRB8","CRB9","CRB10","CRB11","CRB12","CRB13")))) +
  labs(x='Anonymized ID', y='DIN')+
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1))+
  scale_color_manual(label=c(CRB1="516",CRB2="258",CRB3="180",CRB4="70",CRB5="80",CRB6="85",CRB7="154",CRB8="267",CRB9="217",CRB10="107",CRB11="158",CRB12="239",CRB13="430"), name ="Sample size", values = coul) +
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"), panel.background = element_rect(fill = "white", colour = "grey80"), axis.title=element_text(size=12, color="grey15"), legend.text=element_text(size=11))+
  theme(legend.position="right")
crb_boxplot2




colnames(raw_table)
pal_bc <- wes_palette(1, name = "Darjeeling1", type = "continuous")
bc_storage1 <- raw_table %>%
  select(delai_ec, DIN_BBP1) %>%
  ggplot(aes(x= delai_ec, y = DIN_BBP1)) +
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  geom_hline(yintercept = 7, size=2, color="orange") + #linetype="dashed"
  geom_point(color="darkgreen", fill="white", shape=1) +
  labs(x='Buffy coat storage time (months)', y='DIN')+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1))+
  scale_x_continuous(limits = c(15, 60), breaks = seq(15,60,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position = "none")
bc_storage1


bc_storage2 <- raw_table %>%
  select(delai_ec, DIN_BBP1) %>%
  ggplot(aes(x= delai_ec, y = DIN_BBP1)) +
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  geom_hline(yintercept = 7, size=1, color="#00688B") + #linetype="dashed"
  geom_point(color="#00B2EE", fill="white", shape=1) +
  labs(x='Buffy coat storage time (months)', y='DIN')+
  scale_y_continuous(limits = c(5, 10), breaks = seq(5,10,1))+
  scale_x_continuous(limits = c(15, 60), breaks = seq(15,60,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position = "none")
bc_storage2

# fig1 
colnames(raw_table)
bc_storage3 <- raw_table %>%
  select(delai_ec, DIN_BBP1) %>%
  ggplot(aes(x= delai_ec, y = DIN_BBP1)) +
  geom_hline(yintercept = 7, size=0.6, color="#3A5894") + #linetype="dashed"
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  geom_point(color="#4682B4",fill="white", shape=21) +
  labs(x='Buffy coat storage before gDNA extraction (months)', y='DIN')+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1))+
  scale_x_continuous(limits = c(15, 60), breaks = seq(15,60,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.2))
bc_storage3

# fig1a
bc_storage3a <- raw_table %>%
  select(delai_ec, DIN_BBP1) %>%
  ggplot(aes(x= delai_ec, y = DIN_BBP1)) +
  geom_hline(yintercept = 7, size=0.6, color="#3A5894") + #linetype="dashed"
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  #geom_point(color="#4682B4",fill="white", shape=21) +
  geom_point(color="#4682B4", alpha=0.5, shape=1) +
  labs(x='Buffy coat storage before gDNA extraction (months)', y='DIN')+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1))+
  scale_x_continuous(limits = c(15, 60), breaks = seq(15,60,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.2))
bc_storage3a
###########################################################
# DIN < 7
less_than7 <- raw_table %>%
  select(delai_ec, DIN_BBP1,delai_pc_hr) %>%
  filter(DIN_BBP1<=7.5) %>%
  ggplot(aes(x= delai_ec, y = DIN_BBP1, size=delai_pc_hr)) +
  geom_point(color="steelblue", shape=21, fill="white") +
  labs(x='Buffy coat storage time (months)', y='DIN', size="Blood storage (hours)")+
  scale_y_continuous(limits = c(5.5,7.5), breaks = seq(5.5,8,0.5))+
  scale_x_continuous(limits = c(25, 55), breaks = seq(25,55,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.2))
less_than7

####################################################
timeVStime <- raw_table %>%
  select(delai_ec,delai_pc_hr, DIN_BBP1) %>%
  filter(DIN_BBP1<=7.5) %>%
  ggplot(aes(x= delai_ec, y =delai_pc_hr, size=DIN_BBP1)) +
  geom_point(color="steelblue", shape=21, fill="white") +
  labs(x='Buffy coat storage time (months)', y='Time between blood collection and buffy coat freezing (hours)', size="DIN")+
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25, 5))+
  scale_x_continuous(limits = c(15, 65), breaks = seq(15,65,5))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.8))
timeVStime



#segments list cantains more than one number vectors
gg.gap(plot=bc_storage3,segments=c(5,6),tick_width = c(5,1),
       rel_heights=c(0.3,0.005,1),
       ylim=c(1,10)) 


colnames(raw_table)
blood_time1 <- raw_table %>%
  select(delai_pc_hr, DIN_BBP1) %>%
  ggplot(aes(x= delai_pc_hr, y = DIN_BBP1)) +
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  geom_vline(xintercept = 6, size=.6, color="#3A5894") + #linetype="dashed"
  geom_point(color="#4682B4",fill="white", shape=21) +
  labs(x='Blood storage before buffy coat generation (hours)', y='DIN')+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1))+
  scale_x_continuous(limits = c(0, 36), breaks = seq(0,36,6))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.2))
blood_time1


blood_time2 <- raw_table %>%
  select(delai_pc_hr, DIN_BBP1) %>%
  ggplot(aes(x= delai_pc_hr, y = DIN_BBP1)) +
  #geom_point(color="darkgreen", fill="white", shape=1, alpha=.5, stroke=1.5) +
  geom_vline(xintercept = 6, size=.6, color="#3A5894") + #linetype="dashed"
  #geom_point(color="#4682B4",fill="white", shape=21) +
  geom_point(color="#4682B4", alpha=0.5, shape=1) +
  labs(x='Blood storage before buffy coat generation (hours)', y='DIN')+
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10,1))+
  scale_x_continuous(limits = c(0, 36), breaks = seq(0,36,6))+
  theme_minimal() +
  theme(text=element_text(size=12, color="grey15"),
        panel.background = element_rect(fill = "white", colour = "grey80"),
        legend.position=c(0.8, 0.2))
blood_time2



####################################################################
sop_table <- read_excel("Desktop/2020_08_07_biobank_din.xlsx",sheet = "sop_h")
head(sop_table)

sop <- sop_table %>%  # 1820
  filter(time_h <=6) %>%
  count()
v_sop <- (1820/2636)*100 # 69%

same_din <- din_table %>%  # 32 have identical DIN
  filter(Difference == 0) %>%
  count()
v1 <- (32/494)*100 # 6.5%

din_0.5 <- din_table %>%  # 297 have DIN diff >= 0.5
  filter(abs(Difference) <= 0.5) %>%
  count()
v2 <- (297/494)*100 # 60%

din_less_than_1 <- din_table %>%  # 384 have DIN diff <= 1
  filter(abs(Difference) <= 1) %>%
  count()

din_more_than_1 <- din_table %>%  # 10 have DIN diff > 1
  filter(abs(Difference) > 1) %>%
  count()
v3 <- (10/494)*100 # 2%
head(din_more_than_1, 10)

concHigher100 <-raw_table %>% # 1769 samples
  select(conc_ADN, DIN_BBP1) %>%
  filter(conc_ADN>100) %>%
  count()

