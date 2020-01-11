library(vegan)
library(ape)
library(ggplot2)

otu<-read.csv("otu_tmm.csv",row.names=1,check.names = F)
group<-read.csv("group.csv",row.names=1,check.names = F)
otu=otu[match(rownames(group),rownames(otu)),]

dis<-vegdist(otu,method="bray")
bray.pc<-pcoa(dis)
summary(bray.pc)
head(bray.pc$values)
bray.pc=bray.pc$vectors[,1:4]
bray.pc=bray.pc[match(rownames(group),rownames(bray.pc)),]
bray.data <- cbind(bray.pc, group)

pch<-c(15,16,17,18)
comp.cols<-c("#E41A1C", "#0000CD", "#4DAF4A", "yellow" )
p=ggplot(bray.data, aes(x = Axis.1, y = Axis.2, color = diameter,shape=compartment)) +
  geom_vline(xintercept=0,alpha = 0.5)+
  geom_hline(yintercept=0,alpha = 0.5)+
  geom_point(size =3,aes(size = factor(vertical))) +
  theme_bw() +
  labs(x = "PCo 1 (12.14%)", y = "PCo 2 (9.05%)") +
  scale_color_manual(values = comp.cols) +
  scale_shape_manual(values = pch)+scale_size_manual(values=c(3,2,1,0.5))+
  theme(text = element_text(size = 15), legend.key = element_blank())+
  stat_ellipse(aes(group=compartment),level=0.95)
p+stat_ellipse(aes(group=group2),level=0.95)

adonis(dis ~ group$compartment) 
anosim(dis, group$compartment,permutations = 999, distance = "bray")
adonis(dis ~ group$diameter) 
anosim(dis, group$diameter,permutations = 999, distance = "bray")
adonis(dis ~ group$vertical) 
anosim(dis, group$vertical,permutations = 999, distance = "bray")

group=group[order(group$vertical),]
otu=otu[match(rownames(group),rownames(otu)),]
otu_sd1=otu[1:54,];otu_sd2=otu[55:108,];otu_sd3=otu[109:162,];otu_sd4=otu[163:216,]
group_sd1=group[1:54,];group_sd2=group[55:108,];group_sd3=group[109:162,];group_sd4=group[163:216,]

dis_sd1<-vegdist(otu_sd1,method="bray")
bray.pc<-pcoa(dis_sd1)
summary(bray.pc)
head(bray.pc$values)
bray.pc=bray.pc$vectors[,1:4]
bray.pc=bray.pc[match(rownames(group_sd1),rownames(bray.pc)),]
bray.data_sd1 <- cbind(bray.pc, group_sd1)

ggplot(bray.data_sd1, aes(x = Axis.1, y = Axis.2, color = diameter,shape=compartment)) +
  geom_vline(xintercept=0,alpha = 0.5)+
  geom_hline(yintercept=0,alpha = 0.5)+
  geom_point(size =3,aes(size = factor(vertical))) +
  theme_bw() +
  labs(x = "PCo 1 ()", y = "PCo 2 ()") +
  scale_color_manual(values = comp.cols) +
  scale_shape_manual(values = pch)+scale_size_manual(values=c(3,2,1,0.5))+
  theme(text = element_text(size = 15), legend.key = element_blank())

adonis(dis_sd1 ~ group_sd1$compartment) 
anosim(dis_sd1, group_sd1$compartment,permutations = 999, distance = "bray")
adonis(dis_sd1 ~ group_sd1$diameter) 
anosim(dis_sd1, group_sd1$diameter,permutations = 999, distance = "bray")

dis_sd2<-vegdist(otu_sd2,method="bray")
bray.pc<-pcoa(dis_sd2)
summary(bray.pc)
head(bray.pc$values)
bray.pc=bray.pc$vectors[,1:4]
bray.pc=bray.pc[match(rownames(group_sd2),rownames(bray.pc)),]
bray.data_sd2 <- cbind(bray.pc, group_sd2)

ggplot(bray.data_sd2, aes(x = Axis.1, y = Axis.2, color = diameter,shape=compartment)) +
  geom_vline(xintercept=0,alpha = 0.5)+
  geom_hline(yintercept=0,alpha = 0.5)+
  geom_point(size =3,aes(size = factor(vertical))) +
  theme_bw() +
  labs(x = "PCo 1 ()", y = "PCo 2 ()") +
  scale_color_manual(values = comp.cols) +
  scale_shape_manual(values = pch)+scale_size_manual(values=c(3,2,1,0.5))+
  theme(text = element_text(size = 15), legend.key = element_blank())

adonis(dis_sd2 ~ group_sd2$compartment) 
anosim(dis_sd2, group_sd2$compartment,permutations = 999, distance = "bray")
adonis(dis_sd2 ~ group_sd2$diameter) 
anosim(dis_sd2, group_sd2$diameter,permutations = 999, distance = "bray")



dis_sd3<-vegdist(otu_sd3,method="bray")
bray.pc<-pcoa(dis_sd3)
summary(bray.pc)
head(bray.pc$values)
bray.pc=bray.pc$vectors[,1:4]
bray.pc=bray.pc[match(rownames(group_sd3),rownames(bray.pc)),]
bray.data_sd3 <- cbind(bray.pc, group_sd3)

ggplot(bray.data_sd3, aes(x = Axis.1, y = Axis.2, color = diameter,shape=compartment)) +
  geom_vline(xintercept=0,alpha = 0.5)+
  geom_hline(yintercept=0,alpha = 0.5)+
  geom_point(size =3,aes(size = factor(vertical))) +
  theme_bw() +
  labs(x = "PCo 1 ()", y = "PCo 2 ()") +
  scale_color_manual(values = comp.cols) +
  scale_shape_manual(values = pch)+scale_size_manual(values=c(3,2,1,0.5))+
  theme(text = element_text(size = 15), legend.key = element_blank())

adonis(dis_sd3 ~ group_sd3$compartment) 
anosim(dis_sd3, group_sd3$compartment,permutations = 999, distance = "bray")
adonis(dis_sd3 ~ group_sd3$diameter) 
anosim(dis_sd3, group_sd3$diameter,permutations = 999, distance = "bray")








dis_sd4<-vegdist(otu_sd4,method="bray")
bray.pc<-pcoa(dis_sd4)
summary(bray.pc)
head(bray.pc$values)
bray.pc=bray.pc$vectors[,1:4]
bray.pc=bray.pc[match(rownames(group_sd4),rownames(bray.pc)),]
bray.data_sd4 <- cbind(bray.pc, group_sd4)

ggplot(bray.data_sd4, aes(x = Axis.1, y = Axis.2, color = diameter,shape=compartment)) +
  geom_vline(xintercept=0,alpha = 0.5)+
  geom_hline(yintercept=0,alpha = 0.5)+
  geom_point(size =3,aes(size = factor(vertical))) +
  theme_bw() +
  labs(x = "PCo 1 ()", y = "PCo 2 ()") +
  scale_color_manual(values = comp.cols) +
  scale_shape_manual(values = pch)+scale_size_manual(values=c(3,2,1,0.5))+
  theme(text = element_text(size = 15), legend.key = element_blank())

adonis(dis_sd4 ~ group_sd4$compartment) 
anosim(dis_sd4, group_sd4$compartment,permutations = 999, distance = "bray")
adonis(dis_sd4 ~ group_sd4$diameter) 
anosim(dis_sd4, group_sd4$diameter,permutations = 999, distance = "bray")








