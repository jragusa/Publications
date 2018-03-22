# boot ####
source("lib/lib.R")
source("lib/function.R")
source("lib/data_HM.R")

# variables ####
col.tHM <- c("#f2c54b" ,"#f28322" ,"#ff1a28", "#6bc4ff", "#569dcc", "#366280", "#837266", "#4c3a2c", "#99bb44")
col.Mica <- c("#4c3a2c", "#837266", "#d0b5a2")

list.tHM <- c("Ap","Mnz","Grt.tot","Rt","St","Ttn","Tur","Zrn","tHM.trace")
list.Mica <- c("Bt","Ms","Chl")
list.Grt <- c("Grt","Alm","Alm.Prp","Alm.Prp.Grs","Alm.Sps","Grs")
list.Grt2 <- c("Undetermined \n Garnet","Almandine","Almandine \n Pyrope","Almandine \n Pyrope \n Grossular","Almandine \n Spessartine","Grossular")
list.Tur <- c("Tur","Drv","Srl")
list.HM <- colnames(density)

# tHMC and HMC ratios ####
HM.weight <- NULL

for (i in list.HM)
{HM.weight[[paste(i)]] <- density[[i]]*HM.composition[[i]]
}

HM.weight <- as.data.frame(HM.weight)
HM.weight$total <- rowSums(HM.weight)
HM.weight$count <- HM.composition$total
HM.weight$sample <- weight$sample

HM.weight$delta.HM <- HM.weight$total/HM.weight$count

HM.weight <- merge(weight,HM.weight,by="sample")
HM.weight <- HM.weight[order(HM.weight$provenance,HM.weight$strati_order),]

# HM.weight$delta.LM <- HM.weight$LM.d.weight/HM.weight$LM.d.volume # correction non mesurée
HM.weight$H <- HM.weight$D/(HM.weight$D+HM.weight$LM)*100
HM.weight$HM <- HM.weight$H*(1-((HM.composition$LM + HM.composition$tHM.diag)/HM.composition$total)) # *(data$delta.LM/data.d$delta.HM)
# weight$HMC <- weight$HM/(1-data$D.nci/data$total)*100
HM.weight$HMC <- HM.weight$HM
HM.weight$tHMC <- HM.weight$HMC * (1 - HM.composition$Op.ratio)

HM.weight %>%
  melt(id.vars=c('provenance','sample'), measure.vars=c('HMC','tHMC')) %>%
  ggplot(aes(x=sample, y=value, group=interaction(variable, provenance), colour=provenance, linetype=variable)) +
  scale_colour_manual(values = col.prov) +
  scale_x_discrete(limits=unique(HM.composition$sample)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  geom_line() +
  geom_point(size = 2) +
  xlab("") +
  ylab("tHMC & HMC") +
  ylim(0,2) +
  theme_bw(base_size = 20) +
  theme(panel.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey")) +
  coord_flip() +
  ggsave("graphs/tHMC.svg",width=5,height=10)

# tHM distribution ####
tHM <- HM.composition[list.tHM]/HM.composition$tHM*100 # put in percentage
tHM <- cbind(tHM,sample=HM.composition$sample,         # add additional columns
             provenance=HM.composition$provenance,
             strati_order=HM.composition$strati_order)

tHM[,c(-10:-12)] <-round(tHM[,c(-10:-12)],2)           # round values

subset(tHM, provenance=="quartzose")[,-11] %>%
  melt(id.vars=c('strati_order','sample'), measure.vars=list.tHM) %>%
  tHM.barplot(col.tHM,list.tHM) +
  ggsave("graphs/tHM_quartzose.svg", width=10, height=6)

subset(tHM, provenance=="feldspathic")[,-11] %>%
  melt(id.vars=c('strati_order','sample'), measure.vars=list.tHM) %>%
  tHM.barplot(col.tHM,list.tHM) +
  ggsave("graphs/tHM_feldspathic.svg", width=10, height=3.5)

# mean tHM distribution per petrofacies ####
tHM %>%
  group_by(provenance) %>%
  select(-c(sample,strati_order)) %>%
  summarise_each(funs(mean)) %>%
  melt(id.vars="provenance") %>%
  ggplot(aes(provenance, value, fill=variable)) +
    geom_bar(stat="identity", colour = "black") +
    scale_fill_manual(values = col.tHM, labels = list.tHM) +
    xlab("") +
    ylab("") +
    # scale_x_discrete(labels=c("Quartzose","Feldspathic")) +
    theme_bw() +
    theme(legend.position = "top", 
          legend.key = element_rect(fill="white", colour = "black"),
          legend.key.size = unit(0.5, "cm")) +
    guides(colour = guide_legend(override.aes = list(size=4)),
           fill = guide_legend(label.hjust = 5, nrow = 1, override.aes = list(colour = NULL), title = NULL)) +
  coord_flip() +
  ggsave("graphs/tHM_mean.svg",height=3, width=15 )

# Ratios ####
ratio <- HM.composition[,c(1,4,72,73,75)]
ratio$type <- "qemscan"
ratio <- merge(ratio,HM.weight[,c(1,40,41)], by="sample")

ratio1 <- master[,c(1,4,26,27,29)]
ratio1$type <- "master"
ratio1 <- merge(ratio1,master.weight[,c(1,19,20)], by="sample")

ratio <- rbind(ratio,ratio1)
rm(ratio1)
labels <- c(master = "63-400 µm", qemscan = "63-125 µm")

ratio %>%
  ggplot(aes(GZi, ZTR, colour=provenance)) +
  scale_colour_manual(values = col.prov,guide=FALSE) +
  scale_shape_manual(values=c(15,19),guide=FALSE) +
  geom_point(size=2) +
  theme_bw(base_size = 20) +
  facet_wrap(~type, labeller=labeller(type = labels)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        plot.title = element_text(size = 12))
  ggsave("graphs/tHM_ratio1.svg", width=8, height=10)

ratio %>%
  ggplot(aes(ATi, ZTR, colour=provenance, shape=type)) +
  scale_colour_manual(values = col.prov) +
  scale_shape_manual(values=c(15,19)) +
  geom_point(size=2) +
  theme_bw(base_size = 20) +
  ylab("") +
  facet_wrap(~type, labeller=labeller(type = labels)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        plot.title = element_text(size = 12))
  ggsave("graphs/tHM_ratio2.svg", width=10, height=10)

# Mica distribution ####
mica <- HM.composition[list.Mica]/HM.composition$M*100
mica <- cbind(mica,sample=HM.composition$sample,provenance=HM.composition$provenance,strati_order=HM.composition$strati_order)
mica[,c(-4:-6)] <-round(mica[,c(-4:-6)],2)
key <- substring(paste0(list.Mica,"            "),1,4) # add space to labels

mica.voirons <- subset(mica, provenance=="voirons")[,-5]
mica.melt <- melt(mica.voirons,id.vars=c('strati_order','sample'), measure.vars=list.Mica)
p1 <- tHM.barplot(mica.melt,col.Mica)
p1 + ggsave("graphs/mica_voirons.pdf", width=10, height=6)

mica.vouan <- subset(mica, provenance=="vouan")[,-5]
mica.melt <- melt(mica.vouan,id.vars=c('strati_order','sample'), measure.vars=list.Mica)
p2 <- tHM.barplot(mica.melt,col.Mica)
p2 + ggsave("graphs/mica_vouan.pdf", width=10, height=3.5)

# Garnet distribution ####
garnet_bis <- HM.composition[list.Grt]/HM.composition$Grt.tot*100
colnames(garnet_bis) <- list.Grt2
garnet_bis <- round(garnet_bis,2)
garnet_bis <- cbind(garnet_bis,sample=HM.composition$sample,provenance=HM.composition$provenance,strati_order=HM.composition$strati_order)
garnet_bis.melt <- melt(garnet_bis,id.vars=c('provenance','strati_order','sample'), measure.vars=list.Grt2)    

ggplot(data=garnet_bis.melt,aes(variable, value, group = sample, fill=provenance)) +
  geom_bar(stat="identity", colour = "black",position="dodge", size=0.1) +
  scale_fill_manual(values = col.prov) +
  xlab("") +
  ylab("counts") +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x=element_blank(),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text.x = element_text(size=14, face="bold"),
        legend.position = "top") +
  guides(fill = guide_legend(override.aes = list(colour = NULL), title = NULL)) +
  # guides(fill=FALSE) +
  # facet_grid(.~provenance, scale = "free_x", space = "free_x") +
  ggsave("graphs/garnet.svg", width=14, height=6)

tern.garnet <- ternDickinson(lines_garnet,"Grt.Mg","Grt.FeMn","Grt.Ca") + 
  geom_point(data = HM.composition, 
             aes(Grt.FeMn,Grt.Mg,Grt.Ca,colour = provenance)) +
  scale_colour_manual(values=c("#549ccc","#ff1926")) +
  xlab("Fe+Mn") +
  ylab("Mg") +
  zlab("Ca") +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/garnet_metam.svg", width=10, height=10)

# Tourmaline distribution ####
tourmaline <- HM.composition[list.Tur]/HM.composition$Tur.tot*100
colnames(tourmaline) <- c("Undet. tourmaline", "Dravite", "Schorl")
tourmaline <- round(tourmaline,2)
tourmaline <- cbind(tourmaline,sample=HM.composition$sample,provenance=HM.composition$provenance,strati_order=HM.composition$strati_order)
tourmaline.melt <- melt(tourmaline,id.vars=c('provenance','strati_order','sample'), measure.vars=c("Undet. tourmaline", "Dravite", "Schorl")) 

ggplot(data=tourmaline.melt,aes(variable, value, group = sample, fill=provenance)) +
  geom_bar(stat="identity", colour = "black",position="dodge", size=0.1) +
  scale_fill_manual(values = col.prov) +
  xlab("") +
  ylab("") +
  ylim(0,100) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x=element_blank(),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text.x = element_text(size=14, face="bold"),
        legend.position = "top") +
  guides(fill = guide_legend(override.aes = list(colour = NULL), title = NULL)) +
  # guides(fill=FALSE) +
  # facet_grid(.~provenance, scale = "free_x", space = "free_x") +
  ggsave("graphs/tourmaline.svg", width=8, height=6)

# Garzanti ternary ####
ggtern(HM.composition, aes(x = Px+Spl, y = Hbl+Grt.tot+St+AKS, z = Zrn+Tur+Rt+Ep+Chl+Ap+Brt+Spl+Ttn, colour = provenance)) +
  geom_point() + 
  scale_colour_manual(values = col.prov) +
  Tlab("Ap+Grt+HgM") + Llab("Px+Ol+Spl") + Rlab("ZTR+LgM+&HM") +
  theme_bw(base_size=20) +
  theme_noarrows() +
  ggsave("graphs/garzanti_metam.svg", width=10, height=10)

# Master ####
list.master <- c("Ap","Grt","Rt","St","Ttn","Tur","Zrn","tHM.trace")
col.tHM.master <- c("#f2c54b","#ff1a28","#6bc4ff","#569dcc","#366280","#837266","#4c3a2c","#99bb44")

write.csv(master, "reports/HM_master.csv")

# master <- subset(master, site == "Moutonnière" | site == "Molière")
relative <- master[list.master]/master$tHM*100
# relative$total <- rowSums(relative[,1:8])
relative$sample <- master$sample
relative$provenance <- master$provenance

tHM.master.voirons <- subset(relative, provenance=="quartzose")[,-10]
tHM.melt <- melt(tHM.master.voirons,id.vars='sample')
tHM.barplot(tHM.melt,col.tHM.master,list.master) +
  ggsave("graphs/tHM_master_quartzose.pdf", width=10, height=6)

tHM.master.vouan <- subset(relative, provenance=="feldspathic")[,-10]
tHM.melt <- melt(tHM.master.vouan,id.vars='sample')
tHM.barplot(tHM.melt,col.tHM.master,list.master) +
  ggsave("graphs/tHM_master_feldspath.pdf", width=10, height=3.5)

# HM.moy.master <- by(relative[, 1:8], relative$provenance, colMeans)
# HM.master <- as.data.frame(rbind(Quartzose = HM.moy.master[["quartzose"]],
#                                  Feldspathic = HM.moy.master[["feldspathic"]]))
# HM.master <- cbind(provenance = rownames(HM.master), HM.master)
master.melt <- melt(relative, id.vars=c("sample","provenance"))

ggplot(master.melt, aes(sample, value, fill=variable)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(values = col.tHM.master) +
  xlab("") +
  ylab("") +
  theme_bw(base_size = 20) +
  theme(legend.position="top") +
  guides(fill = guide_legend(override.aes = list(colour = NULL), title = NULL)) +
  coord_flip() +
  ggsave("graphs/tHM_master.svg", width=20, height=4)

final <- data.frame(sample = HM.composition$sample, 
                    location = HM.composition$site,
                    petrofacies = HM.composition$provenance,
                    Ap = HM.composition$Ap, 
                    Mz = HM.composition$Mnz,
                    Chl = HM.composition$Chl,
                    FeO = HM.composition$FeO + HM.composition$Hem + HM.composition$Ilm + HM.composition$Lim,
                    Other = HM.composition$Fsp + HM.composition$Kln + HM.composition$Qz,
                    Opaque = HM.composition$OO + HM.composition$Py,
                    Grt_undet = HM.composition$ Grt,
                    Alm = HM.composition$Alm,
                    And = HM.composition$And,
                    Grs = HM.composition$Grs,
                    Prp = HM.composition$Prp,
                    Sps= HM.composition$Sps,
                    Uv = HM.composition$Uv,
                    Alm.Prp = HM.composition$Alm.Prp,
                    Alm.Prp.Grs = HM.composition$Alm.Prp.Grs,
                    Alm.Sps = HM.composition$Sps,
                    otHM = HM.composition$Hbl + HM.composition$Px + HM.composition$Srp + HM.composition$Spl + HM.composition$Tlc + HM.composition$Rhy,
                    Bt = HM.composition$Bt,
                    Ms = HM.composition$Ms,
                    Rt = HM.composition$Rt,
                    St = HM.composition$St,
                    Ttn = HM.composition$Ttn,
                    Tur_undet = HM.composition$Tur,
                    Drv = HM.composition$Drv,
                    Srl = HM.composition$Srl,
                    Zrn = HM.composition$Zrn,
                    Undet = HM.composition$Undet)

final$total <- rowSums(final[,-c(1:5)])
final <- arrange(final, sample)
weight <- arrange(weight, sample)
final$HMC <- weight$HMC
final$tHMC <- weight$tHMC
write.csv(final, "reports/HM_final.csv")
