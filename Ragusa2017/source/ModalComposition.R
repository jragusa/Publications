# boot ####
source("lib/lib.R")
source("lib/function.R")
source("lib/data_modal.R")

# Clustered group - Stratigraphic formation relations ####
provenance.freq <- table(composition$provenance)
provenance.freq <- as.data.frame(provenance.freq)
provenance.freq$Relative.freq <- provenance.freq$Freq/sum(provenance.freq$Freq)*100

member.freq <- table(composition$provenance,composition$unit)
member.freq <- melt(member.freq)

ggplot(member.freq, aes(Var2, value, fill = Var1)) + 
  geom_bar(stat = "identity", position="dodge") +
  xlab("") + ylab("Count") +
  scale_fill_manual(values=c(col.vouan,col.voirons)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour="black")) +
  guides(fill=guide_legend(title="provenance")) +
  ggsave("graphs/freq_provenance.svg", width=10, height=10)

ggplot(member.freq, aes(Var1, value, fill = Var2)) + 
  geom_bar(stat = "identity", position="dodge") +
  xlab("") + ylab("Count") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour="black")) +
  guides(fill=guide_legend(title="formation")) +
  ggsave("graphs/freq_formation.pdf", width=10, height=10)

# Zuffa distribution
select(composition,NCE,NCI,CE,CI,C,Pore) %>%
  mutate(total = NCE + NCI + CE + CI + C + Pore) -> zuffa

zuffa.perc <- zuffa[c("NCE","NCI","CE","CI","C","Pore")]/zuffa$total*100
  melt(zuffa.perc) %>%
  ggplot(aes(variable, value, fill=variable)) +
    stat_boxplot(geom ='errorbar', width = 0.2) + 
    geom_boxplot() +
    scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey"),
                    guide=FALSE) +
    xlab("") +
    ylab("%") +
    theme_bw(base_size = 20) +
    theme(axis.text.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold")) +
    ggsave("graphs/zuffa.svg", width=10, height=10)

# QmFLt ternary diagram ####
ternDickinson(lines_qmflt,"Qm","F","Lt") + 
  geom_point(data = composition, 
             aes(F,Qm,Lt,colour = provenance)) +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(F,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(F,Qm,Lt+Lci), 
                  breaks = 0.9, 
                  linetype = 2, 
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lt > 0), 
                  aes(F,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lt > 0), 
                  aes(F,Qm,Lt+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qmflt.svg", width=10, height=10)

# QmFLt ternary diagram with formation affiliation ####
ternDickinson(lines_qmflt,"Qm","F","Lt") + 
  geom_point(data = composition, 
             aes(F,Qm,Lt,colour = unit)) + 
  scale_colour_brewer(palette = "Set1",
                      # values = c("#DA334C", "#FF832C", "purple", "#45788C", "#DA334C", "#362D15","#DBB467"),
                      label = c("Boëge Marl","Bruant Sandstone","Gurnigel flyschs","Ultrahelvetic ?", "Vouan Conglomerate","Voirons Sandstone")) +
  guides(colour=guide_legend(title="Formation")) +
    geom_confidence_tern(data = subset(composition, provenance == "Feldspathic"), 
                    aes(F,Qm,Lt), 
                    breaks = 0.9, 
                    linetype = 1, 
                    colour = col.vouan) +
    geom_confidence_tern(data = subset(composition, provenance == "Feldspathic"), 
                    aes(F,Qm,Lt+Lci), 
                    breaks = 0.9, 
                    linetype = 2, 
                    colour = col.vouan) +
    geom_confidence_tern(data = subset(composition, provenance == "Quartzose" & Lt > 0), 
                    aes(F,Qm,Lt), 
                    breaks = 0.9, 
                    linetype = 1,
                    colour = col.voirons) +
    geom_confidence_tern(data = subset(composition, provenance == "Quartzose" & Lt > 0), 
                    aes(F,Qm,Lt+Lci), 
                    breaks = 0.9, 
                    linetype = 2,
                    colour = col.voirons) +
  theme_bw(base_size = 20) +
  theme_noarrows() +
  ggsave("graphs/qmflt_formation.svg", width=10, height=10)
  
# QFL ternary diagram ####
ternDickinson(lines_qfl,"Q","F","L") + 
  geom_point(data = composition, 
             aes(F,Q,L,colour = provenance)) +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0), 
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qfl.svg", width=10, height=10)

# QpLsmLvm ####
ggtern(data=composition, aes(Lsm,Qp,Lvm,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Lsm > 0 & Lvm > 0), 
                  aes(Lsm,Qp,Lvm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Lsm > 0 & Lvm > 0), 
                  aes(Lsm+Lci,Qp,Lvm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lsm > 0 & Lvm > 0 & Qp > 0), 
                  aes(Lsm,Qp,Lvm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lsm > 0 & Lvm > 0 & Qp > 0), 
                  aes(Lsm+Lci,Qp,Lvm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qplsmlvm.svg", width=10, height=10)

# LvLsLm ####
ggtern(composition, aes(Ls,Lv,Lm,colour = provenance, group = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, Lv > 0 & Ls > 0 & Lm > 0), 
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour =  col.vouan) +
  geom_confidence(data = subset(composition, Lv > 0 & Ls > 0 & Lm > 0), 
                  aes(Ls+Lci,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour =  col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lv > 0 & Ls > 0 & Lm > 0), 
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lv > 0 & Ls > 0 & Lm > 0), 
                  aes(Ls+Lci,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/lslvlm.svg", width=10, height=10)

# QAP igneous ####
ternDickinson(lines_qap,"Q","A","P") + 
  geom_point(data = composition, 
             aes(Krg,Qrg,PArg,colour = provenance)) +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Krg > 0 & PArg > 0 & Qrg > 0), 
                  aes(Krg,Qrg,PArg), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Lt > 0 & Krg > 0 & PArg > 0 & Qrg > 0), 
                  aes(Krg,Qrg,PArg), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  ggsave("graphs/Lp.svg", width=10, height=10)

# QAP volcanic ####
ternDickinson(lines_qap,"Q","A","P") +
  geom_point(data = composition, 
             aes(Krv,Qrv,PArv,colour = provenance)) +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  ggsave("graphs/Lv.svg", width=10, height=10)

tot <- ddply(composition, .(provenance), summarise, Rm1.tot=sum(Rm1e), Rm3.tot=sum(Rm3e),Rm5.tot=sum(Rm5e))

composition$MI[is.na(composition$MI)] <- 0
tot <- composition %>%
  select(provenance,Rm1e,Rm3e,Rm5e) %>%
  group_by(provenance) %>%
  summarise_each(funs(mean))

# Metamorphic grade ####
ggtern(data=composition, aes(Rm3e, Rm1e, Rm5e, colour = provenance)) +
  geom_point() +
  geom_point(data=tot,aes(Rm3e, Rm1e, Rm5e),size=4) +
  scale_colour_manual(values=c(col.vouan,col.voirons)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  guides(colour = guide_legend(override.aes = list(size=2), title="provenance")) +
  xlab("Rm3+") + ylab("Rm1+") + zlab("Rm5+") +
  ggsave("graphs/Rm.svg", width=10, height=10)

composition$QpTLt <- composition$QpT/composition$Lt*100
composition$QpTLt[composition$QpTLt==0] <- 0.00000001
composition$QpTLt[composition$QpTLt=="NaN"] <- 0.00000001

# QpT/Lt ####
QpTLt_mean <- composition %>%
  select(provenance,QpTLt) %>%
  group_by(provenance) %>%
  summarise_each(funs(mean))

ggplot(data=composition,aes(provenance, QpTLt , group=provenance, fill = provenance)) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", size=3, color = "black") +
  scale_fill_manual(values=c(col.vouan,col.voirons)) +
  xlab("") +
  ylab("Qpt/Lt") +
  theme_bw(base_size = 24) +
  guides(fill=guide_legend(title="provenance")) +
  expand_limits(y=c(0,40)) +
  ggsave("graphs/QpTLt.svg", width=10, height=8)

# MI index ####
MI %>% 
  group_by(provenance) %>%
  summarise_each(funs(mean))

select(composition,provenance,MI) %>%
  melt() %>%
  # subset(value > 1) %>%   # remove free Lm samples
ggplot(aes(provenance, value, fill=provenance)) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", size=3, color = "black") +
  scale_fill_manual(values=c(col.vouan,col.voirons)) +
  xlab("") +
  ylab("MI index") +
  theme_bw(base_size = 24) +
  guides(fill=guide_legend(title="provenance")) +
  ggsave("graphs/MI.svg", width=10, height=8)

# KPA ternary diagram ####
ggtern(data=composition, aes(P,K,A,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & F > 0 & A > 0), 
                  aes(P,K,A), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & K > 0 & A > 0 & P > 0), 
                  aes(P,K,A), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  xlab("An") + ylab("K") + zlab("Ab") + 
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/kpa.svg", width=10, height=10)

# QrQpQms ternary diagram ####
ggtern(data=composition, aes(Qp,Qms,Qr,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(Qp,Qms,Qr), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Qr > 0 & Qp > 0), 
                  aes(Qp,Qms,Qr), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qrqpqms.svg", width=10, height=10)

# QmKP ternary diagram ####
ggtern(data=composition, aes(K,Qm,P,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(K,Qm,P), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & K > 0 & P > 0), 
                  aes(K,Qm,P), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qmkp.svg", width=10, height=10)

# Kr ternary diagram ####
ggtern(data=composition, aes(Krg,Ks,Krm,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
#   geom_confidence(data = subset(data, cluster.prov2 == "Vouan" & Ks > 0 & Krg > 0 & Krm > 0), 
#                   aes(Krg,Ks,Krm), 
#                   breaks = 0.9, 
#                   linetype = 1,
#                   colour = col.vouan) +
#   geom_confidence(data = subset(data, cluster.prov2 == "Voirons" & Ks > 0 & Krg > 0 & Krm > 0), 
#                   aes(Krg,Ks,Krm), 
#                   breaks = 0.9, 
#                   linetype = 1,
#                   colour = col.voirons) +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/kr.svg", width=10, height=10)
  
# Pr ternary diagram ####
ggtern(data=composition, aes(PArg,PAs,PArm,colour = provenance)) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
#   geom_confidence(data = subset(data, cluster.prov2 == "Vouan" & PAs > 0 & PArg > 0 & PArm > 0), 
#                   aes(PArg,PAs,PArm), 
#                   breaks = 0.9, 
#                   linetype = 1,
#                   colour = col.vouan) +
#   geom_confidence(data = subset(data, cluster.prov2 == "Voirons" & PAs > 0 & PArg > 0 & PArm > 0), 
#                   aes(PArg,PAs,PArm), 
#                   breaks = 0.9, 
#                   linetype = 1,
#                   colour = col.voirons) +
  xlab("Prg") + ylab("Ps") + zlab("Prm") + 
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/pr.svg", width=10, height=10)

# test
test <- data.frame(prov=data$cluster.prov2,test=data$test*100)
test$test[test$test=="NaN"] <- 0.00000001

# QFR ternary diagram ####
ternDickinson(lines_folk,"Q","F","R") +
  geom_point(data = composition, 
             aes(F.folk, Q.folk, R.folk,colour = provenance)) +
  geom_confidence(data = subset(composition, provenance == "Vouan"), 
                  aes(F.folk,Q.folk,R.folk), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & R.folk > 0), 
                  aes(F.folk,Q.folk,R.folk), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  xlab("F") + ylab("Q") + zlab("R") +
  theme_bw(base_size = 20) +
  theme_noarrows() +
  guides(colour=guide_legend(title="provenance")) +
  ggsave("graphs/qfr.svg", width=10, height=10)

# porosity cement relation ####
ggplot(composition, aes(porecement, IE, colour = provenance)) +
  geom_point() +
  scale_colour_manual(values=c(col.voirons,col.vouan)) +
  xlim(-2,3) + ylim(0,3) +
  xlab("Log(Cement/Porosity)") + ylab("Log(NCE+CE)/(NCI+CI)") +
  theme_bw(base_size = 40) +
  ggsave("graphs/cement_pore.svg", width=10, height=10)

# Ratio table ####
composition <- mutate(composition,
             QF = Q/F,
             QpQ = Qp/Q*100,
             LcL = Lc/(L+Lc)*100,
             LcQFL = Lc/(Q+F+L+Lc)*100,
             LmL = Lm/L*100,
             QrgQr = Qrg/Qr*100)
# composition$RmR[is.infinite(composition$RmR)] <- 0 
# ratio <- aggregate(cbind(QF, QpQ, LcL, LcQFL, LmL, QrgQr) ~ provenance, composition, mean)
#                    function(x) c(mean = mean(x), sd = sd(x))
# ratio$MI <- MI_mean$MI
# ratio$QpTLt <- QpTLt_mean$QpTLt
# 
# print(xtable(ratio,
#              align = "llllllllllllll",
#              caption =c("Mean of value of ratios with standard deviation in brackets"),
#              label="tab:ratio"),
#       file="reports/ratio.tex",
#       type="latex",
#       include.rownames=FALSE,      
#       caption.placement="top",
#       floating=TRUE)

ratio_framework <- data.frame(provenance = composition$provenance,
                    QF = composition$QF, 
                    QpQ = composition$QpQ, 
                    LcL = composition$LcL, 
                    LcQFL = composition$LcQFL,
                    QrgQr = composition$QrgQr,
                    LmL = composition$LmL)
ratio_framework %>% 
  melt(id.vars="provenance") %>%
  ggplot(aes(variable, value, group = interaction(provenance,variable), fill = provenance)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot() + 
  scale_fill_manual(values=c(col.vouan,col.voirons),
                    label = c("Quartzose","Feldspathic")) +
  scale_x_discrete("",labels = c("QF" = "Q/F",
                                 "QpQ" = "Qp/Q",
                                 "LcL" = "Lc/L",
                                 "LcQFL" = "Lc/QFL",
                                 "QrgQr" = "Qrg/Qr",
                                 "LmL" = "Lm/L")) + 
  ylab("%") +
  theme_bw(base_size = 20) + 
  theme(legend.position="top",
        legend.title=element_blank()) +
  ggsave("graphs/ratio_framework.svg", width=20, height=10)

# Export dataframe ####
colnames(composition)[colnames(composition)=="Member"] <- "unit"
composition$unit[composition$unit == "UH"] <- "FS"

composition$formation[composition$unit == "FS"] <- "VS"
composition$formation[composition$unit == "AS"] <- "VS"
composition$formation[composition$unit == "VS"] <- "VS"
composition$formation[composition$unit == "VC"] <- "VC"
composition$formation[composition$unit == "BM"] <- "BM"
composition$formation[composition$unit == "BS"] <- "BS"
composition$formation[composition$unit == "Gu"] <- "Gu"

composition$formation[composition$provenance=="Vouan" & composition$formation == "VS"] <- "VC"
composition$formation[composition$provenance=="Vouan" & composition$formation == "VS"] <- "VC"

compo_final <- data.frame(sample = composition$Sample,
                          location = composition$Site, 
                          unit = composition$Member,
                          dickinson = composition$Dickinson,
                          Qms = composition$Qms,
                          Qps = composition$Qps,
                          QpT = composition$QpT,
                          Qrv = composition$Qrv,
                          Qrm = composition$Qrm,
                          Qrg = composition$Qrg,
                          Ks = composition$Kan + composition$Kpg + composition$Kmi + composition$Kp,
                          Krv = composition$Krv,
                          Krg = composition$Krg,
                          Krm = composition$Krm,
                          An = composition$Ps + composition$Ptw + composition$Pmy,
                          Anrv = composition$Prv,
                          Anrg = composition$Prg,
                          Anrm = composition$Prm,
                          Ab = composition$As + composition$Atw,
                          Abrg = composition$Arg,
                          Abrv = composition$Arm,
                          Abrm = composition$Arm,
                          Lcmi = composition$Lcmi + composition$Lcmif + composition$Lcmir + composition$Lcmic + composition$Lcmich,
                          Ls = composition$Lssc + composition$Lcsf + composition$Lcs + composition$Lss + composition$Lsp,
                          Lch = composition$Lch + composition$Lchb,
                          Lv = composition$Lva + composition$Lvf + composition$Lvg,
                          Lmf1 = composition$Lmf1,
                          Lmf2 = composition$Lmf2,
                          Lmf3 = composition$Lmf3,
                          Lmf4 = composition$Lmf4,
                          Rmf5 = composition$Rmf5,
                          Lmp1 = composition$Lmp1,
                          Lmp2 = composition$Lmp2,
                          Lmp3 = composition$Lmp3,
                          Lmp4 = composition$Lmp4,
                          Rmp5 = composition$Rmp5,
                          Lmb1 = composition$Lmb1,
                          C = composition$C,
                          D = composition$Ap + composition$Gr + composition$Hb + composition$Px + composition$Rt + composition$St + composition$Zr,
                          M = composition$Mb + composition$Mm + composition$Mrg + composition$Mrm,
                          Chl = composition$Chl,
                          Glt = composition$Gl,
                          Op = composition$Op,
                          Ph = composition$Ph + composition$Fph,
                          Si = composition$Si,
                          Bc = composition$AR + composition$Bz + composition$Sh + composition$Das + composition$Ech + composition$Fc + composition$Md + composition$Oo + composition$Ra + composition$Se,
                          Pore = composition$Pore)

write.csv(compo_final, "reports/ESM_1.csv", row.names = FALSE)
