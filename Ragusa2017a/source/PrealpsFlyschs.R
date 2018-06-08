# 1 - Boot ####
source("lib/lib.R")
source("lib/data_flysch.R")
source("lib/function.R")

# 2 - QFL diagrams ####
prealps.sorted <- filter(flysch_ternary, nappe == "Niesen" | group_unit == "Upper Prealpine" | formation == "Médianes Flysch")

ternDickinson(lines_qfl,"Q","F","L") +
  geom_point(data = prealps.sorted, aes(F,Q,L,colour = nappe)) +
  geom_point(data = composition, aes(F,Q,L,colour = provenance)) +
  geom_polygon(data = upn, aes(F,Q,L, fill = formation), colour = "black") +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0),
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0),
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Vouan"),
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan"),
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Sarine"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.sarine) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Dranses"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.dranses) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Niesen"), 
                  aes(F,Q,L),
                  breaks = 0.9,
                  linetype = 1,
                  colour = col.niesen) +
  geom_confidence(data = subset(prealps.sorted, formation == "Médianes Flysch"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.medianes) +
  scale_colour_manual(values = c(col.dranses,col.niesen,col.medianes,col.sarine,col.voirons,col.vouan),
                      labels = c("Dranses Flysch","Niesen flyschs", "Médianes Flysch", "Sarine Flysch", "Voirons provenance", "Vouan provenance")) +
  scale_fill_manual(values = c(NA, NA, NA),
                    labels = c("Simme Flysch","Simme Flysch","Simme Flysch")) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  theme_noarrows() +
  ggsave("graphs/qfl_prealps.svg", width=10, height=10)
  
prealps.sorted <- filter(flysch_ternary, group_unit == "Gurnigel")

ternDickinson(lines_qfl,"Q","F","L") +
  geom_point(data = prealps.sorted, aes(F,Q,L,colour = nappe)) +
  geom_point(data = flysch_composition, aes(F,Q,L,colour = nappe)) +
  geom_point(data = composition, aes(F,Q,L,colour = provenance)) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0),
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0),
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Vouan"),
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan"),
                  aes(F,Q,L+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
    geom_confidence(data = subset(prealps.sorted, nappe == "Voirons-Schlieren"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.winkler1984) +
    geom_confidence(data = subset(prealps.sorted, nappe == "Wägital"), 
                  aes(F,Q,L), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.wagital) +
    geom_confidence(data = flysch_composition, 
                    aes(F,Q,L), 
                    breaks = 0.9, 
                    linetype = 1, 
                    colour = col.schlieren) +
  scale_colour_manual(values = c(col.schlieren,col.voirons,col.winkler1984,col.vouan,col.wagital)) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  ggsave("graphs/qfl_gurnigel.svg", width=10, height=10)

# 4 - QmFLt diagram ####
prealps.sorted <- filter(flysch_ternary, group_unit == "Gurnigel")

ternDickinson(lines_qmflt,"Qm","F","Lt")  + 
  geom_point(data = prealps.sorted, aes(F.1,Qm,Lt,colour = nappe)) +
  geom_point(data = flysch_composition, aes(F,Qm,Lt,colour = nappe)) +
  geom_point(data = composition, aes(F,Qm,Lt,colour = provenance)) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & L > 0),
                  aes(F,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) + 
  geom_confidence(data = subset(composition, provenance == "Vouan"),
                  aes(F,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Wägital"), 
                  aes(F.1,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.wagital) +
  geom_confidence(data = flysch_composition, 
                  aes(F,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.schlieren) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Voirons-Schlieren"), 
                  aes(F.1,Qm,Lt), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.winkler1984) +
  scale_colour_manual(values = c(col.schlieren,col.voirons,col.winkler1984,col.vouan,col.wagital)) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  ggsave("graphs/qmflt_gurnigel.svg", width=10, height=10)

# 5 - LvLsLm diagram ####
prealps.sorted <- filter(flysch_ternary, Ls > 0)

ggtern() + 
  geom_point(data = prealps.sorted, aes(Ls,Lv,Lm,colour = nappe)) +
  geom_point(data = flysch_composition, aes(Ls,Lv,Lm,colour = nappe)) +
  geom_point(data = composition, aes(Ls,Lv,Lm,colour = provenance)) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Ls+Lci,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Ls+Lci,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Wägital"), 
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.wagital) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Voirons-Schlieren"), 
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.winkler1984) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Niesen"), 
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.niesen) +
  geom_confidence(data = subset(flysch_composition, Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Ls,Lv,Lm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.schlieren) +
  theme_noarrows() +
  scale_colour_manual(values = c(col.niesen,col.schlieren,col.voirons,col.winkler1984,col.vouan,col.wagital)) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  ggsave("graphs/lvlslm_gurnigel.svg", width=10, height=10)
  
# 5 - QpLsmLvm diagram ####
prealps.sorted <- filter(flysch_ternary, Qp > 0)
ggtern() + 
  geom_point(data = prealps.sorted, aes(Lvm,Qp,Lsm,colour = nappe)) +
  geom_point(data = flysch_composition, aes(Lvm,Qp,Lsm,colour = nappe)) +
  geom_point(data = composition, aes(Lvm,Qp,Lsm,colour = provenance)) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Lvm,Qp,Lsm), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Voirons" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Lvm,Qp,Lsm+Lci), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.voirons) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Lvm,Qp,Lsm+Lci), 
                  breaks = 0.9, 
                  linetype = 1,
                  colour = col.vouan) +
  geom_confidence(data = subset(composition, provenance == "Vouan" & Ls > 0 & Lm > 0 & Lv > 0),
                  aes(Lvm,Qp,Lsm), 
                  breaks = 0.9, 
                  linetype = 2,
                  colour = col.vouan) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Wägital"), 
                  aes(Lvm,Qp,Lsm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.wagital) +
  geom_confidence(data = subset(prealps.sorted, nappe == "Voirons-Schlieren"), 
                  aes(Lvm,Qp,Lsm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.winkler1984) +
  geom_confidence(data = subset(flysch_composition, Qp > 0 & Lsm > 0 & Lvm > 0),
                  aes(Lvm,Qp,Lsm), 
                  breaks = 0.9, 
                  linetype = 1, 
                  colour = col.schlieren) +
  theme_noarrows() +
  scale_colour_manual(values = c(col.schlieren,col.voirons,col.winkler1984,col.vouan,col.wagital)) +
  guides(colour=guide_legend(title="provenance")) +
  theme_bw(base_size = 24) +
  ggsave("graphs/qplvmlsm_gurnigel.svg", width=10, height=10)
  
# Heavy minerals flysch ####
list.prealps <- c("Ap","Mnz","Grt","Rt","St","Ttn.tot","Tur","Zrn","Spl","tHM.trace")
col.prealps <- c("#f2c54b","#f28322","#ff1a28","#6bc4ff","#569dcc","#366280","#837266","#4c3a2c","#8E4B75","#99bb44")
label.HM <- c("Apatite ","Monazite  ","Garnet  ","Rutile  ","Staurolite  ","Titanite  ","Tourmaline  ","Zircon  ","Spinel  ","Others HM  ")
list.flysch <- c("Niesen","Voirons","Niremont","Gurnigel","Schlieren","Wägital","Médianes","Brèche","Sarine","Dranse","Simme","Gets")

relative <- prealps[list.prealps]/prealps$tHM*100
relative$total <- rowSums(relative[,1:10])
relative$nappe <- prealps$nappe
relative$unit <- prealps$unit

HM.moy.nappe <- by(relative[, 1:10], relative$nappe, colMeans)
HM.moy.unit <- by(relative[, 1:10], relative$unit, colMeans)

prealps.sorted <- as.data.frame(rbind(Gets = HM.moy.nappe[["Gets"]],
                                      Simme = HM.moy.nappe[["Simme"]],
                                      Dranse = HM.moy.nappe[["Dranse"]],
                                      Sarine = HM.moy.nappe[["Sarine"]],
                                      Brèche = HM.moy.nappe[["Brèche"]],
                                      Médianes = HM.moy.nappe[["Médianes"]],
                                      Wägital = HM.moy.unit[["Wägital"]],
                                      Schlieren = HM.moy.unit[["Schlieren"]],
                                      Gurnigel = HM.moy.nappe[["Gurnigel"]],
                                      Niremont = HM.moy.unit[["Niremont"]],
                                      Voirons = HM.moy.unit[["Voirons"]],
                                      Niesen = HM.moy.nappe[["Niesen"]]))

prealps.sorted <- cbind(units = rownames(prealps.sorted), prealps.sorted)
prealps.sorted <- transform(prealps.sorted, units = factor(units, levels = list.flysch))

prealps.melt <- melt(prealps.sorted, id.vars="units")

ggplot(prealps.melt, aes(units, value, fill=variable)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(values = col.prealps, 
                    labels = label.HM) +
  xlab("") +
  ylab("") +
  theme_bw(base_size = 20) +
  theme(legend.position="top") +
  guides(fill = guide_legend(override.aes = list(colour = NULL), title = NULL)) +
  coord_flip() +
  ggsave("graphs/flysch_tHM.svg", width=20, height=10)
