# title: garnet and tourmaline geochemistry using QemScan analysis from the Voirons Flysch
# date: 2016/11/24

# load dataset ####
Grt_geoch <- read.csv("data/GarnetGeoch.csv", dec=",", header = TRUE)
Tur_geoch <- read.csv("data/TourmalineGeoch.csv", dec=",", header = TRUE)

# add provenance column ####
Grt_geoch$provenance[Grt_geoch$sample=="JR349"|Grt_geoch$sample=="JR341"|Grt_geoch$sample=="JR329"|Grt_geoch$sample=="JR352"|Grt_geoch$sample=="JR75"|Grt_geoch$sample=="JR273"|Grt_geoch$sample=="JR278"|Grt_geoch$sample=="JR330"|Grt_geoch$sample=="JR353"|Grt_geoch$sample=="JR370"|Grt_geoch$sample=="JR376"|Grt_geoch$sample=="JR5"|Grt_geoch$sample=="JR57"] <- "quartzose"
Grt_geoch$provenance[Grt_geoch$sample=="JR331"|Grt_geoch$sample=="JR275"|Grt_geoch$sample=="JR20"|Grt_geoch$sample=="JR21"|Grt_geoch$sample=="JR347"] <- "feldspathic"

Tur_geoch$provenance[Tur_geoch$sample=="JR349"|Tur_geoch$sample=="JR341"|Tur_geoch$sample=="JR329"|Tur_geoch$sample=="JR352"|Tur_geoch$sample=="JR75"|Tur_geoch$sample=="JR273"|Tur_geoch$sample=="JR278"|Tur_geoch$sample=="JR330"|Tur_geoch$sample=="JR353"|Tur_geoch$sample=="JR370"|Tur_geoch$sample=="JR376"|Tur_geoch$sample=="JR5"|Tur_geoch$sample=="JR57"] <- "quartzose"
Tur_geoch$provenance[Tur_geoch$sample=="JR331"|Tur_geoch$sample=="JR275"|Tur_geoch$sample=="JR20"|Tur_geoch$sample=="JR21"|Tur_geoch$sample=="JR347"] <- "feldspathic"

# convert element to oxide ####
Grt_geoch <- mutate(Grt_geoch,
                    SiO2=Si*2.1393,
                    TiO2=Ti*1.6683,
                    Al2O3=Al*1.8895,
                    FeO=Fe*1.2865,
                    MnO=Mn*1.2912,
                    MgO=Mg*1.6583,
                    CaO=Ca*1.3992,
                    BaO=Ba/0.89534,
                    Cs2O=Cs/0.94323,
                    ZnO=Zn/0.80337,
                    Cr2O3=Cr*1.4616,
                    NiO=Ni/0.78584)
Grt_geoch[is.na(Grt_geoch)] <- 0
Grt_geoch$total_oxide <- rowSums(Grt_geoch[,c(71:82)])

# recalculate to 100% ####
list.oxide <- c("SiO2","TiO2","Al2O3","FeO","MnO","MgO","CaO","BaO","Cs2O","ZnO","Cr2O3","NiO")
Grt_geoch[,c(71:82)] <- Grt_geoch[list.oxide]/Grt_geoch$total_oxide*100
Grt_geoch$total_oxide <- rowSums(Grt_geoch[,c(71:82)])
write.csv(Grt_geoch, "reports/geoch_garnet_mod.csv", row.names = FALSE)

# import end-member after calculation ####
Grt_geoch <- read.csv("reports/geoch_garnet_final.csv",header = TRUE)

# Morton2007 discrimination ternary diagram ####
ggtern(Grt_geoch, aes(Mn+Fe, Mg, Ca, colour=provenance)) + 
  geom_point(size=2) +
  scale_colour_manual(values = col.prov) +
  geom_mask() + 
  theme_bw(base_size = 20) +
  ggsave("graphs/garnet_ternary.svg", width=6, height=6)

# garnet end-member distribution by QemScan class ####
melt(Grt_geoch[,c(2,84:89)]) %>%
  ggplot(aes(variable,value)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot() + 
  xlab("") +
  ylab("%") +
  facet_wrap(~type, nrow = 2) +
  theme_bw(base_size = 20) +
  ggsave("graphs/garnet_geochemistry.svg", width=20, height=6)

# garnet distribution by Qemscan class in Morton ternary diagram ####
ggtern(Grt_geoch, aes(Mn+Fe, Mg, Ca, colour=type)) + geom_point() + geom_mask() + theme_bw() +
  facet_wrap(~type, nrow = 2) 

# comparison with Alpine dataset ####
Grt_alps <- read.csv("data/GarnetAlps.csv", dec=",", header = TRUE)

ggtern(Grt_alps, aes(Spessartine, Almandine, Grossular, colour = domain)) + 
  geom_point(data=em, aes(Spessartine, Almandine, Grossular), color = "grey") +
  geom_point() +
  geom_mask() + 
  scale_colour_manual(values = c("#2b83ba","#abdda4","#fdae61","#d7191c","grey")) +
  theme_bw(base_size = 20) + 
  theme_rotate(degrees = 180) +
  guides(colour=FALSE) +
  ggsave("graphs/garnet_alps1.svg", height = 6, width = 6)

ggtern(Grt_alps, aes(Pyrope, Grossular, Almandine, colour = domain)) + 
  geom_point(data=em, aes(Pyrope, Grossular, Almandine, color = "Voirons Flysch")) +
  geom_point() + 
  geom_mask() +
  scale_colour_manual(values = c("#2b83ba","#abdda4","#fdae61","#d7191c","grey")) +
  theme_bw(base_size = 20) + 
  ggsave("graphs/garnet_alps2.svg", height = 6, width = 12)

# Henry1985 discrimination ternary diagram ####
ternDickinson(lines_tur,"Al","Fe","Mg") +
  geom_point(data = Tur_geoch, 
             aes(Fe,Al,Mg,colour = provenance)) +
  scale_colour_manual(values = col.prov) +
  tern_limits(1,0.5,0.5) +
  theme_bw(base_size = 20) +
  ggsave("graphs/tourmaline_ternary.svg",height=12,width=12)