# 1 - files ####
weight <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015/5_Mineraux-lourds/data/HM_weight.csv", header = TRUE, comment.char = "#", dec = ",")
HM.composition <- read.csv("data/HM.csv", header = TRUE, comment.char = "#", dec = ",")
master <- read.csv("data/HM_master.csv", header = TRUE, comment.char = "#", dec = ",")
density <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015/5_Mineraux-lourds/data/HM_density.csv", header = TRUE, comment.char = "#", dec = ",")

# 2 - Groups species ####
HM.composition <- mutate(HM.composition,
       FeO.tot = FeO + Hem + Ilm + Lim,
       Grt.tot = Grt + Alm + And + Grs + Prp + Sps + Uv + Alm.Prp + Alm.Prp.Grs + Alm.Sps,
       Grt.Mg = 0.5*Alm.Prp,
       Grt.FeMn = Alm+0.5*Alm.Prp+0.3*Alm.Prp.Grs+Alm.Sps,
       Grt.Ca = And+Grs+Uv,
       Srl = Srl + Tur,
       Tur.tot = Tur + Drv + Srl,
       M = Bt + Ms + Chl)

# 3 - Major groups ####
HM.composition <- mutate(HM.composition,
       tHM.trace = AKS + Brt + Brl + Cst + Crn + Ep + Hbl + Px + Spl + Tlc + Xtm + Rhy,
       tHM = Ap + Mnz + Grt.tot + Rt + St + Ttn + Tur + Zrn + tHM.trace,
       Op = FeO.tot + OO + Py,
       LM = Qz + Fsp + Kln,
       D = tHM + Op + M,
       total = D + LM )

# 4 - Index ####
HM.composition <- mutate(HM.composition,
                         Ustable = Rt + Tur + Zrn,
                         Stable = Ap + Grt.tot + St + Ep + AKS + Ttn + Xtm + Mnz,
                         Instable = Hbl + Px,
                         GZi = Grt.tot/(Grt.tot + Zrn)*100,
                         ATi = Ap/(Ap + Tur)*100,
                         ATi.corr = Ap/(Ap + Srl + Drv)*100,
                         ZTR = (Zrn + Tur + Rt)/(D - Op)*100,
                         Op.ratio = Op/total,
                         Udense = (Grt.tot + Rt + Brt + Spl + Zrn + Xtm + Mnz)/tHM,
                         MMI = (1/3*St + 2/3*AKS + AKS)/(Chl + St + AKS)*100,
                         # Other.ratio = O / total * 100,
                         D.false = Fsp + Qz + Cal + Tlc + Kln,
                         tHM.diag = Cal)

# 5 - Sorting ####
HM.composition <- HM.composition[order(HM.composition$provenance,HM.composition$strati_order),] 
weight <- weight[order(weight$provenance,weight$strati_order),] 

# HM.composition.split <- split(HM.composition, f = HM.composition$provenance)
# voirons <- HM.composition.split[[1]]
# vouan <- HM.composition.split[[2]]

col.quartzose <- "#569dcc"
col.feldspathic <- "#ff1a28"
col.prov <- c("feldspathic" = "#ff1a28", "quartzose"="#569dcc")

# HM Master ####
master <- mutate(master,
                 tHM.trace = Hb,
                 Tur = Tur_yellowgreen + Tur_blue + Tur_iso,
                 tHM = Ap + Grt + Rt + St + Ttn + Tur + Zrn + tHM.trace,
                 M = Bt + Ms + Chl,
                 GZi = Grt/(Grt + Zrn)*100,
                 ATi = Ap/(Ap + Tur)*100,
                 total = tHM + M + Op + Glt,
                 ZTR = (Zrn + Tur + Rt)/(total - Op)*100)