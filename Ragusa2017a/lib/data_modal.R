# 1 - files ####
# composition <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015a/3_Provenance/data/composition.csv", header = TRUE)
composition <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2016a/data/composition.csv", header = TRUE)

# 2 - Voirons flysch ####
composition <- na.omit(composition)
composition <- mutate(composition,
                      Qr = Qrg + Qrm + Qrv,
                      Qp = Qps + QpT,
                      Q = Qms + Qp + Qr,
                      Qm = Qms + Qrg + Qrm + Qrv,
                      Ks = Kor + Kan + Kpg + Kmi + Kp,
                      Kr = Krg + Krm + Krv,
                      K = Ks + Kr,
                      A = Arg + Arm + Arv + As + Atw,
                      P = Prg + Prm + Prv + Ps + Pmy + Ptw,
                      PAs = Ps + Pmy + Ptw + As + Atw,
                      PArg = Prg + Arg,
                      PArm = Prm + Arm,
                      PArv = Prv + Arv,
                      PA = P + A,
                      F = PA + K,
                      Lci = Lcmi + Lcmif + Lcmir + Lcmic + Lcmich + Lssc,
                      Lce = Lcsf + Lcs,
                      Lc = Lce + Lci,
                      Ls = Lch + Lchb + Lce + Lss + Lsp,
                      Lm = Lmf1 + Lmf2 + Lmf3 + Lmf4 + Lmp1 + Lmp2 + Lmp3 + Lmp4 + Lmb1,
                      Lv = Lva + Lvf + Lvg,
                      Lsm = Ls + Lm - Lmb1,
                      Lvm = Lv + Lmb1,
                      L = Ls + Lm + Lv,
                      Lt = L + Qps + QpT,
                      M = Mb + Mm + Mrg + Mrm,
                      D = Ap + Gr + Hb + Px + Rt + St + Zr,
                      Rm1 = Lmp1 + Lmf1 + Lmb1,
                      Rm2 = Lmp2 + Lmf2,
                      Rm3 = Lmp3 + Lmf3,
                      Rm4 = Lmp4 + Lmf4,
                      Rm5 = Rmp5 + Rmf5,
                      Rm1e = Rm1 + 0.5 * Rm2,
                      Rm3e = Rm3 + 0.5 * Rm2 + 0.5 * Rm4,
                      Rm5e = Rm5 + 0.5 * Rm4,
                      Rm = Rm1 + Rm2 + Rm3 + Rm4 + Rm5,
                      MI = Rm1/Rm * 100 + Rm2/Rm * 200 + Rm3/Rm * 300 + Rm4/Rm * 400 + Rm5/Rm * 500,
                      QpTLt = QpT/Lt*100,
                      NCE = Q + F + M + D + Ls + Lv + Lm,
                      CE = Lc,
                      NCI = Chl + Gl + Op + Ph + Si + Fph,
                      CI = Lci,
                      IE = log10((NCE + CE)/(NCI+CI)),
                      porecement = log10(C/Pore))

composition$unit <- as.character(composition$unit)
composition$unit[composition$unit=="wf"] <- "BS"
composition$unit[composition$unit=="AS"] <- "VS"
composition$MI[is.nan(composition$MI)] <- 0.001

# Colours ####
col.voirons <- "#549ccc"
col.vouan <- "#ff1926"

# Cluster analysis ####
list.cluster <- c("Qms","Qr","Qp","K","PA","Ls","Lci","Lm","Lv")
cluster.total <- composition$Qms + composition$Qr + composition$Qp + composition$K + composition$PA + composition$Ls + composition$Lci + composition$Lm + composition$Lv
cluster <- composition[list.cluster]/cluster.total*100

# cluster
d <- dist(cluster, method = "euclidean") 
fit <- hclust(d, method="ward.D")

# main branchs
provenance <- cutree(fit, k=2)
provenance <- as.data.frame(provenance) 
composition$provenance <- provenance$provenance

# convert provenance value to provenance name
composition$provenance[composition$provenance==1] <- "Quartzose"
composition$provenance[composition$provenance==2] <- "Feldspathic"