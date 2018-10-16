####
# author: Jérémy Ragusa
# date: 2018-05-22
# description: prepare dataset for Ragusa2018b
####

# library ####
library(cowplot)
library(dplyr)
library(factoextra)
library(ggplot2)
# library(ggtern)
library(magrittr)
library(matrixStats) # colCounts for GrainSize
library(compositions)
# library(reshape2)
library(tidyr)
library(viridis)

source("source/function.R")

# load datasets ####
composition <- read.csv("data/composition.csv", header = TRUE, comment.char = "#")
mutti <- read.csv("data/mutti.csv", header = TRUE, comment.char = "#")

# classification diagrams
ProvenanceTernary <- read.csv("/home/jeremy/UniGE/Git/Provenance/data/ProvenanceTernary.csv", header = TRUE, comment.char = "#")

# reshape dataframe ####
framework <- NULL

framework$raw <- composition %>% 
  transmute(sample,
            location = site,
            unit,
            QFm = Qms + Kor + Kan + Kpg + Kmi + Kp + Ps + Ptw + Pmy + As + Atw + Qps + QpT,
            QFr = Qrv + Qrm + Qrg + Krv + Krg + Krm + Prv + Prg + Prm + Arg + Arv + Arm,
            L = Lssc + Lcsf + Lcs + Lss + Lsp + Lch + Lchb + Lva + Lvf + Lvg + Lmf1 + Lmf2 + Lmf3 + Lmf4 + Lmp1 + Lmp2 + Lmp3 + Lmp4 + Lmb1,
            Lc = Lcmi + Lcmif + Lcmir + Lcmic + Lcmich,
            C,
            D = Ap + Gr + Hb + Px + Rt + St + Zr,
            M = Mb + Mm + Mrg + Mrm + Chl,
            Glt = Gl,
            Other = Op + Si,
            Ph = Ph + Fph,
            Fc,
            RA = AR,
            Bc = Bz + Sh + Das + Ech + Md + Oo + Ra + Se,
            Pore)

composition <- composition %>%
  mutate(Qm = Qms,
         F = Kor + Kan + Kpg + Kmi + Kp + Ps + Ptw + Pmy + As + Atw,
         Lt = Lssc + Lcsf + Lcs + Lss + Lsp + Lch + Lchb + Lva + Lvf + Lvg + Lmf1 + Lmf2 + Lmf3 + Lmf4 + Lmp1 + Lmp2 + Lmp3 + Lmp4 + Lmb1 + Qps + QpT,
         Ltc = Lt + Lcmi + Lcmif + Lcmir + Lcmic + Lcmich)

# replace NA values by 0.001 ####
framework$raw[is.na(framework$raw)] <- 0.001
framework$raw[framework$raw==0] <- 0.001

# shift member to formation ####
framework$raw$unit[framework$raw$unit == "AS"] <- "VS" # Allinges Sandstone Mb
framework$raw$unit[framework$raw$unit == "FS"] <- "VS" # Fenalet Sandstone
framework$raw$unit <- factor(framework$raw$unit, levels = c("VS", "VC", "BM", "BS", "Gu")) # stratigraphic order

# clr transformation ####
framework$clr <- cbind(framework$raw[, c(1:3)], clr(framework$raw[, c(4:17)]))

# percentage ####
list.cluster <- c("QFm", "QFr", "L", "Lc", "C", "D", "M", "Glt", "Other", "Ph", "Fc", "RA", "Bc", "Pore")
framework$percent <- cbind(framework$raw[, c(1:3)],framework$raw[list.cluster]/rowSums(framework$raw[, c(4:17)]) * 100)

# cluster analyse ####
row.names(framework$clr) <- framework$clr$sample
fit <- hclust(dist(framework$clr[, -c(1:3)], method = "euclidean"), method = "ward.D")

cluster <- cutree(fit, k = 5)
cluster <- as.data.frame(cluster) 
framework$raw$cluster <- cluster$cluster

# manual correction of the cluster distribution
framework$raw$lithofacies[framework$raw$cluster == 1] <- "L3"
framework$raw$lithofacies[framework$raw$cluster == 2] <- "L2"
framework$raw$lithofacies[framework$raw$cluster == 3] <- "L5"
framework$raw$lithofacies[framework$raw$cluster == 4] <- "L4"
framework$raw$lithofacies[framework$raw$cluster == 5] <- "L1"
framework$raw$lithofacies[framework$raw$sample == "JR238"] <- "L6"

framework$raw <- framework$raw[,-18]
framework$clr$lithofacies <- framework$raw$lithofacies
framework$percent$lithofacies <- framework$raw$lithofacies

rm(cluster)

# add grain-size and Mutti dataset ####
source("source/GrainSize.R")
framework$raw <- merge(framework$raw, diameter$moment[,-6], by = "sample", all.x = TRUE)
framework$percent <- merge(framework$percent, diameter$moment[, -6], by = "sample", all.x = TRUE)
framework$clr <- merge(framework$clr, diameter$moment[,-6], by = "sample", all.x = TRUE)

# add lithofacies and unit to Mutti facies ####
mutti <- mutti[,-5] %>% 
  na.omit() %>% 
  merge(framework$raw[,c("sample","unit","lithofacies")], by = "sample")

# add counting grains ####
framework$raw %>% 
  mutate(dickinson = QFm + QFr + L + Lc + D + M, 
         total = dickinson + C + Glt + Other + Ph + Fc + RA + Bc + Pore,
         diff = total - dickinson) -> framework$raw

# export electronic supplementary material ####
write.csv(framework$raw, "reports/ESM_1.csv", row.names = FALSE)