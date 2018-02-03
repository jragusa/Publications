# 1 - files ####
flysch_composition <- read.csv("/home/jeremy/UniGE/Git/Prealpine flyschs/flysch_modalcomposition.csv", header = TRUE,  comment.char = "#", dec = ",")
flysch_ternary <- read.csv("/home/jeremy/UniGE/Git/Prealpine flyschs/flysch_modalternary.csv", header = TRUE,  comment.char = "#", dec = ",")
prealps <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/HM_prealps.csv", header = TRUE, comment.char = "#", dec = ",")
upn <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Caron1989_UPN.csv", header = TRUE, comment.char = "#", dec = ",")

# 2 - Modal composition ####
flysch_composition <- mutate(flysch_composition,
                             Q = Qm + Qp+ QpT,
                             F = P + K,
                             Lm = Lmp3 + Lmp4,
                             L = Ls + Lv + Lm,
                             Lt = L + Qp + QpT,
                             Lsm = Ls + Lm,
                             Lvm = Lv)

# 3 - Heavy minerals ####
prealps[is.na(prealps)] <- 0
prealps <- mutate(prealps,
                  Ttn.tot = Ant + Brk + Ttn,
                  tHM.trace = others + Ep + Sp,
                  tHM = Ap + Mnz + Grt + Rt + Spl + St + Ttn.tot + Tur + Zrn + tHM.trace)

# 4 - Colours ####
col.niesen <- "#6DA02D"     # vert
col.medianes <- "#b399bb"   # violet
col.sarine <- "#CC8F46"     # marron clair
col.dranses <- "#8C5A2C"    # marron foncé
col.schlieren <- "#9C803D"  # vert clair
col.wagital <- "#362D15"    # vert foncé
col.winkler1984 <- "#DBB467"      # vert