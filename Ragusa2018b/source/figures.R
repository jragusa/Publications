source("source/data.R")

biplot <- ggplot() +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Fig.3: Dickinson ternary ####
dickinson <- merge(composition[,c(1, 89:93)], diameter$moment[,c(1, 2, 6)], all.y = TRUE)

ggtern(dickinson, aes(F, Qm, Lt, fill = provenance)) + geom_point_swap()
  geom_line(data = subset(ProvenanceTernary, type == "QmFLt"), aes(x, y, z, group = line)) +
  geom_point(shape = 21, size = 3) +
  geom_confidence_tern(data = dickinson, aes(F, Qm, Lt, colour = provenance), breaks = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c("grey40", "grey40")) +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(T = "Qm",
       R = "Lt",
       L = "F",
       fill = expression(paste("mean (",phi,")"))) +
  theme_bw(base_size = 16) +
  theme_nogrid()

# Fig.4: correlation of lithofacies with grain-size ####
# a: kurtosis vs. mean
biplot +
  geom_vline(xintercept = c(1, 2, 3, 4), colour = "grey", linetype = "dashed") +
  geom_hline(yintercept = c(1.7, 2.55, 3.7, 7.4)) +
  geom_point(data = diameter$moment, aes(mean, kurtosis, fill = lithofacies), shape = 21, size = 3) +
  # geom_errorbar(data = diameter$mean, aes(ymin = kurtosis - SD_kurtosis, ymax = kurtosis + SD_kurtosis), width = .1, colour = "black") +
  # geom_errorbarh(data = diameter$mean, aes(xmin = mean - SD_mean, xmax = mean + SD_mean), height = .2, colour = "black") +
  # geom_point(data = diameter$mean, aes(mean, kurtosis, fill = lithofacies), colour = "black", size = 5, shape = 23) +
  annotate("text", y = 0.3, x = 0.5:4.5, label = c("coarse", "medium", "fine", "very fine", "siltstone")) +
  annotate("text", y = c(1.5, 2, 2.85, 4, 7.7), x = 4.75, hjust = 1, fontface = 'italic', label = c("very platykurtic", "platykurtic", "mesokurtic", "leptokurtic", "very leptokurtic")) +
  annotate("text", y = 9.75, x = 0.25, size = 7, label = "A") +
  labs(x = expression(paste("mean (",phi,")"))) +
  theme(legend.background = element_rect(size = 0.3, linetype = "solid", colour = "black", fill = "white"),
        legend.justification = c(0.02, 0.98), 
        legend.position = c(0.02, 0.7)) +
  ggsave("graphs/Fig4a.svg", width = 7, height = 7)

# b: mean vs. sorting
biplot +
  geom_vline(xintercept = c(1, 2, 3, 4), colour = "grey", linetype = "dashed") +
  geom_hline(yintercept = c(0.35, 0.5, 0.7, 1)) +
  geom_point(data = diameter$moment, aes(mean, sorting, fill = lithofacies), shape = 21, size = 3) + 
  # geom_errorbar(data = diameter$mean, aes(ymin = sorting - SD_sorting, ymax = sorting + SD_sorting), width = .1, colour = "black") +
  # geom_errorbarh(data = diameter$mean, aes(xmin = mean - SD_mean, xmax = mean + SD_mean), height = .03, colour = "black") +
  # geom_point(data = diameter$mean, aes(mean, sorting, fill = lithofacies), colour = "black", size = 5, shape = 23) +
  annotate("text", y = 0.3, x = 0.5:4.5, label = c("coarse", "medium", "fine", "very fine", "siltstone")) +
  annotate("text", y = c(0.37, 0.52, 0.72, 1.02), x = 4.75, hjust = 1, fontface = 'italic', label = c("well sorted", "moderatly well sorted", "moderatly sorted", "poorly sorted")) +
  annotate("text", y =1.35, x = 0.25, size = 7, label = "B") +
  labs(x = expression(paste("mean (",phi,")"))) +
  theme(legend.position = "none") +
  ggsave("graphs/Fig4b.svg", width = 7, height = 7)

# c: grain-size distribution vs. stratigraphic units
biplot +
  geom_hline(yintercept = c(1, 2, 3, 4), colour = "grey", linetype = "dashed") +
  geom_boxplot(data = framework$raw, aes(unit, mean, group = unit), fill = "grey") + 
  annotate("text", x = 0.5, y = 0.5:4.5, label = c("coarse", "medium", "fine", "very fine", "siltstone")) +
  annotate("text", x = 5.25, y = 0.25, size = 7, label = "C") +
  labs(x = "formation",
       y = expression(paste("mean (",phi,")"))) +
  coord_flip() +
  ggsave("graphs/Fig4c.svg", width = 7, height = 6.2)

# d: cumulative frequency
framework$raw %>%
  na.omit() %>%
  select(mean, QFm, QFr, L, Lc, C, Pore, Glt, Ph, Fc, RA, Bc, D, M) %>%
  mutate(interval = cut(mean, breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(interval) %>%
  summarise(QFm = sum(QFm),
            QFr = sum(QFr),
            L = sum(L),
            Lc = sum(Lc),
            C = sum(C),
            Glt = sum(Glt),
            Ph = sum(Ph),
            Fc = sum(Fc),
            RA = sum(RA),
            Bc = sum(Bc),
            D = sum(D),
            M = sum(M),
            Pore = sum(Pore)) -> framework$cumul

framework$cumul <- mutate(framework$cumul,
                          group1 = Pore + L + QFr,
                          group2 = QFm + RA + C + D + M,
                          group3 = Ph + Fc + Glt)
framework$cumul$middle <- seq(0.25, 4.75, 0.5)

# create mini ternary diagram
Fig5d_mini <- ggplotGrob(
  ggtern(framework$cumul, aes(group1, group2, group3)) +
  geom_point() +
    geom_line() +
    geom_text(aes(label = paste("phi ==",middle)), parse = T, hjust = -.25, vjust = 0, size = 1) + 
    geom_mask() +
    labs(L= "Group 1", R = "Group 3", T = "Group 2") +
    theme_bw(base_size = 10) +
    theme_nogrid() +
    theme(plot.background = element_blank())
  )

# compute to 100%
framework$cumul$QFm <- cumsum(framework$cumul$QFm) / sum(framework$cumul$QFm) * 100
framework$cumul$QFr <- cumsum(framework$cumul$QFr) / sum(framework$cumul$QFr) * 100
framework$cumul$L <- cumsum(framework$cumul$L) / sum(framework$cumul$L) * 100
framework$cumul$Lc <- cumsum(framework$cumul$Lc) / sum(framework$cumul$Lc) * 100
framework$cumul$C <- cumsum(framework$cumul$C) / sum(framework$cumul$C) * 100
framework$cumul$Pore <- cumsum(framework$cumul$Pore) / sum(framework$cumul$Pore) * 100
framework$cumul$Glt <- cumsum(framework$cumul$Glt) / sum(framework$cumul$Glt) * 100
framework$cumul$Ph <- cumsum(framework$cumul$Ph) / sum(framework$cumul$Ph) * 100
framework$cumul$Fc <- cumsum(framework$cumul$Fc) / sum(framework$cumul$Fc) * 100
framework$cumul$RA <- cumsum(framework$cumul$RA) / sum(framework$cumul$RA) * 100
framework$cumul$Bc <- cumsum(framework$cumul$Bc) / sum(framework$cumul$Bc) * 100
framework$cumul$D <- cumsum(framework$cumul$D) / sum(framework$cumul$D) * 100
framework$cumul$M <- cumsum(framework$cumul$M) / sum(framework$cumul$M) * 100

# add Zuffa class to grains
framework$cumul_melt <- framework$cumul[, -c(1, 15:17)] %>% gather(key = grain, value, -middle)
framework$cumul_melt$type[framework$cumul_melt$grain == "QFm" | framework$cumul_melt$grain == "QFr"| framework$cumul_melt$grain == "L" | framework$cumul_melt$grain == "D" | framework$cumul_melt$grain == "M"] <- "extrabasinal"
framework$cumul_melt$type[framework$cumul_melt$grain == "C" | framework$cumul_melt$grain == "Pore"] <- "intergranular"
framework$cumul_melt$type[framework$cumul_melt$grain == "Lc" | framework$cumul_melt$grain == "Glt" | framework$cumul_melt$grain == "Ph" | framework$cumul_melt$grain == "Fc" | framework$cumul_melt$grain == "RA" | framework$cumul_melt$grain == "Bc"] <- "intrabasinal"

ggplot(framework$cumul_melt, aes(middle, value, group = interaction(grain, type))) +
  geom_vline(xintercept = c(1, 2 ,3, 4), colour = "grey", linetype = "dashed") +
  annotate("text", y = -3, x = 0.5:4.5, label = c("coarse", "medium", "fine", "very fine", "siltstone")) +
  annotate("text", y = 97.5, x = 0.25, size = 7, label = "D") +
  geom_line(aes(linetype = type, colour = grain)) +
  scale_colour_manual(name = "framework\ngrains",
                      values = c("#DBCEC6", "#837266", "#4c3a2c", "#BFB38E", "#f2c54bff", "#99bb44", "#FF7800", "#569dcc", "#ff1a28", "black", "purple", "pink", "#4A4C58")) +
  labs(x = expression(paste("mean (",phi,")")),
       y = "cumulative percentage (%)") +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # annotation_custom(grob = Fig5d_mini, xmin = 2.15, xmax = 5, ymin = -1, ymax = 60) +
  ggsave("graphs/Fig4d.svg", width = 8.8, height = 6.5)

# plot_grid(a, b, c, a, ncol = 2)
# save_plot("graphs/Fig5test.pdf", Fig5, ncol = 2, nrow = 2, base_aspect_ratio = 1)

# Fig.5: Main figure of lithofacies correlation ####
# a: Mutti facies distribution according to lithofacies
mutti.freq <- t(table(mutti$lithofacies, mutti$mutti))
mutti.freq <- mutti.freq / rbind(colSums(mutti.freq), colSums(mutti.freq), colSums(mutti.freq), colSums(mutti.freq), colSums(mutti.freq), colSums(mutti.freq)) * 100
mutti.melt <- melt(mutti.freq, id.vars = "", measure.vars = unique(framework$raw$lithofacies))
mutti.melt$Var2 <- factor(mutti.melt$Var2, levels = c("L1", "L2", "L3", "L4", "L5", "L6"))

mutti.melt %>%
  ggplot(aes(Var1, value, fill = Var1)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_grey(guide = FALSE) +
  # scale_fill_viridis(option = "C", discrete = TRUE, direction = -1, guide = FALSE) +
  labs(title = "A: Mutti facies") +
  facet_wrap(~ Var2, ncol = 1) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ggsave("graphs/Fig5a.svg",width = 7, height = 14) +
  rm(mutti.freq,mutti.melt)

# b: distribution of the framework composition
df <- transmute(framework$percent,
                lithofacies,
                QFm,
                QFr,
                L,
                Lc,
                C,
                Glt,
                Ph,
                Other = Other + M + D + Bc,
                Fc,
                RA,
                Pore)

df %>%
  group_by(lithofacies) %>%
  summarise_all(funs(mean)) %>%
  gather(key = grain, value = percent, -lithofacies) -> df
df$grain <- factor(df$grain, levels = c("Glt", "Ph", "QFm", "QFr", "L", "Lc", "C", "Fc", "RA", "Other", "Pore"))
df$lithofacies <- factor(df$lithofacies, levels = rev(c("L1", "L2", "L3", "L4", "L5", "L6")))

col.grains <- c("QFm" = "#DBCEC6", "QFr" = "#837266", "L" = "#4c3a2c", "Lc" = "#BFB38E", "C" = "#f2c54bff", "Glt" = "#99bb44", 
                "Ph" = "#FF7800", "Fc" = "#569dcc", "RA" = "#ff1a28", "Other" = "white", "Pore" = "#4A4C58")
  
ggplot(df, aes(lithofacies, percent, fill = factor(grain))) +
  geom_col(colour = "black", position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = col.grains, name = NULL) +
  labs(title = "B: framework composition") + 
  coord_flip() +
  theme_bw(base_size = 16) +
  # facet_wrap(~ lithofacies, ncol = 1, scales = "free") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom") +
  ggsave("graphs/Fig5b.svg",width = 7, height = 14) +
  rm(df)
  
# c: grain-size distribution according to lithofacies
diameter$freq %>%
  gather(key = grainsize, value = value, -c(sample, lithofacies)) %>%
  ggplot(aes(grainsize, value, group = sample)) +
  geom_line(colour = "grey", size = 0.25) +
  geom_line(data = diameter$mean[, -c(2:5,38:41)] %>% gather(key = grainsize, value = value, -lithofacies),
            aes(grainsize, value, colour = lithofacies, group = lithofacies), size = 1) +
  scale_colour_viridis(option = "D", discrete = TRUE, direction = -1, guide = FALSE) +
  scale_x_discrete(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6)) +
  labs(title = "C: grain-size distribution",
       x = expression(paste("grain-size (",phi,")")),
       y = "") +
  geom_vline(xintercept = c(4, 24), colour = "grey") +
  annotate("text", x = 2.25, y = 24, label = "gravel", size = 2) +
  annotate("text", x = 14, y = 24, label = "sand", size = 2) +
  annotate("text", x = 28.5, y = 24, label = "silt", size = 2) +
  theme_bw(base_size = 16) +
  facet_wrap(~ lithofacies, ncol = 1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggsave("graphs/Fig5c.svg", width = 7, height = 14)

# Fig.6: Feldspars vs mean grain-size ####
composition %>% 
  select(sample, Qps:Arv) %>% 
  mutate(Q = Qps + QpT + Qms + Qrv + Qrm + Qrg,
         F = Kor + Kan + Kpg + Kmi + Kp + Krg + Krm + Krv + Ps + Pmy + Ptw + Prg + Prm + Prv + As  + Atw + Arg + Arm + Arv) -> feldspar

feldspar <- merge(select(feldspar, sample, F, Q), select(framework$raw, sample, lithofacies, mean, dickinson), by = "sample")
feldspar %>% 
  mutate(Q = Q / dickinson * 100,
         F = F / dickinson * 100) -> feldspar

biplot + 
  geom_point(data = feldspar, aes(mean, Q), colour = "grey75", size = 3, alpha = 0.5) +
  geom_smooth(data = feldspar, aes(mean, Q), method = lm, se = FALSE, colour = "grey75") +
  geom_point(data = feldspar, aes(mean, F, fill = lithofacies), shape = 21, size = 3) + 
  geom_smooth(data = feldspar, aes(mean, F), method = lm, se = FALSE, colour = "grey25") +
  annotate("text", x = 1.5, y = 95, size = 5, label = lm_eqn(lm(Q ~ mean, feldspar)), parse = TRUE, colour = "grey50") +
  annotate("text", x = 1.5, y = 3, size = 5, label = lm_eqn(lm(F ~ mean, feldspar)), parse = TRUE) +
  labs(x = expression(paste("Mean grain-size (",phi,")")),
       y = "Total feldspar (%)") +
  # guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "none") +
  ggsave("graphs/Fig6.svg", width = 7, height = 7) +
  rm(feldspar)

# Fig.7: Intergranular volume ####
# a: IGV vs grain-size
df <- transmute(framework$percent,
                IGV = (Pore + C),
                Pore,
                mean,
                lithofacies)

a <- biplot + 
  geom_hline(yintercept = 45, linetype = "dashed", colour = "grey50") +
  geom_point(data = df, aes(mean, IGV, fill = lithofacies), shape = 21, size = 3) +
  geom_smooth(data = df, aes(mean, IGV), method = lm, se = FALSE, colour = "black") +
  labs(x = expression(paste("Mean grain-size (",phi,")")),
       y = "IGV (%)") +
  annotate("text", x = 0.25, y = 58, size = 7, label = "A") +
  annotate("text", x = 2.5, y = 15, hjust = 0, size = 5, label = lm_eqn(lm(IGV ~ mean, df)), parse = TRUE) +
  theme(legend.position="none")

#b: IGV - porosity vs. lithofacies
df %>%
  select(-mean) %>% 
  group_by(lithofacies) %>%
  summarise_all(funs(mean)) %>% 
  gather(key = key, value = percent, -lithofacies) -> df1

b <- ggplot(df1, aes(percent, lithofacies)) + 
  geom_line() + 
  geom_point(aes(fill = lithofacies, shape = key), size = 5) +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1, guide = FALSE) +
  scale_shape_manual(values = c(23, 21),
                     labels = c("IGV", "Porosity")) +
  labs(x = "Mean intergranular volume and porosity (%)", 
       shape = NULL) +
  annotate("text", x = 0.25, y = 6.25, size = 7, label = "B") +
  theme_bw(base_size = 16) +
  theme(legend.background = element_rect(size = 0.3, linetype = "solid", colour = "black", fill = "white"),
        legend.justification = c(0, 1),
        legend.position = c(0.7, 0.975),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank())

plot_grid(a, b, ncol = 2) +
  ggsave("graphs/Fig7.svg", width = 14, height = 7) +
  rm(df, df1, a, b)
  
# Fig.9: Cluster diagram ####
svg("graphs/Fig7.svg",width=7,height=7)
par(cex=0.25,font=3)
plot(fit,lwd=1)
rect.hclust(fit, k=5, border="red") 
dev.off()

# Fig.12: PCA ####
df <- na.omit(framework$clr[,-c(1:3,20:22)])
# df <- na.omit(framework$raw[,-c(1:3,18,21:23)])
pca <- prcomp(df[,-15], scale = TRUE)

fviz_pca_biplot(pca,
                axes = c(1,2),
                label = "var", 
                habillage = df$lithofacies,
                # addEllipses=TRUE,
                # ellipse.level=0.95,
                col.var = "black",
                title = NULL) +
  scale_shape_manual(values = c(16,16,16,16,16,16)) +
  scale_colour_viridis(option = "D", discrete = TRUE, direction = -1) +
  annotate("text", x = 4, y = 3.5, size = 4, label = "NCE poly") +
  annotate("text", x = 4, y = -5, size = 4, label = "NCE mono") +
  annotate("text", x = -4, y = 3.5, size = 4, label = "CE+CI") +
  annotate("text", x = -4, y = -5, size = 4, label = "NCI") +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # coord_fixed(ratio = 1) +
  ggsave("graphs/Fig12.svg",width = 7, height = 7) 
  rm(df)

# Fig.13 : Biplots ####
# a: Extra vs. Intra
df <- mutate(framework$percent,
             Extra = QFm + QFr + L + D + M,
             Intra = Bc + Fc + RA + Glt + Ph + Other + Lc)

a <- biplot +
  geom_point(data = df, aes(C, log10(Extra/Intra), fill = lithofacies), shape = 21, size = 3) +
  geom_smooth(data = df, aes(C, log10(Extra/Intra)), method = "loess", se = FALSE, color = "black", size = 0.5) +
  annotate("text", x = 3, y = 4.7, size = 7, label = "A") +
  labs(x = "C (%)",
       y = "log (NCE+CE / NCI+CI)") +
  annotate("text", x = 30, y = 2.5, hjust = 0, size = 5, label = lm_eqn(lm(log10(Extra/Intra) ~ C, df)), parse = TRUE) +
  theme(legend.position = "none")

# b: Single grains vs. embbed grains
b <- biplot +
  geom_point(data = framework$raw, aes(mean, log10(QFm / QFr), fill = lithofacies), shape = 21, size = 3) +
  geom_smooth(data = framework$raw, aes(mean, log10(QFm / QFr)), method = "lm", se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  annotate("text", x = 0, y = 5.5, size = 7, label = "B") +
  labs(x = expression(paste("Mean grain-size (",phi,")")),
       y = "log QFm/QFr") +
  annotate("text", x = 2.5, y = -0.5, hjust = 0, size = 5, label = lm_eqn(lm(log10(QFm / QFr) ~ mean, framework$raw)), parse = TRUE) +
  # xlim(0, 4.25) +
  # ylim(0, 2.15)
  theme(legend.position = "none")
    
plot_grid(a, b, ncol = 2) +
  ggsave("graphs/Fig13.svg", width = 14, height = 7) 
  rm(df, a, b)

# Fig.14: Ratios ####
ratio <- data.frame(lithofacies = framework$percent$lithofacies,
                    GQ = log10(framework$percent$Glt/framework$percent$QFm),
                    QFL = log10(framework$percent$QFm/(framework$percent$L+framework$percent$QFr)),
                    LcL = log10(framework$percent$Lc/framework$percent$L))
ratio$QFL[is.nan(ratio$QFL)] <- 0.001
ratio$QFL[!is.finite(ratio$QFL)] <- 0.001
ratio$LcL[is.nan(ratio$LcL)] <- 0.001

ratio %>%
  gather(key = ratio, value = value, -lithofacies) %>%
  ggplot(aes(lithofacies, value, group = ratio)) +
  geom_boxplot(aes(group = lithofacies, fill = lithofacies)) +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1, guide = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  facet_wrap(~ratio, ncol = 3, scales = "free_y", labeller = as_labeller(c("GQ" = "log(Glt/QFm)", "QFL" = "log(QFm/(L+QFr))", "LcL" = "log(Lc/L)"))) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) +
  ggsave("graphs/Fig14.pdf",width = 14, height = 5) +
  rm(ratio)

# Fig.16: Porosity loss ####
framework$percent <- mutate(framework$percent,
                            COPL = 45 - (((100 - 45) * (Pore + C) / (100 - (Pore + C)))),
                            CEPL = (45 - COPL) * (C / (C + Pore)),
                            ICOMPACT = COPL / (COPL + CEPL))

negative <- biplot + 
  geom_point(data = framework$percent, aes(CEPL, COPL, fill = lithofacies), shape = 21, size = 3) + 
  labs(x = NULL,
       y = NULL) +
  xlim(40, 80) +
  ylim(-40, 00) +
  theme(legend.position = "none")

biplot + 
  geom_segment(aes(x = 0, y = 0, xend = 50, yend = 50)) +
  geom_point(data = framework$percent, aes(CEPL, COPL, fill = lithofacies), shape = 21, size = 3) + 
  labs(x = "Cementational porosity loss (%)",
       y = "Compactional porosity loss (%)") +
  xlim(0, 50) +
  ylim(0, 50) +
  annotation_custom(
    ggplotGrob(negative), 
    xmin = 25, xmax = 50, ymin = 25, ymax = 50) +
  theme(legend.position = "none") +
  ggsave("graphs/Fig16.svg", width = 7, height = 7) +
  rm(negative)
  
framework$percent %>% 
  filter(COPL < 0 & lithofacies == "L5") %>% 
  nrow()

# Fig.17: Lithofacies vs formation ####
list.lithofacies <- c("L1","L2","L3","L4","L5","L6")
strati <- as.data.frame.matrix(table(framework$raw$unit,framework$raw$lithofacies))
strati <- strati[list.lithofacies]/rowSums(strati)*100
strati$unit <- rownames(strati)
strati$unit <- factor(strati$unit, levels = c("VS","VC","BM","BS","Gu"))

strati %>%
  gather(key = lithofacies, value = percent, -unit) %>% 
  ggplot(aes(unit, percent, fill = lithofacies)) +
  geom_col(colour = "black", position = position_fill(reverse = TRUE), show.legend = FALSE) +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  labs(x = "Formation",
       y = "Lithofacies (%)") +
  theme_bw(base_size = 16) +
  coord_flip() +
  ggsave("graphs/Fig17b.svg",width = 7, height = 7)
  rm(strati)

# Fig.18: QmFLt diagram with mean grain-size and lithofacies ####
ggtern() + 
  geom_line(data = subset(ProvenanceTernary, type == "QmFLt"), aes(x,y,z, group = line)) +
  geom_point(data = dickinson, aes(F, Qm, Lt, fill = mean), shape = 21, size = 3) +
  geom_confidence_tern(data = dickinson, aes(F, Qm, Lt, colour = provenance), breaks = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c("grey40", "grey40")) +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(T = "Qm",
       R = "Lt",
       L = "F",
       fill = expression(paste("mean (",phi,")"))) +
  theme_bw(base_size = 16) +
  theme_nogrid()
ggsave("graphs/Fig18a.svg",width = 7, height = 7)

ggtern() + 
  geom_line(data = subset(ProvenanceTernary, type == "QmFLt"), aes(x,y,z, group = line)) +
  geom_point(data = dickinson, aes(F, Qm, Lt, fill = lithofacies), shape = 21, size = 3) +
  geom_confidence_tern(data = dickinson, aes(F, Qm, Lt, colour = provenance), breaks = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c("grey40", "grey40")) +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
  labs(T = "Qm",
       R = "Lt",
       L = "F") +
  theme_bw(base_size = 16) +
  theme_nogrid() +
  ggsave("graphs/Fig18b.svg",width = 7, height = 7) +
  rm(dickinson)
