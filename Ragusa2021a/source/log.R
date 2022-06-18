library(dplyr)
library(ggplot2)
library(magrittr)

log <- read.csv("data/log.csv", header = TRUE)

# Extract locality and section ####
list <- aggregate(log$unit, by = list(site = log$site, index = log$index, unit = log$unit), FUN = mean)
list.index <- list$index
list.site <- list$site

# Compute sand:marl ratio ####
Rgm <- NULL

for (i in list.index) {
  Rgm<-rbind(Rgm, data.frame(site = NA,
                             section = NA,
                             unit = NA,
                             g = sum(log$thick[log$index == i & log$rock == "g"]),
                             m = sum(log$thick[log$index == i & log$rock == "m"]),
                             h = sum(log$thick[log$index == i & log$rock == "h"]),
                             total = sum(log$thick[log$index == i & log$rock == "m"]) + sum(log$thick[log$index == i & log$rock == "g"]) + sum(log$thick[log$index == i & log$rock == "h"])))
  }

Rgm$site <- list.site
Rgm$section <- list.index
Rgm$unit <- list$unit %>% factor(levels = c("Bons Mb.","Allinges Mb.","Voirons Sandstone","Vouan Conglomerate","Boëge Marl","Bruant Sandstone","Fenalet Sandstone"))

Rgm$m[Rgm$m == 0] <- 0.1 #rounded zero
Rgm$R <- log10(Rgm$g / Rgm$m)
Rgm$g.ratio <- Rgm$g / (Rgm$tot - Rgm$h)
Rgm$m.ratio <- Rgm$m / (Rgm$tot - Rgm$h)

# min - max values ####
Rgm$R[is.infinite(Rgm$R) | is.nan(Rgm$R) ] <- NA 
Rgm %>% 
  select(unit, R) %>% 
  na.omit() %>% 
  group_by(unit) %>% 
  summarize(min = min(R),max = max(R), mean = mean(R)) %>% 
  View()

# Log ratio sand:marl ####
ggplot(Rgm, aes(unit, R, fill = unit)) + 
  geom_hline(yintercept = 0, color = "grey", linetype ="dashed") +
  geom_violin(fill = "grey90") +
  geom_point(shape = 21, size = 3, position = position_jitter(0.2, seed=123), fill = "grey50") +
  annotate("text", y = c(-0.75, 4.75), x = c(0.75, 0.75), hjust = 0, size = 5, fontface = 4, label = c("marly", "sandy")) +
  annotate("text", y = c(-0.75, 1, 4.75), x = c(7.25, 7.25, 7.25), hjust = 1, size = 5, fontface = 3, label = c("outerfan", "lobe", "channel")) +
  scale_x_discrete(labels = c("Fenalet Sandstone" = "FS",
                              "Bons Mb." = "Bons",
                              "Allinges Mb." = "All",
                              "Voirons Sandstone" = "VS",
                              "Vouan Conglomerate" = "VC",
                              "Boëge Marl" = "BM",
                              "Bruant Sandstone" = "BrS")) +
  labs(x = NULL, y = "sand:marl log ratio") +
  coord_cartesian(clip = 'off') +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  ggsave("graphs/Fig7.pdf", width = 7, height = 7)

# OLD VERSION ####
low_values <- 
  ggplot(Rgm, aes(g.ratio, m.ratio, fill = unit)) +  
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis(name = "", option = "plasma", discrete = TRUE, direction = -1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0.95, 1, 0.05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 0.05, 0.05)) +
  coord_cartesian(ylim=c(0, 0.05), xlim = c(0.95, 1)) +
  labs(x = NULL,
       y = NULL) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))
  
ggplot(Rgm, aes(g.ratio, m.ratio, fill = unit)) +
  geom_segment(aes(x = 0.2, y = 0.75, xend = 0.3, yend = 0.75), color = "grey50", size = 0.25, linetype ="solid") +
  geom_segment(aes(x = 0.25, y = 0.7, xend = 0.25, yend = 0.8), color = "grey50", size = 0.25, linetype ="solid") +
  geom_segment(aes(x = 0.4, y = 0.55, xend = 0.5, yend = 0.55), color = "grey50", size = 0.25, linetype ="solid") +
  geom_segment(aes(x = 0.45, y = 0.5, xend = 0.45, yend = 0.6), color = "grey50", size = 0.25, linetype ="solid") +
  # geom_segment(aes(x = 0.2, y = 0.7, xend = 0.3, yend = 0.8), color = "grey", linetype ="dashed") +
  # geom_segment(aes(x = 0.45, y = 0.45, xend = 0.55, yend = 0.55), color = "grey", linetype ="dashed") +
  # geom_segment(aes(x = 0.8, y = 0.3, xend = 0.7, yend = 0.2), color = "grey", linetype ="dashed") +
  # annotate("text", y = c(0.79,0.59,0.34,0.09), x = c(0.12,0.33,0.59,0.84), angle = -43, hjust = 0.5, label = c("marl","sandy marl","marly sand","sand")) +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis(name = NULL, option = "plasma", discrete = TRUE, direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "sandstones/(total-hiatus)",
       y = "marls/(total-hiatus)") +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.background = element_rect(size = 0.3, linetype = "solid", colour = "black", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0.02, 0.98), 
        legend.position = c(0.56, 1.01),
        aspect.ratio = 1) +
  geom_rect(xmin = 0.95, xmax = 1, ymin = 0, ymax = 0.05, fill = NA, colour = "grey50", size = 0.3) +
  geom_curve(x = 0.5, y = 0.25, xend = 0.9, yend = 0.025, curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))) +
  annotation_custom(
    ggplotGrob(low_values),
    xmin = 0, xmax = 0.45, ymin = 0, ymax = 0.45)
ggsave("graphs/Fig4.pdf", width = 7, height = 7)