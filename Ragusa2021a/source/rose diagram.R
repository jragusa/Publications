library(ggplot2)
library(magrittr)
library(dplyr)

current <- read.csv("data/palaeocurrent.csv", header = TRUE) 
winkler1984 <- read.csv("data/palaeocurrent_winkler1984.csv", header = TRUE) #palaeocurrent measurements from Winkler1984

#sort stratigraphic units along the vertical succession ####
current$unit <- factor(current$unit, levels = c("Voirons Sandstone", "Vouan Conglomerate", "Boëge Marl"))

# add 180° equivalent for groove cast ####
current %>% 
  filter(type == "groove cast") %>% 
  mutate(azimut = azimut + 180) -> current180

current <- rbind(current, current180)

# rose diagram with raw values ####
ggplot(current, aes(x = azimut, fill = unit)) + 
  stat_bin(breaks = (0:36 - 0.5)/36 * 360) +
  geom_bar(width = 0.8) + 
  coord_polar(theta = "x") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = theme_blank()) +
  facet_grid(. ~ unit)

# fix value with clockwise rotation ####
current <- mutate(current,
                  corrected = azimut + 20)

# fix azimut > 360 ####
current$corrected[current$corrected == 361] <- 1
current$corrected[current$corrected == 366] <- 6

# merge with winkler1984 dataset
current_tot <- rbind(current, winkler1984)

# rose diagram with corrected values ####
label_num <- as_labeller(c("Voirons Sandstone" = "Voirons Sandstone (n = 17)", 
                           "Vouan Conglomerate" = "Vouan Conglomerate (n = 4)", 
                           "Boëge Marl" = "Boëge Marl (n = 5)", 
                           "Winkler (1984)" = "Winkler (1984) (n = 15)")
                         )

ggplot(current_tot, aes(x = corrected)) + 
  stat_bin(breaks = (0:36 - 0.5)/36 * 360) +
  geom_bar(width = 0.8) + 
  coord_polar(theta = "x") +
  scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text.x = element_text(size = 12),
        panel.border = element_rect(colour = "white")) +
  facet_wrap(vars(unit), nrow = 2, labeller = label_num)
  ggsave("graphs/Fig7.pdf", width = 7, height = 7)
  