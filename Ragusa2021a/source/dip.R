library(dplyr)
library(magrittr)
library(RFOC)

tectonic <- read.csv("data/tectonic.csv", comment.char = "#")
tectonic_substratum <- read.csv("data/tectonic_substratum.csv", comment.char = "#")

svg("graphs/VS.svg", width = 5, height = 5)
net()
focpoint(tectonic$azimut[tectonic$formation == "Voirons Sandstone"], 
         tectonic$dip[tectonic$formation == "Voirons Sandstone"] + 90, 
         col = 'black', pch = 16, lab = "", UP = FALSE)
dev.off()

svg("graphs/AS.svg", width = 5, height = 5)
net()
focpoint(tectonic$azimut[tectonic$sector == "Allinges"], tectonic$dip[tectonic$sector == "Allinges"] + 90, col = '#46035e', pch = 16, lab = "", UP = FALSE)
dev.off()

svg("graphs/VC.svg", width = 5, height = 5)
net()
focpoint(tectonic$az[tectonic$formation == "Vouan Conglomerate" & tectonic$sector == "Voirons"], 
         tectonic$dip[tectonic$formation ==  "Vouan Conglomerate" | tectonic$sector == "Voirons"] + 90, col = '#3f6f8c', pch = 16, lab = "", UP = FALSE)
dev.off()

svg("graphs/BM.svg", width = 5, height = 5)
net()
focpoint(tectonic$az[tectonic$formation == "Boëge Marl" & tectonic$sector == "Voirons"], 
         tectonic$dip[tectonic$formation == "Boëge Marl" | tectonic$sector == "Voirons"] + 90, col = '#5dc368', pch = 16, lab = "", UP = FALSE)
dev.off()

pdf("graphs/BS.pdf", width = 5, height = 5)
net()
focpoint(tectonic$az[tectonic$formation == "Bruant Sandstone"], tectonic$dip[tectonic$formation == "Bruant Sandstone"] + 90, col = '#ede920', pch = 16, lab = "", UP = FALSE)
dev.off()

svg("graphs/Sub.svg", width = 5, height = 5)
net()
focpoint(tectonic_substratum$azimut[tectonic_substratum$formation == "Bellevue Sandstone"], tectonic_substratum$dip[tectonic_substratum$formation == "Bellevue Sandstone"] + 90,
         col = "red", pch = 16, lab = "", UP = FALSE)
focpoint(tectonic_substratum$azimut[tectonic_substratum$formation == "Montauban Sandstone"], tectonic_substratum$dip[tectonic_substratum$formation == "Montauban Sandstone"] + 90, 
         col = "orange", pch = 16, lab = "", UP = FALSE)
dev.off()

svg("graphs/PM_voirons.svg", width = 5, height = 5)
net()
focpoint(tectonic_substratum$azimut[tectonic_substratum$tectonic_unit == "Préalpes médianes nappe" & tectonic_substratum$section == "Voirons"], 
         tectonic_substratum$dip[tectonic_substratum$tectonic_unit == "Préalpes médianes nappe" & tectonic_substratum$section == "Voirons"] + 90, 
         col = "blue", pch = 16, lab = "", UP=FALSE)
dev.off()

svg("graphs/PM_allinges.svg", width = 5, height = 5)
net()
focpoint(tectonic_substratum$azimut[tectonic_substratum$tectonic_unit == "Préalpes médianes nappe" & tectonic_substratum$section == "Allinges"], 
         tectonic_substratum$dip[tectonic_substratum$tectonic_unit == "Préalpes médianes nappe" & tectonic_substratum$section == "Allinges"] + 90, 
         col = "blue", pch = 16, lab = "", UP = FALSE)
dev.off()
