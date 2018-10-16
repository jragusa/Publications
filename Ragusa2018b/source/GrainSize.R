####
# author: Jérémy Ragusa
# date: 31/12/2015
# description: grain-size analysis of Ragusa207b
####
# bibliography:
# Johnsson1994. doi:
####

# load data ####
# a and b axis are put in two separate csv files (grain-sze_*) and samples organised in columns
list <- c("a","b")
grainsize <- NULL
phi <- NULL

for (i in list){
  grainsize[[i]] <- read.csv(paste0("data/grain-size_",i,".csv"), header = TRUE, comment.char = "#")
  grainsize[[i]] <-as.matrix(sapply(grainsize[[i]], as.numeric))
  grainsize[[i]][is.na(grainsize[[i]])] <- 0
  phi[[i]] <- -log2(grainsize[[i]]) # conversion mm in phi
}

# 3D correction for elliptic grains following Johnson (1994) ####
grainsize$d <- (phi$a+phi$b)/2
grainsize$D <- grainsize$d - 0.4*(phi$a-grainsize$d)^2
grainsize$D[is.na(grainsize$D)] <- 0

# weighted relative percentage ####
# this percentage does not consider the relative counting but the total length for each grain-size range
# see Johnson (1994) for explanations

# specify your grain-size range here
interval <- 0.25
range <- seq(-1.75, 6, by = interval) 

# total length per grain-size range
length <- NULL
# count of grains per grain-size range
count <- NULL
df <- NULL
df2 <- NULL

for (i in range){
df <- ifelse(grainsize$D <= i ,0, grainsize$D)
df <- ifelse(df > (i+0.25) ,0, df)
df2 <- colCounts(subset(df!=0))
df <- colSums(as.data.frame(abs(df)))
length <- as.data.frame(rbind(length,df))
count <- as.data.frame(rbind(count,df2))
}

# polish dataframes
rownames(count) <- range
colnames(count) <- colnames(length)
rownames(length) <- range
length <- as.data.frame(t(length))

middle <- range + interval/2 # midpoint of the grain-size class
measurement <- colSums(!is.na(grainsize[["a"]])) # number of measurement per sample

# frequencies dataset ####
diameter <- NULL
diameter$freq <- cbind(length,
                       total = rowSums(length, na.rm=TRUE))
diameter$freq <- diameter$freq/diameter$freq$total*100
diameter$freq <- select(diameter$freq,-total)
freq.t <- as.data.frame(t(diameter$freq))

# add sample, lithofacies and stratigraphic units to freq dataset
diameter$freq$sample = rownames(length)
diameter$freq %>%
  merge(framework$raw[,c("sample","lithofacies")], by="sample", all.x = TRUE) %>%
  na.omit() -> diameter$freq

# mean grain-size distribution per lithofacies
diameter$mean <- diameter$freq %>% 
  select(-c(sample)) %>%
  group_by(lithofacies) %>%
  summarise_all(funs(mean))

# logarithmic method of moments ####
diameter$moment <- data.frame(mean = colSums(freq.t*middle)/100)

mean.matrix <- do.call(rbind, replicate(32, diameter$moment$mean, simplify=FALSE))

diameter$moment <- cbind(diameter$moment,
                    sorting = sqrt((colSums(freq.t*(middle-mean.matrix)^2)/100)))

sorting.matrix <- do.call(rbind, replicate(32, diameter$moment$sorting, simplify=FALSE))
                    
diameter$moment <- cbind(sample = rownames(diameter$moment),
                         diameter$moment,
                         skewness = colSums(freq.t*((middle-mean.matrix)^3))/(100*diameter$moment$sorting^3),
                         kurtosis = colSums(freq.t*((middle-mean.matrix)^4))/(100*diameter$moment$sorting^4))
diameter$moment <- merge(diameter$moment,framework$raw[,c("sample","lithofacies")], by="sample", all.x = TRUE) %>% na.omit()

# mean moment measures per lithofacies 
diameter$mean <- diameter$moment %>%
  select(-c(sample)) %>%
  group_by(lithofacies) %>%
  summarise_all(funs(mean)) %>%
  merge(diameter$mean, by ="lithofacies")

# Add SD ####
SD <- aggregate(diameter$moment[,c(2:5)], list(diameter$moment$lithofacies), sd)
colnames(SD) <- c("lithofacies","SD_mean","SD_sorting","SD_skewness","SD_kurtosis")

diameter$mean <- merge(diameter$mean, SD, by="lithofacies")

# remove useless variables ####
rm(df,df2,middle,mean.matrix,sorting.matrix,freq.t,length,i,interval,list,measurement,range,count,phi,SD)
