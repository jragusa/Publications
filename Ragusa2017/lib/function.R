# load dataset for polygons
lines_qmflt <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Dickinson_QmFLt_line.csv", header = TRUE, comment.char = "#")
lines_qfl <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Dickinson_QFL_line.csv", header = TRUE, comment.char = "#")
lines_qap <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Dickinson_QAP_line.csv", header = TRUE, comment.char = "#")
lines_garnet <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Single_Garnet_line.csv", header = TRUE, comment.char = "#")
lines_folk <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2015b/data/Sandstone_Folk_line.csv", header = TRUE, comment.char = "#")
lines_tur <- read.csv("/home/jeremy/UniGE/Publication/Ragusa2016a/data/single_tourmaline.csv", header = TRUE, comment.char = "#")

# Dickinson ternary diagrams
ternDickinson <- function(data,v,t,k){
  ggtern() + 
    geom_line(data=data, 
              aes(y=z, x=x, z=y, color = line, group = line),
              size = 0.5, 
              color = 'grey') +
    xlab(t) + ylab(v) + zlab(k)
}

# tHM barplot (coord_flip issue in ggplot)
tHM.barplot <- function(data,col,list){
  p <- ggplot(data, aes(sample, value, fill=variable)) +
    geom_bar(stat="identity", colour = "black") +
    scale_fill_manual(values = col, labels = list) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(legend.position = "top", 
          legend.key = element_rect(fill="white", colour = "black"),
          legend.key.size = unit(0.5, "cm")) +
    guides(colour = guide_legend(override.aes = list(size=4)),
           fill = guide_legend(label.hjust = 5, override.aes = list(colour = NULL), title = NULL)) +
    # annotate("Voirons", y = 105, angle = 90) +
    coord_flip()
    # ggsave(paste("graphs/",name,".svg",sep=""), width=10, height=3)
return(p)  
}

# Multiplot of ggplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}