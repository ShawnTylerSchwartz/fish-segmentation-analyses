# FishBONE Boxplots
# Shawn T. Schwartz, February 22, 2020
# <shawnschwartz@ucla.edu>

rm(list=ls())

library(ggplot2)

wd <- "~/Developer/fish-segmentation-analyses"
setwd(wd)

data_file <- "data/rmanova_color_human_deeplearning.csv"
data <- read.csv(data_file, sep = ",", header = T)
head(data)

plot_boxes <- function(color_measure, title, XLABFLAG=0, YLABFLAG=0) {
  p <- ggplot(data, aes(x=method, y=color_measure)) +
    stat_boxplot(geom = "errorbar", width = 0.15) +
    geom_boxplot(width=0.15, outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
    scale_x_discrete(labels = c('Deep Learning','Human')) +
    ggtitle(title) +
    theme_classic() +
    theme(axis.text.x = element_text(face="bold", color="#000000", size=14), axis.text.y = element_text(face="bold", color="#000000", size=14),
          axis.title.x = element_text(face="bold", color="#000000", size=16), axis.title.y = element_text(face="bold", color="#000000", size=16),
          plot.title = element_text(color="#000000", size=15, face="bold.italic", hjust = 0.5))
  if(XLABFLAG == 1 && YLABFLAG == 0) {
    return(p + xlab("Method") + ylab(""))
  } else if(XLABFLAG == 1 && YLABFLAG == 1) {
    return(p + xlab("Method") + ylab("Color Pattern Geometry"))
  } else if(XLABFLAG == 0 && YLABFLAG == 1) {
    return(p + xlab("") + ylab("Color Pattern Geometry"))
  } else {
    return(p + xlab("") + ylab(""))
  }
}

m_plot <- plot_boxes(data$m, "Transition Density (m)", 0, 1)
m_plot

A_plot <- plot_boxes(data$A, "Aspect Ratio (A)", 0, 1)
A_plot

Jc_plot <- plot_boxes(data$Jc, "Color Diversity (Jc)", 0, 0)
Jc_plot

Jt_plot <- plot_boxes(data$Jt, "Transition Diversity (Jt)", 1, 0)
Jt_plot

m_dS_plot <- plot_boxes(data$m_dS, "Chromatic B.S. (m_dS)")
m_dS_plot

m_dL_plot <- plot_boxes(data$m_dL, "Achromatic B.S. (m_dL)")
m_dL_plot

#### Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

pdf("six_panel_colorpatgeo_fig.pdf", 11.5, 8.5)
six_panel_fig <- multiplot(m_plot, A_plot, Jc_plot, Jt_plot, m_dS_plot, m_dL_plot, cols=3)
dev.off()
