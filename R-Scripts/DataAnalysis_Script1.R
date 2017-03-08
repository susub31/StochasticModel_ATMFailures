library(gridExtra)
library(ggplot2)
library(dplyr)

setwd("~/Sudha/Analytics/Papers-Related/Datasets")

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

output8 <- read.csv("XLA0.8Services.csv")
output1 <- read.csv("XLA0.1Services.csv")
output95 <- read.csv("XLA0.95Services.csv")

ERA <- 0.88388
EXA <- 1.25
MNA <- 1.7619
HEA <- 2.8059

ERS <- 0.70711
EXS <- 1
HES <- 2.24472

RHON <- -0.48891
RHOP <- 0.48891

ERER <- filter(output8, SDA==ERA & SDS==ERS)
EREX <- filter(output8, SDA==ERA & SDS==EXS)
ERHE <- filter(output8, SDA==ERA & SDS==HES)

EXER <- filter(output8, SDA==EXA & SDS==ERS)
EXEX <- filter(output8, SDA==EXA & SDS==EXS)
EXHE <- filter(output8, SDA==EXA & SDS==HES)

HEER <- filter(output8, SDA==HEA & SDS==ERS)
HEEX <- filter(output8, SDA==HEA & SDS==EXS)
HEHE <- filter(output8, SDA==HEA & SDS==HES)

MNER <- filter(output8, SDA==MNA & SDS==ERS & RHO==RHON)
MNEX <- filter(output8, SDA==MNA & SDS==EXS & RHO==RHON)
MNHE <- filter(output8, SDA==MNA & SDS==HES & RHO==RHON)

MPER <- filter(output8, SDA==MNA & SDS==ERS & RHO==RHOP)
MPEX <- filter(output8, SDA==MNA & SDS==EXS & RHO==RHOP)
MPHE <- filter(output8, SDA==MNA & SDS==HES & RHO==RHOP)

myplot <- function(xcol, ycol, xl, yl) {
  p1 <- ggplot(ERER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ErA-ErS")
  p2 <- ggplot(EREX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ErA-ExS")
  p3 <- ggplot(ERHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ErA-HeS")

  p4 <- ggplot(EXER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ExA-ErS")
  p5 <- ggplot(EXEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ExA-ExS")
  p6 <- ggplot(EXHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ExA-HeS")
  
  p7 <- ggplot(HEER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HeA-ErS")
  p8 <- ggplot(HEEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HeA-ExS")
  p9 <- ggplot(HEHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HeS-HeS")
  
  p10 <- ggplot(MNER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MnA-ErS")
  p11 <- ggplot(MNEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MnA-ExS")
  p12 <- ggplot(MNHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MnA-HeS")
  
  p13 <- ggplot(MPER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MpA-ErS")
  p14 <- ggplot(MPEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MpA-ExS")
  p15 <- ggplot(MPHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MpA-HeS")

  #plot all 15 graphs on one page
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol=5)
  
}

sampleplot <- function(xcol, ycol, xl, yl) {
  p1 <- ggplot(ERER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ER")
  p5 <- ggplot(EXEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("EX")
  p9 <- ggplot(HEHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HE")
  p10 <- ggplot(MNER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MN")
  p13 <- ggplot(MPER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MP")

  #plot all 15 graphs on one page
  grid.arrange(p1, p5, p9, p10, p13, ncol=2)
  
}


# plot graphs for P.DOWN. vs. S
myplot("S", "P.DOWN.", 0.04, 0.25)

# plot graphs for P(loss) vs. S
myplot("S", "P.LOSS.", 0.00, 0.35)

# plot graphs for Mean waiting time in system vs. S
myplot("S", "MEANWTS", 0.9, 5)

# plot graphs for Mean cash level in system vs. S
myplot("S", "MEAN_CL", 0, 120)

# plot graphs for RLWSR vs. S
myplot("S", "RLWSR", 0.25, 0.65)

# plot graphs for RDLOS vs. S
myplot("S", "RDLOS", 0.00, 0.20)

# plot graphs for RCLOS vs. S
myplot("S", "RCLOS", 0.12, 0.42)

# plot graphs for P(down due to cash shortage) vs. S
myplot("S", "P.CS_SH.", 0.00, 0.25)

# plot graphs for P(down due to shock) vs. S
myplot("S", "P.DN_SK.", 0.035, 0.05)

#sample plots one of each arrival type
sampleplot("S", "P.DOWN.", 0.04, 0.25)
sampleplot("S", "P.LOSS.", 0.00, 0.35)
sampleplot("S", "MEANWTS", 0.9, 5)
sampleplot("S", "MEAN_CL", 0, 120)
sampleplot("S", "RLWSR", 0.25, 0.65)
sampleplot("S", "RDLOS", 0.00, 0.20)
sampleplot("S", "RCLOS", 0.12, 0.42)
sampleplot("S", "P.CS_SH.", 0.00, 0.25)
sampleplot("S", "P.DN_SK.", 0.035, 0.05)

rm(p1)
xcol="S"
ycol="RCLOS"
xl=0.11
yl=0.25
p1 <- ggplot(ERER, aes_string(x =xcol, y =ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())  + ggtitle("ER-ER") + scale_y_continuous(limits=c(xl, yl))

p1



