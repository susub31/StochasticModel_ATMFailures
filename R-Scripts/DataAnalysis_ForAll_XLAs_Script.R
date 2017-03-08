library(data.table)
library(gridExtra)
library(ggplot2)
library(dplyr)

setwd("~/Sudha/Analytics/Papers-Related/Datasets")

output8 <- read.csv("XLA0.8Services.csv")
output1 <- read.csv("XLA0.1Services.csv")
output95 <- read.csv("XLA0.95Services.csv")

output8=fread('XLA0.8Services.csv',select = c('XLA','S','RHO', 'SDA', 'SDS', 'P(DOWN)', 'P(IDLE)', 'P(LOSS)', 'MEANWTQ', 'RLWSR', 'RDLOS', 'RCLOS', 'P(CS_SH)', 'P(DN_SK)'))
output8=setNames(output8, c('XLA8','S','RHO8', 'SDA8', 'SDS8', 'PDOWN8', 'PIDLE8', 'PLOSS8', 'MEANWTQ8', 'RLWSR8', 'RDLOS8', 'RCLOS8', 'PCS_SH8', 'PDN_SK8'))

output95=fread('XLA0.95Services.csv',select = c('XLA','RHO', 'SDA', 'SDS', 'P(DOWN)', 'P(IDLE)', 'P(LOSS)', 'MEANWTQ', 'RLWSR', 'RDLOS', 'RCLOS', 'P(CS_SH)', 'P(DN_SK)'))
output95=setNames(output95, c('XLA95','RHO95', 'SDA95', 'SDS95', 'PDOWN95', 'PIDLE95', 'PLOSS95', 'MEANWTQ95', 'RLWSR95', 'RDLOS95', 'RCLOS95', 'PCS_SH95', 'PDN_SK95'))

output1=fread('XLA0.1Services.csv',select = c('XLA','RHO', 'SDA', 'SDS', 'P(DOWN)', 'P(IDLE)', 'P(LOSS)', 'MEANWTQ', 'RLWSR', 'RDLOS', 'RCLOS', 'P(CS_SH)', 'P(DN_SK)'))
output1=setNames(output1, c('XLA1', 'RHO1', 'SDA1', 'SDS1', 'PDOWN1', 'PIDLE1', 'PLOSS1', 'MEANWTQ1', 'RLWSR1', 'RDLOS1', 'RCLOS1', 'PCS_SH1', 'PDN_SK1'))

output <- cbind(output8, output1)
output <- cbind(output, output95)


ERA <- 0.88388
EXA <- 1.25
MNA <- 1.7619
HEA <- 2.8059

ERS <- 0.70711
EXS <- 1
HES <- 2.24472

RHON <- -0.48891
RHOP <- 0.48891

ERER <- filter(output, SDA8==ERA & SDS8==ERS)
EREX <- filter(output, SDA8==ERA & SDS8==EXS)
ERHE <- filter(output, SDA8==ERA & SDS8==HES)

EXER <- filter(output, SDA8==EXA & SDS8==ERS)
EXEX <- filter(output, SDA8==EXA & SDS8==EXS)
EXHE <- filter(output, SDA8==EXA & SDS8==HES)

HEER <- filter(output, SDA8==HEA & SDS8==ERS)
HEEX <- filter(output, SDA8==HEA & SDS8==EXS)
HEHE <- filter(output, SDA8==HEA & SDS8==HES)

MNER <- filter(output, SDA8==MNA & SDS8==ERS & RHO8==RHON)
MNEX <- filter(output, SDA8==MNA & SDS8==EXS & RHO8==RHON)
MNHE <- filter(output, SDA8==MNA & SDS8==HES & RHO8==RHON)

MPER <- filter(output, SDA8==MNA & SDS8==ERS & RHO8==RHOP)
MPEX <- filter(output, SDA8==MNA & SDS8==EXS & RHO8==RHOP)
MPHE <- filter(output, SDA8==MNA & SDS8==HES & RHO8==RHOP)

ERER$Arrival="ER"
EXER$Arrival="EX"
HEER$Arrival="HE"
MNER$Arrival="MN"
MPER$Arrival="MP"


ERS <- rbind(select(ERER, S, Arrival, MEANWTQ1), select(EXER, S, Arrival, MEANWTQ1), select(HEER, S, Arrival, MEANWTQ1), select(MNER, S, Arrival, MEANWTQ1), select(MPER, S, Arrival, MEANWTQ1))
p1 <- ggplot(ERS, aes(x=S, y=MEANWTQ1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Mean wait time in system") + 
  ggtitle(expression(paste(lambda, "=0.1")))
 
ERS <- rbind(select(ERER, S, Arrival, MEANWTQ8), select(EXER, S, Arrival, MEANWTQ8), select(HEER, S, Arrival, MEANWTQ8), select(MNER, S, Arrival, MEANWTQ8), select(MPER, S, Arrival, MEANWTQ8))
p2 <- ggplot(ERS, aes(x=S, y=MEANWTQ8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.8")))

ERS <- rbind(select(ERER, S, Arrival, MEANWTQ95), select(EXER, S, Arrival, MEANWTQ95), select(HEER, S, Arrival, MEANWTQ95), select(MNER, S, Arrival, MEANWTQ95), select(MPER, S, Arrival, MEANWTQ95))
p3 <- ggplot(ERS, aes(x=S, y=MEANWTQ95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.95")))

ERS <- rbind(select(ERER, S, Arrival, PLOSS1), select(EXER, S, Arrival, PLOSS1), select(HEER, S, Arrival, PLOSS1), select(MNER, S, Arrival, PLOSS1), select(MPER, S, Arrival, PLOSS1))
p4 <- ggplot(ERS, aes(x=S, y=PLOSS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("P(Loss)") 

ERS <- rbind(select(ERER, S, Arrival, PLOSS8), select(EXER, S, Arrival, PLOSS8), select(HEER, S, Arrival, PLOSS8), select(MNER, S, Arrival, PLOSS8), select(MPER, S, Arrival, PLOSS8))
p5 <- ggplot(ERS, aes(x=S, y=PLOSS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, PLOSS95), select(EXER, S, Arrival, PLOSS95), select(HEER, S, Arrival, PLOSS95), select(MNER, S, Arrival, PLOSS95), select(MPER, S, Arrival, PLOSS95))
p6 <- ggplot(ERS, aes(x=S, y=PLOSS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RLWSR1), select(EXER, S, Arrival, RLWSR1), select(HEER, S, Arrival, RLWSR1), select(MNER, S, Arrival, RLWSR1), select(MPER, S, Arrival, RLWSR1))
p7 <- ggplot(ERS, aes(x=S, y=RLWSR1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of leaving with service") 

ERS <- rbind(select(ERER, S, Arrival, RLWSR8), select(EXER, S, Arrival, RLWSR8), select(HEER, S, Arrival, RLWSR8), select(MNER, S, Arrival, RLWSR8), select(MPER, S, Arrival, RLWSR8))
p8 <- ggplot(ERS, aes(x=S, y=RLWSR8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RLWSR95), select(EXER, S, Arrival, RLWSR95), select(HEER, S, Arrival, RLWSR95), select(MNER, S, Arrival, RLWSR95), select(MPER, S, Arrival, RLWSR95))
p9 <- ggplot(ERS, aes(x=S, y=RLWSR95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RDLOS1), select(EXER, S, Arrival, RDLOS1), select(HEER, S, Arrival, RDLOS1), select(MNER, S, Arrival, RDLOS1), select(MPER, S, Arrival, RDLOS1))
p10 <- ggplot(ERS, aes(x=S, y=RDLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss at arrival") 

ERS <- rbind(select(ERER, S, Arrival, RDLOS8), select(EXER, S, Arrival, RDLOS8), select(HEER, S, Arrival, RDLOS8), select(MNER, S, Arrival, RDLOS8), select(MPER, S, Arrival, RDLOS8))
p11 <- ggplot(ERS, aes(x=S, y=RDLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RDLOS95), select(EXER, S, Arrival, RDLOS95), select(HEER, S, Arrival, RDLOS95), select(MNER, S, Arrival, RDLOS95), select(MPER, S, Arrival, RDLOS95))
p12 <- ggplot(ERS, aes(x=S, y=RDLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.justification=c(1,1), legend.position=c(1,1), legend.key=element_rect(size = 0.2), legend.title=element_text(size=9, face='bold'), legend.text=element_text(size=6), title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RCLOS1), select(EXER, S, Arrival, RCLOS1), select(HEER, S, Arrival, RCLOS1), select(MNER, S, Arrival, RCLOS1), select(MPER, S, Arrival, RCLOS1))
p13 <- ggplot(ERS, aes(x=S, y=RCLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.00, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss after admission") 

ERS <- rbind(select(ERER, S, Arrival, RCLOS8), select(EXER, S, Arrival, RCLOS8), select(HEER, S, Arrival, RCLOS8), select(MNER, S, Arrival, RCLOS8), select(MPER, S, Arrival, RCLOS8))
p14 <- ggplot(ERS, aes(x=S, y=RCLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERS <- rbind(select(ERER, S, Arrival, RCLOS95), select(EXER, S, Arrival, RCLOS95), select(HEER, S, Arrival, RCLOS95), select(MNER, S, Arrival, RCLOS95), select(MPER, S, Arrival, RCLOS95))
p15 <- ggplot(ERS, aes(x=S, y=RCLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 


grid.arrange(p1, p2, p3, p7, p8, p9, p13, p14, p15, p10, p11, p12, ncol=3)

#grid.arrange(p13, p14, p15, ncol=3)
#grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol=3)



myplot <- function(xcol, ycol, xl, yl) {
  p1 <- ggplot(ERER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ER-ER")
  p2 <- ggplot(EREX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ER-EX")
  p3 <- ggplot(ERHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("ER-HE")

  p4 <- ggplot(EXER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("EX-ER")
  p5 <- ggplot(EXEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("EX-EX")
  p6 <- ggplot(EXHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("EX-HE")
  
  p7 <- ggplot(HEER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HE-ER")
  p8 <- ggplot(HEEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HE-EX")
  p9 <- ggplot(HEHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("HE-HE")
  
  p10 <- ggplot(MNER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MN-ER")
  p11 <- ggplot(MNEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MN-EX")
  p12 <- ggplot(MNHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MN-HE")
  
  p13 <- ggplot(MPER, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MP-ER")
  p14 <- ggplot(MPEX, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MP-EX")
  p15 <- ggplot(MPHE, aes_string(x = xcol, y = ycol)) + geom_line() + theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(limits=c(xl, yl)) + ggtitle("MP-HE")

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



