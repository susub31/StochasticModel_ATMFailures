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

EREX$Arrival="ErA"
EXEX$Arrival="ExA"
HEEX$Arrival="HeA"
MNEX$Arrival="MnA"
MPEX$Arrival="MpA"


ERX <- rbind(select(EREX, S, Arrival, MEANWTQ1), select(EXEX, S, Arrival, MEANWTQ1), select(HEEX, S, Arrival, MEANWTQ1), select(MNEX, S, Arrival, MEANWTQ1), select(MPEX, S, Arrival, MEANWTQ1))
p1 <- ggplot(ERX, aes(x=S, y=MEANWTQ1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Mean wait time in system") + 
  ggtitle(expression(paste(lambda, "=0.1")))
 
ERX <- rbind(select(EREX, S, Arrival, MEANWTQ8), select(EXEX, S, Arrival, MEANWTQ8), select(HEEX, S, Arrival, MEANWTQ8), select(MNEX, S, Arrival, MEANWTQ8), select(MPEX, S, Arrival, MEANWTQ8))
p2 <- ggplot(ERX, aes(x=S, y=MEANWTQ8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.8")))

ERX <- rbind(select(EREX, S, Arrival, MEANWTQ95), select(EXEX, S, Arrival, MEANWTQ95), select(HEEX, S, Arrival, MEANWTQ95), select(MNEX, S, Arrival, MEANWTQ95), select(MPEX, S, Arrival, MEANWTQ95))
p3 <- ggplot(ERX, aes(x=S, y=MEANWTQ95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.95")))

ERX <- rbind(select(EREX, S, Arrival, PLOSS1), select(EXEX, S, Arrival, PLOSS1), select(HEEX, S, Arrival, PLOSS1), select(MNEX, S, Arrival, PLOSS1), select(MPEX, S, Arrival, PLOSS1))
p4 <- ggplot(ERX, aes(x=S, y=PLOSS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("P(Loss)") 

ERX <- rbind(select(EREX, S, Arrival, PLOSS8), select(EXEX, S, Arrival, PLOSS8), select(HEEX, S, Arrival, PLOSS8), select(MNEX, S, Arrival, PLOSS8), select(MPEX, S, Arrival, PLOSS8))
p5 <- ggplot(ERX, aes(x=S, y=PLOSS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, PLOSS95), select(EXEX, S, Arrival, PLOSS95), select(HEEX, S, Arrival, PLOSS95), select(MNEX, S, Arrival, PLOSS95), select(MPEX, S, Arrival, PLOSS95))
p6 <- ggplot(ERX, aes(x=S, y=PLOSS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RLWSR1), select(EXEX, S, Arrival, RLWSR1), select(HEEX, S, Arrival, RLWSR1), select(MNEX, S, Arrival, RLWSR1), select(MPEX, S, Arrival, RLWSR1))
p7 <- ggplot(ERX, aes(x=S, y=RLWSR1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of leaving with service") 

ERX <- rbind(select(EREX, S, Arrival, RLWSR8), select(EXEX, S, Arrival, RLWSR8), select(HEEX, S, Arrival, RLWSR8), select(MNEX, S, Arrival, RLWSR8), select(MPEX, S, Arrival, RLWSR8))
p8 <- ggplot(ERX, aes(x=S, y=RLWSR8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RLWSR95), select(EXEX, S, Arrival, RLWSR95), select(HEEX, S, Arrival, RLWSR95), select(MNEX, S, Arrival, RLWSR95), select(MPEX, S, Arrival, RLWSR95))
p9 <- ggplot(ERX, aes(x=S, y=RLWSR95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RDLOS1), select(EXEX, S, Arrival, RDLOS1), select(HEEX, S, Arrival, RDLOS1), select(MNEX, S, Arrival, RDLOS1), select(MPEX, S, Arrival, RDLOS1))
p10 <- ggplot(ERX, aes(x=S, y=RDLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss at arrival") 

ERX <- rbind(select(EREX, S, Arrival, RDLOS8), select(EXEX, S, Arrival, RDLOS8), select(HEEX, S, Arrival, RDLOS8), select(MNEX, S, Arrival, RDLOS8), select(MPEX, S, Arrival, RDLOS8))
p11 <- ggplot(ERX, aes(x=S, y=RDLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RDLOS95), select(EXEX, S, Arrival, RDLOS95), select(HEEX, S, Arrival, RDLOS95), select(MNEX, S, Arrival, RDLOS95), select(MPEX, S, Arrival, RDLOS95))
p12 <- ggplot(ERX, aes(x=S, y=RDLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.justification=c(1,1), legend.position=c(1,1), legend.key=element_rect(linetype = "blank"), legend.title=element_blank(), legend.text=element_text(size=7), title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RCLOS1), select(EXEX, S, Arrival, RCLOS1), select(HEEX, S, Arrival, RCLOS1), select(MNEX, S, Arrival, RCLOS1), select(MPEX, S, Arrival, RCLOS1))
p13 <- ggplot(ERX, aes(x=S, y=RCLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.00, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss after admission") 

ERX <- rbind(select(EREX, S, Arrival, RCLOS8), select(EXEX, S, Arrival, RCLOS8), select(HEEX, S, Arrival, RCLOS8), select(MNEX, S, Arrival, RCLOS8), select(MPEX, S, Arrival, RCLOS8))
p14 <- ggplot(ERX, aes(x=S, y=RCLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

ERX <- rbind(select(EREX, S, Arrival, RCLOS95), select(EXEX, S, Arrival, RCLOS95), select(HEEX, S, Arrival, RCLOS95), select(MNEX, S, Arrival, RCLOS95), select(MPEX, S, Arrival, RCLOS95))
p15 <- ggplot(ERX, aes(x=S, y=RCLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 


grid.arrange(p1, p2, p3, p7, p8, p9, p13, p14, p15, p10, p11, p12, ncol=3)
p12
#grid.arrange(p13, p14, p15, ncol=3)
#grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol=3)




