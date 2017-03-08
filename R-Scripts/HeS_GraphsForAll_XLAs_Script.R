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

ERHE$Arrival="ErA"
EXHE$Arrival="ExA"
HEHE$Arrival="HeA"
MNHE$Arrival="MnA"
MPHE$Arrival="MpA"


HES <- rbind(select(ERHE, S, Arrival, MEANWTQ1), select(EXHE, S, Arrival, MEANWTQ1), select(HEHE, S, Arrival, MEANWTQ1), select(MNHE, S, Arrival, MEANWTQ1), select(MPHE, S, Arrival, MEANWTQ1))
p1 <- ggplot(HES, aes(x=S, y=MEANWTQ1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Mean wait time in system") + 
  ggtitle(expression(paste(lambda, "=0.1")))
 
HES <- rbind(select(ERHE, S, Arrival, MEANWTQ8), select(EXHE, S, Arrival, MEANWTQ8), select(HEHE, S, Arrival, MEANWTQ8), select(MNHE, S, Arrival, MEANWTQ8), select(MPHE, S, Arrival, MEANWTQ8))
p2 <- ggplot(HES, aes(x=S, y=MEANWTQ8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.8")))

HES <- rbind(select(ERHE, S, Arrival, MEANWTQ95), select(EXHE, S, Arrival, MEANWTQ95), select(HEHE, S, Arrival, MEANWTQ95), select(MNHE, S, Arrival, MEANWTQ95), select(MPHE, S, Arrival, MEANWTQ95))
p3 <- ggplot(HES, aes(x=S, y=MEANWTQ95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) + 
  ggtitle(expression(paste(lambda, "=0.95")))

HES <- rbind(select(ERHE, S, Arrival, PLOSS1), select(EXHE, S, Arrival, PLOSS1), select(HEHE, S, Arrival, PLOSS1), select(MNHE, S, Arrival, PLOSS1), select(MPHE, S, Arrival, PLOSS1))
p4 <- ggplot(HES, aes(x=S, y=PLOSS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("P(Loss)") 

HES <- rbind(select(ERHE, S, Arrival, PLOSS8), select(EXHE, S, Arrival, PLOSS8), select(HEHE, S, Arrival, PLOSS8), select(MNHE, S, Arrival, PLOSS8), select(MPHE, S, Arrival, PLOSS8))
p5 <- ggplot(HES, aes(x=S, y=PLOSS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, PLOSS95), select(EXHE, S, Arrival, PLOSS95), select(HEHE, S, Arrival, PLOSS95), select(MNHE, S, Arrival, PLOSS95), select(MPHE, S, Arrival, PLOSS95))
p6 <- ggplot(HES, aes(x=S, y=PLOSS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0, 0.25)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, RLWSR1), select(EXHE, S, Arrival, RLWSR1), select(HEHE, S, Arrival, RLWSR1), select(MNHE, S, Arrival, RLWSR1), select(MPHE, S, Arrival, RLWSR1))
p7 <- ggplot(HES, aes(x=S, y=RLWSR1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of leaving with service") 

HES <- rbind(select(ERHE, S, Arrival, RLWSR8), select(EXHE, S, Arrival, RLWSR8), select(HEHE, S, Arrival, RLWSR8), select(MNHE, S, Arrival, RLWSR8), select(MPHE, S, Arrival, RLWSR8))
p8 <- ggplot(HES, aes(x=S, y=RLWSR8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, RLWSR95), select(EXHE, S, Arrival, RLWSR95), select(HEHE, S, Arrival, RLWSR95), select(MNHE, S, Arrival, RLWSR95), select(MPHE, S, Arrival, RLWSR95))
p9 <- ggplot(HES, aes(x=S, y=RLWSR95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.04, 0.7)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, RDLOS1), select(EXHE, S, Arrival, RDLOS1), select(HEHE, S, Arrival, RDLOS1), select(MNHE, S, Arrival, RDLOS1), select(MPHE, S, Arrival, RDLOS1))
p10 <- ggplot(HES, aes(x=S, y=RDLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.3)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss at arrival") 

HES <- rbind(select(ERHE, S, Arrival, RDLOS8), select(EXHE, S, Arrival, RDLOS8), select(HEHE, S, Arrival, RDLOS8), select(MNHE, S, Arrival, RDLOS8), select(MPHE, S, Arrival, RDLOS8))
p11 <- ggplot(HES, aes(x=S, y=RDLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.3)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, RDLOS95), select(EXHE, S, Arrival, RDLOS95), select(HEHE, S, Arrival, RDLOS95), select(MNHE, S, Arrival, RDLOS95), select(MPHE, S, Arrival, RDLOS95))
p12 <- ggplot(HES, aes(x=S, y=RDLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.004, 0.3)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.justification=c(1,1), legend.position=c(1,1), legend.key=element_rect(linetype = "blank"), legend.title=element_blank(), legend.text=element_text(size=7), title=element_text(size=9, face='bold')) 


HES <- rbind(select(ERHE, S, Arrival, RCLOS1), select(EXHE, S, Arrival, RCLOS1), select(HEHE, S, Arrival, RCLOS1), select(MNHE, S, Arrival, RCLOS1), select(MPHE, S, Arrival, RCLOS1))
p13 <- ggplot(HES, aes(x=S, y=RCLOS1)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.00, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) +
  ylab("Rate of loss after admission") 

HES <- rbind(select(ERHE, S, Arrival, RCLOS8), select(EXHE, S, Arrival, RCLOS8), select(HEHE, S, Arrival, RCLOS8), select(MNHE, S, Arrival, RCLOS8), select(MPHE, S, Arrival, RCLOS8))
p14 <- ggplot(HES, aes(x=S, y=RCLOS8)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 

HES <- rbind(select(ERHE, S, Arrival, RCLOS95), select(EXHE, S, Arrival, RCLOS95), select(HEHE, S, Arrival, RCLOS95), select(MNHE, S, Arrival, RCLOS95), select(MPHE, S, Arrival, RCLOS95))
p15 <- ggplot(HES, aes(x=S, y=RCLOS95)) + geom_line(aes(linetype=Arrival)) + 
  scale_linetype_manual(values=c("solid", "dotdash", "twodash", "dotted", "dashed")) +
  scale_y_continuous(limits=c(0.01, 0.5)) +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", title=element_text(size=9, face='bold')) 


grid.arrange(p1, p2, p3, p7, p8, p9, p13, p14, p15, p10, p11, p12, ncol=3)
p12

#grid.arrange(p10, p11, p12, ncol=3)
#grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, ncol=3)




