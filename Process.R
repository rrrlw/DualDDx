# get required packages
.libPaths("C:/Users/wadhwar/Documents/R/win-library/3.4")
library(ggplot2)
library(tidyr)
library(dplyr)

# function to calculate se
se <- function(vec) {
  return(sd(vec)/sqrt(length(vec)))
}

# rounds to four digits and returns as character
cust.round <- function(num, numdig = 4) {
  as.character(round(num, digits = numdig))
}

# read in data
dataset <- read.csv("Results.csv",
                    header = TRUE,
                    na.strings = c("NA",""),
                    colClasses = c(rep("character", 3),
                                   rep("integer", 6)))

# if folder doesn't exist, make it
if (!file.exists("Figs")) {
  dir.create("Figs")
}

#####SINGLE THRESHOLD#####
# combine ranks for both diagnoses into one
fz <- c(dataset$FZC1, dataset$FZC2)
sci<- c(dataset$SCC1I, dataset$SCC2I)
scn<- c(dataset$SCC1W, dataset$SCC2W)

# convert ranks to boolean for proportion testing
# see how good each tool is overall
fz <- fz <= 20
sci<- sci<= 20
scn<- scn<= 20

# store means and sd into data frame for bar chart
df.props <- data.frame(Condition = c("FZ", "SCI", "SCN"),
                       Mean = c(mean(fz),
                                mean(sci),
                                mean(scn)),
                       SD = c(sd(fz),
                              sd(sci),
                              sd(scn)))

# plot as bar
ggplot(data = df.props, aes(x = Condition, y = Mean, color = I("black"))) +
  geom_bar(stat = "identity", fill = "red") +
  theme_bw() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.35) +
  ylab("Proportion detected") +
  xlab("DDx Tool")
ggsave("Figs/BarplotSingle.png", width = 6, height = 6)

# check how good a tool is at detecting both in a pair
fz <- (dataset$FZC1 <= 20 & dataset$FZC2 <= 20)
sci<- (dataset$SCC1I<= 20 & dataset$SCC2I<= 20)
scn<- (dataset$SCC1W<= 20 & dataset$SCC2W<= 20)

# store means and sd into data frame for bar chart
df.props <- data.frame(Condition = c("FZ", "SCI", "SCN"),
                       Mean = c(mean(fz),
                                mean(sci),
                                mean(scn)),
                       SD = c(sd(fz),
                              sd(sci),
                              sd(scn)),
                       SE = c(se(fz),
                              se(sci),
                              se(scn)))

# plot as bar
ggplot(data = df.props, aes(x = Condition, y = Mean, color = I("black"))) +
  geom_bar(stat = "identity", fill = "red") +
  theme_bw() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.35) +
  ylab("Proportion detected") +
  xlab("DDx Tool")
ggsave("Figs/BarplotPairSD.png", width = 6, height = 6)

ggplot(data = df.props, aes(x = Condition, y = Mean, color = I("black"))) +
  geom_bar(stat = "identity", fill = "red") +
  theme_bw() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.35) +
  ylab("Proportion detected") +
  xlab("DDx Tool")
ggsave("Figs/BarplotPairSE.png", width = 6, height = 6)

#####VARYING THRESHOLD#####
# see how good each tool is at detecting both in a pair based on varying
# threshold of what counts as detection (less than what rank)
vals <- 1:20
fz <- numeric(length(vals))
sci<- numeric(length(vals))
scn<- numeric(length(vals))
counter <- 1
for (i in vals) {
  # get vectors for current threshold
  temp.fz <- (dataset$FZC1 <= i & dataset$FZC2 <= i)
  temp.sci<- (dataset$SCC1I<= i & dataset$SCC2I<= i)
  temp.scn<- (dataset$SCC1W<= i & dataset$SCC2W<= i)
  
  # store mean of each column
  fz[counter] <- mean(temp.fz)
  sci[counter]<- mean(temp.sci)
  scn[counter]<- mean(temp.scn)
  
  # update loop counter
  counter <- counter + 1
}

# store values into data frame and reshape
df.thresh <- data.frame(Threshold = vals,
                        FZ   = fz,
                        SCI  = sci,
                        SCN  = scn)
df.thresh <- df.thresh %>%
  gather("Tool", "Prop.Detect", 2:4)

# plot as line plot
ggplot(df.thresh, aes(x = Threshold, y = Prop.Detect, colour = Tool)) +
  geom_line() +
  ylab(label = "Proportion of dual diagnoses detected") +
  xlab("Disease Detection Rank Threshold") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2)) +
  scale_colour_manual(values = c("red", "blue", "black"),
                      label = c("FindZebra",
                                "SC (Incidence)",
                                "SC (No incidence)"))
ggsave("Figs/VaryThreshold.png", width = 6, height = 6)


#####EFFECT OF SINGLE DDX#####
# list of conditions
single.ddx <- data.frame(conds = c("TSD", "SLO", "MMA", "GSD", "PKU", "FXS", "AT", "CF", "TOI", "MD", "NS", "SS", "HS", "SDS", "TSC"),
                         fz = c(1, 6, 5, 3, 1, 5, 8, 6, 1, 1, 3, 2, 10, 9, 4),
                         sci= c(2, 1, 5, 3, 1, 3, 5, 1, 4, 1, 1, 8, 1, 1, 1),
                         scn= c(1, 6, 5, 1, 2, 2, 35, 1, 17, 1, 5, 26, 1, 1, 1))

# function to detect what proportion of the time this disease was detected by given tool
prop.detected <- function(condition, tool, rank.thresh) {
  # only choose columns important for tool of interest
  if (tool == "FZ") {
    interest <- select(dataset, "Condition.1", "Condition.2", "FZC1", "FZC2")
  } else if (tool == "SCI") {
    interest <- select(dataset, "Condition.1", "Condition.2", "SCC1I", "SCC2I")
  } else if (tool == "SCN") {
    interest <- select(dataset, "Condition.1", "Condition.2", "SCC1W", "SCC2W")
  } else {
    stop(paste("ERROR: tool name -", tool, "- not recognized."))
  }
  
  # only choose rows important for condition of interest
  interest <- interest[(interest$Condition.1 == condition) | (interest$Condition.2 == condition), ]
  names(interest) <- c("C1", "C2", "D1", "D2")
  
  # count number of times detection met threshold
  total <- nrow(interest)
  count <- 0
  for (i in 1:nrow(interest)) {
    if (interest$C1[i] == condition && interest$D1[i] <= rank.thresh ||
        interest$C2[i] == condition && interest$D2[i] <= rank.thresh) {
      count <- count + 1
    }
  }
  
  # return proportion
  return(count / total)
}

# make data frame containing proportion detection results
df.prop.det <- data.frame(Condition = single.ddx$conds,
                          FZ.Single = single.ddx$fz,
                          FZ  = vapply(X = single.ddx$conds,
                                             FUN=function(x) prop.detected(x, "FZ", 20),
                                             FUN.VALUE = numeric(1)),
                          SCI.Single= single.ddx$sci,
                          SCI = vapply(X = single.ddx$conds,
                                             FUN=function(x) prop.detected(x, "SCI", 20),
                                             FUN.VALUE = numeric(1)),
                          SCN.Single= single.ddx$scn,
                          SCN = vapply(X = single.ddx$conds,
                                             FUN=function(x) prop.detected(x, "SCN", 20),
                                             FUN.VALUE = numeric(1)))

# plot coordinates to see if single ddx rank affects dual ddx rank
common <- ggplot(df.prop.det) +
  theme_classic() +
  ylab("Proportion detected")

# FindZebra
fz.model <- lm(FZ ~ FZ.Single, data = df.prop.det)
common +
  geom_point(aes(x = FZ.Single, y = FZ)) +
  xlab("FindZebra Single Dx Rank") +
  annotate("text", x = 7.5, y = 0.3,
           label = paste("italic(R) ^ 2 ==",
                         cust.round(cor(df.prop.det$FZ.Single, df.prop.det$FZ) ^ 2, 4)),
           parse = TRUE) +
  annotate("text", x = 7.5, y = 0.27,
           label = paste("italic(m) ==",
                         cust.round(fz.model$coefficients[2])),
           parse = TRUE) +
  annotate("text", x = 7.5, y = 0.24,
           label = paste("italic(p) ==",
                         cust.round(summary(fz.model)$coefficients[2, 4])),
           parse = TRUE)
ggsave("Figs/SingleDual-FZ.png", width = 6, height = 6)

# SC (Incidence)
sci.model <- lm(SCI ~ SCI.Single, data = df.prop.det)
common +
  geom_point(aes(x = SCI.Single, y = SCI)) +
  xlab("SC (Incidence) Single Dx Rank") +
  annotate("text", x = 6, y = 0.3,
           label = paste("italic(R) ^ 2 ==",
                         cust.round(cor(df.prop.det$SCI.Single, df.prop.det$SCI) ^ 2, 4)),
           parse = TRUE) +
  annotate("text", x = 6, y = 0.27,
           label = paste("italic(m) ==",
                         cust.round(sci.model$coefficients[2])),
           parse = TRUE) +
  annotate("text", x = 6, y = 0.24,
           label = paste("italic(p) ==",
                         cust.round(summary(sci.model)$coefficients[2, 4])),
           parse = TRUE)
ggsave("Figs/SingleDual-SCI.png", width = 6, height = 6)

# SC (No incidence)
scn.model <- lm(SCN ~ SCN.Single, data = df.prop.det)
common +
  geom_point(aes(x = SCN.Single, y = SCN)) +
  xlab("SC (No incidence) Single Dx Rank") +
  annotate("text", x = 25, y = 0.3,
           label = paste("italic(R) ^ 2 ==",
                         cust.round(cor(df.prop.det$SCN.Single, df.prop.det$SCN) ^ 2, 4)),
           parse = TRUE) +
  annotate("text", x = 25, y = 0.27,
           label = paste("italic(m) ==",
                         cust.round(scn.model$coefficients[2])),
           parse = TRUE) +
  annotate("text", x = 25, y = 0.24,
           label = paste("italic(p) ==",
                         cust.round(summary(scn.model)$coefficients[2, 4])),
           parse = TRUE)
ggsave("Figs/SingleDual-SCN.png", width = 6, height = 6)

# reshape for dodge bar plot
df.bias <- df.prop.det %>%
  select(seq(1, 7, by = 2)) %>%
  gather("Tool", "Proportion", 2:4)

ggplot(df.bias, aes(x = Condition, y = Proportion, fill = Tool)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "blue", "gray"),
                    label = c("FindZebra", "SC (Incidence)", "SC (No incidence)")) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_vline(xintercept = seq(1.5, by = 1, length = length(single.ddx$conds) - 1))
ggsave("Figs/CondBias.png", width = 6, height = 3)
