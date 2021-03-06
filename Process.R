# get required packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
cat("Packages loaded.\n")

# function to calculate se
se <- function(vec) {
  return(sd(vec)/sqrt(length(vec)))
}

# rounds to four digits and returns as character
cust.round <- function(num, numdig = 4) {
  as.character(round(num, digits = numdig))
}
cat("Personal functions defined.\n")

# read in data
dataset <- read.csv("Results.csv",
                    header = TRUE,
                    na.strings = c("NA", "", "Inf", "NR"),
                    colClasses = c(rep("character", 3),
                                   rep("integer", 6)))
cat("Data input completed.\n")

# processing: change all NA values to 1000 (after all thresholds)
dataset[is.na(dataset)] <- 1000

# if folder doesn't exist, make it
if (!file.exists("Figs")) {
  dir.create("Figs")
  cat("Folder for plots created.\n")
}

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

# plot as line plot with smoother
df.thresh <- df.thresh %>%
  dplyr::mutate(SE = sqrt(Prop.Detect * (1 - Prop.Detect) / nrow(dataset)),
                ymin = Prop.Detect - SE,
                ymax = Prop.Detect + SE)
df.thresh[df.thresh$Tool != "SCI",]$SE <- 0
df.thresh[df.thresh$Tool != "SCI",]$ymin <- 0
df.thresh[df.thresh$Tool != "SCI",]$ymax <- 0
ggplot(df.thresh, aes(x = Threshold, y = Prop.Detect, linetype = Tool)) +
  geom_ribbon(aes(ymin = ymin,
                  ymax = ymax),
                  fill = "grey90") +
  geom_line() +
  ylab("Proportion of dual diagnoses detected") +
  xlab("Disease Detection Rank Threshold") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2)) +
  scale_colour_manual(values = c("red", "blue", "black"),
                      label = c("FindZebra",
                                "SCI",
                                "SCN")) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.425), expand = c(0, 0))
ggsave("Figs/StatVaryThresh.png", width = 4, height = 4)
cat("Plot for varying rank threshold created.\n")

#####EFFECT OF SINGLE DDX#####
# list of conditions
single.ddx <- data.frame(conds = c("TSD", "SLO", "MMA", "GSD", "PKU", "FXS", "AT", "CF", "TOI", "MD", "NS", "SS", "HS", "SDS", "TSC", "FXS2", "HS2", "AT2"),
                         fz = c(1, 6, 5, 3, 1, 5, 8, 6, 1, 1, 3, 2, 10, 9, 4, 5, 6, 3),
                         sci= c(2, 1, 5, 3, 1, 3, 5, 1, 4, 1, 1, 8, 1, 1, 1, 6, 4, 2),
                         scn= c(1, 6, 5, 1, 2, 2, 35, 1, 17, 1, 5, 26, 1, 1, 1, 10, 5, 2))

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
  xlab("FindZebra Single Dx Rank")
ggsave("Figs/SingleDual-FZ.png", width = 6, height = 6)

# SC (Incidence)
sci.model <- lm(SCI ~ SCI.Single, data = df.prop.det)
common +
  geom_point(aes(x = SCI.Single, y = SCI)) +
  xlab("SCI Single Dx Rank")
ggsave("Figs/SingleDual-SCI.png", width = 6, height = 6)

# SC (No incidence)
scn.model <- lm(SCN ~ SCN.Single, data = df.prop.det)
common +
  geom_point(aes(x = SCN.Single, y = SCN)) +
  xlab("SCN Single Dx Rank")
ggsave("Figs/SingleDual-SCN.png", width = 6, height = 6)
cat("Plots for tool bias created.\n")

# reshape for dodge bar plot
df.bias <- df.prop.det %>%
  select(seq(1, 7, by = 2)) %>%
  gather("Tool", "Proportion", 2:4)

ggplot(df.bias, aes(x = Condition, y = Proportion, fill = Tool)) +
  geom_bar(stat = "identity", position = "dodge",
           color = "black", size = 0.25) +
  scale_fill_manual(values = c("white", "black", "gray"),
                    label = c("FindZebra", "SCI", "SCN")) +
  theme_bw() +
  theme(legend.position = "top", panel.grid = element_blank()) +
  geom_vline(xintercept = seq(1.5, by = 1,
                              length = length(single.ddx$conds) - 1),
             size = 0.25) +
  ylab("Proportion Detected") +
  scale_y_continuous(limits = c(0, 1.025), expand = c(0, 0))
ggsave("Figs/CondBias.png", width = 6, height = 3)
cat("Plot for conditional bias created.\n")

cat("Script completed.\n")