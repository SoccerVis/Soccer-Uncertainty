library(ggplot2)
library(reshape2)
library(plyr)

#First, set a working directory (see lesson 'Analyzing Patient Data' for more info)
setwd('C:/MyPapers/football_uncertainty/study_football_uncertainty')
#Import the data and look at the first six rows
# Data by question
#df <- read.csv(file="result.csv",encoding='utf-8')
# Data by task
df <- read.csv(file="resultByTask.csv")
head(df)
df $Group <- as.factor(df $Group)
df $funScore <- df$soccer_familiarity>3
df $funScore <- as.factor(df $funScore )
df $familiarity <- as.factor(df $soccer_familiarity )
head(df)

data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

# Basic violin plot
p <- ggplot(df , aes(x=Group, y=T1, fill=Group )) + 
    geom_violin() + 
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +   
scale_fill_brewer(palette = "Paired") 
# only for time we use log10
# + scale_y_log10()
p = p+labs(x=element_blank(),y=element_blank(),
               title="T1")
# Moving axis label in middle
p + theme(
  axis.title.x = element_text(hjust=0.5),
  axis.title.y = element_text(hjust=0.5),
  plot.title = element_text(hjust = 0.5, size=16,face="bold"),
  legend.position="none"
)+ scale_x_discrete(labels=c("C" = "Control","E" = "Experiment"))

dev.print(pdf,'T1x.pdf')

# Basic violin plot
p <- ggplot(df , aes(x=Group, y=T2 )) + 
    geom_violin() + 
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +   
scale_fill_brewer(palette = "Paired") 
# only for time we use log10
# + scale_y_log10()
p
dev.print(pdf,'T2x.pdf')

p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(width=0.2,height=0)
p + stat_summary(fun.data=data_summary, 
                 geom="pointrange", color="red")