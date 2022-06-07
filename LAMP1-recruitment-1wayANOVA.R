#taken from http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# If .txt tab file, use this
#my_data <- read.delim(file.choose())

library(readxl)
input <- read_excel("D:/Martin/Desktop/Stats/FluidPhaseMarkerO.xlsx")
#LAMP1_Recruitment <- read_excel("D:/Martin/Desktop/Stats/Fig2E-LAMP-comb.xlsx")

my_data <- input
#View(my_data)

ylabels<-"mean fluorescence (% of control)"
xlabels<-"Treatment"


names <- c("Ctrl","aTox 10 ug/ml", "10% SNT WT","10% SNT dhla")
#names<- c("10 ug/ml a-toxin",		"10% SNT WT",		"10% SNT Δhla",		"10% SNT Δhlb")
#names<- c("Ctrl",		"10 ug/ml a-toxin",		"10% SNT WT",		"10% SNT Δhla",		"10% SNT Δhlb")
#names  <- c("alpha Toxin", "Sa WT SNT",  "Sa delta hla", "Sa delta hlb")
#names  <- c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla")
colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#00ff00", "#ff0000", "#00AFBB", "#E7B800", "#FC4E07", "#00ff00", "#ff0000")

# File should contain treatment "group" and "EventsPerCell" information
# > head (my_data)
# # A tibble: 6 x 2
# group        EventsPerCell
# <ord>                <dbl>
#   1 ionomycin            25.3 
# 2 alpha_Toxin          14.3 
# 3 Sa WT SNT            12.2 
# 4 Sa delta_hla          7.4


#If the levels are not automatically in the correct order, re-order them as follow:
my_data$group <- ordered(my_data$group, levels = names)

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(EventsPerCell, na.rm = TRUE),
    sd = sd(EventsPerCell, na.rm = TRUE)
  )

#####################
# Box plots
# 
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "EventsPerCell", 
          color = "group", palette = colors,
          order = names,
          ylab = ylabels, xlab = xlabels)

#####################
# Mean plots
# 
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
#library("ggpubr")
# ggline(my_data, x = "group", y = "EventsPerCell", 
#        add = c("mean_se", "jitter"), 
#        order = names,
#        ylab = "LAMP1 Events/cell (% of ionomycin)", xlab = "Treatment")
# 

# #If you still want to use R base graphs, type the following scripts:
#   
# # Box plot
# boxplot(EventsPerCell ~ group, data = my_data,
#           xlab = "Treatment", ylab = "LAMP1 Events/cell (% of ionomycin)",
#           frame = FALSE, col = colors)

#####################
# plotmeans
#
library("gplots")
plotmeans(EventsPerCell ~ group, data = my_data,
          xlab = xlabels, ylab = ylabels,
          main="Mean Plot with 95% CI") 


################################
#Compute one-way ANOVA test
#
#We want to know if there is any significant difference between the average weights of plants in the 3 experimental conditions.

#The R function aov() can be used to answer to this question. The function summary.aov() is used to summarize the analysis of variance model.

# Compute the analysis of variance
res.aov <- aov(EventsPerCell ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)


#The output includes the columns F value and Pr(>F) corresponding to the p-value of the test.
#Interpret the result of one-way ANOVA tests
#As the p-value is less than the significance level 0.05, we can conclude that there are 
# significant differences between the groups highlighted with "*" in the model summary.
#
#Multiple pairwise-comparison between the means of groups
#
#In one-way ANOVA test, a significant p-value indicates that some of the group means are different, 
#but we dont know which pairs of groups are different.
#
#It is possible to perform multiple pairwise-comparison, to determine if the mean difference 
#between specific pairs of group are statistically significant.
#Tukey multiple pairwise-comparisons

#As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences, 
#R function: TukeyHSD()) 
#for performing multiple pairwise-comparison between the means of groups.

#The function TukeyHD() takes the fitted ANOVA as an argument.
TukeyHSD(res.aov)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = weight ~ group, data = my_data)
# $group
# diff        lwr       upr     p adj
# trt1-ctrl -0.371 -1.0622161 0.3202161 0.3908711
# trt2-ctrl  0.494 -0.1972161 1.1852161 0.1979960
# trt2-trt1  0.865  0.1737839 1.5562161 0.0120064

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.

# It can be seen from the output, that only the difference between trt2 and trt1 is significant 
# with an adjusted p-value of 0.012.


##################### 
# Pairwise t-test
#
# The function pairewise.t.test() can be also used to calculate pairwise comparisons 
# between group levels with corrections for multiple testing.

pairwise.t.test(my_data$EventsPerCell, my_data$group,
                #p.adjust.method = "BH")
                p.adjust.method = "none")
# 
# 
# Pairwise comparisons using t tests with pooled SD 
# data:  my_data$weight and my_data$group 
# ctrl  trt1 
# trt1 0.194 -    
#   trt2 0.132 0.013
# P value adjustment method: BH 
# 
# The result is a table of p-values for the pairwise comparisons. 
# Here, the p-values have been adjusted by the Benjamini-Hochberg method.
# Check ANOVA assumptions: test validity?
#   
# The ANOVA test assumes that, the data are normally distributed and the variance across groups are homogeneous. 
# We can check that with some diagnostic plots.
# Check the homogeneity of variance assumption
# The residuals versus fits plot can be used to check the homogeneity of variances.
# In the plot below, there is no evident relationships between residuals and fitted values (the mean of each groups), 
# which is good. So, we can assume the homogeneity of variances.

# 1. Homogeneity of variances
###plot(res.aov, 1)

##################################################
#  Kruskal-Wallis rank sum test
# Non-parametric alternative to one-way ANOVA test
# 
# Note that, a non-parametric alternative to one-way ANOVA is 
#Kruskal-Wallis rank sum test, which can be used when ANNOVA assumptions are not met.

kruskal.test(EventsPerCell ~ group, data = my_data)

# Kruskal-Wallis rank sum test
# data:  weight by group
# Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842

