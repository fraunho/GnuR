#taken from http://www.sthda.com/english/wiki/two-way-anova-test-in-r

#install.packages("ggpubr")

# If .txt tab file, use this
#my_data <- read.delim(file.choose())

library(readxl)
LAMP1_Recruitment <- read_excel("D:/Martin/Desktop/Stats/Fig2E-LAMP-comb.xlsx")
View(LAMP1_Recruitment)
#my_data_o <- PlantGrowth
my_data <- LAMP1_Recruitment

# Show a random sample
#set.seed(1234)
dplyr::sample_n(my_data, 6)

# Show the levels
levels(my_data$group)

#If the levels are not automatically in the correct order, re-order them as follow:
my_data$group <- ordered(my_data$group,
                           levels = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"))

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(EventsPerCell, na.rm = TRUE),
    sd = sd(EventsPerCell, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "EventsPerCell", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00ff00", "#ff0000", "#0000ff"),
          order = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"),
          ylab = "LAMP1 Events/cell", xlab = "Treatment")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
#library("ggpubr")
ggline(my_data, x = "group", y = "EventsPerCell", 
       add = c("mean_se", "jitter"), 
       order = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"),
       ylab = "LAMP1 Events/cell", xlab = "Treatment")


#If you still want to use R base graphs, type the following scripts:
  
# Box plot
boxplot(EventsPerCell ~ group, data = my_data,
          xlab = "group", ylab = "EventsPerCell",
          frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07","#00ff00", "#ff0000", "#0000ff" ))

# plotmeans
library("gplots")
plotmeans(EventsPerCell ~ group, data = my_data,
          xlab = "group", ylab = "EventsPerCell",
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

#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the groups highlighted with “*" in the model summary.

#
#Multiple pairwise-comparison between the means of groups
#
#In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don’t know which pairs of groups are different.
#
#It is possible to perform multiple pairwise-comparison, to determine if the mean difference between specific pairs of group are statistically significant.
#Tukey multiple pairwise-comparisons

#As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) 
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


# 
# Pairewise t-test
# The function pairewise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(my_data$EventsPerCell, my_data$group,
                p.adjust.method = "BH")
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
plot(res.aov, 1)

# 
# Non-parametric alternative to one-way ANOVA test
# 
# Note that, a non-parametric alternative to one-way ANOVA is 
#Kruskal-Wallis rank sum test, which can be used when ANNOVA assumptions are not met.

kruskal.test(EventsPerCell ~ group, data = my_data)

# Kruskal-Wallis rank sum test
# data:  weight by group
# Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842

