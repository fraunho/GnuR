#taken from http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# If .txt tab file, use this
#my_data <- read.delim(file.choose())

# #READ GRAPHPAD
# library(pzfx)
# my_data<-pzfx_tables("D:/Martin/Desktop/Stats/ASM Activity Assay Statistics.pzfx")
# View(my_data)

library(readxl)
LAMP1_Recruitment <- read_excel("D:/Martin/Desktop/Stats/Fluid Phase Marker.xlsx")
#LAMP1_Recruitment <- read_excel("D:/Martin/Desktop/Stats/Fig2E-LAMP-comb.xlsx")
View(LAMP1_Recruitment)
my_data <- LAMP1_Recruitment

names  <- c("alpha Toxin", "Sa WT SNT",  "Sa delta hla", "Sa delta hlb")
colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#00ff00", "#ff0000")

#If the levels are not automatically in the correct order, re-order them as follow:
my_data$group <- ordered(my_data$group,
                           levels = names)

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
          color = "group", palette = colors,
          order = names,
          ylab = "LAMP1 Events/cell (% of ionomycin)", xlab = "Treatment")



pairwise.t.test(my_data$EventsPerCell, my_data$group,
#    p.adjust.method = "BH")
#    p.adjust.method = "bonferroni")
#    p.adjust.method = "BY")
#    p.adjust.method = "hommel")
#    p.adjust.method = "hochberg")
#    p.adjust.method = "fdr")
    p.adjust.method = "none")