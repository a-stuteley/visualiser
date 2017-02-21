#install.packages("VennDiagram")
library(VennDiagram)
grid.newpage()
draw.pairwise.venn(3115, 25990, 0, category = c("Maori", "Non-Maori"), lty = rep("blank",2), 
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 
                    0), cat.dist = rep(0.025, 2))

grid.newpage()
draw.pairwise.venn(1234, 27871, 0, category = c("Pacific", "Non-Pacific"), lty = rep("blank",2), 
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 
                    0), cat.dist = rep(0.025, 2))

grid.newpage()
draw.pairwise.venn(1234, 3115, 50, category = c("Pacific", "Maori"), lty = rep("blank",2), 
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 
                    0), cat.dist = rep(0.025, 2))


######################
grid.newpage()
draw.pairwise.venn(area1 = 3115, area2 = 25990, cross.area = 0, category = c("Maori", 
                            "Non-Maori"), lty = rep("blank", 2), fill = c("green", "orange"), 
                   alpha = c(0.7,0.9), euler.d = TRUE, sep.dist = 0.01, cat.dist = c(0.05,0.05), 
                   rotation.degree = 27, cat.pos =c(0,0), cex = 3, cat.cex = 3)

grid.newpage()
draw.pairwise.venn(area1 = 3115, area2 = 1234, cross.area = 0, category = c("Maori", 
                   "Pacific"), lty = rep("blank", 2), fill = c("green","purple"), 
                   alpha = c(0.7,0.5), cat.pos = c(0, 0), euler.d = TRUE, sep.dist = 0.03, 
                   rotation.degree = 0,  cex = 3, cat.cex = 3)

grid.newpage()
draw.pairwise.venn(area1 = 3115, area2 = 1234, cross.area = 50, category = c("Maori", 
                            "Pacific"), lty = rep("blank", 2), fill = c("green","purple"), 
                   alpha = c(0.7,0.5), cat.pos = c(0, 0), euler.d = TRUE, sep.dist = 0.03, 
                   rotation.degree = 0,  cex = 3, cat.cex = 3)
######################

grid.newpage()
draw.triple.venn(area1=3115,area2=1234,area3=24806,n12=(24806+1234+3115-29105),n23=0,n13=0,n123=0,
                 category = c("Maori", "Pacific", "Non-Pacific-Non-Maori"), lty = "blank", alpha = c(0.7,0.5,0.9),
                   fill = c("green", "purple", "orange"), rotation.degree = -72, cat.pos = c(0,180,0), 
                 cat.dist = c(0.05,0.02,0.03), cex = c(3,3,3,3,3,3,3), cat.cex = 3)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

grid.newpage()
draw.triple.venn(area1=3115,area2=1234,area3=24806,n12=0,n23=0,n13=0,n123=0,
                 category = c("Maori", "Pacific", "Non-Pacific-Non-Maori"), lty = "blank", alpha = c(0.7,0.5,0.9),
                 fill = c("green", "purple", "orange"), rotation.degree = -72, cat.pos = c(0,180,0), 
                 cat.dist = c(0.05,0.02,0.03), cex = c(3,3,3,3,3,3,3), cat.cex = 3)

grid.newpage()
draw.single.venn(area = 24806, category = "Non-Pacific-Non-Maori", fill = "orange", alpha = 0.9,
                 cex = 3, cat.cex = 3, cat.dist = 0.03, lty = "blank")

grid.newpage()
draw.single.venn(area = 3115, category = "Maori", fill = "green", alpha = 0.7,
                 cex = 3, cat.cex = 3, cat.dist = 0.03, lty = "blank")

grid.newpage()
draw.single.venn(area = 1234, category = "Pacific", fill = "purple", alpha = 0.5,
                 cex = 3, cat.cex = 3, cat.dist = 0.03, lty = "blank")
