##Population pyramid trialing
##FUCK GGPLOT

library(ggplot2)
library(RColorBrewer)
library(plyr)
#setwd("H:/appui")
######################################
#####need for age standardisation#####
######################################
allcause <- read.csv("data/numerator/mort0dep0637nonmaorinat.csv",header=T)
allcause <- allcause[which(allcause$Year==2013),]
acnames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
              "45-49","50-54","55-59","60-64","65-69","70-74","75-79")
acmat <- matrix(NA,nc=2,nr=16)
colnames(acmat) <- c("Age","Population")
acdf <- as.data.frame(acmat)
acdf$Population <- as.numeric(t(allcause[3,3:18])) / sum(as.numeric(t(allcause[3,3:19]))) * 100
acdf$Age <- 1:16
acdf$Ethnicity <- "Non-Maori"
ac <- ggplot(acdf, aes(x=Age,y=Population, fill = Ethnicity, colour = Ethnicity)) +
  #geom_line(size = 1) + 
  geom_hline(yintercept = 12.5, col = "white") +
  geom_bar(stat = "identity", colour = "black") +
  #scale_y_continuous(breaks = seq(0, 3000, 250), 
  #                   labels = as.character(seq(0, 3000, 250))) + 
  scale_x_continuous(breaks = seq(1, 16, 1), 
                    labels = acnames) + 
  scale_y_continuous(breaks = seq(0, 100, 2.5), 
                     labels = paste(seq(0, 100, 2.5), c(".0%", "%"), sep ="")) +
  
  #ylim(0, 25) +
  labs(y="Percentage") +
  labs(x="Age (5-year bands)") +
  labs(title="Non-Maori all cause mortality 2013") +
  scale_fill_manual(values = brewer.pal(8, "Paired")[8]) + 
  theme_bw() +
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ac

allcause <- read.csv("data/numerator/mort0dep0637maorinat.csv",header=T)
allcause <- allcause[which(allcause$Year==2013),]
acnames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
             "45-49","50-54","55-59","60-64","65-69","70-74","75-79")
acmat <- matrix(NA,nc=2,nr=16)
colnames(acmat) <- c("Age","Population")
acdf <- as.data.frame(acmat)
acdf$Population <- as.numeric(t(allcause[3,3:18])) / sum(as.numeric(t(allcause[3,3:19]))) * 100
acdf$Age <- 1:16
acdf$Ethnicity <- "Maori"
ac <- ggplot(acdf, aes(x=Age,y=Population, fill = Ethnicity, colour = Ethnicity)) +
  #geom_line(size = 1) + 
  geom_hline(yintercept = 12.5, col = "white") +
  
  geom_bar(stat = "identity", colour = "black") +
  #scale_y_continuous(breaks = seq(0, 3000, 250), 
  #                   labels = as.character(seq(0, 3000, 250))) + 
  scale_x_continuous(breaks = seq(1, 16, 1), 
                     labels = acnames) + 
  scale_y_continuous(breaks = seq(0, 100, 2.5), 
                     labels = paste(seq(0, 100, 2.5), c(".0%", "%"), sep ="")) +
  labs(y="Percentage") +
  labs(x="Age (5-year bands)") +
  labs(title="Maori all cause mortality 2013") +
  scale_fill_manual(values = brewer.pal(8, "Paired")[4]) + 
  theme_bw() +
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ac

#####################################################################################
#####################################################################################

allcause <- read.csv("H:/dessert/rr gender.csv",header=T)
acnames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
             "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
acmat <- matrix(NA,nc=5,nr=17)
colnames(acmat) <- c("Age","rr","low","upp","Gender")
acdf <- as.data.frame(acmat)
acdf$rr <- allcause[,1]
acdf$low <- allcause[,2]
acdf$upp <- allcause[,3]
acdf$Age <- 1:17
acdf$Gender <- rep("Male", 17)
ac <- ggplot(acdf, aes(x = Age, y = rr, col = Gender)) +
  geom_errorbar(data = subset(acdf, Gender == "Male"), stat = "identity", aes(y = rr, ymin = low, ymax = upp), size = 1) +
  geom_point(data = subset(acdf, Gender == "Male"), stat = "identity", aes(y = rr, ymin = low, ymax = upp), size = 3) +
#   geom_line(aes(y = rr, col = "Male"), stat = "identity", size = 1) +
#   #geom_line(data = subset(acdf, Gender == "Male"), stat = "identity", color = "blue", size = 1) +
#   geom_ribbon(aes(ymin = low, ymax = upp, fill = "Male"), alpha = 0.2, show_guide = FALSE, lty = 2) +
#   geom_ribbon(aes(ymin = 1, ymax = 1, fill = "Female"), alpha = 0.2, show_guide = FALSE, lty = 2) +
  geom_hline(aes(yintercept = 1, col = "Female"), size = 1, linetype = 2) + 
  scale_x_continuous(breaks = seq(1, 17, 1), 
                     labels = acnames) + 
  ylim(0,4) +
  labs(y="Relative Risk") +
  labs(x="Age (5-year bands)") +
  labs(title="All cause mortality by gender 2013") +
  scale_colour_manual(values = brewer.pal(8, "Paired")[c(2,6)]) + 
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ac
######################################ghs

gasinc <- read.csv("H:/dessert/gasinc.csv",header=T)
acnames <- paste(1991:2013,sep=' ')
acmat <- matrix(NA,nc=5,nr=46)
colnames(acmat) <- c("year","rate","ratelow","rateupp","pop")
acdf <- as.data.frame(acmat)
acdf$rate <- gasinc[,7]
acdf$ratelow <- gasinc[,8]
acdf$rateupp <- gasinc[,9]
acdf$year <- 1991:2013
acdf$Series <- as.factor(c(rep("Rolling average", 23), rep("Default rate", 23)))

ac <- ggplot(acdf, aes(x = year, y = rate, col = Series, fill = Series)) +
  geom_hline(aes(yintercept = 0), size = 1, color = "white") +
  geom_line(data = subset(acdf, Series == "Rolling average"), stat = "identity", size = 1) +
  geom_line(data = subset(acdf, Series == "Default rate"), stat = "identity", size = 0.5) +
  geom_ribbon(data = subset(acdf, Series == "Rolling average"),aes(ymin = ratelow, ymax = rateupp), alpha = 0.4, lty = 0) +
  geom_ribbon(data = subset(acdf, Series == "Default rate"),aes(ymin = ratelow, ymax = rateupp), alpha = 0.2, lty = 0) +
  scale_x_continuous(breaks = seq(1991, 2013, 1), 
                     labels = acnames) + 
  scale_y_continuous(breaks = seq(0,12, 1), 
                     labels = paste(0:12,sep=' ')) + 
  labs(y="Incidence rate per 100,000") +
  labs(x="Year") +
  labs(title="Total Gastric incidence with rolling average series overlaid 1991-2013") +
  
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ac

########

########



agestruct("Total",1996,data_popest91_total_nat)
agestruct("Total",2001,data_popest91_total_nat)
agestruct("Total",2006,data_popest91_total_nat)
agestruct("Total",2013,data_popest91_total_nat)

agestruct("Maori",1996,data_popest91_maori_nat)
agestruct("Maori",2001,data_popest91_maori_nat)
agestruct("Maori",2006,data_popest91_maori_nat)
agestruct("Maori",2013,data_popest91_maori_nat)

agestruct("Non-Maori",1996,data_popest91_nonmaori_nat)
agestruct("Non-Maori",2001,data_popest91_nonmaori_nat)
agestruct("Non-Maori",2006,data_popest91_nonmaori_nat)
agestruct("Non-Maori",2013,data_popest91_nonmaori_nat)

agestruct("Pacific",1996,data_popcen_pacific_nat)
agestruct("Pacific",2001,data_popcen_pacific_nat)
agestruct("Pacific",2006,data_popcen_pacific_nat)
agestruct("Pacific",2013,data_popcen_pacific_nat)

agestruct("Asian",1996,data_popcen_asian_nat)
agestruct("Asian",2001,data_popcen_asian_nat)
agestruct("Asian",2006,data_popcen_asian_nat)
agestruct("Asian",2013,data_popcen_asian_nat)

agestruct <- function(popu,chosen,d1){
  if(is.na(chosen)){
    
  }else{
    d1 <- d1[which(d1$Year==chosen):(which(d1$Year==chosen)+1),]
  }
  agenames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
  
  pymat <- matrix(NA,nc=3,nr=34)
  colnames(pymat) <- c("Age","Gender","Population")
  pydf <- as.data.frame(pymat)
  
  pydf$Population <- c(-t(d1[1,3:19])/d1[1,20],t(d1[2,3:19])/d1[2,20])
  levels(pydf$Age) <- agenames
  pydf$Age <- rep(agenames,2)
  levels(pydf$Gender) <- c("Male","Female")
  pydf$Gender <- rep(c("Male","Female"),each=17)
  
  gg <- ggplot(pydf, aes(x = Age, y = Population, fill = Gender)) + 
    geom_bar(data = subset(pydf,Gender == "Female"), stat = "identity") + 
    geom_bar(data = subset(pydf,Gender == "Male"), stat = "identity") + 
    
    
    ############make scale work better!!!
    scale_y_continuous(breaks = seq(-0.14, 0.14, .02), 
                       labels = (as.character(c(seq(0.14, 0, -.02), seq(.02, 0.14, .02))))) + 
    
    
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    labs(y="Proportion") +
    labs(title=paste("Age Structure of",popu,"population in",chosen,sep=' '))+
    theme_bw()
  gg
}


##actually useful code

refstruct("WHO")
refstruct("Segi")

refstruct <- function(ref){
  data_standard <- read.csv("data/standard/refpopext.csv",header=T)
  
  agenames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
  
  pymat <- matrix(NA,nc=3,nr=34)
  colnames(pymat) <- c("Age","Gender","Population")
  pydf <- as.data.frame(pymat)
  
  pydf$Population <- c(-t(data_standard[which(data_standard==ref),2:18]),t(data_standard[which(data_standard==ref),2:18]))
  levels(pydf$Age) <- agenames
  pydf$Age <- rep(agenames,2)
  levels(pydf$Gender) <- c("Male","Female")
  pydf$Gender <- rep(c("Male","Female"),each=17)
  
  gg <- ggplot(pydf, aes(x = Age, y = Population, fill = Gender)) + 
    geom_bar(data = subset(pydf,Gender == "Female"), stat = "identity") +
    geom_bar(data = subset(pydf,Gender == "Male"), stat = "identity") +
    
    
    ############make scale work better!!!
    scale_y_continuous(breaks = seq(-0.14, 0.14, .02), 
                       labels = (as.character(c(seq(0.14, 0, -.02), seq(.02, 0.14, .02))))) + 
    
    
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    labs(y="Proportion") +
    labs(title=paste("Age Structure of",ref,"population",sep=' '))+
    theme_bw()
    
  
    #+guides(fill=F)
    
  gg
}



#########################
#########################
#########################

data_popest91_total_nat <- read.csv("data/denominator/popest91totalnat.csv",header=T)
data_popest91_maori_nat <- read.csv("data/denominator/popest91maorinat.csv",header=T)
data_popest91_nonmaori_nat <- read.csv("data/denominator/popest91nonmaorinat.csv",header=T,sep=';')
data_popcen_pacific_nat <- read.csv("data/denominator/popcenpacificnat.csv",header=T,sep=";")
data_popcen_asian_nat <- read.csv("data/denominator/popcenasiannat.csv",header=T,sep=";")

struct <- function(popu,chosen,d1,ref){
  data_standard <- read.csv("data/standard/refpopext.csv",header=T)
  num = if(popu == "Maori") 4 else 8
  if(is.na(chosen)){
    
  }else{
    d1 <- d1[which(d1$Year==chosen):(which(d1$Year==chosen)+1),]
  }
  agenames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
  
  pymat <- matrix(NA,nc=3,nr=68)
  colnames(pymat) <- c("Age","Gender","Population")
  pydf <- as.data.frame(pymat)
  
  pydf$Population <- c(-t(d1[1,3:19])/d1[1,20],t(d1[2,3:19])/d1[2,20],-t(data_standard[which(data_standard==ref),2:18]),t(data_standard[which(data_standard==ref),2:18]))
  levels(pydf$Age) <- agenames
  pydf$Age <- rep(agenames,4)
  levels(pydf$Gender) <- c("Male","Female",paste("Male",ref,sep=' '),paste("Female",ref,sep=' '))
  pydf$Gender <- rep(c("Male","Female",paste("Male",ref,sep=' '),paste("Female",ref,sep=' ')),each=17)
  
  gg <- ggplot(pydf, aes(x = Age, y = Population, fill = Gender)) + 

    geom_bar(data = subset(pydf,Gender == paste("Male",ref,sep=' ') | Gender == "Male"), position = "dodge",stat = "identity") +
    geom_bar(data = subset(pydf,Gender == paste("Female",ref,sep=' ') | Gender == "Female"), position = "dodge",stat = "identity") +
    
    scale_y_continuous(breaks = seq(-0.10,0.10,by=0.05), labels = outer(c(seq(from = 0.10,0.05,by=-0.05),seq(from = 0,0.10,by=0.05))*100,"%",paste,sep="")[,1], limits=c(-0.13,0.13)) +
    
    coord_flip() + 
    #scale_fill_brewer(palette = "Paired",type="seq") + 
    scale_fill_manual(values = brewer.pal(8, "Paired")[(num-3):num]) + 
    labs(y="Proportion") +
    labs(title=paste("Age Structure of",popu,"population in",chosen,"with",ref,"comparison",sep=' '))+
    theme_bw()
  #+theme(panel.background = element_rect(fill = "black"))
    
  gg  
}

struct("Maori",2013,data_popest91_maori_nat,"WHO")
struct("Maori",2013,data_popest91_maori_nat,"Segi")
struct("Non-Maori",2013,data_popest91_nonmaori_nat,"WHO")
struct("Non-Maori",2013,data_popest91_nonmaori_nat,"Segi")







library(grid)
library(gridExtra)
gd = read.csv("gamma data.csv", header = TRUE)[,c(3,8:12)]
gd$Series = rep(c("Non-Maori Female", "Maori Female"), each = 18)
acnames <- paste(1996:2013,sep=' ')

gd2 = gd
gd3 = gd[, c(1, 3, 2, 5, 6, 7)]
gd3 = gd[, c(1, 3, 2, 4, 5, 6, 7)]
gd4 = gd[, c(1, 4, 2, 3, 5, 6, 7)]
gd3$Series = "Default interval"
gd4$Series = "Default interval 2"
gd = as.data.frame(rbind(as.matrix(gd2), as.matrix(gd3), as.matrix(gd4)))
gd$year = as.integer(gd$year)
gd$rate = as.numeric(gd$rate)
gd$ratelow = as.numeric(gd$ratelow)
gd$rateupp = as.numeric(gd$rateupp)
gd$rategammalow = as.numeric(gd$rategammalow)
gd$rategammaupp = as.numeric(gd$rategammaup)
gd$Series = as.character(gd$rategammaup)

ac <- ggplot(gd, aes(x = year, y = rate, col = Series, fill = Series)) +
  geom_ribbon(data = subset(gd, Series == "Non-Maori Female"),aes(ymin = rategammalow, ymax = rategammaupp), alpha = 0.6, lty = 1, col = "black") +
  
  geom_line(data = subset(gd, Series == "Non-Maori Female"), stat = "identity", size = 1) +
  geom_line(data = subset(gd, Series == "Non-Maori Female"), aes(y = rateupp, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  geom_line(data = subset(gd, Series == "Non-Maori Female"), aes(y = ratelow, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  scale_x_continuous(breaks = seq(1996, 2013, 1), 
                     labels = acnames) +   
  scale_y_continuous(breaks = seq(0, 50, by =  10), labels = (seq(0, 50, by =  10)), limits = c(0,55)) +
  labs(y="Incidence rate per 100,000") +
  labs(x="Year") +
  labs(title="Non-Maori female ages 5-9 mortality with gamma intervals 1996-2013") +
  scale_color_manual(name = "", values = brewer.pal(8, "Paired")[c(6, 8)]) + 
  scale_fill_manual(name = "",values = rep("yellow", 3), labels = "Gamma interval") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
ac

bc <- ggplot(gd, aes(x = year, y = rate, col = Series, fill = Series)) +
  geom_ribbon(data = subset(gd, Series == "Maori Female"),aes(ymin = rategammalow, ymax = rategammaupp), alpha = 0.6, lty = 1, col = "black") +
  
  geom_line(data = subset(gd, Series == "Maori Female"), stat = "identity", size = 1) +
  geom_line(data = subset(gd, Series == "Maori Female"),aes(y = ratelow, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  geom_line(data = subset(gd, Series == "Maori Female"),aes(y = rateupp, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  scale_x_continuous(breaks = seq(1996, 2013, 1), 
                     labels = acnames) +
  scale_y_continuous(breaks = seq(0, 50, by =  10), labels = (seq(0, 50, by =  10)), limits = c(0,55)) +
  labs(y="Incidence rate per 100,000") +
  labs(x="Year") +
  labs(title="Maori female ages 5-9 mortality with gamma intervals 1996-2013") +
  scale_color_manual(name = "",values = brewer.pal(8, "Paired")[c(2, 4)], labels = c("Default interval","Maori Female        ")) + 
  scale_fill_manual(name = "",values = "yellow", labels = "Gamma interval") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
bc

grid.arrange(ac,bc)


gc = read.csv("gamma data 2.csv", header = TRUE)[,c(3,5:12)]
gc$var = ((gc$rateupp - gc$rate) / 1.96) ^ 2
gc$sam1 = gc$rate + rnorm(1, 0, sqrt(gc$var))



simulate = function(size, gc){
  set.seed(12)
  gc$var = ((gc$rateupp - gc$rate) / 1.96) ^ 2
  sam = rep(gc$rate, each = size) + rnorm(36 * size, 0, rep(sqrt(gc$var), each = size))
  dim(sam) = c(size, 36)
  sim = t(sam)
  cat("Simulations of size:", size, "\n\n")
  cat("\tNon-Maori simulation\n", sum(sim[1:18,] > rep(gc$rateupp[1:18], size) | sim[1:18,] < rep(gc$ratelow[1:18], size)) / 18 / size,
  sum(sim[1:18,] > rep(gc$rategammaupp[1:18], size) | sim[1:18,] < rep(gc$rategammalow[1:18], size)) / 18 / size)
  cat("\n")
  cat("\tMaori simulation\n", sum(sim[19:36,] > rep(gc$rateupp[19:36], size) | sim[19:36,] < rep(gc$ratelow[19:36], size)) / 18 / size,
  sum(sim[19:36,] > rep(gc$rategammaupp[19:36], size) | sim[19:36,] < rep(gc$rategammalow[19:36], size)) / 18 / size)
}

#gastric incidence
gc = read.csv("H:/gamma data 2.csv", header = TRUE)[,c(3,5:12)]
cat("Created by adding normal noise to the rates using age-standardised variances,
It then calculates the proportion of times that the value falls outside the limits,
Left column is default intervals, right column is gamma-based intervals")
simulate(100, gc)
simulate(1000, gc)
simulate(10000, gc)

sum(gc$rateupp - gc$ratelow)
sum(gc$rategammaupp - gc$rategammalow)
sum((gc$rateupp - gc$ratelow) - (gc$rategammaupp - gc$rategammalow))


gc$Series = rep(c("Non-Maori Male", "Maori Male"), each = 18)
acnames <- paste(1996:2013,sep=' ')
gd = gc
ac <- ggplot(gd, aes(x = year, y = rate, col = Series, fill = Series)) +
  geom_ribbon(data = subset(gd, Series == "Non-Maori Male"),aes(ymin = rategammalow, ymax = rategammaupp), alpha = 0.6, lty = 1, col = "black") +
  
  geom_line(data = subset(gd, Series == "Non-Maori Male"), stat = "identity", size = 1) +
  geom_line(data = subset(gd, Series == "Non-Maori Male"),aes(y = ratelow, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  geom_line(data = subset(gd, Series == "Non-Maori Male"),aes(y = rateupp, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  scale_x_continuous(breaks = seq(1996, 2013, 1), 
                     labels = acnames) +   
  scale_y_continuous(breaks = seq(0, 30, by =  5), labels = (seq(0, 30, by =  5)), limits = c(0,30)) +
  labs(y="Incidence rate per 100,000") +
  labs(x="Year") +
  labs(title="Non-Maori Male gastric incidence with gamma intervals 1996-2013") +
  scale_color_manual(name = "",values = brewer.pal(8, "Paired")[c(6, 8)]) + 
  scale_fill_manual(name = "",values = "yellow", labels = "Gamma interval") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
ac

bc <- ggplot(gd, aes(x = year, y = rate, col = Series, fill = Series)) +
  geom_ribbon(data = subset(gd, Series == "Maori Male"),aes(ymin = rategammalow, ymax = rategammaupp), alpha = 0.6, lty = 1, col = "black") +
  
  geom_line(data = subset(gd, Series == "Maori Male"), stat = "identity", size = 1) +
  geom_line(data = subset(gd, Series == "Maori Male"),aes(y = ratelow, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  geom_line(data = subset(gd, Series == "Maori Male"),aes(y = rateupp, col = "Default interval"), alpha = 1, lty = 2, size = 1) +
  scale_x_continuous(breaks = seq(1996, 2013, 1), 
                     labels = acnames) +
  scale_y_continuous(breaks = seq(0, 30, by =  5), labels = (seq(0, 30, by =  5)), limits = c(0,30)) +
  labs(y="Incidence rate per 100,000") +
  labs(x="Year") +
  labs(title="Maori Male gastric incidence with gamma intervals 1996-2013") +
  scale_color_manual(name = "",values = brewer.pal(8, "Paired")[c(2, 4)], labels = c("Default interval", "Maori Male")) + 
  scale_fill_manual(name = "",values = "yellow", labels = "Gamma interval") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
bc

grid.arrange(ac,bc)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# struct <- function(chosen, left, right, ref){
#   data_standard <- read.csv("data/standard/refpopext.csv",header=T)
#   if(is.na(chosen)){
#     
#   }else{
#     left <- left[which(left$Year==chosen):(which(left$Year==chosen)+1),]
#     right <- right[which(right$Year==chosen):(which(right$Year==chosen)+1),]
#   }
#   agenames <- c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
#                 "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
#   
#   pymat <- matrix(NA,nc=3,nr=68)
#   colnames(pymat) <- c("Age","Ethnicity","Population")
#   pydf <- as.data.frame(pymat)
#   
#   pydf$Population[1:34] <- c(-t(left[1,3:19])/left[1,20],t(right[1,3:19])/right[1,20])
#   pydf$Population[35:68] <- c(-t(data_standard[which(data_standard==ref),2:18]),t(data_standard[which(data_standard==ref),2:18]))
#   
#   levels(pydf$Age) <- agenames
#   pydf$Age <- rep(agenames,4)
#   levels(pydf$Ethnicity) <- c("Maori","Non-Maori",paste("Maori",ref,sep=' '),paste("Non-Maori",ref,sep=' '))
#   pydf$Ethnicity <- rep(c("Maori","Non-Maori",paste("Maori",ref,sep=' '),paste("Non-Maori",ref,sep=' ')),each=17)
#   
#   gg <- ggplot(pydf, aes(x = Age, y = Population, fill = Ethnicity)) + 
# 
#     geom_bar(data = subset(pydf,Ethnicity == paste("Maori",ref,sep=' ') | Ethnicity == "Maori"),position = "dodge",stat = "identity") +
#     geom_bar(data = subset(pydf,Ethnicity == paste("Non-Maori",ref,sep=' ') | Ethnicity == "Non-Maori"),position = "dodge",stat = "identity") +
#     
#     coord_flip() + 
# 
#     scale_y_continuous(breaks = seq(-0.10,0.10,by=0.05), labels = outer(c(seq(from = 0.10,0.05,by=-0.05),seq(from = 0,0.10,by=0.05))*100,"%",paste,sep="")[,1], limits=c(-0.13,0.13)) +
#     scale_fill_brewer(palette = "Paired") + 
#     
#     labs(y="Proportion") +
#     labs(title=paste("Age Structure of Maori and Non-Maori Males in",chosen,"with",ref,"comparison",sep=' '))+
#     theme_bw()
#   
#   gg  
# }
# 
# struct(2013,data_popest91_maori_nat,data_popest91_nonmaori_nat,"WHO")
# struct(2013,data_popest91_maori_nat,data_popest91_nonmaori_nat,"Segi")
