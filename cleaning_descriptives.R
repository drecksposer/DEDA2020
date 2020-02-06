data <- read.csv("full_data.csv")

#correlation plots
library(corrplot)
library(dplyr)
nums <- unlist(lapply(data, is.numeric))
data_num <- data[, nums]
c <- cor(data_num)
head(round(c, 2))
corrplot(c, method="circle")
binary <- c("balcony", "built_in_kitchen", "cellar", "courtage", "garden", "is_vacant",
            "lift", "private_seller")
data_num1 <- data_num[, ! names(data_num) %in% binary]

#some descriptive statistics
boxplot(data$price)     # need to remove outliers

#remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
price_wo <- remove_outliers(data_num1$price)
boxplot(price_wo)
hist(price_wo)

# x[!x %in% boxplot.stats(x)$out]
#----------------------------------------------------------------------------
data_num2 <- data_num1[!data_num1 %in% boxplot.stats(data_num1$price)$out]
boxplot.stats(data_num1$price)$out


#----------------------------------------------------------------------------
# PCA
pca <- prcomp(data_num1, center = TRUE,scale. = TRUE)
summary(pca)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca, labels = rownames(pca))
str(pca)
pca$x
#--------------------------------------------------------------------------
#which area is suitable for RD analyisis?
n <- table(data$location)
df <- data[data$location %in% names(n[n >100]), ]
library(plyr)
count(df$location)
#----------------------------------------------------------------------------
#select area Reinickendorf-Wedding - works
data.rw <- data[data$lat > 52.5570 & data$lat < 52.569 & 
               data$lng > 13.320 & data$lng < 13.370 , ]
data.rw <- data.rw[data.rw$price < 500000,]
count(data.rw$location)   #31 Reinickendorf, 13 Wedding
PriLoc <- data.rw[data.rw$price < 500000, c("price_per_sqm", "location")]
library(ggplot2)
ggplot(PriLoc, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5)
         #outlier aus Reinickendorf raushauen und schon sieht man dass der Wedding teurer ist
ggplot(PriLoc, aes(price_per_sqm, fill = location)) + geom_histogram(alpha = 0.5)
# Regression Discontinuity
data.rw$wedding <- ifelse(data.rw$location =="Wedding (Wedding)", 1,0) #creates dummy for Wedding

reg.1 <- lm(formula = price_per_sqm ~ wedding
            , data = data.rw)
reg.x2 <- lm(formula = price_per_sqm ~ wedding + wedding^2
            , data = data.rw)
summary(reg.1)
reg.2 <- lm(formula = price_per_sqm ~ wedding+ is_vacant+number_rooms + courtage +
              food_drink_total_activity + park_total_activity  +
              distance_medical + park_distance_high_value_parks + distance_transit
            , data = data.rw)
reg.3 <- lm(formula = price_per_sqm ~ wedding+ is_vacant+ courtage + 
              grocery_rating_score + built_in_kitchen +
              park_weighted_average_rating + park_total_activity +
              education_distance_university + education_distance_kita +
              cellar + education_distance_adult_education
            , data = data.rw)
summary(reg.2)
summary(reg.3)
plot(data.rw$wedding, data.rw$price_per_sqm)

#remove too big apartments, as price/sqm is non-linear in sqm

#select area Steglitz-Lichterfelde
data.sl <- data[data$lat > 52.442 & data$lat < 52.455 & 
                  data$lng > 13.312 & data$lng < 13.326 , ]
count(data.sl$location)
PriLoc.sl <- data.sl[, c("price_per_sqm", "location")]
ggplot(PriLoc.sl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) + scale_fill_grey() + theme_classic()
ggplot(PriLoc.sl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) +scale_fill_manual(values=wes_palette(n=2,name="Moonrise1")) + theme(panel.background = element_blank())
ggplot(PriLoc.sl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) + scale_color_brewer(palette="Dark3")
ggplot(PriLoc.sl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) + scale_fill_manual(values= c("#CCCC99", "#CCCC66"))
ggplot(PriLoc.sl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) + scale_fill_manual(values= c("#FFD954", "#E4B660")) + theme(panel.background = element_blank())
ggplot(PriLoc, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5) + scale_fill_manual(values= c("#85B8CD", "#1D6A96")) + theme(panel.background = element_blank())

install.packages("wesanderson")
library(wesanderson)



#Kreuzberg Schöneberg
data.ks <- data[data$lat > 52.491 & data$lat < 52.495 & 
                  data$lng > 13.364 & data$lng < 13.375 , ]
#--------------------------------------------------------------------------------------
#Prenzlberg Mitte - works
data.pm <- data[data$lat > 52.530 & data$lat < 52.540 & 
                  data$lng > 13.400 & data$lng < 13.411 , ]
data.pm <- data.pm[data.pm$price_per_sqm<10000 & data.pm$price_per_sqm > 4500,]
count(data.pm$location)
PriLoc.pm <- data.pm[, c("price_per_sqm", "location")]
ggplot(PriLoc.pm, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5)
data.pm$mitte <- ifelse(data.pm$location =="Mitte (Mitte)", 1,0) #creates dummy for Mitte

reg.pm <- lm(formula = price_per_sqm ~ mitte
            , data = data.pm)
summary(reg.pm)
#------------------------------------------------------------------------------------
#Pankow Weißensee Prenzlberg DONE
pwp <- data[data$location == c("Weißensee (Weißensee)", "Pankow (Pankow)", "Prenzlauer Berg (Prenzlauer Berg)"),]
m <- aggregate(pwp$price_per_sqm, by= list(pwp$location), FUN=mean)
pwp$location <- replace(pwp$location, pwp$location == "Weißensee (Weißensee)", "Pankow (Pankow)") 
pwp <- pwp[pwp$lat > 52.538 & pwp$lat < 52.559 & 
                  pwp$lng > 13.410 & pwp$lng < 13.456 , ]
PriceLoc.pwp <- pwp[,c("price_per_sqm", "location")]
ggplot(PriceLoc.pwp, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5)
pwp$pankow <- ifelse(pwp$location =="Pankow (Pankow)", 1,0) #creates dummy for Wedding
reg.pwp <- lm(formula = price_per_sqm ~ pankow
             , data = pwp)
summary(reg.pwp)


#kreuzberg Neukölln DONE
data.kn <- data[data$lat > 52.486 & data$lat < 52.496 & 
                    data$lng > 13.415 & data$lng < 13.441 , ]
count(data.kn$location)
PriLoc.kn <- data.kn[, c("price_per_sqm", "location")]
ggplot(PriLoc.kn, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5)
data.kn$neukölln <- ifelse(data.kn$location =="Neukölln (Neukölln)", 1,0) #creates dummy for Wedding
m.kn <- aggregate(data.kn$price_per_sqm, by= list(data.kn$location), FUN=mean)
m.kn
reg.kn <- lm(formula = price_per_sqm ~ neukölln
            , data = data.kn)
summary(reg.kn)

# FHain Lichtenberg
data.fl <- data[data$lat > 52.500 & data$lat < 52.513 & 
                  data$lng > 13.471 & data$lng < 13.484 , ]
data.rw <- data.rw[data.rw$price < 500000,]
count(data.fl$location)   #31 Reinickendorf, 13 Wedding
PriLoc.fl <- data.fl[,c("price_per_sqm", "location")]
ggplot(PriLoc.fl, aes(price_per_sqm, fill = location)) + geom_density(alpha = 0.5)

# 









#------------------------------------------------------------------------------------
# descriptives
pricesqm.means <-  aggregate(data$price_per_sqm, by = list(data$location), FUN = mean)
plot(pricesqm.means$x)
top.means <- head(pricesqm.means[order(pricesqm.means$x, decreasing = T),], 10)
top.means$Group.1 <- droplevels(top.means$Group.1)
plot(top.means$Group.1, sort(top.means$x))
df.tm <- as.data.frame(top.means)
df.tm$count <- c(1:10)
levels(df.tm$Group.1) <- c(1:10)
row.names(df.tm) <- c(1:10)
plot(row.names(df.tm), df.tm$x)
plot(x~count, df.tm, las=2, 
     xlab= "", ylab = "", main="", xaxt='n')
df.tm$Group.1 <- factor(df.tm$Group.1, labels=c(1:10))

df.tm$Group.1 <- as.character(df.tm$Group.1)
df.tm$Group.1 <- factor(df.tm$Group.1, levels = c(1:10))
factor(df$g, levels = letters[4:1])

namen <- levels(df.tm$Group.1)

