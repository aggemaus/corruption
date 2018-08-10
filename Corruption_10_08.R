#=====================================================================================================================
# Set-up
#=====================================================================================================================
# rm(list=ls())
# install.packages("foreign")           # Zugriff auf STATA (.dta) files mit read.dta Befehl
library(foreign)                          
# install.packages("ggplot2")           # vervielf?ltigt M?glichkeiten bei der Arbeit mit Plots und verbessert so Verst?ndlichkeit
library(ggplot2)
#install.packages("gridExtra")           # Kombinieren von Plots
library(gridExtra)
# install.packages("dplyr")             # A fast, consistent tool for working with data frame like objects, both in memory and out of memory
library(dplyr)
# install.packages("tidyr")             # entfernen der NA's aus einem Data Frame
library(tidyr)
# install.packages("devtools")          # Installation von Packages ?ber den GitHub Link
library(devtools)
# install.packages("scales")
library(scales)
# install_github("vqv/ggbiplot")          # ggbiplot visualisiert Ergebnisse der PCA-Analyse
library(ggbiplot)
# install.packages("corrplot")          # Korrelation der Vaqriablen untereinander grafisch ab zu bilden
library(corrplot)

setwd("~/Google Drive/UNI/aktuelles Semester/Datenanalyse")

# suppress warnings, nicht geladene Variablen haben keinen Einfluss auf Analyse
options(warn=-1) 
data = read.dta("qog_std_cs_15may13.dta")
# turn warnings back on
options(warn=0) 

#=====================================================================================================================
# Kategorisieren der Ausprägungen der einzelnen Variablen/ Hinzufügen dieser zu DF
#=====================================================================================================================

corr_df <- data.frame("country"    = data$cname,
                      "region"     = data$ht_region,
                      "gdp"        = data$unna_gdp,
                      "corruption" = data$hf_corrupt,
                      "democratic" = data$eiu_dpc,          
                      "educationF" = data$ihme_ayef,        
                      "educationM" = data$ihme_ayem,         
                      "primary"    = data$une_pet,           
                      "secondary"  = data$une_set,          
                      "tertiary"   = data$une_tet,
                      "ti_cpi"     = data$ti_cpi,
                      "ti_cpi_min" = data$ti_cpi_min,
                      "financerep" = data$idea_frpr,
                      "logcorrupt" = log(data$hf_corrupt)
                      )

# Kategoriesiere Var. democratic in 4 Kategorien
corr_df$democratcat   <- ifelse(corr_df$democratic <=2.5, 0,           # Very Low
                                corr_df$democratic)               
corr_df$democratcat   <- ifelse(corr_df$democratic >2.5 &              # Low
                                corr_df$democratic <= 5.0, 1,       
                                corr_df$democratcat)                                           
corr_df$democratcat   <- ifelse(corr_df$democratic >5 &                # High
                                corr_df$democratic <= 7.5, 2,         
                                corr_df$democratcat)
corr_df$democratcat   <- ifelse(corr_df$democratic >7.5 &              # Very High
                                corr_df$democratic <= 10, 3,        
                                corr_df$democratcat)
                              
# Kategoriesiere Var. corruption in 4 Kategorien
corr_df$corruptioncat <- ifelse(corr_df$corruption <= 25,  "Very High",
                                corr_df$corruption)
corr_df$corruptioncat <- ifelse(corr_df$corruption >=25 & 
                                corr_df$corruption <=50,   "High",     
                                corr_df$corruptioncat)
corr_df$corruptioncat <- ifelse(corr_df$corruption > 50 & 
                                corr_df$corruption <= 75,  "Low",     
                                corr_df$corruptioncat)
corr_df$corruptioncat <- ifelse(corr_df$corruption >75,    "Very Low",    
                                corr_df$corruptioncat)


# Kategoriesiere Var. primary in 5 Kategorien
# (0-25)--> 1    (25-50)--> 2  (50-75)--> 3  (75-100)--> 4   (100-150) --> 5

corr_df$primcat       <- ifelse(corr_df$primary <=25, 0, 
                                corr_df$primary)         
corr_df$primcat       <- ifelse(corr_df$primary >25 & 
                                corr_df$primary <= 50, 1,     
                                corr_df$primcat)
corr_df$primcat       <- ifelse(corr_df$primary >50 & 
                                corr_df$primary <= 75, 2,     
                                corr_df$primcat)
corr_df$primcat       <- ifelse(corr_df$primary >75 & 
                                corr_df$primary <= 100, 3,    
                                corr_df$primcat)
corr_df$primcat       <- ifelse(corr_df$primary >100 & 
                                corr_df$primary <= 150, 4,    
                                corr_df$primcat)
corr_df$primcat       <- ifelse(corr_df$primary >150, 5, 
                                corr_df$primcat)

# Kategoriesiere Var. secondary in 5 Kategorien
# (0-25)--> 1    (25-50)--> 2  (50-75)--> 3  (75-100)--> 4   (100-150) --> 5

corr_df$seccat        <- ifelse(corr_df$secondary <=25, 0, 
                                corr_df$secondary)         
corr_df$seccat        <- ifelse(corr_df$secondary >25 & 
                                corr_df$secondary <= 50, 1,     
                                corr_df$seccat)
corr_df$seccat        <- ifelse(corr_df$secondary >50 & 
                                corr_df$secondary <= 75, 2,     
                                corr_df$seccat)
corr_df$seccat        <- ifelse(corr_df$secondary >75 & 
                                corr_df$secondary <= 100, 3,    
                                corr_df$seccat)
corr_df$seccat        <- ifelse(corr_df$secondary >100 & 
                                corr_df$secondary <= 150, 4,    
                                corr_df$seccat)
corr_df$seccat        <- ifelse(corr_df$secondary >150, 5, 
                                corr_df$seccat)


# Extrahiere die Identifikationsziffer aus der Spalte Reion
corr_df$regionumber   <- ifelse(corr_df$region =="10", 10, 
                                corr_df$region)

# NA's löschen
corr_df <- corr_df %>% drop_na(corruption, 
                               gdp,
                               democratic,
                               primary,
                               secondary,
                               tertiary 
                              )

#=====================================================================================================================
# Plots erstellen um Eindrücke zu bekommen wie sich Daten verhalten
#=====================================================================================================================

# Histogramm von Anazhl der Länder die bestimmtes Leven al Korruption aufweisen
pHistCC     <- ggplot(corr_df, aes(corr_df$corruption)) + 
                      geom_histogram(color ="darkgrey", 
                                     fill = "white", 
                                     binwidth = 5) +
                      labs(x = "Corruption Index",
                           y = "Number of Countries", 
                           title = "Histogram of Corruption") + 
                      geom_vline(aes(xintercept = mean(corr_df$corruption), 
                                 color = "mean"), 
                                 show.legend = TRUE,
                                 size = 1) +
                      geom_vline(aes(xintercept = median(corr_df$corruption),
                                 color = "median"),
                                 show.legend = TRUE,
                                 size = 1) +
                      scale_color_manual(name ="Statistics",
                                         values = c(mean = "lightskyblue", 
                                                    median = "orange")
                                         )


# Histogramm der logarithmierten Korruptionswerte
pHistCCLog  <- ggplot(corr_df, aes(corr_df$logcorrupt)) + 
                      geom_histogram(aes(y = ..density..),
                                     color ="darkgrey", 
                                     fill = "white", 
                                     binwidth = 0.1) +
                      labs(x = "Log - Corruption Index", 
                          y = "Density", 
                          title = "Histogram of Log-Corruption") + 
                      geom_density() + 
                      geom_vline(aes(xintercept = mean(corr_df$logcorrupt),
                                 color = "mean"),
                                 show.legend = TRUE,
                                 size = 1) +
                      geom_vline(aes(xintercept = median(corr_df$logcorrupt),
                                 color = "median"),
                                 show.legend = TRUE,
                                 size = 1) +
                      scale_color_manual(name ="Statistics",
                                         values = c(mean = "lightskyblue", 
                                                    median = "orange")
                                         )

pHistCCLog


# Kombination der beiden Histogramme                                                     )
png("~/Google Drive/UNI/aktuelles Semester/Datenanalyse/Plots/CombinationHistCC&HistCCLog.png")
grid.arrange(pHistCC, pHistCCLog, nrow = 1)
dev.off()
# Boxplot Corruption/Democratic Category
pBoxCD      <- ggplot(corr_df, aes(as.factor(democratcat), corruption)) + 
                      geom_boxplot(color = "darkgrey", 
                                   fill = "white") +
                      labs(x="Democratic Category", 
                           y="Level of Corruption", 
                           title = "Zusammenhang Corruption \n & Democratic") +
                      coord_flip() +
                      scale_x_discrete(labels=c("Very Low",
                                                "Low",
                                                "High",
                                                "Very High")
                                       )

# Violinplot Corruption/Democraticcat (zeigt mit Form die Verteilung an) 
pViolinCD   <- ggplot(corr_df, aes(as.factor(democratcat), corruption)) + 
                      geom_violin(color = "darkgrey", 
                                  fill = "white") +
                      labs(x="Democratic Category", 
                           y="Level of Corruption", 
                           title = "Coherence Corruption \n & Democratic") +
                      coord_flip() +
                      scale_x_discrete(labels=c("Very Low",
                                                "Low",
                                                "High",
                                                "Very High")
                                      )
# Kombination der beiden Histogramme    
png("~/Google Drive/UNI/aktuelles Semester/Datenanalyse/Plots/CombinationpBoxCD&pViolinCD.png")
grid.arrange(pBoxCD, pViolinCD, nrow = 1)
dev.off()
#Dotplot Korruption in Verbindung mit den Regionen
pDotCR      <- ggplot(corr_df, aes(regionumber, corruption)) + 
                      geom_point(aes(color= region)) + coord_flip()
png("~/Google Drive/UNI/aktuelles Semester/Datenanalyse/Plots/Dotplot_Corruption&Regionumber.png")
dev.off()

# Scatterplot Corruption gdp
# ggplot(corr_df, aes(x=log(gdp), y=corruption, color=corruptioncat)) + geom_point()


# qplot(gdp, corruption, data = corr_df, geom = c("point", "smooth"))



#qPlot Corruption/Democratic Category
#qplot(democratcat, corruption, data=corr_df, geom = c("point","smooth"))

# #qPlot Seccat and Corruption
# qplot(seccat, corruption, data=corr_df, geom = c("point","smooth"))
# 
# #qPlot Facetts Democratcat and Corruption
# qplot(as.factor(democratcat), corruption,data=corr_df, facets = .~democratcat)


# qplot(regionumber, corruption,data=corr_df, facets = .~regionumber) +  geom_point(aes(color= region))

#=====================================================================================================================
# Korrelation der einzelnen Variablen untereinander betrachten
#=====================================================================================================================

#Subset the corr_df Frame for the correlation matrix

corr_subset <- data.frame((corr_df[,c(4,3,5,8:10)])) 

# Lösche NA's mit Hilfe des oben geladenen tidyr Packages
corr_subset <- corr_subset %>%         
               drop_na(corruption, 
                       gdp,
                       democratic,
                       primary,
                       secondary,
                       tertiary 
                       )

# Korrelationsmatrix erstellen
pCorrelation  <- corrplot(cor(corr_subset[,c(1:6)]), 
                          order="AOE",
                          method="color", 
                          addCoef.col = "gray")


# Normalverteilung prüfen
qqnorm(corr_df$logcorrupt)
test_norm <- ggplot(corr_df, aes(y=corr_df$logcorrupt)) + geom_boxplot()
test_norm
shapiro.test(corr_df$logcorrupt) # nicht normalverteilt

shapiro.test(corr_df$corruption)
t.test(corr_df$logcorrupt)

#=====================================================================================================================
# Prinzipalkomponentenanalyse - PCA
#=====================================================================================================================

corr_subset$corruptioncat <- ifelse(corr_subset$corruption <= 25,  "Very High",
                                    corr_subset$corruption)
corr_subset$corruptioncat <- ifelse(corr_subset$corruption >=25 & 
                                    corr_subset$corruption <=50,   "High",     
                                    corr_subset$corruptioncat)
corr_subset$corruptioncat <- ifelse(corr_subset$corruption > 50 & 
                                    corr_subset$corruption <= 75,  "Low",     
                                    corr_subset$corruptioncat)
corr_subset$corruptioncat <- ifelse(corr_subset$corruption >75,    "Very Low",    
                                    corr_subset$corruptioncat)

# my.pca$corruptioncat <- NULL


log.corr_subset <- log(corr_subset[,c(2:6)])
corr.pca        <- prcomp(log.corr_subset, 
                          center = TRUE, 
                          scale. = TRUE)

print   (corr.pca)
plot    (corr.pca, type = "l")
summary (corr.pca)
predict (corr.pca, 
         newdata=tail(log.corr_subset, 2)
         )


corr.pca <- ggbiplot(corr.pca, 
                     obs.scale = 1, 
                     var.scale = 1, 
                     groups = corr_subset$corruptioncat, 
                     ellipse = TRUE, 
                     circle = TRUE)
corr.pca <- corr.pca + 
            scale_color_discrete(name = '')
corr.pca <- corr.pca + 
            theme(legend.direction = 'vertical', 
                  legend.position = 'right'
                  )
print(corr.pca)

