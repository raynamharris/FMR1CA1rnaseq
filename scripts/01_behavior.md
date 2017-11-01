``` r
## load libraries 
library(tidyr) ## for respahing data
library(plyr) ## for renmaing factors
library(dplyr) ## for filtering and selecting rows
library(reshape2) ## for melting dataframe
library(ggplot2)
library(cowplot)
library(pheatmap)
library(viridis)
library(tidyverse) # for loop anova

## load color settings
source("figureoptions.R")
```

``` r
## read intermediate data (raw data from video tracker program analyzed in matlab)
behavior <- read.csv("../data/fmr1.csv", header = T)

behavior$APA <- as.factor(behavior$APA)
behavior$APA2 <- as.factor(behavior$APA2)
levels(behavior$APA2)
```

    ## [1] "conflict"          "consistent"        "controlconflict"  
    ## [4] "controlconsistent"

``` r
## relevel then rename factors treatment
behavior$APA2 <- factor(behavior$APA2, levels = c("controlconsistent", "controlconflict", "consistent", "conflict"))
levels(behavior$APA2) <- c("yoked-consistent","yoked-conflict","consistent", "conflict")

#relevel APA
levels(behavior$APA) <- c("control","consistent","conflict")

#relevel genotype
levels(behavior$Genotype) <- c("WT","FMR1KO")

behavior$Time1stEntrLog <- log(behavior$Time1stEntr)  ## log transformation
behavior$conflict <- ifelse(grepl("conflict", behavior$APA2), "conflict", "consistent") # for splitting
levels(behavior$conflict) <- c("consistent","conflict")
behavior <- behavior[c(1,3,7,8,10,60,14:59)] # supset data
behavior <- subset(behavior, !is.na(behavior$NumEntrances)) # remove nas
```

``` r
behaviorsummaryNum <- dplyr::summarise(group_by(behavior, Genotype, APA2, conflict, TrainSessionComboNum), m = mean(NumEntrances), se = sd(NumEntrances)/sqrt(length(NumEntrances)), len = length(NumEntrances))
behaviorsummaryNum <- as.data.frame(behaviorsummaryNum)
levels(behaviorsummaryNum$Genotype) <- c("WT","FMR1KO")
levels(behaviorsummaryNum$APA2) <- c("yoked-consistent","yoked-conflict", "consistent", "conflict")
levels(behaviorsummaryNum$conflict) <- c("consistent","conflict")

behaviorsummaryNum$genoAPA <- as.factor(paste(behaviorsummaryNum$Genotype,behaviorsummaryNum$APA2,sep="_"))
```

``` r
behaviorsummaryboth <- behavior
behaviorsummaryboth$APA1 <- ifelse(grepl("control", behaviorsummaryboth$APA), "yoked", "trained")
behaviorsummaryboth <- dplyr::summarise(group_by(behaviorsummaryboth, Genotype, APA1, TrainSessionComboNum), m = mean(NumEntrances), se = sd(NumEntrances)/sqrt(length(NumEntrances)), len = length(NumEntrances))
behaviorsummaryboth <- as.data.frame(behaviorsummaryboth)
levels(behaviorsummaryboth$Genotype) <- c("WT","FMR1KO")
levels(behaviorsummaryboth$APA1) <- c("yoked","trained")
behaviorsummaryboth$genoAPA <- as.factor(paste(behaviorsummaryboth$Genotype,behaviorsummaryboth$APA1,sep="_"))
```

``` r
# Standard error of the mean
behaviorsummaryboth$behvaior <- "Number of Entrances"
habretest <- behaviorsummaryboth %>%
  filter(TrainSessionComboNum <= 5) %>%
  ggplot(aes(x=TrainSessionComboNum, y=m, 
                                colour=APA1,shape=Genotype)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.1) +
    geom_line(aes(linetype=Genotype)) +
    geom_point(aes(shape=Genotype)) +
    theme_cowplot(font_size = 8, line_size = 0.25) +
    scale_color_manual(values = colorvalAPA5) +
    scale_y_continuous(name="Number of Entrances",
                       limits = c(0,35)) +
    scale_x_continuous(name = "Training Session", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest")) +
    theme(legend.position="none") +
    scale_linetype_manual(values = c(1,2,1,2)) +
    scale_shape_manual(values=c(16, 1)) 
habretest
```

![](01_behavior_files/figure-markdown_github/splitfigure-1.png)

``` r
pdf(file="../figures/01_behavior/habretest.pdf", width=2.5, height=2.25)
plot(habretest)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

Plotting number of consistent
-----------------------------

``` r
numentrance1consistent<- behaviorsummaryNum %>%
  filter(conflict == "consistent") %>%
  filter(TrainSessionComboNum >= 5) %>%
  ggplot(aes(x= TrainSessionComboNum, y=m, shape=Genotype)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
  geom_line(aes(colour=APA2, linetype=Genotype)) +
  geom_point(size = 2.5, aes(colour=APA2, shape=Genotype)) +
  scale_color_manual(values = colorvalAPA5) +
    scale_y_continuous(name="Number of Entrances",
                       limits = c(0,35)) +
    scale_x_continuous(name = "Training Session", 
                       breaks = c(5, 6, 7, 8, 9),
                       labels=c("5" = "Retest", "6" = "T4",
                                "7" = "T5", "8" = "T6", "9"= "Reten.")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  #background_grid(major = "y", minor = "y") +
  theme( legend.position="none") +
  scale_shape_manual(values=c(16, 1))
numentrance1consistent
```

![](01_behavior_files/figure-markdown_github/numentrance-1.png)

``` r
pdf(file="../figures/01_behavior/numentrance1consistent2.pdf", width=3.25, height=2.25)
plot(numentrance1consistent)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
behaviorsummaryNum$head2 <- "Conflict Training"
numentrance1conflict<- behaviorsummaryNum %>%
  filter(conflict == "conflict") %>%
  filter(TrainSessionComboNum >= 5) %>%
  ggplot(aes(x= TrainSessionComboNum, y=m, shape=Genotype)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
  geom_line(aes(colour=APA2, linetype=Genotype)) +
  geom_point(size = 2.5, aes(colour=APA2, shape=Genotype)) +
  scale_color_manual(values = colorvalAPA4) +
    scale_y_continuous(name="Number of Entrances",
                       limits = c(0,35)) +
    scale_x_continuous(name = "Training Session", 
                       breaks = c(5, 6, 7, 8, 9),
                       labels=c("5" = "Retest", "6" = "C1",
                                "7" = "C2", "8" = "C3", "9"= "Reten.")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  #background_grid(major = "y", minor = "y") +
  theme( legend.position="none") +
  scale_shape_manual(values=c(16, 1)) 
numentrance1conflict
```

![](01_behavior_files/figure-markdown_github/numentrance-2.png)

``` r
pdf(file="../figures/01_behavior/numentrance1conflict2.pdf", width=3.25, height=2.25)
plot(numentrance1conflict)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# num entrances
numentr <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum), m = mean(NumEntrances), se = sd(NumEntrances)/sqrt(length(NumEntrances)), len = length(NumEntrances))

## speed
speedsummary <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum), m = mean(Speed1), se = sd(Speed1)/sqrt(length(Speed1)))

## max avoidance time
maxavoid <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum), m = mean(MaxTimeAvoid), se = sd(MaxTimeAvoid)/sqrt(length(MaxTimeAvoid)))

## create the column for faceting
numentr$measure <- "Number of Entrances"
speedsummary$measure <- "Speed"
maxavoid$measure <- "Max Avoidance Time"

# rbind
threeplots <- rbind(numentr,speedsummary,maxavoid)
behaviorwrap <- ggplot(threeplots, aes(x=, TrainSessionComboNum, y=m, color=APA2, shape=Genotype)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.1) +
    geom_point(size = 2) +
   geom_line(aes(colour=APA2, linetype=Genotype)) +
   scale_y_continuous(name= NULL) +
    scale_x_continuous(name="Training Session", 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels = c( "Hab.", "T1", "T2", "T3",
                                   "Retest", "T4", "T5", "T6", "Reten.")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  #background_grid(major = "y", minor = "y") +
  #scale_color_manual(values = colorvalAPA00)  +
  theme(legend.title=element_blank()) +
  #theme(legend.position="none") +
  facet_wrap(~measure, ncol=1, scales = "free_y")
behaviorwrap
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
numentrance1consistent<- speedsummary %>%
  filter(APA2 %in% c("yoked-conflict", "conflict")) %>%
  filter(TrainSessionComboNum >= 5) %>%
  ggplot(aes(x= TrainSessionComboNum, y=m, shape=Genotype)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
  geom_line(aes(colour=APA2, linetype=Genotype)) +
  geom_point(size = 2.5, aes(colour=APA2, shape=Genotype)) +
  #scale_color_manual(values = colorvalAPA5) +
    scale_y_continuous(name="Speed") +
    scale_x_continuous(name = "Training Session", 
                       breaks = c(5, 6, 7, 8, 9),
                       labels=c("5" = "Retest", "6" = "T4",
                                "7" = "T5", "8" = "T6", "9"= "Reten.")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  #background_grid(major = "y", minor = "y") +
  #theme( legend.position="none") +
  scale_shape_manual(values=c(16, 1))
numentrance1consistent
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-1-2.png)

Anovas ALL data
---------------

``` r
summary(aov(NumEntrances ~ APA2 * Genotype * TrainSession, data=behavior)) 
```

    ##                             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2                         3   5286  1761.9  71.621  < 2e-16 ***
    ## Genotype                     1     31    31.0   1.259    0.263    
    ## TrainSession                11  14435  1312.3  53.345  < 2e-16 ***
    ## APA2:Genotype                3     11     3.5   0.144    0.933    
    ## APA2:TrainSession           21   1758    83.7   3.402 1.36e-06 ***
    ## Genotype:TrainSession       11    161    14.6   0.595    0.833    
    ## APA2:Genotype:TrainSession  21    426    20.3   0.824    0.690    
    ## Residuals                  315   7749    24.6                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
hab <- behavior %>%
  filter(TrainSession == "Hab") 
T1 <- behavior %>%
  filter(TrainSession == "T1") 
T2 <- behavior %>%
  filter(TrainSession == "T2") 
T3 <- behavior %>%
  filter(TrainSession == "T3") 
Retest <- behavior %>%
  filter(TrainSession == "Retest") 
T4 <- behavior %>%
  filter(TrainSession %in% c("T4", "C1")) 
T5 <- behavior %>%
  filter(TrainSession %in% c("T5", "C2")) 
T6 <- behavior %>%
  filter(TrainSession %in% c("T6", "C3")) 
Retention <- behavior %>%
  filter(TrainSession == "Retention")

summary(aov(NumEntrances ~ Genotype * APA2, data=hab)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype       1    6.1    6.11   0.236  0.630
    ## APA2           3  130.1   43.36   1.674  0.190
    ## Genotype:APA2  3  127.6   42.55   1.643  0.197
    ## Residuals     35  906.6   25.90

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T1)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1    0.1    0.07   0.006 0.9401  
    ## APA2           3  149.6   49.88   4.000 0.0153 *
    ## Genotype:APA2  3   54.3   18.10   1.452 0.2449  
    ## Residuals     34  423.9   12.47                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T2)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1    0.0    0.02   0.001 0.975500    
    ## APA2           3  438.8  146.28   7.386 0.000559 ***
    ## Genotype:APA2  3   13.2    4.39   0.222 0.880602    
    ## Residuals     36  712.9   19.80                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T3))
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1  119.1   119.1   8.081  0.00751 ** 
    ## APA2           3 1011.7   337.2  22.882 2.74e-08 ***
    ## Genotype:APA2  3    8.2     2.7   0.184  0.90621    
    ## Residuals     34  501.1    14.7                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retest)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   97.5   97.55   3.046 0.091154 .  
    ## APA2           3  775.7  258.56   8.075 0.000432 ***
    ## Genotype:APA2  3  103.1   34.38   1.074 0.375079    
    ## Residuals     30  960.6   32.02                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T4)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1    6.3     6.3   0.231    0.634    
    ## APA2           3 1050.5   350.2  12.867 7.25e-06 ***
    ## Genotype:APA2  3   41.7    13.9   0.511    0.677    
    ## Residuals     36  979.7    27.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T5)) 
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)    
    ## Genotype       1  101.1  101.07   3.256   0.080 .  
    ## APA2           3  897.8  299.25   9.640 9.5e-05 ***
    ## Genotype:APA2  3   22.3    7.43   0.239   0.868    
    ## Residuals     34 1055.4   31.04                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=T6)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   54.7    54.7   2.987    0.093 .  
    ## APA2           3 1165.0   388.3  21.217 6.32e-08 ***
    ## Genotype:APA2  3   68.1    22.7   1.241    0.310    
    ## Residuals     34  622.3    18.3                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retention)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   33.3    33.3   0.883    0.353    
    ## APA2           3 1590.7   530.2  14.037 1.77e-06 ***
    ## Genotype:APA2  3   37.4    12.5   0.330    0.803    
    ## Residuals     42 1586.5    37.8                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# linear model
summary(lm(NumEntrances ~ Genotype * APA2 * TrainSession , data=behavior)) 
```

    ## 
    ## Call:
    ## lm(formula = NumEntrances ~ Genotype * APA2 * TrainSession, data = behavior)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.6667  -3.0000  -0.4444   2.5000  18.0000 
    ## 
    ## Coefficients: (24 not defined because of singularities)
    ##                                                         Estimate
    ## (Intercept)                                              23.5000
    ## GenotypeFMR1KO                                           -2.7500
    ## APA2yoked-conflict                                       -4.5000
    ## APA2consistent                                          -13.5000
    ## APA2conflict                                            -10.0556
    ## TrainSessionC2                                           -7.3333
    ## TrainSessionC3                                          -10.5556
    ## TrainSessionHab                                           5.7500
    ## TrainSessionRetention                                    -2.0000
    ## TrainSessionRetest                                       -8.2500
    ## TrainSessionT1                                          -10.0000
    ## TrainSessionT2                                          -10.7500
    ## TrainSessionT3                                           -9.0000
    ## TrainSessionT4                                           -9.7500
    ## TrainSessionT5                                           -6.5000
    ## TrainSessionT6                                           -7.2500
    ## GenotypeFMR1KO:APA2yoked-conflict                         2.7500
    ## GenotypeFMR1KO:APA2consistent                             5.0000
    ## GenotypeFMR1KO:APA2conflict                              -0.1944
    ## GenotypeFMR1KO:TrainSessionC2                             5.0833
    ## GenotypeFMR1KO:TrainSessionC3                             6.5556
    ## GenotypeFMR1KO:TrainSessionHab                            3.7500
    ## GenotypeFMR1KO:TrainSessionRetention                      3.9643
    ## GenotypeFMR1KO:TrainSessionRetest                         3.8333
    ## GenotypeFMR1KO:TrainSessionT1                            -2.0833
    ## GenotypeFMR1KO:TrainSessionT2                             3.4000
    ## GenotypeFMR1KO:TrainSessionT3                             6.2500
    ## GenotypeFMR1KO:TrainSessionT4                             4.8000
    ## GenotypeFMR1KO:TrainSessionT5                             2.5500
    ## GenotypeFMR1KO:TrainSessionT6                            -0.5000
    ## APA2yoked-conflict:TrainSessionC2                         4.3333
    ## APA2consistent:TrainSessionC2                                 NA
    ## APA2conflict:TrainSessionC2                                   NA
    ## APA2yoked-conflict:TrainSessionC3                         7.8889
    ## APA2consistent:TrainSessionC3                                 NA
    ## APA2conflict:TrainSessionC3                                   NA
    ## APA2yoked-conflict:TrainSessionHab                        2.9167
    ## APA2consistent:TrainSessionHab                           12.3750
    ## APA2conflict:TrainSessionHab                             10.6944
    ## APA2yoked-conflict:TrainSessionRetention                  3.3333
    ## APA2consistent:TrainSessionRetention                      0.6250
    ## APA2conflict:TrainSessionRetention                       -1.4444
    ## APA2yoked-conflict:TrainSessionRetest                     0.2500
    ## APA2consistent:TrainSessionRetest                         5.0000
    ## APA2conflict:TrainSessionRetest                          -0.9722
    ## APA2yoked-conflict:TrainSessionT1                         5.0000
    ## APA2consistent:TrainSessionT1                            10.6250
    ## APA2conflict:TrainSessionT1                               4.6667
    ## APA2yoked-conflict:TrainSessionT2                         6.0833
    ## APA2consistent:TrainSessionT2                            10.0000
    ## APA2conflict:TrainSessionT2                               3.8611
    ## APA2yoked-conflict:TrainSessionT3                         5.0000
    ## APA2consistent:TrainSessionT3                             3.7500
    ## APA2conflict:TrainSessionT3                                   NA
    ## APA2yoked-conflict:TrainSessionT4                             NA
    ## APA2consistent:TrainSessionT4                             4.5000
    ## APA2conflict:TrainSessionT4                                   NA
    ## APA2yoked-conflict:TrainSessionT5                             NA
    ## APA2consistent:TrainSessionT5                             1.0000
    ## APA2conflict:TrainSessionT5                                   NA
    ## APA2yoked-conflict:TrainSessionT6                             NA
    ## APA2consistent:TrainSessionT6                                 NA
    ## APA2conflict:TrainSessionT6                                   NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC2         -6.5833
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC2                  NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC2                    NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC3         -7.8889
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC3                  NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC3                    NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionHab        -8.9167
    ## GenotypeFMR1KO:APA2consistent:TrainSessionHab            -2.6964
    ## GenotypeFMR1KO:APA2conflict:TrainSessionHab              -4.6944
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetention  -6.6976
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetention      -5.0615
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetention        -3.2198
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetest      4.5000
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetest         -7.6333
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetest            2.2222
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT1          2.5833
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT1             -1.2202
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT1                7.1667
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT2         -4.2333
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT2             -8.0429
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT2               -1.7611
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT3         -4.0000
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT3             -7.1071
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT3                    NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT4              NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT4             -5.5143
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT4                    NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT5              NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT5             -2.3000
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT5                    NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT6              NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT6                  NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT6                    NA
    ##                                                         Std. Error t value
    ## (Intercept)                                                 3.4084   6.895
    ## GenotypeFMR1KO                                              5.6672  -0.485
    ## APA2yoked-conflict                                          4.4516  -1.011
    ## APA2consistent                                              3.0373  -4.445
    ## APA2conflict                                                2.9805  -3.374
    ## TrainSessionC2                                              2.3381  -3.136
    ## TrainSessionC3                                              2.3381  -4.515
    ## TrainSessionHab                                             4.2151   1.364
    ## TrainSessionRetention                                       4.2151  -0.474
    ## TrainSessionRetest                                          4.2151  -1.957
    ## TrainSessionT1                                              4.2151  -2.372
    ## TrainSessionT2                                              4.2151  -2.550
    ## TrainSessionT3                                              2.3381  -3.849
    ## TrainSessionT4                                              4.2151  -2.313
    ## TrainSessionT5                                              4.2151  -1.542
    ## TrainSessionT6                                              4.2151  -1.720
    ## GenotypeFMR1KO:APA2yoked-conflict                           6.8167   0.403
    ## GenotypeFMR1KO:APA2consistent                               4.2715   1.171
    ## GenotypeFMR1KO:APA2conflict                                 4.8201  -0.040
    ## GenotypeFMR1KO:TrainSessionC2                               4.2151   1.206
    ## GenotypeFMR1KO:TrainSessionC3                               4.2151   1.555
    ## GenotypeFMR1KO:TrainSessionHab                              6.6646   0.563
    ## GenotypeFMR1KO:TrainSessionRetention                        6.4639   0.613
    ## GenotypeFMR1KO:TrainSessionRetest                           6.8167   0.562
    ## GenotypeFMR1KO:TrainSessionT1                               6.8167  -0.306
    ## GenotypeFMR1KO:TrainSessionT2                               6.5717   0.517
    ## GenotypeFMR1KO:TrainSessionT3                               4.2151   1.483
    ## GenotypeFMR1KO:TrainSessionT4                               6.5717   0.730
    ## GenotypeFMR1KO:TrainSessionT5                               6.5717   0.388
    ## GenotypeFMR1KO:TrainSessionT6                               6.5717  -0.076
    ## APA2yoked-conflict:TrainSessionC2                           5.0958   0.850
    ## APA2consistent:TrainSessionC2                                   NA      NA
    ## APA2conflict:TrainSessionC2                                     NA      NA
    ## APA2yoked-conflict:TrainSessionC3                           4.6762   1.687
    ## APA2consistent:TrainSessionC3                                   NA      NA
    ## APA2conflict:TrainSessionC3                                     NA      NA
    ## APA2yoked-conflict:TrainSessionHab                          5.8453   0.499
    ## APA2consistent:TrainSessionHab                              4.2954   2.881
    ## APA2conflict:TrainSessionHab                                4.2151   2.537
    ## APA2yoked-conflict:TrainSessionRetention                    5.8453   0.570
    ## APA2consistent:TrainSessionRetention                        4.2954   0.146
    ## APA2conflict:TrainSessionRetention                          4.2151  -0.343
    ## APA2yoked-conflict:TrainSessionRetest                       5.8453   0.043
    ## APA2consistent:TrainSessionRetest                           4.2954   1.164
    ## APA2conflict:TrainSessionRetest                             4.2151  -0.231
    ## APA2yoked-conflict:TrainSessionT1                           5.8453   0.855
    ## APA2consistent:TrainSessionT1                               4.2954   2.474
    ## APA2conflict:TrainSessionT1                                 4.2151   1.107
    ## APA2yoked-conflict:TrainSessionT2                           5.8453   1.041
    ## APA2consistent:TrainSessionT2                               4.2954   2.328
    ## APA2conflict:TrainSessionT2                                 4.2151   0.916
    ## APA2yoked-conflict:TrainSessionT3                           4.6762   1.069
    ## APA2consistent:TrainSessionT3                               4.2954   0.873
    ## APA2conflict:TrainSessionT3                                     NA      NA
    ## APA2yoked-conflict:TrainSessionT4                               NA      NA
    ## APA2consistent:TrainSessionT4                               4.2954   1.048
    ## APA2conflict:TrainSessionT4                                     NA      NA
    ## APA2yoked-conflict:TrainSessionT5                               NA      NA
    ## APA2consistent:TrainSessionT5                               4.2954   0.233
    ## APA2conflict:TrainSessionT5                                     NA      NA
    ## APA2yoked-conflict:TrainSessionT6                               NA      NA
    ## APA2consistent:TrainSessionT6                                   NA      NA
    ## APA2conflict:TrainSessionT6                                     NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC2            7.1111  -0.926
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC2                    NA      NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC2                      NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC3            6.9655  -1.133
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC3                    NA      NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC3                      NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionHab           8.5509  -1.043
    ## GenotypeFMR1KO:APA2consistent:TrainSessionHab               6.0938  -0.442
    ## GenotypeFMR1KO:APA2conflict:TrainSessionHab                 6.6646  -0.704
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetention     8.3218  -0.805
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetention         5.8067  -0.872
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetention           6.3680  -0.506
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetest        8.7874   0.512
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetest            6.3711  -1.198
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetest              6.9655   0.319
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT1            8.6700   0.298
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT1                6.2598  -0.195
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT1                  6.8167   1.051
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT2            8.4787  -0.499
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT2                5.9921  -1.342
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT2                  6.5717  -0.268
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT3            6.8167  -0.587
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT3                6.2598  -1.135
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT3                      NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT4                NA      NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT4                5.9921  -0.920
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT4                      NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT5                NA      NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT5                6.0408  -0.381
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT5                      NA      NA
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT6                NA      NA
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT6                    NA      NA
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT6                      NA      NA
    ##                                                         Pr(>|t|)    
    ## (Intercept)                                             2.96e-11 ***
    ## GenotypeFMR1KO                                          0.627838    
    ## APA2yoked-conflict                                      0.312857    
    ## APA2consistent                                          1.22e-05 ***
    ## APA2conflict                                            0.000834 ***
    ## TrainSessionC2                                          0.001872 ** 
    ## TrainSessionC3                                          8.98e-06 ***
    ## TrainSessionHab                                         0.173495    
    ## TrainSessionRetention                                   0.635482    
    ## TrainSessionRetest                                      0.051201 .  
    ## TrainSessionT1                                          0.018272 *  
    ## TrainSessionT2                                          0.011234 *  
    ## TrainSessionT3                                          0.000143 ***
    ## TrainSessionT4                                          0.021360 *  
    ## TrainSessionT5                                          0.124059    
    ## TrainSessionT6                                          0.086413 .  
    ## GenotypeFMR1KO:APA2yoked-conflict                       0.686913    
    ## GenotypeFMR1KO:APA2consistent                           0.242660    
    ## GenotypeFMR1KO:APA2conflict                             0.967848    
    ## GenotypeFMR1KO:TrainSessionC2                           0.228729    
    ## GenotypeFMR1KO:TrainSessionC3                           0.120889    
    ## GenotypeFMR1KO:TrainSessionHab                          0.574059    
    ## GenotypeFMR1KO:TrainSessionRetention                    0.540122    
    ## GenotypeFMR1KO:TrainSessionRetest                       0.574282    
    ## GenotypeFMR1KO:TrainSessionT1                           0.760095    
    ## GenotypeFMR1KO:TrainSessionT2                           0.605262    
    ## GenotypeFMR1KO:TrainSessionT3                           0.139136    
    ## GenotypeFMR1KO:TrainSessionT4                           0.465687    
    ## GenotypeFMR1KO:TrainSessionT5                           0.698259    
    ## GenotypeFMR1KO:TrainSessionT6                           0.939401    
    ## APA2yoked-conflict:TrainSessionC2                       0.395763    
    ## APA2consistent:TrainSessionC2                                 NA    
    ## APA2conflict:TrainSessionC2                                   NA    
    ## APA2yoked-conflict:TrainSessionC3                       0.092589 .  
    ## APA2consistent:TrainSessionC3                                 NA    
    ## APA2conflict:TrainSessionC3                                   NA    
    ## APA2yoked-conflict:TrainSessionHab                      0.618143    
    ## APA2consistent:TrainSessionHab                          0.004236 ** 
    ## APA2conflict:TrainSessionHab                            0.011657 *  
    ## APA2yoked-conflict:TrainSessionRetention                0.568907    
    ## APA2consistent:TrainSessionRetention                    0.884405    
    ## APA2conflict:TrainSessionRetention                      0.732065    
    ## APA2yoked-conflict:TrainSessionRetest                   0.965912    
    ## APA2consistent:TrainSessionRetest                       0.245289    
    ## APA2conflict:TrainSessionRetest                         0.817734    
    ## APA2yoked-conflict:TrainSessionT1                       0.392985    
    ## APA2consistent:TrainSessionT1                           0.013903 *  
    ## APA2conflict:TrainSessionT1                             0.269081    
    ## APA2yoked-conflict:TrainSessionT2                       0.298801    
    ## APA2consistent:TrainSessionT2                           0.020541 *  
    ## APA2conflict:TrainSessionT2                             0.360357    
    ## APA2yoked-conflict:TrainSessionT3                       0.285781    
    ## APA2consistent:TrainSessionT3                           0.383312    
    ## APA2conflict:TrainSessionT3                                   NA    
    ## APA2yoked-conflict:TrainSessionT4                             NA    
    ## APA2consistent:TrainSessionT4                           0.295610    
    ## APA2conflict:TrainSessionT4                                   NA    
    ## APA2yoked-conflict:TrainSessionT5                             NA    
    ## APA2consistent:TrainSessionT5                           0.816062    
    ## APA2conflict:TrainSessionT5                                   NA    
    ## APA2yoked-conflict:TrainSessionT6                             NA    
    ## APA2consistent:TrainSessionT6                                 NA    
    ## APA2conflict:TrainSessionT6                                   NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC2        0.355267    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC2                  NA    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC2                    NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionC3        0.258255    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionC3                  NA    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionC3                    NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionHab       0.297852    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionHab           0.658443    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionHab             0.481716    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetention 0.421525    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetention     0.384057    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetention       0.613472    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionRetest    0.608941    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionRetest        0.231771    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionRetest          0.749912    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT1        0.765927    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT1            0.845571    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT1              0.293909    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT2        0.617922    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT2            0.180482    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT2              0.788887    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT3        0.557763    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT3            0.257086    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT3                    NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT4              NA    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT4            0.358139    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT4                    NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT5              NA    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT5            0.703647    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT5                    NA    
    ## GenotypeFMR1KO:APA2yoked-conflict:TrainSessionT6              NA    
    ## GenotypeFMR1KO:APA2consistent:TrainSessionT6                  NA    
    ## GenotypeFMR1KO:APA2conflict:TrainSessionT6                    NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.96 on 315 degrees of freedom
    ## Multiple R-squared:  0.7405, Adjusted R-squared:  0.6819 
    ## F-statistic: 12.66 on 71 and 315 DF,  p-value: < 2.2e-16

Anovas Consistent only - No significant effect of genotype
----------------------------------------------------------

``` r
consistent <- behavior %>%
  filter(conflict == "consistent") 
T1 <- consistent %>%
  filter(TrainSession == "T1") 
T2 <- consistent %>%
  filter(TrainSession == "T2") 
T3 <- consistent %>%
  filter(TrainSession == "T3") 
Retest <- consistent %>%
  filter(TrainSession == "Retest") 
T4 <- consistent %>%
  filter(TrainSession %in% c("T4", "C1")) 
T5 <- consistent %>%
  filter(TrainSession %in% c("T5", "C2")) 
T6 <- consistent %>%
  filter(TrainSession %in% c("T6", "C3")) 
Retention <- consistent %>%
  filter(TrainSession == "Retention") 

summary(aov(NumEntrances ~ Genotype *APA2, data=hab)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype       1    6.1    6.11   0.236  0.630
    ## APA2           3  130.1   43.36   1.674  0.190
    ## Genotype:APA2  3  127.6   42.55   1.643  0.197
    ## Residuals     35  906.6   25.90

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T1)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype       1  28.44  28.438   2.569  0.126
    ## APA2           1   6.98   6.976   0.630  0.438
    ## Genotype:APA2  1  16.78  16.784   1.516  0.234
    ## Residuals     18 199.26  11.070

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T2)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1    4.2    4.17   0.192 0.6661  
    ## APA2           1  144.6  144.63   6.660 0.0179 *
    ## Genotype:APA2  1   12.9   12.90   0.594 0.4499  
    ## Residuals     20  434.3   21.72                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T3))
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   15.8    15.8   1.129    0.302    
    ## APA2           1  543.5   543.5  38.923 6.94e-06 ***
    ## Genotype:APA2  1    5.2     5.2   0.374    0.549    
    ## Residuals     18  251.4    14.0                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retest)) 
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype       1    0.2     0.2   0.008 0.93170   
    ## APA2           1  417.4   417.4  15.189 0.00128 **
    ## Genotype:APA2  1    7.6     7.6   0.278 0.60539   
    ## Residuals     16  439.7    27.5                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T4)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   37.5    37.5   2.311    0.144    
    ## APA2           1  479.7   479.7  29.565 2.54e-05 ***
    ## Genotype:APA2  1    0.4     0.4   0.023    0.882    
    ## Residuals     20  324.5    16.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T5)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   44.6    44.6   1.430 0.246527    
    ## APA2           1  668.8   668.8  21.435 0.000183 ***
    ## Genotype:APA2  1    9.8     9.8   0.315 0.581165    
    ## Residuals     19  592.8    31.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T6)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1    7.1     7.1   0.288    0.598    
    ## APA2           1  649.3   649.3  26.264 6.02e-05 ***
    ## Genotype:APA2  1   33.7    33.7   1.363    0.257    
    ## Residuals     19  469.7    24.7                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retention)) 
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   43.6    43.6   1.139    0.296    
    ## APA2           1 1101.0  1101.0  28.788 1.65e-05 ***
    ## Genotype:APA2  1    0.0     0.0   0.000    0.990    
    ## Residuals     24  917.9    38.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Anovas Conflict only
--------------------

``` r
conflict <- behavior %>%
  filter(conflict == "conflict") 
T1 <- conflict %>%
  filter(TrainSession == "T1") 
T2 <- conflict %>%
  filter(TrainSession == "T2") 
T3 <- conflict %>%
  filter(TrainSession == "T3") 
Retest <- conflict %>%
  filter(TrainSession == "Retest") 
T4 <- conflict %>%
  filter(TrainSession %in% c("T4", "C1")) 
T5 <- conflict %>%
  filter(TrainSession %in% c("T5", "C2")) 
T6 <- conflict %>%
  filter(TrainSession %in% c("T6", "C3")) 
Retention <- conflict %>%
  filter(TrainSession == "Retention") 


summary(aov(NumEntrances ~ Genotype *APA2, data=hab)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype       1    6.1    6.11   0.236  0.630
    ## APA2           3  130.1   43.36   1.674  0.190
    ## Genotype:APA2  3  127.6   42.55   1.643  0.197
    ## Residuals     35  906.6   25.90

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T1)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1  37.41   37.41   2.664 0.1221  
    ## APA2           1 111.31  111.31   7.928 0.0124 *
    ## Genotype:APA2  1   2.84    2.84   0.203 0.6587  
    ## Residuals     16 224.64   14.04                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T2)) 
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype       1   3.68    3.68   0.211 0.65214   
    ## APA2           1 272.00  272.00  15.619 0.00114 **
    ## Genotype:APA2  1   0.24    0.24   0.014 0.90875   
    ## Residuals     16 278.64   17.41                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T3))      # * significant
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1  140.8   140.8   9.023  0.00841 ** 
    ## APA2           1  430.0   430.0  27.552 7.95e-05 ***
    ## Genotype:APA2  1    1.2     1.2   0.076  0.78689    
    ## Residuals     16  249.7    15.6                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retest)) # * significant
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1  220.0  220.03   5.914 0.0290 *
    ## APA2           1  294.8  294.82   7.924 0.0138 *
    ## Genotype:APA2  1   24.5   24.54   0.660 0.4303  
    ## Residuals     14  520.9   37.21                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T4)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1    0.0    0.03   0.001 0.9776  
    ## APA2           1  204.8  204.76   5.000 0.0399 *
    ## Genotype:APA2  1    9.2    9.18   0.224 0.6423  
    ## Residuals     16  655.2   40.95                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T5)) 
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype       1   55.6   55.64   1.804 0.1992  
    ## APA2           1  226.2  226.23   7.335 0.0162 *
    ## Genotype:APA2  1   11.9   11.92   0.386 0.5435  
    ## Residuals     15  462.6   30.84                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype *APA2, data=T6))  # * significant
```

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype       1   67.0    67.0   6.588   0.0215 *  
    ## APA2           1  506.8   506.8  49.828 3.88e-06 ***
    ## Genotype:APA2  1   23.8    23.8   2.339   0.1470    
    ## Residuals     15  152.6    10.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(NumEntrances ~ Genotype * APA2, data=Retention)) 
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype       1    0.1     0.1   0.002 0.96483   
    ## APA2           1  480.0   480.0  12.922 0.00207 **
    ## Genotype:APA2  1    0.3     0.3   0.009 0.92519   
    ## Residuals     18  668.7    37.1                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# percent contribution
behaviormatrix <- behavior[c(13:52)]  # for 2nd pca analysis
behaviormatrix %>% 
  scale() %>%                 # scale to 0 mean and unit variance
  prcomp() ->                 # do PCA
  pca                         # store result as `pca`
percent <- round(100*pca$sdev^2/sum(pca$sdev^2),2)
perc_data <- data.frame(percent=percent, PC=1:length(percent))
res.pca <- prcomp(behaviormatrix,  scale = TRUE)
ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  xlim(0, 10)
```

    ## Warning: Removed 30 rows containing missing values (position_stack).

    ## Warning: Removed 30 rows containing missing values (geom_text).

![](01_behavior_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# PCA
longdata <- melt(behavior, id = c(1:12));
  longdata <- longdata %>% drop_na();
  longdata$bysession <- as.factor(paste(longdata$TrainSessionCombo, longdata$variable, sep="_"));
  longdata <- dcast(longdata, ID + APA2 + Genotype ~ bysession, value.var= "value", fun.aggregate = mean)
Z <- longdata[,4:363]
Z <- Z[ , apply(Z, 2, function(x) !any(is.na(x)))]
pc = prcomp(Z, scale=TRUE)
loadings <- pc$rotation
scores <- pc$x
scoresdf <- as.data.frame(scores)
scoresdf$ID <-  longdata$ID
scoresdf$APA2 <- longdata$APA2
scoresdf$Genotype <- longdata$Genotype
scoresdf$APA2 <- factor(scoresdf$APA2, levels = c("yoked-consistent" ,"consistent", "yoked-conflict", "conflict"))
rotationdf <- data.frame(pc$rotation, variable=row.names(pc$rotation))

pca12 <- ggplot(scoresdf, aes(PC1,PC2, color=APA2, shape=Genotype)) +
    geom_point(size=3, alpha = 0.7) +
    xlab(paste0("PC 1: ", percent[1],"% variance")) +
    ylab(paste0("PC 2: ", percent[2],"% variance")) +
    scale_colour_manual(values=c(colorvalAPA00)) +
    scale_shape_manual(values=c(16, 1)) +
    #theme(legend.position="none") + 
    theme_cowplot(font_size = 8, line_size = 0.25) 
pca12
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-2-2.png)

Heatmap
-------

``` r
## make annotation df and ann_colors for pheatmap
  averagedata <- melt(behavior, id = c(1:12));  #longdata <- melt(behavior, id = c(1:18))
  averagedata <- averagedata %>% drop_na();
  # then widen with group averages, add row names, scale, and transpose
  averagedata$GenoAPA2session <- as.factor(paste(averagedata$Genotype, averagedata$APA2,averagedata$TrainSessionCombo, sep="_"))
  averagedata <- dcast(averagedata, GenoAPA2session ~ variable, value.var= "value", fun.aggregate=mean);
  rownames(averagedata) <- averagedata$GenoAPA2session;    
  averagedata[1] <- NULL;
  scaledaveragedata <- scale(averagedata)
  scaledaveragedata <- t(scaledaveragedata)
  scaledaveragedata <- scaledaveragedata[-1,]

  
makecolumnannotations <- function(data){  
  columnannotations <- as.data.frame(colnames(data))
  names(columnannotations)[names(columnannotations)=="colnames(data)"] <- "column"
  rownames(columnannotations) <- columnannotations$column
  columnannotations$Genotype <- sapply(strsplit(as.character(columnannotations$column),'\\_'), "[", 1)
  columnannotations$APA2    <- sapply(strsplit(as.character(columnannotations$column),'\\_'), "[", 2)
  #columnannotations$Session <- sapply(strsplit(as.character(columnannotations$column),'\\_'), "[", 3)
  columnannotations$column <- NULL
  return(columnannotations)
}
  
# create a rubric for the color coding and load the colors from figureoptions.R
df2 <- as.data.frame(makecolumnannotations(scaledaveragedata))
ann_colors <- list(
  Genotype =  c('FMR1KO' = (values=c("white")), 
            'WT' = (values=c("#404040"))),
  APA2 = c('yoked-conflict' = (values=c("#bababa")),
           'yoked-consistent' = (values=c("#404040")), 
           'conflict' = (values=c("#f4a582")),
           'consistent' = (values=c("#ca0020"))))


# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(scaledaveragedata), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(scaledaveragedata)/paletteLength, max(scaledaveragedata), length.out=floor(paletteLength/2)))

## pheatmap for markdown
pheatmap(scaledaveragedata, show_colnames=F, show_rownames = F,
         annotation_col=df2, 
         annotation_colors = ann_colors,
         treeheight_row = 50, treeheight_col = 50,
         border_color = "grey60" ,
         color = viridis(30),
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation" ,
         clustering_distance_rows = "correlation"
         )
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-3-1.png)

anova habituation
-----------------

``` r
slim1 <- behavior[,c(2,4,8,13:52)]
slim2 <- slim1 %>% filter(TrainSession == "Hab") 
Genotype <- slim2[,1]
APA <- slim2[,3]
slim3 <- slim2[,c(4:43)]

for(y in names(slim3)){
  ymod<- summary(aov(slim3[[y]] ~ Genotype * APA))
  cat(paste('\nDependent var:', y, '\n'))
  print(ymod)
}
```

    ## 
    ## Dependent var: SdevSpeedArena 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1  0.017 0.01695   0.096  0.758
    ## APA           3  0.669 0.22288   1.263  0.302
    ## Genotype:APA  3  0.586 0.19547   1.108  0.359
    ## Residuals    35  6.177 0.17649               
    ## 
    ## Dependent var: Linearity.Arena. 
    ##              Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.00005 0.0000531   0.026  0.874
    ## APA           3 0.00450 0.0015009   0.722  0.546
    ## Genotype:APA  3 0.00123 0.0004116   0.198  0.897
    ## Residuals    35 0.07278 0.0020793               
    ## 
    ## Dependent var: NumEntrances 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    6.1    6.11   0.236  0.630
    ## APA           3  130.1   43.36   1.674  0.190
    ## Genotype:APA  3  127.6   42.55   1.643  0.197
    ## Residuals    35  906.6   25.90               
    ## 
    ## Dependent var: Time1stEntr 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    3.1    3.05   0.039  0.845
    ## APA           3  302.7  100.90   1.286  0.294
    ## Genotype:APA  3  179.3   59.77   0.762  0.523
    ## Residuals    35 2746.6   78.48               
    ## 
    ## Dependent var: Path1stEntr 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1  0.025 0.02459   0.124  0.727
    ## APA           3  0.946 0.31535   1.589  0.210
    ## Genotype:APA  3  0.098 0.03277   0.165  0.919
    ## Residuals    35  6.947 0.19848               
    ## 
    ## Dependent var: Speed1stEntr.cm.s. 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    8.0   8.018   0.366  0.549
    ## APA           3   44.0  14.655   0.669  0.577
    ## Genotype:APA  3   51.0  17.013   0.776  0.515
    ## Residuals    35  766.8  21.910               
    ## 
    ## Dependent var: Dist1stEntr.m. 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1 0.0017 0.00173   0.061 0.80562   
    ## APA           3 0.3922 0.13073   4.653 0.00771 **
    ## Genotype:APA  3 0.0361 0.01205   0.429 0.73368   
    ## Residuals    35 0.9834 0.02810                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: NumShock 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1      0    0.01   0.000  0.994
    ## APA           3    245   81.83   0.886  0.458
    ## Genotype:APA  3     93   31.07   0.336  0.799
    ## Residuals    35   3233   92.37               
    ## 
    ## Dependent var: MaxTimeAvoid 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1      6     5.9   0.011  0.916
    ## APA           3   3382  1127.4   2.148  0.112
    ## Genotype:APA  3    485   161.6   0.308  0.819
    ## Residuals    35  18367   524.8               
    ## 
    ## Dependent var: Time2ndEntr 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   40.2   40.24   0.666 0.4198  
    ## APA           3  682.6  227.54   3.768 0.0192 *
    ## Genotype:APA  3  401.2  133.75   2.215 0.1037  
    ## Residuals    35 2113.4   60.38                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Path2ndEntr 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1  0.011  0.0110   0.063 0.80367   
    ## APA           3  2.308  0.7693   4.404 0.00991 **
    ## Genotype:APA  3  0.433  0.1444   0.827 0.48811   
    ## Residuals    35  6.114  0.1747                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed2ndEntr 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1    0.7    0.72   0.056  0.815  
    ## APA           3   37.4   12.47   0.963  0.421  
    ## Genotype:APA  3  151.0   50.32   3.884  0.017 *
    ## Residuals    35  453.4   12.95                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: TimeTarget 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1     35    34.6   0.095  0.760
    ## APA           3    684   227.9   0.626  0.603
    ## Genotype:APA  3     88    29.3   0.081  0.970
    ## Residuals    35  12733   363.8               
    ## 
    ## Dependent var: pTimeTarget 
    ##              Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.00040 0.0003954   0.178  0.676
    ## APA           3 0.00366 0.0012196   0.549  0.652
    ## Genotype:APA  3 0.00088 0.0002938   0.132  0.940
    ## Residuals    35 0.07780 0.0022229               
    ## 
    ## Dependent var: pTimeCCW 
    ##              Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.00008 0.0000753   0.024  0.878
    ## APA           3 0.00902 0.0030082   0.961  0.422
    ## Genotype:APA  3 0.00766 0.0025548   0.816  0.494
    ## Residuals    35 0.10959 0.0031311               
    ## 
    ## Dependent var: pTimeOPP 
    ##              Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.00007 0.0000709   0.043  0.836
    ## APA           3 0.00522 0.0017405   1.066  0.376
    ## Genotype:APA  3 0.00643 0.0021428   1.312  0.286
    ## Residuals    35 0.05717 0.0016334               
    ## 
    ## Dependent var: pTimeCW 
    ##              Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.00039 0.0003894   0.139  0.712
    ## APA           3 0.00418 0.0013934   0.497  0.687
    ## Genotype:APA  3 0.00555 0.0018505   0.660  0.582
    ## Residuals    35 0.09820 0.0028056               
    ## 
    ## Dependent var: RayleigLength 
    ##              Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype      1 0.00070 0.000698   0.169  0.684
    ## APA           3 0.00387 0.001291   0.312  0.816
    ## Genotype:APA  3 0.02303 0.007676   1.857  0.155
    ## Residuals    35 0.14466 0.004133               
    ## 
    ## Dependent var: RayleigAngle 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1      2       2   0.000  0.992
    ## APA           3  13670    4557   0.313  0.816
    ## Genotype:APA  3  21666    7222   0.496  0.687
    ## Residuals    35 509123   14546               
    ## 
    ## Dependent var: PolarAvgVal 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1      1    1.17   0.009  0.926
    ## APA           3    773  257.81   1.914  0.145
    ## Genotype:APA  3    116   38.65   0.287  0.835
    ## Residuals    35   4715  134.72               
    ## 
    ## Dependent var: PolarSdVal 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    0.0    0.05   0.001  0.970
    ## APA           3   13.5    4.50   0.137  0.937
    ## Genotype:APA  3   70.7   23.55   0.717  0.548
    ## Residuals    35 1149.1   32.83               
    ## 
    ## Dependent var: PolarMinVal 
    ##              Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 2.000e-08 1.800e-08   0.003  0.955
    ## APA           3 2.999e-05 9.997e-06   1.754  0.174
    ## Genotype:APA  3 7.630e-06 2.545e-06   0.447  0.721
    ## Residuals    35 1.994e-04 5.699e-06               
    ## 
    ## Dependent var: PolarMinBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   3175    3175   0.335  0.566
    ## APA           3  11675    3892   0.411  0.746
    ## Genotype:APA  3  12673    4224   0.446  0.722
    ## Residuals    35 331394    9468               
    ## 
    ## Dependent var: Min50.RngLoBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    286     286   0.042  0.839
    ## APA           3  28346    9449   1.377  0.266
    ## Genotype:APA  3   7191    2397   0.349  0.790
    ## Residuals    35 240204    6863               
    ## 
    ## Dependent var: Min50.RngHiBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   5612    5612   0.353  0.556
    ## APA           3   1250     417   0.026  0.994
    ## Genotype:APA  3  43355   14452   0.910  0.446
    ## Residuals    35 555644   15876               
    ## 
    ## Dependent var: PolarMaxVal 
    ##              Df    Sum Sq   Mean Sq F value  Pr(>F)   
    ## Genotype      1 0.0000051 5.140e-06   0.183 0.67150   
    ## APA           3 0.0000825 2.749e-05   0.977 0.41445   
    ## Genotype:APA  3 0.0003867 1.289e-04   4.583 0.00827 **
    ## Residuals    35 0.0009843 2.812e-05                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarMaxBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   6851    6851   0.746 0.3935  
    ## APA           3  98528   32843   3.578 0.0234 *
    ## Genotype:APA  3   6791    2264   0.247 0.8632  
    ## Residuals    35 321278    9179                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Max50.RngLoBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   2977    2977   0.176  0.677
    ## APA           3  18709    6236   0.369  0.776
    ## Genotype:APA  3  36563   12188   0.722  0.546
    ## Residuals    35 591114   16889               
    ## 
    ## Dependent var: Max50.RngHiBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   1131    1131   0.136  0.715
    ## APA           3  44676   14892   1.785  0.168
    ## Genotype:APA  3   7960    2653   0.318  0.812
    ## Residuals    35 292079    8345               
    ## 
    ## Dependent var: AnnularMinVal 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype      1 0.000393 0.0003931   0.940 0.3390  
    ## APA           3 0.005065 0.0016883   4.035 0.0145 *
    ## Genotype:APA  3 0.001090 0.0003634   0.869 0.4665  
    ## Residuals    35 0.014643 0.0004184                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMinBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   6.19   6.190   3.416  0.073 .
    ## APA           3  11.38   3.792   2.093  0.119  
    ## Genotype:APA  3   2.37   0.791   0.437  0.728  
    ## Residuals    35  63.43   1.812                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMaxVal 
    ##              Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Genotype      1 0.00696 0.006956   2.947 0.0949 .
    ## APA           3 0.00450 0.001502   0.636 0.5968  
    ## Genotype:APA  3 0.00259 0.000862   0.365 0.7785  
    ## Residuals    35 0.08262 0.002361                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMaxBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   3.14   3.137   1.543  0.222  
    ## APA           3   8.53   2.842   1.398  0.260  
    ## Genotype:APA  3  18.25   6.082   2.991  0.044 *
    ## Residuals    35  71.16   2.033                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularAvg 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1  0.126  0.1262   0.432  0.515
    ## APA           3  1.580  0.5268   1.804  0.164
    ## Genotype:APA  3  0.228  0.0761   0.261  0.853
    ## Residuals    35 10.218  0.2920               
    ## 
    ## Dependent var: AnnularSd 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   0.51   0.512   0.128  0.723
    ## APA           3  23.52   7.841   1.954  0.139
    ## Genotype:APA  3   2.17   0.722   0.180  0.909
    ## Residuals    35 140.47   4.013               
    ## 
    ## Dependent var: AnnularSkewnes 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1 0.0524 0.05239   0.688  0.412
    ## APA           3 0.2159 0.07195   0.945  0.429
    ## Genotype:APA  3 0.0825 0.02750   0.361  0.781
    ## Residuals    35 2.6645 0.07613               
    ## 
    ## Dependent var: AnnularKurtosis 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   0.02  0.0193   0.018  0.895
    ## APA           3   4.85  1.6165   1.489  0.235
    ## Genotype:APA  3   2.33  0.7758   0.714  0.550
    ## Residuals    35  38.01  1.0859               
    ## 
    ## Dependent var: Speed1 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype      1 0.001660 0.0016604   3.300 0.0778 .
    ## APA           3 0.000774 0.0002580   0.513 0.6761  
    ## Genotype:APA  3 0.001233 0.0004110   0.817 0.4933  
    ## Residuals    35 0.017608 0.0005031                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed2 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype      1 0.000280 0.0002805   3.059 0.0890 .
    ## APA           3 0.000478 0.0001595   1.740 0.1768  
    ## Genotype:APA  3 0.000995 0.0003317   3.618 0.0224 *
    ## Residuals    35 0.003209 0.0000917                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Time1stEntrLog 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1   2.17   2.166   1.321  0.258
    ## APA           3   2.72   0.906   0.553  0.650
    ## Genotype:APA  3   5.13   1.709   1.042  0.386
    ## Residuals    35  57.39   1.640

anova conflict
--------------

``` r
slim1 <- behavior[,c(2,9,8,13:52)]
slim2 <- slim1 %>% filter(TrainSessionCombo == "T4_C1", APA2 == "conflict") 
Genotype <- slim2[,1]
APA <- slim2[,3]
slim3 <- slim2[,c(4:43)]

for(y in names(slim3)){
  ymod<- summary(aov(slim3[[y]] ~ Genotype ))
  cat(paste('\nDependent var:', y, '\n'))
  print(ymod)
}
```

    ## 
    ## Dependent var: SdevSpeedArena 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0169 0.01687    0.44  0.521
    ## Residuals   11 0.4215 0.03832               
    ## 
    ## Dependent var: Linearity.Arena. 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.00038 0.000382   0.103  0.754
    ## Residuals   11 0.04072 0.003701               
    ## 
    ## Dependent var: NumEntrances 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   24.0   24.01   0.519  0.486
    ## Residuals   11  509.2   46.29               
    ## 
    ## Dependent var: Time1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      2     2.1   0.002  0.969
    ## Residuals   11  14114  1283.1               
    ## 
    ## Dependent var: Path1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.009  0.0085   0.016  0.902
    ## Residuals   11  5.870  0.5336               
    ## 
    ## Dependent var: Speed1stEntr.cm.s. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   8.97   8.967   0.829  0.382
    ## Residuals   11 118.97  10.816               
    ## 
    ## Dependent var: Dist1stEntr.m. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0637  0.0637   0.459  0.512
    ## Residuals   11 1.5263  0.1388               
    ## 
    ## Dependent var: NumShock 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   44.3   44.31   0.466  0.509
    ## Residuals   11 1046.0   95.09               
    ## 
    ## Dependent var: MaxTimeAvoid 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    164     164   0.032   0.86
    ## Residuals   11  55557    5051               
    ## 
    ## Dependent var: Time2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   8333    8333   0.992  0.341
    ## Residuals   11  92437    8403               
    ## 
    ## Dependent var: Path2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   4.38   4.375   0.944  0.352
    ## Residuals   11  51.00   4.636               
    ## 
    ## Dependent var: Speed2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.69   0.689    0.17  0.688
    ## Residuals   11  44.47   4.043               
    ## 
    ## Dependent var: TimeTarget 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   82.3   82.31   0.412  0.534
    ## Residuals   11 2196.1  199.65               
    ## 
    ## Dependent var: pTimeTarget 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.000362 0.0003623   0.316  0.585
    ## Residuals   11 0.012613 0.0011466               
    ## 
    ## Dependent var: pTimeCCW 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.00093 0.000927   0.081  0.781
    ## Residuals   11 0.12551 0.011410               
    ## 
    ## Dependent var: pTimeOPP 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0053 0.00530   0.188  0.673
    ## Residuals   11 0.3103 0.02821               
    ## 
    ## Dependent var: pTimeCW 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0071 0.00711   0.216  0.651
    ## Residuals   11 0.3624 0.03294               
    ## 
    ## Dependent var: RayleigLength 
    ##             Df  Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.03627 0.03627   1.499  0.246
    ## Residuals   11 0.26622 0.02420               
    ## 
    ## Dependent var: RayleigAngle 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     68      68   0.015  0.906
    ## Residuals   11  50565    4597               
    ## 
    ## Dependent var: PolarAvgVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    214   214.3   0.174  0.685
    ## Residuals   11  13541  1231.0               
    ## 
    ## Dependent var: PolarSdVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     43    42.7   0.095  0.764
    ## Residuals   11   4968   451.6               
    ## 
    ## Dependent var: PolarMinVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 8.200e-08 8.210e-08   0.103  0.754
    ## Residuals   11 8.766e-06 7.969e-07               
    ## 
    ## Dependent var: PolarMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  56892   56892   4.395   0.06 .
    ## Residuals   11 142400   12945                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Min50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    262     262   0.046  0.834
    ## Residuals   11  62231    5657               
    ## 
    ## Dependent var: Min50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     36      36   0.008  0.932
    ## Residuals   11  52564    4779               
    ## 
    ## Dependent var: PolarMaxVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0002507 0.0002506   1.045  0.329
    ## Residuals   11 0.0026391 0.0002399               
    ## 
    ## Dependent var: PolarMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   2267    2267   0.489  0.499
    ## Residuals   11  50964    4633               
    ## 
    ## Dependent var: Max50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     69      69   0.019  0.894
    ## Residuals   11  40700    3700               
    ## 
    ## Dependent var: Max50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   1888    1888   0.438  0.522
    ## Residuals   11  47389    4308               
    ## 
    ## Dependent var: AnnularMinVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0000642 6.417e-05    1.11  0.315
    ## Residuals   11 0.0006362 5.784e-05               
    ## 
    ## Dependent var: AnnularMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   6.42   6.417   0.395  0.543
    ## Residuals   11 178.72  16.247               
    ## 
    ## Dependent var: AnnularMaxVal 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.00045 0.000452   0.036  0.854
    ## Residuals   11 0.13946 0.012679               
    ## 
    ## Dependent var: AnnularMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  2.693  2.6928   6.025  0.032 *
    ## Residuals   11  4.916  0.4469                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularAvg 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.056 0.05623    0.18   0.68
    ## Residuals   11  3.442 0.31292               
    ## 
    ## Dependent var: AnnularSd 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  73.37   73.37    6.73  0.025 *
    ## Residuals   11 119.92   10.90                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularSkewnes 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.592  0.5924   1.494  0.247
    ## Residuals   11  4.363  0.3966               
    ## 
    ## Dependent var: AnnularKurtosis 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   76.3    76.3   2.535   0.14
    ## Residuals   11  331.2    30.1               
    ## 
    ## Dependent var: Speed1 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0000042 4.220e-06   0.032  0.861
    ## Residuals   11 0.0014493 1.318e-04               
    ## 
    ## Dependent var: Speed2 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0000107 1.065e-05   0.217   0.65
    ## Residuals   11 0.0005393 4.903e-05               
    ## 
    ## Dependent var: Time1stEntrLog 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.04   0.041    0.01  0.921
    ## Residuals   11  42.93   3.903

``` r
# * AnnularSd, AnnularMaxBin 
# . PolarMinBin 

slim1 <- behavior[,c(2,9,8,13:52)]
slim2 <- slim1 %>% filter(TrainSessionCombo == "T3", APA2 == "conflict") 
Genotype <- slim2[,1]
APA <- slim2[,3]
slim3 <- slim2[,c(4:43)]

for(y in names(slim3)){
  ymod<- summary(aov(slim3[[y]] ~ Genotype ))
  cat(paste('\nDependent var:', y, '\n'))
  print(ymod)
}
```

    ## 
    ## Dependent var: SdevSpeedArena 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0093 0.00933   0.087  0.773
    ## Residuals   11 1.1757 0.10688               
    ## 
    ## Dependent var: Linearity.Arena. 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.000426 0.0004256   0.229  0.642
    ## Residuals   11 0.020468 0.0018607               
    ## 
    ## Dependent var: NumEntrances 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  30.26   30.26    2.12  0.173
    ## Residuals   11 156.97   14.27               
    ## 
    ## Dependent var: Time1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    362   362.2   0.476  0.504
    ## Residuals   11   8366   760.5               
    ## 
    ## Dependent var: Path1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.062  0.0623   0.173  0.685
    ## Residuals   11  3.952  0.3593               
    ## 
    ## Dependent var: Speed1stEntr.cm.s. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.21   0.211   0.022  0.885
    ## Residuals   11 106.33   9.666               
    ## 
    ## Dependent var: Dist1stEntr.m. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.1123 0.11231   2.743  0.126
    ## Residuals   11 0.4504 0.04094               
    ## 
    ## Dependent var: NumShock 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  79.59   79.59   2.961  0.113
    ## Residuals   11 295.64   26.88               
    ## 
    ## Dependent var: MaxTimeAvoid 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  59456   59456   4.789 0.0511 .
    ## Residuals   11 136580   12416                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Time2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  12280   12280   0.321  0.583
    ## Residuals   11 421285   38299               
    ## 
    ## Dependent var: Path2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   9.12   9.122   0.408  0.536
    ## Residuals   11 246.07  22.370               
    ## 
    ## Dependent var: Speed2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   2.22   2.224   0.153  0.703
    ## Residuals   11 160.21  14.564               
    ## 
    ## Dependent var: TimeTarget 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  179.7  179.73   3.095  0.106
    ## Residuals   11  638.7   58.06               
    ## 
    ## Dependent var: pTimeTarget 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.001094 0.0010942   3.164  0.103
    ## Residuals   11 0.003805 0.0003459               
    ## 
    ## Dependent var: pTimeCCW 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0003 0.00032   0.011   0.92
    ## Residuals   11 0.3329 0.03026               
    ## 
    ## Dependent var: pTimeOPP 
    ##             Df  Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.01561 0.01561   0.684  0.426
    ## Residuals   11 0.25123 0.02284               
    ## 
    ## Dependent var: pTimeCW 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.01203 0.012029   2.622  0.134
    ## Residuals   11 0.05047 0.004588               
    ## 
    ## Dependent var: RayleigLength 
    ##             Df  Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.05321 0.05321   3.168  0.103
    ## Residuals   11 0.18476 0.01680               
    ## 
    ## Dependent var: RayleigAngle 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     29    29.5   0.071  0.795
    ## Residuals   11   4597   417.9               
    ## 
    ## Dependent var: PolarAvgVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  187.7   187.7   0.727  0.412
    ## Residuals   11 2840.0   258.2               
    ## 
    ## Dependent var: PolarSdVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    437   437.0   2.995  0.111
    ## Residuals   11   1605   145.9               
    ## 
    ## Dependent var: PolarMinVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 4.561e-06 4.561e-06   2.814  0.122
    ## Residuals   11 1.783e-05 1.621e-06               
    ## 
    ## Dependent var: PolarMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  42503   42503   1.657  0.224
    ## Residuals   11 282189   25654               
    ## 
    ## Dependent var: Min50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    694   694.2   0.743  0.407
    ## Residuals   11  10275   934.1               
    ## 
    ## Dependent var: Min50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     94    94.2   0.139  0.717
    ## Residuals   11   7475   679.5               
    ## 
    ## Dependent var: PolarMaxVal 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.000889 0.0008887   1.926  0.193
    ## Residuals   11 0.005075 0.0004613               
    ## 
    ## Dependent var: PolarMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    513   513.0   0.566  0.467
    ## Residuals   11   9964   905.8               
    ## 
    ## Dependent var: Max50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     62    61.8   0.085  0.777
    ## Residuals   11   8031   730.1               
    ## 
    ## Dependent var: Max50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   2311    2311   1.641  0.226
    ## Residuals   11  15489    1408               
    ## 
    ## Dependent var: AnnularMinVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype     1 0.0003708 0.0003708   3.771 0.0782 .
    ## Residuals   11 0.0010818 0.0000983                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   46.0   46.05   1.584  0.234
    ## Residuals   11  319.7   29.06               
    ## 
    ## Dependent var: AnnularMaxVal 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.00002 0.000024   0.001   0.97
    ## Residuals   11 0.17715 0.016105               
    ## 
    ## Dependent var: AnnularMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  1.872   1.872   1.279  0.282
    ## Residuals   11 16.096   1.463               
    ## 
    ## Dependent var: AnnularAvg 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  1.078  1.0779   1.746  0.213
    ## Residuals   11  6.792  0.6174               
    ## 
    ## Dependent var: AnnularSd 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1   76.9   76.90   3.568 0.0856 .
    ## Residuals   11  237.1   21.56                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularSkewnes 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.918  0.9182    1.44  0.255
    ## Residuals   11  7.012  0.6375               
    ## 
    ## Dependent var: AnnularKurtosis 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   56.5   56.54   1.322  0.275
    ## Residuals   11  470.6   42.78               
    ## 
    ## Dependent var: Speed1 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0000433 4.330e-05   0.944  0.352
    ## Residuals   11 0.0005045 4.587e-05               
    ## 
    ## Dependent var: Speed2 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 6.590e-06 6.590e-06   0.263  0.618
    ## Residuals   11 2.759e-04 2.508e-05               
    ## 
    ## Dependent var: Time1stEntrLog 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  3.836   3.836   2.504  0.142
    ## Residuals   11 16.847   1.532

``` r
# . AnnularSd, AnnularMinVal, MaxTimeAvoid 

hist(slim3$AnnularSd)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
boxplot(slim3$AnnularSd ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
summary(aov(slim3$AnnularSd ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1   76.9   76.90   3.568 0.0856 .
    ## Residuals   11  237.1   21.56                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$AnnularSd ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$AnnularSd by Genotype
    ## t = -1.4854, df = 3.7981, p-value = 0.2153
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -15.329371   4.789926
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             11.07778             16.34750

``` r
hist(slim3$AnnularMinVal)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
boxplot(slim3$AnnularMinVal ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-4.png)

``` r
summary(aov(slim3$AnnularMinVal ~ Genotype)) 
```

    ##             Df    Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype     1 0.0003708 0.0003708   3.771 0.0782 .
    ## Residuals   11 0.0010818 0.0000983                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$AnnularMinVal ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$AnnularMinVal by Genotype
    ## t = -1.3096, df = 3.1932, p-value = 0.2766
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03875485  0.01561041
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##          0.003877778          0.015450000

``` r
hist(slim3$MaxTimeAvoid)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-5.png)

``` r
boxplot(slim3$MaxTimeAvoid ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-5-6.png)

``` r
summary(aov(slim3$MaxTimeAvoid ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  59456   59456   4.789 0.0511 .
    ## Residuals   11 136580   12416                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$MaxTimeAvoid ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$MaxTimeAvoid by Genotype
    ## t = 2.1136, df = 5.3987, p-value = 0.08412
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -27.79532 320.85088
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             374.7778             228.2500

habituation genotype
--------------------

``` r
slim1 <- behavior[,c(2,9,8,13:52)]
slim2 <- slim1 %>% filter(TrainSessionCombo == "Hab") 
Genotype <- slim2[,1]
APA <- slim2[,3]
slim3 <- slim2[,c(4:43)]

for(y in names(slim3)){
  ymod<- summary(aov(slim3[[y]] ~ Genotype ))
  cat(paste('\nDependent var:', y, '\n'))
  print(ymod)
}
```

    ## 
    ## Dependent var: SdevSpeedArena 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.017 0.01695   0.094  0.761
    ## Residuals   41  7.432 0.18127               
    ## 
    ## Dependent var: Linearity.Arena. 
    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.00005 0.0000531   0.028  0.869
    ## Residuals   41 0.07851 0.0019150               
    ## 
    ## Dependent var: NumEntrances 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    6.1   6.105   0.215  0.645
    ## Residuals   41 1164.4  28.399               
    ## 
    ## Dependent var: Time1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      3    3.05   0.039  0.845
    ## Residuals   41   3229   78.75               
    ## 
    ## Dependent var: Path1stEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.025 0.02459   0.126  0.724
    ## Residuals   41  7.991 0.19491               
    ## 
    ## Dependent var: Speed1stEntr.cm.s. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    8.0   8.018   0.381   0.54
    ## Residuals   41  861.8  21.021               
    ## 
    ## Dependent var: Dist1stEntr.m. 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0017 0.00173    0.05  0.824
    ## Residuals   41 1.4117 0.03443               
    ## 
    ## Dependent var: NumShock 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      0    0.01       0  0.994
    ## Residuals   41   3572   87.11               
    ## 
    ## Dependent var: MaxTimeAvoid 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      6     5.9   0.011  0.917
    ## Residuals   41  22234   542.3               
    ## 
    ## Dependent var: Time2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     40   40.24   0.516  0.477
    ## Residuals   41   3197   77.98               
    ## 
    ## Dependent var: Path2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.011 0.01096   0.051  0.823
    ## Residuals   41  8.855 0.21597               
    ## 
    ## Dependent var: Speed2ndEntr 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    0.7   0.722   0.046  0.831
    ## Residuals   41  641.8  15.653               
    ## 
    ## Dependent var: TimeTarget 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1     35    34.6   0.105  0.747
    ## Residuals   41  13504   329.4               
    ## 
    ## Dependent var: pTimeTarget 
    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.00040 0.0003954   0.197   0.66
    ## Residuals   41 0.08234 0.0020084               
    ## 
    ## Dependent var: pTimeCCW 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.00008 7.53e-05   0.024  0.877
    ## Residuals   41 0.12628 3.08e-03               
    ## 
    ## Dependent var: pTimeOPP 
    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.00007 0.0000709   0.042  0.838
    ## Residuals   41 0.06882 0.0016785               
    ## 
    ## Dependent var: pTimeCW 
    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.00039 0.0003894   0.148  0.703
    ## Residuals   41 0.10793 0.0026324               
    ## 
    ## Dependent var: RayleigLength 
    ##             Df Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype     1 0.0007 0.000698   0.167  0.685
    ## Residuals   41 0.1716 0.004184               
    ## 
    ## Dependent var: RayleigAngle 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      2       2       0  0.991
    ## Residuals   41 544459   13279               
    ## 
    ## Dependent var: PolarAvgVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      1    1.17   0.009  0.927
    ## Residuals   41   5605  136.70               
    ## 
    ## Dependent var: PolarSdVal 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1      0   0.048   0.002  0.968
    ## Residuals   41   1233  30.080               
    ## 
    ## Dependent var: PolarMinVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 2.000e-08 1.800e-08   0.003  0.956
    ## Residuals   41 2.371e-04 5.782e-06               
    ## 
    ## Dependent var: PolarMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   3175    3175   0.366  0.549
    ## Residuals   41 355741    8677               
    ## 
    ## Dependent var: Min50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1    286     286   0.043  0.838
    ## Residuals   41 275741    6725               
    ## 
    ## Dependent var: Min50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   5612    5612   0.383  0.539
    ## Residuals   41 600248   14640               
    ## 
    ## Dependent var: PolarMaxVal 
    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.0000051 5.140e-06   0.145  0.705
    ## Residuals   41 0.0014535 3.545e-05               
    ## 
    ## Dependent var: PolarMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   6851    6851   0.658  0.422
    ## Residuals   41 426596   10405               
    ## 
    ## Dependent var: Max50.RngLoBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   2977    2977   0.189  0.666
    ## Residuals   41 646386   15766               
    ## 
    ## Dependent var: Max50.RngHiBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   1131    1131   0.135  0.716
    ## Residuals   41 344715    8408               
    ## 
    ## Dependent var: AnnularMinVal 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.000393 0.0003931   0.775  0.384
    ## Residuals   41 0.020798 0.0005073               
    ## 
    ## Dependent var: AnnularMinBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1   6.19   6.190   3.289 0.0771 .
    ## Residuals   41  77.18   1.882                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMaxVal 
    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Genotype     1 0.00696 0.006956   3.179  0.082 .
    ## Residuals   41 0.08971 0.002188                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMaxBin 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   3.14   3.137   1.313  0.258
    ## Residuals   41  97.93   2.389               
    ## 
    ## Dependent var: AnnularAvg 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.126  0.1262    0.43  0.516
    ## Residuals   41 12.027  0.2933               
    ## 
    ## Dependent var: AnnularSd 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.51   0.512   0.126  0.724
    ## Residuals   41 166.16   4.053               
    ## 
    ## Dependent var: AnnularSkewnes 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1 0.0524 0.05239   0.725  0.399
    ## Residuals   41 2.9629 0.07226               
    ## 
    ## Dependent var: AnnularKurtosis 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.02  0.0193   0.017  0.895
    ## Residuals   41  45.18  1.1020               
    ## 
    ## Dependent var: Speed1 
    ##             Df  Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype     1 0.00166 0.0016604   3.471 0.0696 .
    ## Residuals   41 0.01962 0.0004784                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed2 
    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype     1 0.000280 0.0002805   2.456  0.125
    ## Residuals   41 0.004682 0.0001142               
    ## 
    ## Dependent var: Time1stEntrLog 
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   2.17   2.166   1.361   0.25
    ## Residuals   41  65.23   1.591

``` r
# . Speed1, AnnularMinBin 

hist(slim3$Speed1)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
boxplot(slim3$Speed1 ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
summary(aov(slim3$Speed1 ~ Genotype)) 
```

    ##             Df  Sum Sq   Mean Sq F value Pr(>F)  
    ## Genotype     1 0.00166 0.0016604   3.471 0.0696 .
    ## Residuals   41 0.01962 0.0004784                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$Speed1 ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$Speed1 by Genotype
    ## t = 1.8505, df = 37.705, p-value = 0.07209
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.001179299  0.026204878
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##           0.04385850           0.03134571

``` r
hist(slim3$AnnularMinBin)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
boxplot(slim3$AnnularMinBin ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-6-4.png)

``` r
summary(aov(slim3$AnnularMinBin ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1   6.19   6.190   3.289 0.0771 .
    ## Residuals   41  77.18   1.882                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$AnnularMinBin ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$AnnularMinBin by Genotype
    ## t = -2.0041, df = 27.95, p-value = 0.05484
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.54503702  0.01696684
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             15.38333             16.14737

retention genotype
------------------

``` r
slim1 <- behavior[,c(2,9,8,13:52)]
slim2 <- slim1 %>% filter(TrainSessionCombo == "Retention") 
Genotype <- slim2[,1]
APA <- slim2[,3]
slim3 <- slim2[,c(4:43)]

for(y in names(slim3)){
  ymod<- summary(aov(slim3[[y]] ~ Genotype * APA ))
  cat(paste('\nDependent var:', y, '\n'))
  print(ymod)
}
```

    ## 
    ## Dependent var: SdevSpeedArena 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1  0.291  0.2915   1.635 0.2080  
    ## APA           3  1.758  0.5859   3.287 0.0298 *
    ## Genotype:APA  3  0.307  0.1024   0.574 0.6352  
    ## Residuals    42  7.487  0.1783                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Linearity.Arena. 
    ##              Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype      1 0.00033 0.000329   0.062  0.805
    ## APA           3 0.02984 0.009946   1.865  0.150
    ## Genotype:APA  3 0.00814 0.002715   0.509  0.678
    ## Residuals    42 0.22392 0.005331               
    ## 
    ## Dependent var: NumEntrances 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1   33.3    33.3   0.883    0.353    
    ## APA           3 1590.7   530.2  14.037 1.77e-06 ***
    ## Genotype:APA  3   37.4    12.5   0.330    0.803    
    ## Residuals    42 1586.5    37.8                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Time1stEntr 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)    
    ## Genotype      1  16373   16373   0.987 0.32615    
    ## APA           3 333963  111321   6.711 0.00084 ***
    ## Genotype:APA  3  25213    8404   0.507 0.67980    
    ## Residuals    42 696671   16587                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Path1stEntr 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1    4.4    4.40   0.477 0.49371   
    ## APA           3  162.1   54.03   5.851 0.00196 **
    ## Genotype:APA  3    8.8    2.94   0.318 0.81215   
    ## Residuals    42  387.8    9.23                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed1stEntr.cm.s. 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    7.7   7.677   0.552  0.462
    ## APA           3   65.2  21.736   1.563  0.213
    ## Genotype:APA  3   20.8   6.930   0.498  0.686
    ## Residuals    42  584.3  13.911               
    ## 
    ## Dependent var: Dist1stEntr.m. 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1  0.041  0.0409   0.278 0.600486    
    ## APA           3  3.808  1.2694   8.644 0.000139 ***
    ## Genotype:APA  3  0.128  0.0426   0.290 0.832501    
    ## Residuals    42  6.168  0.1469                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: NumShock 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1     94      94   0.241    0.626    
    ## APA           3  12811    4270  10.921 1.97e-05 ***
    ## Genotype:APA  3    377     126   0.321    0.810    
    ## Residuals    42  16423     391                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: MaxTimeAvoid 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1  38313   38313   3.196 0.081054 .  
    ## APA           3 254517   84839   7.076 0.000591 ***
    ## Genotype:APA  3  10341    3447   0.288 0.834141    
    ## Residuals    42 503548   11989                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Time2ndEntr 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1  42088   42088   1.924 0.172740    
    ## APA           3 574685  191562   8.757 0.000125 ***
    ## Genotype:APA  3  28691    9564   0.437 0.727550    
    ## Residuals    42 918788   21876                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Path2ndEntr 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1   12.0   12.02   0.955 0.334118    
    ## APA           3  288.6   96.21   7.641 0.000347 ***
    ## Genotype:APA  3    9.3    3.11   0.247 0.863035    
    ## Residuals    42  528.8   12.59                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed2ndEntr 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1  116.4  116.39  10.577 0.00226 **
    ## APA           3   21.8    7.25   0.659 0.58190   
    ## Genotype:APA  3   40.6   13.55   1.231 0.31040   
    ## Residuals    42  462.2   11.00                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: TimeTarget 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1    171     171   0.110    0.742    
    ## APA           3  49915   16638  10.741 2.29e-05 ***
    ## Genotype:APA  3   1215     405   0.261    0.853    
    ## Residuals    42  65059    1549                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: pTimeTarget 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1 0.0025 0.00249   0.278    0.600    
    ## APA           3 0.3169 0.10563  11.798 9.73e-06 ***
    ## Genotype:APA  3 0.0029 0.00097   0.108    0.955    
    ## Residuals    42 0.3760 0.00895                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: pTimeCCW 
    ##              Df Sum Sq  Mean Sq F value Pr(>F)  
    ## Genotype      1 0.0289 0.028896   2.926 0.0946 .
    ## APA           3 0.0519 0.017302   1.752 0.1711  
    ## Genotype:APA  3 0.0167 0.005554   0.562 0.6429  
    ## Residuals    42 0.4148 0.009876                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: pTimeOPP 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1 0.0156 0.01558   0.915 0.34424   
    ## APA           3 0.3045 0.10149   5.962 0.00176 **
    ## Genotype:APA  3 0.0025 0.00083   0.049 0.98550   
    ## Residuals    42 0.7149 0.01702                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: pTimeCW 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1 0.0090 0.00903   0.777 0.383205    
    ## APA           3 0.3046 0.10153   8.732 0.000128 ***
    ## Genotype:APA  3 0.0130 0.00432   0.372 0.773696    
    ## Residuals    42 0.4883 0.01163                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: RayleigLength 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1 0.0860 0.08600   2.950 0.093226 .  
    ## APA           3 0.6136 0.20453   7.017 0.000626 ***
    ## Genotype:APA  3 0.0737 0.02457   0.843 0.478122    
    ## Residuals    42 1.2243 0.02915                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: RayleigAngle 
    ##              Df Sum Sq Mean Sq F value Pr(>F)   
    ## Genotype      1  34326   34326   6.537 0.0143 * 
    ## APA           3  20704    6901   1.314 0.2824   
    ## Genotype:APA  3  91096   30365   5.783 0.0021 **
    ## Residuals    42 220551    5251                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarAvgVal 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1    159     159   0.253    0.618    
    ## APA           3  27909    9303  14.848 9.83e-07 ***
    ## Genotype:APA  3    509     170   0.271    0.846    
    ## Residuals    42  26316     627                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarSdVal 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1    289   289.1   1.031  0.316  
    ## APA           3   3237  1079.0   3.850  0.016 *
    ## Genotype:APA  3    223    74.4   0.266  0.850  
    ## Residuals    42  11770   280.2                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarMinVal 
    ##              Df    Sum Sq   Mean Sq F value   Pr(>F)    
    ## Genotype      1 0.0000312 3.117e-05   1.620    0.210    
    ## APA           3 0.0005290 1.763e-04   9.162 8.76e-05 ***
    ## Genotype:APA  3 0.0000432 1.441e-05   0.748    0.529    
    ## Residuals    42 0.0008083 1.925e-05                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarMinBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1  11742   11742   0.642  0.427
    ## APA           3  37200   12400   0.678  0.570
    ## Genotype:APA  3  32158   10719   0.586  0.627
    ## Residuals    42 768108   18288               
    ## 
    ## Dependent var: Min50.RngLoBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   1101    1101   0.186 0.6684  
    ## APA           3  70381   23460   3.967 0.0141 *
    ## Genotype:APA  3    932     311   0.053 0.9839  
    ## Residuals    42 248394    5914                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Min50.RngHiBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1  35158   35158   4.405 0.0419 *
    ## APA           3  39477   13159   1.649 0.1926  
    ## Genotype:APA  3  15740    5247   0.657 0.5828  
    ## Residuals    42 335186    7981                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarMaxVal 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)   
    ## Genotype      1 0.000830 0.0008296   2.932 0.0942 . 
    ## APA           3 0.004280 0.0014266   5.041 0.0045 **
    ## Genotype:APA  3 0.000619 0.0002063   0.729 0.5405   
    ## Residuals    42 0.011886 0.0002830                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: PolarMaxBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1    720     720   0.104  0.749
    ## APA           3  45706   15235   2.197  0.103
    ## Genotype:APA  3  42764   14255   2.055  0.121
    ## Residuals    42 291307    6936               
    ## 
    ## Dependent var: Max50.RngLoBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype      1  13760   13760   1.546  0.221
    ## APA           3  24323    8108   0.911  0.444
    ## Genotype:APA  3  10856    3619   0.406  0.749
    ## Residuals    42 373893    8902               
    ## 
    ## Dependent var: Max50.RngHiBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1     21      21   0.003 0.9549  
    ## APA           3  71879   23960   3.787 0.0172 *
    ## Genotype:APA  3   2597     866   0.137 0.9375  
    ## Residuals    42 265754    6327                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMinVal 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.000593 0.0005927   1.832  0.183
    ## APA           3 0.001242 0.0004141   1.280  0.294
    ## Genotype:APA  3 0.001314 0.0004378   1.354  0.270
    ## Residuals    42 0.013585 0.0003235               
    ## 
    ## Dependent var: AnnularMinBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   12.5   12.47   0.398 0.5314  
    ## APA           3  212.3   70.77   2.260 0.0954 .
    ## Genotype:APA  3   62.0   20.66   0.660 0.5814  
    ## Residuals    42 1315.3   31.32                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularMaxVal 
    ##              Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Genotype      1 0.00031 0.000314   0.068  0.795
    ## APA           3 0.02451 0.008171   1.778  0.166
    ## Genotype:APA  3 0.00536 0.001787   0.389  0.762
    ## Residuals    42 0.19306 0.004597               
    ## 
    ## Dependent var: AnnularMaxBin 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1   2.30   2.295   1.144 0.2909  
    ## APA           3  18.55   6.182   3.082 0.0375 *
    ## Genotype:APA  3  11.82   3.940   1.964 0.1340  
    ## Residuals    42  84.24   2.006                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularAvg 
    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype      1   0.48   0.483   0.354 0.55504   
    ## APA           3  19.10   6.366   4.669 0.00665 **
    ## Genotype:APA  3   1.87   0.623   0.457 0.71377   
    ## Residuals    42  57.27   1.364                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularSd 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1    5.2    5.25   0.397 0.531867    
    ## APA           3  284.2   94.74   7.174 0.000539 ***
    ## Genotype:APA  3   10.8    3.60   0.273 0.844825    
    ## Residuals    42  554.7   13.21                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularSkewnes 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1  0.102  0.1023   0.278  0.601  
    ## APA           3  3.654  1.2179   3.312  0.029 *
    ## Genotype:APA  3  0.411  0.1370   0.373  0.773  
    ## Residuals    42 15.443  0.3677                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: AnnularKurtosis 
    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype      1    6.8    6.80   0.663 0.4202  
    ## APA           3   97.1   32.36   3.154 0.0346 *
    ## Genotype:APA  3    3.6    1.20   0.117 0.9497  
    ## Residuals    42  431.0   10.26                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Dependent var: Speed1 
    ##              Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.000016 1.570e-05   0.133  0.717
    ## APA           3 0.000250 8.319e-05   0.704  0.555
    ## Genotype:APA  3 0.000180 5.992e-05   0.507  0.679
    ## Residuals    42 0.004962 1.181e-04               
    ## 
    ## Dependent var: Speed2 
    ##              Df    Sum Sq   Mean Sq F value Pr(>F)
    ## Genotype      1 0.0000507 0.0000507   0.693  0.410
    ## APA           3 0.0003928 0.0001309   1.791  0.164
    ## Genotype:APA  3 0.0000429 0.0000143   0.196  0.899
    ## Residuals    42 0.0030703 0.0000731               
    ## 
    ## Dependent var: Time1stEntrLog 
    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Genotype      1   0.74   0.739   0.387    0.537    
    ## APA           3  65.34  21.779  11.415 1.32e-05 ***
    ## Genotype:APA  3   3.37   1.122   0.588    0.626    
    ## Residuals    42  80.14   1.908                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# ** Speed2ndEntr
# * RayleigAngle 
# . Min50.RngHiBin, RayleigLength, pTimeCCW, MaxTimeAvoid  

hist(slim3$Speed2ndEntr) # bi modal
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
boxplot(slim3$Speed2ndEntr ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
summary(aov(slim3$Speed2ndEntr ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Genotype     1  116.4  116.39   10.65 0.00203 **
    ## Residuals   48  524.6   10.93                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$Speed2ndEntr ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$Speed2ndEntr by Genotype
    ## t = -3.2706, df = 47.962, p-value = 0.001991
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.931379 -1.176441
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             3.369167             6.423077

``` r
hist(slim3$RayleigAngle) # pretty normal!
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
boxplot(slim3$RayleigAngle ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
summary(aov(slim3$RayleigAngle ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  34326   34326   4.957 0.0307 *
    ## Residuals   48 332352    6924                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$RayleigAngle ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$RayleigAngle by Genotype
    ## t = -2.2141, df = 45.824, p-value = 0.03184
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -100.128292   -4.761323
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             151.6225             204.0673

``` r
hist(slim3$Min50.RngHiBin) # not normal
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-5.png)

``` r
boxplot(slim3$Min50.RngHiBin ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-6.png)

``` r
summary(aov(slim3$Min50.RngHiBin ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1  35158   35158   4.323  0.043 *
    ## Residuals   48 390404    8133                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$Min50.RngHiBin ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$Min50.RngHiBin by Genotype
    ## t = 2.0653, df = 45.317, p-value = 0.04464
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##    1.32652 104.82733
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             195.0000             141.9231

``` r
hist(slim3$RayleigLength) # not normal not significant
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-7.png)

``` r
boxplot(slim3$RayleigLength ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-8.png)

``` r
summary(aov(slim3$RayleigLength ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  0.086 0.08600   2.159  0.148
    ## Residuals   48  1.912 0.03982

``` r
t.test(slim3$RayleigLength ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$RayleigLength by Genotype
    ## t = 1.4572, df = 44.374, p-value = 0.1521
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03176933  0.19779498
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##            0.3491667            0.2661538

``` r
hist(slim3$pTimeCCW) # very normal!
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-9.png)

``` r
boxplot(slim3$pTimeCCW ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-10.png)

``` r
summary(aov(slim3$pTimeCCW ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## Genotype     1 0.0289 0.02890   2.869 0.0968 .
    ## Residuals   48 0.4834 0.01007                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t.test(slim3$pTimeCCW ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$pTimeCCW by Genotype
    ## t = -1.6837, df = 45.591, p-value = 0.09908
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.105659616  0.009423078
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##            0.2119125            0.2600308

``` r
hist(slim3$MaxTimeAvoid) # not normal not significant
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-11.png)

``` r
boxplot(slim3$MaxTimeAvoid ~ Genotype)
```

![](01_behavior_files/figure-markdown_github/unnamed-chunk-7-12.png)

``` r
summary(aov(slim3$MaxTimeAvoid ~ Genotype)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  38313   38313   2.393  0.128
    ## Residuals   48 768406   16008

``` r
t.test(slim3$MaxTimeAvoid ~ Genotype)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  slim3$MaxTimeAvoid by Genotype
    ## t = 1.5229, df = 39.624, p-value = 0.1357
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -18.14469 128.95879
    ## sample estimates:
    ##     mean in group WT mean in group FMR1KO 
    ##             205.7917             150.3846
