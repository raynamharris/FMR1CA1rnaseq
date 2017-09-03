``` r
## load libraries 
library(tidyr) ## for respahing data
library(plyr) ## for renmaing factors
library(dplyr) ## for filtering and selecting rows
library(reshape2) ## for melting dataframe
library(ggplot2)
library(cowplot)


## load functions 
source("functions_behavior.R")
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
behavior$APA2 <- revalue(behavior$APA2, c("FMR1" = "FMR1KO")) 


## relevel and/or factors
behavior$APA2 <- factor(behavior$APA2, levels = c("controlconflict", "controlconsistent","conflict", "consistent"))
levels(behavior$APA) <- c("control","consistent","conflict")
levels(behavior$APA2) <- c("yoked-conflict","yoked-consistent","conflict","consistent")
levels(behavior$Genotype) <- c("WT","FMR1KO")



behavior$Time1stEntrLog <- log(behavior$Time1stEntr)  ## log transformation 
behavior <- behavior[c(1,3,7,8,10,14:59)] # supset data
behavior <- subset(behavior, !is.na(behavior$NumEntrances)) # remove nas

behavior$conflict <- ifelse(grepl("conflict", behavior$APA2), "conflict", "consistent") # for splitting
levels(behavior$conflict) <- c("consistent","conflict")
```

``` r
behaviorsummaryNum <- dplyr::summarise(group_by(behavior, Genotype, APA2, conflict, TrainSessionComboNum), m = mean(NumEntrances), se = sd(NumEntrances)/sqrt(length(NumEntrances)), len = length(NumEntrances))
behaviorsummaryNum <- as.data.frame(behaviorsummaryNum)
head(behaviorsummaryNum,50)
```

    ##    Genotype             APA2   conflict TrainSessionComboNum         m
    ## 1        WT   yoked-conflict   conflict                    1 27.666667
    ## 2        WT   yoked-conflict   conflict                    2 14.000000
    ## 3        WT   yoked-conflict   conflict                    3 14.333333
    ## 4        WT   yoked-conflict   conflict                    4 15.000000
    ## 5        WT   yoked-conflict   conflict                    5 11.000000
    ## 6        WT   yoked-conflict   conflict                    6 19.000000
    ## 7        WT   yoked-conflict   conflict                    7 16.000000
    ## 8        WT   yoked-conflict   conflict                    8 16.333333
    ## 9        WT   yoked-conflict   conflict                    9 20.333333
    ## 10       WT yoked-consistent consistent                    1 29.250000
    ## 11       WT yoked-consistent consistent                    2 13.500000
    ## 12       WT yoked-consistent consistent                    3 12.750000
    ## 13       WT yoked-consistent consistent                    4 14.500000
    ## 14       WT yoked-consistent consistent                    5 15.250000
    ## 15       WT yoked-consistent consistent                    6 13.750000
    ## 16       WT yoked-consistent consistent                    7 17.000000
    ## 17       WT yoked-consistent consistent                    8 16.250000
    ## 18       WT yoked-consistent consistent                    9 21.500000
    ## 19       WT         conflict   conflict                    1 29.888889
    ## 20       WT         conflict   conflict                    2  8.111111
    ## 21       WT         conflict   conflict                    3  6.555556
    ## 22       WT         conflict   conflict                    4  4.444444
    ## 23       WT         conflict   conflict                    5  4.222222
    ## 24       WT         conflict   conflict                    6 13.444444
    ## 25       WT         conflict   conflict                    7  6.111111
    ## 26       WT         conflict   conflict                    8  2.888889
    ## 27       WT         conflict   conflict                    9 10.000000
    ## 28       WT       consistent consistent                    1 28.125000
    ## 29       WT       consistent consistent                    2 10.625000
    ## 30       WT       consistent consistent                    3  9.250000
    ## 31       WT       consistent consistent                    4  4.750000
    ## 32       WT       consistent consistent                    5  6.750000
    ## 33       WT       consistent consistent                    6  4.750000
    ## 34       WT       consistent consistent                    7  4.500000
    ## 35       WT       consistent consistent                    8  2.750000
    ## 36       WT       consistent consistent                    9  8.625000
    ## 37   FMR1KO   yoked-conflict   conflict                    1 22.500000
    ## 38   FMR1KO   yoked-conflict   conflict                    2 14.500000
    ## 39   FMR1KO   yoked-conflict   conflict                    3 13.500000
    ## 40   FMR1KO   yoked-conflict   conflict                    4 17.250000
    ## 41   FMR1KO   yoked-conflict   conflict                    5 19.333333
    ## 42   FMR1KO   yoked-conflict   conflict                    6 19.000000
    ## 43   FMR1KO   yoked-conflict   conflict                    7 14.500000
    ## 44   FMR1KO   yoked-conflict   conflict                    8 15.000000
    ## 45   FMR1KO   yoked-conflict   conflict                    9 17.600000
    ## 46   FMR1KO yoked-consistent consistent                    1 30.250000
    ## 47   FMR1KO yoked-consistent consistent                    2  8.666667
    ## 48   FMR1KO yoked-consistent consistent                    3 13.400000
    ## 49   FMR1KO yoked-consistent consistent                    4 18.000000
    ## 50   FMR1KO yoked-consistent consistent                    5 16.333333
    ##           se len
    ## 1  6.9602043   3
    ## 2  4.0000000   3
    ## 3  2.3333333   3
    ## 4  2.8867513   3
    ## 5  3.7859389   3
    ## 6  2.6457513   3
    ## 7  5.0000000   2
    ## 8  1.6666667   3
    ## 9  1.4529663   3
    ## 10 1.9311050   4
    ## 11 2.3979158   4
    ## 12 3.9024565   4
    ## 13 1.9364917   4
    ## 14 2.3228933   4
    ## 15 2.2500000   4
    ## 16 2.3452079   4
    ## 17 3.4247871   4
    ## 18 1.2583057   4
    ## 19 1.3275337   9
    ## 20 1.0599324   9
    ## 21 1.7409377   9
    ## 22 1.0289033   9
    ## 23 1.1276546   9
    ## 24 2.1991862   9
    ## 25 1.6786605   9
    ## 26 0.9345891   9
    ## 27 2.7284509   9
    ## 28 1.6084320   8
    ## 29 1.0680005   8
    ## 30 1.7191152   8
    ## 31 0.8813545   8
    ## 32 1.9341850   8
    ## 33 1.8002976   8
    ## 34 1.9910514   8
    ## 35 1.4236522   8
    ## 36 2.4123011   8
    ## 37 2.5980762   4
    ## 38 1.5545632   4
    ## 39 1.2583057   4
    ## 40 1.8874586   4
    ## 41 4.8074017   3
    ## 42 2.9439203   4
    ## 43 2.1794495   4
    ## 44 1.0000000   3
    ## 45 2.0639767   5
    ## 46 1.7500000   4
    ## 47 1.2018504   3
    ## 48 1.1661904   5
    ## 49 2.0816660   3
    ## 50 3.8441875   3

``` r
levels(behaviorsummaryNum$Genotype) <- c("WT","FMR1KO")
levels(behaviorsummaryNum$APA2) <- c("yoked-conflict","yoked-consistent","conflict","consistent")
levels(behaviorsummaryNum$conflict) <- c("consistent","conflict")
```

``` r
# plotting mean and se for time to total number of entrances
numentrance1WT <- behaviorsummaryNum %>%
  filter(Genotype == "WT") %>%
  ggplot(aes(x=, TrainSessionComboNum, y=m, color=APA2)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
    geom_point(size = 2) +
   geom_line() +
    scale_y_continuous(name="Number of Entrances\nper 10 min training session") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest", "6" = "T4/C1",
                                "7" = "T5/C2", "8" = "T6/C3", "9"= "Retention")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  background_grid(major = "y", minor = "y") +
  scale_color_manual(values = colorvalAPA2) + 
  #theme(legend.position=c(0.7, 0.8))  +
  theme(legend.position="none")
numentrance1WT
```

![](01_behavior_files/figure-markdown_github/numentrance-1.png)

``` r
pdf(file="../figures/01_behavior/numentrance1WT.pdf", width=4, height=2)
plot(numentrance1WT)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# plotting mean and se for time to total number of entrances
numentrance1FMR1 <- behaviorsummaryNum %>%
  filter(Genotype == "FMR1KO") %>%
  ggplot(aes(x=, TrainSessionComboNum, y=m, color=APA2)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
    geom_point(size = 2) +
   geom_line() +
    scale_y_continuous(name="Number of Entrances\nper 10 min training session") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest", "6" = "T4/C1",
                                "7" = "T5/C2", "8" = "T6/C3", "9"= "Retention")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  background_grid(major = "y", minor = "y") +
  scale_color_manual(values = colorvalAPA2) + 
  #theme(legend.position=c(0.7, 0.8))  +
  theme(legend.position="none")  
numentrance1FMR1
```

![](01_behavior_files/figure-markdown_github/numentrance-2.png)

``` r
pdf(file="../figures/01_behavior/numentrance1FMR1.pdf", width=4, height=2)
plot(numentrance1FMR1)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
numentrance1wrap <- behaviorsummaryNum %>%
  ggplot(aes(x=, TrainSessionComboNum, y=m, color=APA2, shape = Genotype)) + 
  facet_wrap(~conflict,nrow =2) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
    geom_point(size = 3) +
   geom_line() +
    scale_y_continuous(name="Number of Entrances\nper 10 min training session") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest", "6" = "T4/C1",
                                "7" = "T5/C2", "8" = "T6/C3", "9"= "Retention")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  background_grid(major = "y", minor = "y") +
  scale_color_manual(values = colorvalAPA2) + 
  #theme(legend.position=c(0.7, 0.8))  +
  theme(legend.position="none")
numentrance1wrap
```

![](01_behavior_files/figure-markdown_github/numentrance-3.png)

``` r
pdf(file="../figures/01_behavior/numentrance1wrap.pdf", width=4, height=4)
plot(numentrance1wrap)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
numentrance1consistent<- behaviorsummaryNum %>%
  filter(conflict == "consistent") %>%
  ggplot(aes(x=, TrainSessionComboNum, y=m, color=APA2, shape=Genotype)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
    geom_point(size = 2) +
   geom_line() +
    scale_y_continuous(name="Number of Entrances\nper 10 min training session") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest", "6" = "T4/C1",
                                "7" = "T5/C2", "8" = "T6/C3", "9"= "Retention")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  background_grid(major = "y", minor = "y") +
  scale_color_manual(values = colorvalAPA5) + 
  #theme(legend.position=c(0.7, 0.8))  +
  theme(legend.position="none")
numentrance1consistent
```

![](01_behavior_files/figure-markdown_github/numentrance-4.png)

``` r
pdf(file="../figures/01_behavior/numentrance1consistent.pdf", width=4, height=2)
plot(numentrance1consistent)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
numentrance1conflict<- behaviorsummaryNum %>%
  filter(conflict == "conflict") %>%
  ggplot(aes(x=, TrainSessionComboNum, y=m, color=APA2, shape=Genotype)) + 
    geom_errorbar(aes(ymin=m-se, ymax=m+se, color=APA2), width=.1) +
    geom_point(size = 2) +
   geom_line() +
    scale_y_continuous(name="Number of Entrances\nper 10 min training session") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels=c("1" = "Habituation ", "2" = "T1", "3" = "T2", 
                                "4" = "T3", "5" = "Retest", "6" = "T4/C1",
                                "7" = "T5/C2", "8" = "T6/C3", "9"= "Retention")) +
  theme_cowplot(font_size = 8, line_size = 0.25) +
  background_grid(major = "y", minor = "y") +
  scale_color_manual(values = colorvalAPA4) + 
  #theme(legend.position=c(0.7, 0.8))  +
  theme(legend.position="none")
numentrance1conflict
```

![](01_behavior_files/figure-markdown_github/numentrance-5.png)

``` r
pdf(file="../figures/01_behavior/numentrance1conflict.pdf", width=4, height=2)
plot(numentrance1conflict)
dev.off()
```

    ## quartz_off_screen 
    ##                 2
