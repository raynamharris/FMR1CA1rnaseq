Experimental design
===================

![Active place avoidance task with conflict
training.](../figures/fig1-01.png)

##### Mice were assigned to one of four groups: consistently-trained (red), yoked-consistent (dark grey), conflict-trained (peach), or yoked-conflict (light grey). Mice were placed on the rotating arena (1 rpm) for training sessions that lasted 10 min and was separated by 2-h intersession interval or overnight (~17 h). The physical conditions are identical for all mice during pre-training and retention. Sample sizes for each treatment group and genotype are shown on the bottom right.

    ## [1] "yoked-consistent" "consistent"       "yoked-conflict"  
    ## [4] "conflict"

    ##                APA2     Genotype 
    ##  yoked-consistent:4   WT    :24  
    ##  consistent      :8   FMR1KO: 0  
    ##  yoked-conflict  :3              
    ##  conflict        :9

    ##                APA2     Genotype 
    ##  yoked-consistent:7   WT    : 0  
    ##  consistent      :9   FMR1KO:26  
    ##  yoked-conflict  :5              
    ##  conflict        :5

![Figure 2: No group differences prior to behavioral
manipulation.](../figures/fig1-04.png)

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "1") 
    summary(aov(pTimeTarget ~  APA2 + Genotype, data=PathNumStats))

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## APA2         3 0.00315 0.001049   0.507  0.680
    ## Genotype     1 0.00091 0.000906   0.438  0.512
    ## Residuals   38 0.07868 0.002071

    summary(aov(pTimeOPP ~  APA2 + Genotype, data=PathNumStats))

    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## APA2         3 0.00528 0.0017596   1.051  0.381
    ## Genotype     1 0.00001 0.0000134   0.008  0.929
    ## Residuals   38 0.06360 0.0016736

    summary(aov(pTimeCW ~  APA2 + Genotype, data=PathNumStats))

    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## APA2         3 0.00415 0.0013839   0.507  0.680
    ## Genotype     1 0.00042 0.0004178   0.153  0.698
    ## Residuals   38 0.10375 0.0027302

    summary(aov(pTimeCCW ~  APA2 + Genotype, data=PathNumStats))

    ##             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## APA2         3 0.00906 0.0030208   0.979  0.413
    ## Genotype     1 0.00004 0.0000374   0.012  0.913
    ## Residuals   38 0.11725 0.0030856

    Anova(lm(data = PathNumStats, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                 Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   0.214554  1 96.5179 1.345e-11 ***
    ## Genotype      0.000011  1  0.0048    0.9454    
    ## APA2          0.001534  3  0.2301    0.8748    
    ## Genotype:APA2 0.000881  3  0.1322    0.9403    
    ## Residuals     0.077803 35                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    mean(PathNumStats$pTimeTarget)

    ## [1] 0.2447605

    mean(PathNumStats$pTimeOPP)

    ## [1] 0.2150884

    mean(PathNumStats$pTimeCW)

    ## [1] 0.2643767

    mean(PathNumStats$pTimeCCW)

    ## [1] 0.2757767

    summary(aov(NumEntrances ~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## APA2           3  133.4   44.48   1.717  0.181
    ## Genotype       1    2.7    2.74   0.106  0.747
    ## APA2:Genotype  3  127.6   42.55   1.643  0.197
    ## Residuals     35  906.6   25.90

    summary(aov(Path1stEntr~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## APA2           3  0.943 0.31429   1.583  0.211
    ## Genotype       1  0.028 0.02777   0.140  0.711
    ## APA2:Genotype  3  0.098 0.03277   0.165  0.919
    ## Residuals     35  6.947 0.19848

    mean(PathNumStats$NumEntrances)

    ## [1] 28.5814

    mean(PathNumStats$Path1stEntr)

    ## [1] 0.4237209

    summary(aov(value ~  APA2 * Genotype + variable, data=proptime))

    ##                 Df Sum Sq Mean Sq F value Pr(>F)    
    ## APA2             3   0.00   0.000     0.0      1    
    ## Genotype         1   0.00   0.000     0.0      1    
    ## variable         3  13.62   4.541   224.5 <2e-16 ***
    ## APA2:Genotype    3   0.00   0.000     0.0      1    
    ## Residuals     1537  31.09   0.020                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

![](../figures/fig1-06.png)

![Figure 2.4](../figures/fig1-07.png)

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("2", "3", "4", "5" ,"6", "7", "8")) 
    Anova(lm(data = timespent, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                Sum Sq  Df  F value Pr(>F)    
    ## (Intercept)   1.36256   1 412.3498 <2e-16 ***
    ## Genotype      0.00493   1   1.4909 0.2231    
    ## APA2          1.27461   3 128.5783 <2e-16 ***
    ## Genotype:APA2 0.00260   3   0.2627 0.8523    
    ## Residuals     0.94505 286                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("2", "3", "4", "5" ,"6", "7", "8")) %>%
      filter(APA2 %in% c("yoked-consistent", "yoked-conflict"))
    Anova(lm(data = timespent, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                Sum Sq Df  F value Pr(>F)    
    ## (Intercept)   1.36256  1 191.1769 <2e-16 ***
    ## Genotype      0.00493  1   0.6912 0.4077    
    ## APA2          0.00094  1   0.1320 0.7171    
    ## Genotype:APA2 0.00135  1   0.1896 0.6642    
    ## Residuals     0.70559 99                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("2", "3", "4")) %>%
      filter(APA2 %in% c("consistent", "conflict"))
    summary(aov(data =  timespent, pTimeTarget ~ Genotype * APA2 ))

    ##               Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Genotype       1 0.00009 0.000085   0.040 0.8414  
    ## APA2           1 0.00729 0.007289   3.438 0.0674 .
    ## Genotype:APA2  1 0.00038 0.000385   0.182 0.6712  
    ## Residuals     80 0.16959 0.002120                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                 Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   0.048546  1 22.9009 7.678e-06 ***
    ## Genotype      0.000269  1  0.1268   0.72272    
    ## APA2          0.006319  1  2.9809   0.08811 .  
    ## Genotype:APA2 0.000385  1  0.1816   0.67117    
    ## Residuals     0.169586 80                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6", "7", "8")) %>%
      filter(APA2 %in% c("consistent", "conflict"))
    Anova(lm(data = timespent, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                 Sum Sq Df F value   Pr(>F)   
    ## (Intercept)   0.004166  1  7.5179 0.007573 **
    ## Genotype      0.001086  1  1.9598 0.165496   
    ## APA2          0.000674  1  1.2166 0.273413   
    ## Genotype:APA2 0.000331  1  0.5973 0.441959   
    ## Residuals     0.043222 78                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, pTimeCW ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeCW
    ##                Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   0.18035  1  7.8726  0.006334 ** 
    ## Genotype      0.00610  1  0.2661  0.607424    
    ## APA2          0.53471  1 23.3405 6.651e-06 ***
    ## Genotype:APA2 0.00061  1  0.0267  0.870690    
    ## Residuals     1.78689 78                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, pTimeCCW ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeCCW
    ##               Sum Sq Df  F value    Pr(>F)    
    ## (Intercept)   3.4318  1 131.5911 < 2.2e-16 ***
    ## Genotype      0.0667  1   2.5578  0.113793    
    ## APA2          0.2160  1   8.2837  0.005159 ** 
    ## Genotype:APA2 0.0668  1   2.5604  0.113612    
    ## Residuals     2.0342 78                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, pTimeOPP ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeOPP
    ##               Sum Sq Df  F value  Pr(>F)    
    ## (Intercept)   6.5392  1 227.5348 < 2e-16 ***
    ## Genotype      0.1363  1   4.7442 0.03242 *  
    ## APA2          0.0854  1   2.9729 0.08863 .  
    ## Genotype:APA2 0.0634  1   2.2059 0.14152    
    ## Residuals     2.2417 78                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    mean(timespent$pTimeTarget)

    ## [1] 0.01923171

    mean(timespent$pTimeOPP)

    ## [1] 0.4571878

    mean(timespent$pTimeCW)

    ## [1] 0.1949951

    mean(timespent$pTimeCCW)

    ## [1] 0.3285878

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6", "7", "8")) %>%
      filter(APA2 %in% c("conflict"))

    mean(timespent$pTimeTarget)

    ## [1] 0.02097949

    mean(timespent$pTimeOPP)

    ## [1] 0.4409872

    mean(timespent$pTimeCW)

    ## [1] 0.3027308

    mean(timespent$pTimeCCW)

    ## [1] 0.2353128

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6", "7", "8")) %>%
      filter(APA2 %in% c("consistent"))

    mean(timespent$pTimeTarget)

    ## [1] 0.01764651

    mean(timespent$pTimeOPP)

    ## [1] 0.4718814

    mean(timespent$pTimeCW)

    ## [1] 0.0972814

    mean(timespent$pTimeCCW)

    ## [1] 0.413186

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6", "7", "8")) %>%
      filter(Genotype %in% c("FMR1KO"))
    mean(timespent$pTimeOPP)

    ## [1] 0.3438088

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6", "7", "8")) %>%
      filter(Genotype %in% c("WT"))
    mean(timespent$pTimeOPP)

    ## [1] 0.4225437

    ##                Df Sum Sq Mean Sq F value Pr(>F)    
    ## APA2            3 2.6064  0.8688 262.921 <2e-16 ***
    ## Genotype        1 0.0031  0.0031   0.952  0.330    
    ## APA2:Genotype   3 0.0026  0.0009   0.263  0.852    
    ## Residuals     286 0.9451  0.0033                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                Df Sum Sq Mean Sq F value Pr(>F)    
    ## APA2            3  2.354  0.7847  39.138 <2e-16 ***
    ## Genotype        1  0.034  0.0344   1.717  0.191    
    ## APA2:Genotype   3  0.004  0.0013   0.067  0.977    
    ## Residuals     286  5.734  0.0200                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                Df Sum Sq Mean Sq F value Pr(>F)    
    ## APA2            3 2.6064  0.8688 262.921 <2e-16 ***
    ## Genotype        1 0.0031  0.0031   0.952  0.330    
    ## APA2:Genotype   3 0.0026  0.0009   0.263  0.852    
    ## Residuals     286 0.9451  0.0033                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2            3  1.143  0.3809  16.824 4.31e-10 ***
    ## Genotype        1  0.001  0.0012   0.053    0.818    
    ## APA2:Genotype   3  0.007  0.0023   0.100    0.960    
    ## Residuals     286  6.475  0.0226                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

![Fig. 2.5.Summary of punishment and estimates of memory in WT and
FMR1-KO mice](../figures/fig1-02.png)

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("5")) 
    Anova(lm(data = timespent, Path1stEntr ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##               Sum Sq Df F value    Pr(>F)    
    ## (Intercept)     0.70  1  0.0417 0.8395866    
    ## Genotype      230.91  1 13.8076 0.0008284 ***
    ## APA2           44.18  3  0.8807 0.4621771    
    ## Genotype:APA2 185.98  3  3.7071 0.0221765 *  
    ## Residuals     501.69 30                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##               Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   930.25  1 29.0520 7.745e-06 ***
    ## Genotype        2.01  1  0.0628   0.80378    
    ## APA2          376.69  3  3.9214   0.01787 *  
    ## Genotype:APA2 103.13  3  1.0736   0.37508    
    ## Residuals     960.61 30                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("2"))  %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 729.00  1 47.0681 1.148e-06 ***
    ## APA2        124.07  3  2.6702   0.07524 .  
    ## Residuals   309.76 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 0.80102  1   7.093 0.01493 *
    ## APA2        0.60102  3   1.774 0.18451  
    ## Residuals   2.25863 20                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("3"))  %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 650.25  1 21.7062 0.0001511 ***
    ## APA2        190.82  3  2.1233 0.1292644    
    ## Residuals   599.14 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   3.240  1  0.3678 0.5510
    ## APA2          6.643  3  0.2514 0.8594
    ## Residuals   176.175 20

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("4")) %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 841.00  1  78.334 2.364e-08 ***
    ## APA2        509.24  3  15.811 1.666e-05 ***
    ## Residuals   214.72 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   2.772  1  0.5469 0.4682
    ## APA2          8.346  3  0.5488 0.6547
    ## Residuals   101.378 20

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("5"))  %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   0.697  1  0.0776 0.7834
    ## APA2         44.182  3  1.6392 0.2121
    ## Residuals   179.695 20

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 930.25  1 41.1792 2.931e-06 ***
    ## APA2        376.69  3  5.5584  0.006105 ** 
    ## Residuals   451.81 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6"))  %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   5.476  1  0.6956 0.4141
    ## APA2         19.785  3  0.8378 0.4890
    ## Residuals   157.444 20

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value   Pr(>F)    
    ## (Intercept) 756.25  1 23.9141 8.85e-05 ***
    ## APA2        586.49  3  6.1819 0.003801 ** 
    ## Residuals   632.47 20                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("7")) %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value  Pr(>F)  
    ## (Intercept)   4.623  1  0.3647 0.55305  
    ## APA2         95.511  3  2.5118 0.08944 .
    ## Residuals   240.823 19                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 1156.00  1 40.6072 4.109e-06 ***
    ## APA2         579.98  3  6.7911  0.002672 ** 
    ## Residuals    540.89 19                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("8")) %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept)   1.24  1  0.0500 0.82541  
    ## APA2        189.01  3  2.5314 0.08615 .
    ## Residuals   497.77 20                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 1056.25  1  63.285 1.271e-07 ***
    ## APA2         898.69  3  17.948 6.821e-06 ***
    ## Residuals    333.81 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("9")) %>%
      filter(Genotype == "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value  Pr(>F)  
    ## (Intercept)   0.325  1  0.0278 0.86937  
    ## APA2        100.775  3  2.8691 0.06213 .
    ## Residuals   234.159 20                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 1849.00  1 41.3859 2.832e-06 ***
    ## APA2         682.96  3  5.0955  0.008804 ** 
    ## Residuals    893.54 20                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("2"))  %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 225.333  1 27.6408 0.0001212 ***
    ## APA2         79.869  3  3.2657 0.0532724 .  
    ## Residuals   114.131 14                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   1.104  1  0.1017 0.7545
    ## APA2         33.294  3  1.0225 0.4124
    ## Residuals   151.956 14

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("3"))  %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 897.80  1  126.22 5.312e-09 ***
    ## APA2        261.19  3   12.24 0.0002053 ***
    ## Residuals   113.81 16                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 163.82  1  8.2359 0.01112 *
    ## APA2        120.06  3  2.0119 0.15287  
    ## Residuals   318.26 16                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("4")) %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value   Pr(>F)    
    ## (Intercept) 972.00  1 47.5211  7.4e-06 ***
    ## APA2        510.59  3  8.3209 0.002011 ** 
    ## Residuals   286.36 14                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value Pr(>F)
    ## (Intercept)   4.014  1  0.1881 0.6711
    ## APA2         32.439  3  0.5067 0.6839
    ## Residuals   298.742 14

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("5"))  %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value   Pr(>F)   
    ## (Intercept) 433.68  1 13.4684 0.004318 **
    ## APA2        171.80  3  1.7785 0.214662   
    ## Residuals   322.00 10                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value  Pr(>F)   
    ## (Intercept) 800.33  1 15.7298 0.00266 **
    ## APA2        502.13  3  3.2896 0.06650 . 
    ## Residuals   508.80 10                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("6"))  %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 248.51  1  7.2444 0.01605 *
    ## APA2        154.92  3  1.5053 0.25123  
    ## Residuals   548.87 16                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 1248.20  1 57.5160 1.101e-06 ***
    ## APA2         505.72  3  7.7677  0.002011 ** 
    ## Residuals    347.23 16                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("7")) %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 159.39  1  5.9686 0.02741 *
    ## APA2         53.53  3  0.6682 0.58456  
    ## Residuals   400.56 15                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##              Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 1411.20  1 41.1389 1.165e-05 ***
    ## APA2         340.08  3  3.3047   0.04928 *  
    ## Residuals    514.55 15                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("8")) %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 170.18  1  4.5522 0.05105 .
    ## APA2         57.77  3  0.5151 0.67855  
    ## Residuals   523.37 14                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 845.00  1 41.0052 1.645e-05 ***
    ## APA2        334.44  3  5.4098   0.01106 *  
    ## Residuals   288.50 14                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    timespent <- behavior %>%
        filter(TrainSessionComboNum %in% c("9")) %>%
      filter(Genotype != "WT")
    Anova(lm(data = timespent, Path1stEntr ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value  Pr(>F)  
    ## (Intercept)   7.406  1  1.0602 0.31436  
    ## APA2         70.141  3  3.3470 0.03757 *
    ## Residuals   153.679 22                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = timespent, NumEntrances ~ APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##             Sum Sq Df F value    Pr(>F)    
    ## (Intercept) 3611.6  1 114.656  3.43e-10 ***
    ## APA2         945.2  3  10.002 0.0002349 ***
    ## Residuals    693.0 22                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

![Fig. 2.6.Retention mice](../figures/fig1-09.png)

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "9") 
    summary(aov(pTimeTarget ~  APA2 + Genotype, data=PathNumStats))

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2         3 0.3168 0.10561  12.542 4.34e-06 ***
    ## Genotype     1 0.0025 0.00253   0.301    0.586    
    ## Residuals   45 0.3789 0.00842                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(pTimeOPP ~  APA2 + Genotype, data=PathNumStats))

    ##             Df Sum Sq Mean Sq F value  Pr(>F)    
    ## APA2         3 0.3152 0.10505   6.589 0.00087 ***
    ## Genotype     1 0.0049 0.00489   0.307 0.58228    
    ## Residuals   45 0.7174 0.01594                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(pTimeCW ~  APA2 + Genotype, data=PathNumStats))

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2         3 0.3136 0.10452   9.382 6.24e-05 ***
    ## Genotype     1 0.0000 0.00005   0.004    0.948    
    ## Residuals   45 0.5013 0.01114                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(pTimeCCW ~  APA2 + Genotype, data=PathNumStats))

    ##             Df Sum Sq  Mean Sq F value Pr(>F)  
    ## APA2         3 0.0646 0.021530   2.246 0.0959 .
    ## Genotype     1 0.0162 0.016211   1.691 0.2001  
    ## Residuals   45 0.4315 0.009588                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = PathNumStats, pTimeTarget ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   0.38057  1 42.5068 7.099e-08 ***
    ## Genotype      0.00038  1  0.0430  0.836796    
    ## APA2          0.14885  3  5.5420  0.002685 ** 
    ## Genotype:APA2 0.00291  3  0.1083  0.954798    
    ## Residuals     0.37603 42                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    mean(PathNumStats$pTimeTarget)

    ## [1] 0.163058

    mean(PathNumStats$pTimeOPP)

    ## [1] 0.325012

    mean(PathNumStats$pTimeCW)

    ## [1] 0.274992

    mean(PathNumStats$pTimeCCW)

    ## [1] 0.236934

    summary(aov(NumEntrances ~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2           3 1622.5   540.8   14.32 1.44e-06 ***
    ## Genotype       1    1.5     1.5    0.04    0.843    
    ## APA2:Genotype  3   37.4    12.5    0.33    0.803    
    ## Residuals     42 1586.5    37.8                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(Path1stEntr~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## APA2           3  166.3   55.43   6.002 0.00169 **
    ## Genotype       1    0.2    0.23   0.025 0.87598   
    ## APA2:Genotype  3    8.8    2.94   0.318 0.81215   
    ## Residuals     42  387.8    9.23                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    mean(PathNumStats$NumEntrances)

    ## [1] 13.6

    mean(PathNumStats$Path1stEntr)

    ## [1] 2.832

    summary(aov(value ~  APA2 * Genotype + variable, data=proptime))

    ##                 Df Sum Sq Mean Sq F value Pr(>F)    
    ## APA2             3   0.00   0.000     0.0      1    
    ## Genotype         1   0.00   0.000     0.0      1    
    ## variable         3  13.62   4.541   224.5 <2e-16 ***
    ## APA2:Genotype    3   0.00   0.000     0.0      1    
    ## Residuals     1537  31.09   0.020                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    PathNumStats <- behavior  %>% 
     filter(TrainSessionComboNum %in% c("9"))  %>% 
      filter(APA2 %in% c("yoked-consisent", "yoked-conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 18.625

    mean(PathNumStats$Path1stEntr)

    ## [1] 0.40625

    PathNumStats <- behavior  %>% 
       filter(TrainSessionComboNum %in% c("9"))  %>%
      filter(APA2 %in% c("consisent", "conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 9.214286

    mean(PathNumStats$Path1stEntr)

    ## [1] 3.442857

    num9 <- behavior %>%
        filter(TrainSessionComboNum %in% c("9")) %>% 
      ggplot(aes(x = as.numeric(TrainSessionComboNum), y = NumEntrances, fill=APA2)) +
      geom_boxplot(outlier.size=0.8, lwd=0.5) +
      facet_wrap(~Genotype) +
      scale_fill_manual(values = colorvalAPA00) +  
     scale_x_continuous(name=NULL, 
                           breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                           labels = NULL) +
        background_grid(major = "y", minor = "none") +
       scale_y_continuous(name = "Retention number of entrances",
                         limits = c(0,35)) +
      geom_hline(yintercept=c(9.21), color="red" , linetype = "dashed") + 
        geom_hline(yintercept=c(18.65), color="black", linetype = "dashed" ) + 

      theme_cowplot(font_size = 8, line_size = 0.25) + 
      theme(legend.position="none") +
        background_grid(major = "y", minor = "none") 

    path9 <- behavior %>%
        filter(TrainSessionComboNum %in% c("9")) %>% 
      ggplot(aes(x = as.numeric(TrainSessionComboNum), y = Path1stEntr, fill=APA2)) +
      geom_boxplot(outlier.size=0.8, lwd=0.5) +
      facet_wrap(~Genotype) +
      scale_fill_manual(values = colorvalAPA00) +  
     scale_x_continuous(name=NULL, 
                           breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                           labels = NULL) +
       scale_y_continuous(name = "Retention path to 1st entrance") +
      geom_hline(yintercept=c(0.41), color="black" , linetype = "dashed") + 
        geom_hline(yintercept=c(3.44), color="red", linetype = "dashed" ) + 

     
      theme_cowplot(font_size = 8, line_size = 0.25) + 
      theme(legend.position="none") +
        background_grid(major = "y", minor = "none") 

    pdf(file="../figures/01_behavior/num9.pdf", width=1.75, height=1.9)
    plot(num9)
    dev.off()

    ## quartz_off_screen 
    ##                 2

    pdf(file="../figures/01_behavior/path9.pdf", width=1.75, height=1.9)
    plot(path9)
    dev.off()

    ## quartz_off_screen 
    ##                 2
