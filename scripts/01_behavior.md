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

    ## pretraining

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

    ## conflict
    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "6") 
    Anova(lm(data = PathNumStats, NumEntrances ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##               Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   756.25  1 27.7891 6.538e-06 ***
    ## Genotype        9.34  1  0.3432 0.5616623    
    ## APA2          586.49  3  7.1837 0.0006702 ***
    ## Genotype:APA2  41.72  3  0.5110 0.6772764    
    ## Residuals     979.70 36                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(NumEntrances ~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2           3 1056.3   352.1  12.939 6.89e-06 ***
    ## Genotype       1    0.4     0.4   0.016    0.900    
    ## APA2:Genotype  3   41.7    13.9   0.511    0.677    
    ## Residuals     36  979.7    27.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    TukeyHSD(aov(NumEntrances~  APA2 * Genotype, data=PathNumStats))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = NumEntrances ~ APA2 * Genotype, data = PathNumStats)
    ## 
    ## $APA2
    ##                                      diff        lwr        upr     p adj
    ## consistent-yoked-consistent     -9.422222 -15.346115 -3.4983293 0.0007251
    ## yoked-conflict-yoked-consistent  4.111111  -2.969295 11.1915175 0.4115373
    ## conflict-yoked-consistent       -2.350427  -8.442806  3.7419511 0.7278856
    ## yoked-conflict-consistent       13.533333   7.102235 19.9644318 0.0000112
    ## conflict-consistent              7.071795   1.747891 12.3956984 0.0053552
    ## conflict-yoked-conflict         -6.461538 -13.048161  0.1250839 0.0561732
    ## 
    ## $Genotype
    ##                diff       lwr      upr     p adj
    ## FMR1KO-WT 0.1957123 -3.007526 3.398951 0.9020735
    ## 
    ## $`APA2:Genotype`
    ##                                                        diff         lwr
    ## consistent:WT-yoked-consistent:WT             -9.000000e+00 -19.2719551
    ## yoked-conflict:WT-yoked-consistent:WT          5.250000e+00  -7.5613788
    ## conflict:WT-yoked-consistent:WT               -3.055556e-01 -10.3854945
    ## yoked-consistent:FMR1KO-yoked-consistent:WT    2.050000e+00  -9.2023630
    ## consistent:FMR1KO-yoked-consistent:WT         -7.464286e+00 -17.9779669
    ## yoked-conflict:FMR1KO-yoked-consistent:WT      5.250000e+00  -6.6110320
    ## conflict:FMR1KO-yoked-consistent:WT           -3.250000e+00 -15.1110320
    ## yoked-conflict:WT-consistent:WT                1.425000e+01   2.8939264
    ## conflict:WT-consistent:WT                      8.694444e+00   0.5437298
    ## yoked-consistent:FMR1KO-consistent:WT          1.105000e+01   1.4873303
    ## consistent:FMR1KO-consistent:WT                1.535714e+00  -7.1456722
    ## yoked-conflict:FMR1KO-consistent:WT            1.425000e+01   3.9780449
    ## conflict:FMR1KO-consistent:WT                  5.750000e+00  -4.5219551
    ## conflict:WT-yoked-conflict:WT                 -5.555556e+00 -16.7382438
    ## yoked-consistent:FMR1KO-yoked-conflict:WT     -3.200000e+00 -15.4500212
    ## consistent:FMR1KO-yoked-conflict:WT           -1.271429e+01 -24.2894677
    ## yoked-conflict:FMR1KO-yoked-conflict:WT       -3.552714e-15 -12.8113788
    ## conflict:FMR1KO-yoked-conflict:WT             -8.500000e+00 -21.3113788
    ## yoked-consistent:FMR1KO-conflict:WT            2.355556e+00  -7.0005527
    ## consistent:FMR1KO-conflict:WT                 -7.158730e+00 -15.6120479
    ## yoked-conflict:FMR1KO-conflict:WT              5.555556e+00  -4.5243834
    ## conflict:FMR1KO-conflict:WT                   -2.944444e+00 -13.0243834
    ## consistent:FMR1KO-yoked-consistent:FMR1KO     -9.514286e+00 -19.3361534
    ## yoked-conflict:FMR1KO-yoked-consistent:FMR1KO  3.200000e+00  -8.0523630
    ## conflict:FMR1KO-yoked-consistent:FMR1KO       -5.300000e+00 -16.5523630
    ## yoked-conflict:FMR1KO-consistent:FMR1KO        1.271429e+01   2.2006045
    ## conflict:FMR1KO-consistent:FMR1KO              4.214286e+00  -6.2993955
    ## conflict:FMR1KO-yoked-conflict:FMR1KO         -8.500000e+00 -20.3610320
    ##                                                      upr     p adj
    ## consistent:WT-yoked-consistent:WT              1.2719551 0.1222755
    ## yoked-conflict:WT-yoked-consistent:WT         18.0613788 0.8861219
    ## conflict:WT-yoked-consistent:WT                9.7743834 1.0000000
    ## yoked-consistent:FMR1KO-yoked-consistent:WT   13.3023630 0.9988551
    ## consistent:FMR1KO-yoked-consistent:WT          3.0493955 0.3302891
    ## yoked-conflict:FMR1KO-yoked-consistent:WT     17.1110320 0.8405486
    ## conflict:FMR1KO-yoked-consistent:WT            8.6110320 0.9859926
    ## yoked-conflict:WT-consistent:WT               25.6060736 0.0059740
    ## conflict:WT-consistent:WT                     16.8451591 0.0295954
    ## yoked-consistent:FMR1KO-consistent:WT         20.6126697 0.0141784
    ## consistent:FMR1KO-consistent:WT               10.2171008 0.9990529
    ## yoked-conflict:FMR1KO-consistent:WT           24.5219551 0.0017907
    ## conflict:FMR1KO-consistent:WT                 16.0219551 0.6241249
    ## conflict:WT-yoked-conflict:WT                  5.6271327 0.7486472
    ## yoked-consistent:FMR1KO-yoked-conflict:WT      9.0500212 0.9893925
    ## consistent:FMR1KO-yoked-conflict:WT           -1.1391037 0.0228639
    ## yoked-conflict:FMR1KO-yoked-conflict:WT       12.8113788 1.0000000
    ## conflict:FMR1KO-yoked-conflict:WT              4.3113788 0.4141170
    ## yoked-consistent:FMR1KO-conflict:WT           11.7116638 0.9914691
    ## consistent:FMR1KO-conflict:WT                  1.2945876 0.1484802
    ## yoked-conflict:FMR1KO-conflict:WT             15.6354945 0.6418261
    ## conflict:FMR1KO-conflict:WT                    7.1354945 0.9798377
    ## consistent:FMR1KO-yoked-consistent:FMR1KO      0.3075819 0.0633444
    ## yoked-conflict:FMR1KO-yoked-consistent:FMR1KO 14.4523630 0.9826704
    ## conflict:FMR1KO-yoked-consistent:FMR1KO        5.9523630 0.7947108
    ## yoked-conflict:FMR1KO-consistent:FMR1KO       23.2279669 0.0089219
    ## conflict:FMR1KO-consistent:FMR1KO             14.7279669 0.8970577
    ## conflict:FMR1KO-yoked-conflict:FMR1KO          3.3610320 0.3190938

    mean(PathNumStats$NumEntrances)

    ## [1] 11.63636

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "6",
             APA2 %in% c("yoked-consistent", "yoked-conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 16.6875

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "6",
             APA2 %in% c("consistent", "conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 8.75

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "7") 
    Anova(lm(data = PathNumStats, Path1stEntr ~ Genotype * APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##               Sum Sq Df F value Pr(>F)
    ## (Intercept)     4.62  1  0.2450 0.6238
    ## Genotype       46.43  1  2.4613 0.1259
    ## APA2           95.51  3  1.6877 0.1880
    ## Genotype:APA2  92.38  3  1.6324 0.2001
    ## Residuals     641.38 34

    summary(aov(Path1stEntr~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## APA2           3   58.5  19.511   1.034  0.390
    ## Genotype       1    0.9   0.937   0.050  0.825
    ## APA2:Genotype  3   92.4  30.795   1.632  0.200
    ## Residuals     34  641.4  18.864

    TukeyHSD(aov(Path1stEntr~  APA2 * Genotype, data=PathNumStats))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Path1stEntr ~ APA2 * Genotype, data = PathNumStats)
    ## 
    ## $APA2
    ##                                       diff       lwr      upr     p adj
    ## consistent-yoked-consistent      0.4526984 -4.559068 5.464465 0.9947986
    ## yoked-conflict-yoked-consistent -1.3777778 -7.560233 4.804677 0.9307621
    ## conflict-yoked-consistent       -2.2905983 -7.377234 2.796037 0.6210479
    ## yoked-conflict-consistent       -1.8304762 -7.554317 3.893365 0.8233614
    ## conflict-consistent             -2.7432967 -7.261423 1.774830 0.3706337
    ## conflict-yoked-conflict         -0.9128205 -6.702329 4.876688 0.9736607
    ## 
    ## $Genotype
    ##                diff      lwr      upr    p adj
    ## FMR1KO-WT 0.2905275 -2.44587 3.026925 0.830459
    ## 
    ## $`APA2:Genotype`
    ##                                                     diff        lwr
    ## consistent:WT-yoked-consistent:WT              4.2875000  -4.294472
    ## yoked-conflict:WT-yoked-consistent:WT         -1.0750000 -13.211741
    ## conflict:WT-yoked-consistent:WT                0.3872222  -8.034325
    ## yoked-consistent:FMR1KO-yoked-consistent:WT    4.5710000  -4.830079
    ## consistent:FMR1KO-yoked-consistent:WT          1.2650000  -7.781193
    ## yoked-conflict:FMR1KO-yoked-consistent:WT      2.2800000  -7.629607
    ## conflict:FMR1KO-yoked-consistent:WT           -0.0625000  -9.972107
    ## yoked-conflict:WT-consistent:WT               -5.3625000 -16.441778
    ## conflict:WT-consistent:WT                     -3.9002778 -10.710004
    ## yoked-consistent:FMR1KO-consistent:WT          0.2835000  -7.705881
    ## consistent:FMR1KO-consistent:WT               -3.0225000 -10.591088
    ## yoked-conflict:FMR1KO-consistent:WT           -2.0075000 -10.589472
    ## conflict:FMR1KO-consistent:WT                 -4.3500000 -12.931972
    ## conflict:WT-yoked-conflict:WT                  1.4622222  -9.493261
    ## yoked-consistent:FMR1KO-yoked-conflict:WT      5.6460000  -6.079206
    ## consistent:FMR1KO-yoked-conflict:WT            2.3400000  -9.102629
    ## yoked-conflict:FMR1KO-yoked-conflict:WT        3.3550000  -8.781741
    ## conflict:FMR1KO-yoked-conflict:WT              1.0125000 -11.124241
    ## yoked-consistent:FMR1KO-conflict:WT            4.1837778  -3.633026
    ## consistent:FMR1KO-conflict:WT                  0.8777778  -6.508408
    ## yoked-conflict:FMR1KO-conflict:WT              1.8927778  -6.528769
    ## conflict:FMR1KO-conflict:WT                   -0.4497222  -8.871269
    ## consistent:FMR1KO-yoked-consistent:FMR1KO     -3.3060000 -11.792081
    ## yoked-conflict:FMR1KO-yoked-consistent:FMR1KO -2.2910000 -11.692079
    ## conflict:FMR1KO-yoked-consistent:FMR1KO       -4.6335000 -14.034579
    ## yoked-conflict:FMR1KO-consistent:FMR1KO        1.0150000  -8.031193
    ## conflict:FMR1KO-consistent:FMR1KO             -1.3275000 -10.373693
    ## conflict:FMR1KO-yoked-conflict:FMR1KO         -2.3425000 -12.252107
    ##                                                     upr     p adj
    ## consistent:WT-yoked-consistent:WT             12.869472 0.7400240
    ## yoked-conflict:WT-yoked-consistent:WT         11.061741 0.9999905
    ## conflict:WT-yoked-consistent:WT                8.808769 0.9999999
    ## yoked-consistent:FMR1KO-yoked-consistent:WT   13.972079 0.7647289
    ## consistent:FMR1KO-yoked-consistent:WT         10.311193 0.9997905
    ## yoked-conflict:FMR1KO-yoked-consistent:WT     12.189607 0.9948924
    ## conflict:FMR1KO-yoked-consistent:WT            9.847107 1.0000000
    ## yoked-conflict:WT-consistent:WT                5.716778 0.7687191
    ## conflict:WT-consistent:WT                      2.909449 0.5935498
    ## yoked-consistent:FMR1KO-consistent:WT          8.272881 1.0000000
    ## consistent:FMR1KO-consistent:WT                4.546088 0.8968335
    ## yoked-conflict:FMR1KO-consistent:WT            6.574472 0.9943514
    ## conflict:FMR1KO-consistent:WT                  4.231972 0.7262044
    ## conflict:WT-yoked-conflict:WT                 12.417705 0.9998465
    ## yoked-consistent:FMR1KO-yoked-conflict:WT     17.371206 0.7731749
    ## consistent:FMR1KO-yoked-conflict:WT           13.782629 0.9975369
    ## yoked-conflict:FMR1KO-yoked-conflict:WT       15.491741 0.9848553
    ## conflict:FMR1KO-yoked-conflict:WT             13.149241 0.9999937
    ## yoked-consistent:FMR1KO-conflict:WT           12.000582 0.6704196
    ## consistent:FMR1KO-conflict:WT                  8.263963 0.9999296
    ## yoked-conflict:FMR1KO-conflict:WT             10.314325 0.9955737
    ## conflict:FMR1KO-conflict:WT                    7.971825 0.9999997
    ## consistent:FMR1KO-yoked-consistent:FMR1KO      5.180081 0.9080559
    ## yoked-conflict:FMR1KO-yoked-consistent:FMR1KO  7.110079 0.9927700
    ## conflict:FMR1KO-yoked-consistent:FMR1KO        4.767579 0.7525618
    ## yoked-conflict:FMR1KO-consistent:FMR1KO       10.061193 0.9999523
    ## conflict:FMR1KO-consistent:FMR1KO              7.718693 0.9997116
    ## conflict:FMR1KO-yoked-conflict:FMR1KO          7.567107 0.9939810

    mean(PathNumStats$Path1stEntr)

    ## [1] 2.859524

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "7",
             Genotype == "WT")
    Anova(lm(data = PathNumStats, Path1stEntr ~  APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##              Sum Sq Df F value  Pr(>F)  
    ## (Intercept)   4.623  1  0.3647 0.55305  
    ## APA2         95.511  3  2.5118 0.08944 .
    ## Residuals   240.823 19                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(Path1stEntr~  APA2, data=PathNumStats))

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## APA2         3  95.51   31.84   2.512 0.0894 .
    ## Residuals   19 240.82   12.67                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    TukeyHSD(aov(Path1stEntr~  APA2, data=PathNumStats))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Path1stEntr ~ APA2, data = PathNumStats)
    ## 
    ## $APA2
    ##                                       diff        lwr        upr     p adj
    ## consistent-yoked-consistent      4.2875000  -1.842761 10.4177614 0.2351052
    ## yoked-conflict-yoked-consistent -1.0750000  -9.744499  7.5944988 0.9849908
    ## conflict-yoked-consistent        0.3872222  -5.628445  6.4028892 0.9978187
    ## yoked-conflict-consistent       -5.3625000 -13.276633  2.5516334 0.2589888
    ## conflict-consistent             -3.9002778  -8.764591  0.9640359 0.1445418
    ## conflict-yoked-conflict          1.4622222  -6.363482  9.2879268 0.9518875

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "7",
             Genotype != "WT")
    Anova(lm(data = PathNumStats, Path1stEntr ~  APA2 ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##             Sum Sq Df F value  Pr(>F)  
    ## (Intercept) 159.39  1  5.9686 0.02741 *
    ## APA2         53.53  3  0.6682 0.58456  
    ## Residuals   400.56 15                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(Path1stEntr~  APA2, data=PathNumStats))

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## APA2         3   53.5   17.84   0.668  0.585
    ## Residuals   15  400.6   26.70

    TukeyHSD(aov(Path1stEntr~  APA2, data=PathNumStats))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Path1stEntr ~ APA2, data = PathNumStats)
    ## 
    ## $APA2
    ##                                    diff        lwr       upr     p adj
    ## consistent-yoked-consistent     -3.3060 -12.324631  5.712631 0.7199517
    ## yoked-conflict-yoked-consistent -2.2910 -12.282051  7.700051 0.9100565
    ## conflict-yoked-consistent       -4.6335 -14.624551  5.357551 0.5552191
    ## yoked-conflict-consistent        1.0150  -8.598893 10.628893 0.9898144
    ## conflict-consistent             -1.3275 -10.941393  8.286393 0.9778648
    ## conflict-yoked-conflict         -2.3425 -12.873992  8.188992 0.9170112

    ## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## quartz_off_screen 
    ##                 2

    ## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

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

    TukeyHSD(aov(data = timespent, pTimeTarget ~ Genotype * APA2 ))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = pTimeTarget ~ Genotype * APA2, data = timespent)
    ## 
    ## $Genotype
    ##                 diff          lwr        upr     p adj
    ## FMR1KO-WT 0.00582024 -0.004852756 0.01649324 0.2809749
    ## 
    ## $APA2
    ##                            diff         lwr        upr     p adj
    ## conflict-consistent 0.004113866 -0.00624914 0.01447687 0.4317379
    ## 
    ## $`Genotype:APA2`
    ##                                           diff          lwr        upr
    ## FMR1KO:consistent-WT:consistent    0.010119737 -0.008857671 0.02909715
    ## WT:conflict-WT:consistent          0.007284259 -0.010053089 0.02462161
    ## FMR1KO:conflict-WT:consistent      0.008975000 -0.012874429 0.03082443
    ## WT:conflict-FMR1KO:consistent     -0.002835478 -0.021341169 0.01567021
    ## FMR1KO:conflict-FMR1KO:consistent -0.001144737 -0.023932333 0.02164286
    ## FMR1KO:conflict-WT:conflict        0.001690741 -0.019750252 0.02313173
    ##                                       p adj
    ## FMR1KO:consistent-WT:consistent   0.5032108
    ## WT:conflict-WT:consistent         0.6887573
    ## FMR1KO:conflict-WT:consistent     0.7036780
    ## WT:conflict-FMR1KO:consistent     0.9778290
    ## FMR1KO:conflict-FMR1KO:consistent 0.9991738
    ## FMR1KO:conflict-WT:conflict       0.9968360

    TukeyHSD(aov(data = timespent, pTimeCW ~ Genotype * APA2 ))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = pTimeCW ~ Genotype * APA2, data = timespent)
    ## 
    ## $Genotype
    ##                    diff         lwr        upr    p adj
    ## FMR1KO-WT -0.0007857052 -0.06941043 0.06783902 0.981873
    ## 
    ## $APA2
    ##                         diff       lwr       upr p adj
    ## conflict-consistent 0.205344 0.1387124 0.2719755     0
    ## 
    ## $`Genotype:APA2`
    ##                                         diff         lwr       upr
    ## FMR1KO:consistent-WT:consistent   0.02397566 -0.09804438 0.1459957
    ## WT:conflict-WT:consistent         0.20514213  0.09366727 0.3166170
    ## FMR1KO:conflict-WT:consistent     0.24057083  0.10008441 0.3810573
    ## WT:conflict-FMR1KO:consistent     0.18116647  0.06217945 0.3001535
    ## FMR1KO:conflict-FMR1KO:consistent 0.21659518  0.07007657 0.3631138
    ## FMR1KO:conflict-WT:conflict       0.03542870 -0.10243157 0.1732890
    ##                                       p adj
    ## FMR1KO:consistent-WT:consistent   0.9550673
    ## WT:conflict-WT:consistent         0.0000389
    ## FMR1KO:conflict-WT:consistent     0.0001379
    ## WT:conflict-FMR1KO:consistent     0.0008182
    ## FMR1KO:conflict-FMR1KO:consistent 0.0012165
    ## FMR1KO:conflict-WT:conflict       0.9063729

    TukeyHSD(aov(data = timespent, pTimeCCW ~ Genotype * APA2 ))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = pTimeCCW ~ Genotype * APA2, data = timespent)
    ## 
    ## $Genotype
    ##                 diff         lwr       upr     p adj
    ## FMR1KO-WT 0.05152245 -0.02169689 0.1247418 0.1652085
    ## 
    ## $APA2
    ##                           diff        lwr         upr   p adj
    ## conflict-consistent -0.1709606 -0.2420533 -0.09986782 7.9e-06
    ## 
    ## $`Genotype:APA2`
    ##                                          diff         lwr         upr
    ## FMR1KO:consistent-WT:consistent    0.07931096 -0.05087865  0.20950058
    ## WT:conflict-WT:consistent         -0.13039352 -0.24933193 -0.01145511
    ## FMR1KO:conflict-WT:consistent     -0.17080833 -0.32070071 -0.02091596
    ## WT:conflict-FMR1KO:consistent     -0.20970448 -0.33665801 -0.08275096
    ## FMR1KO:conflict-FMR1KO:consistent -0.25011930 -0.40644773 -0.09379087
    ## FMR1KO:conflict-WT:conflict       -0.04041481 -0.18750522  0.10667559
    ##                                       p adj
    ## FMR1KO:consistent-WT:consistent   0.3850072
    ## WT:conflict-WT:consistent         0.0259978
    ## FMR1KO:conflict-WT:consistent     0.0190587
    ## WT:conflict-FMR1KO:consistent     0.0002467
    ## FMR1KO:conflict-FMR1KO:consistent 0.0004019
    ## FMR1KO:conflict-WT:conflict       0.8883106

    TukeyHSD(aov(data = timespent, pTimeOPP ~ Genotype * APA2 ))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = pTimeOPP ~ Genotype * APA2, data = timespent)
    ## 
    ## $Genotype
    ##                  diff        lwr        upr     p adj
    ## FMR1KO-WT -0.05656091 -0.1334239 0.02030207 0.1469407
    ## 
    ## $APA2
    ##                            diff        lwr        upr     p adj
    ## conflict-consistent -0.03848289 -0.1131134 0.03614765 0.3077956
    ## 
    ## $`Genotype:APA2`
    ##                                           diff        lwr        upr
    ## FMR1KO:consistent-WT:consistent   -0.113388596 -0.2500569 0.02327968
    ## WT:conflict-WT:consistent         -0.082001852 -0.2068590 0.04285532
    ## FMR1KO:conflict-WT:consistent     -0.078733333 -0.2360848 0.07861818
    ## WT:conflict-FMR1KO:consistent      0.031386745 -0.1018844 0.16465790
    ## FMR1KO:conflict-FMR1KO:consistent  0.034655263 -0.1294526 0.19876311
    ## FMR1KO:conflict-WT:conflict        0.003268519 -0.1511416 0.15767862
    ##                                       p adj
    ## FMR1KO:consistent-WT:consistent   0.1384239
    ## WT:conflict-WT:consistent         0.3182425
    ## FMR1KO:conflict-WT:consistent     0.5571467
    ## WT:conflict-FMR1KO:consistent     0.9259496
    ## FMR1KO:conflict-FMR1KO:consistent 0.9450870
    ## FMR1KO:conflict-WT:conflict       0.9999378

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

![Fig. 2.6.Retention mice](../figures/fig1-09.png)

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum == "9") 

    summary(aov(NumEntrances ~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value   Pr(>F)    
    ## APA2           3 1622.5   540.8   14.32 1.44e-06 ***
    ## Genotype       1    1.5     1.5    0.04    0.843    
    ## APA2:Genotype  3   37.4    12.5    0.33    0.803    
    ## Residuals     42 1586.5    37.8                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(Path1stEntr ~  APA2 * Genotype, data=PathNumStats))

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## APA2           3  166.3   55.43   6.002 0.00169 **
    ## Genotype       1    0.2    0.23   0.025 0.87598   
    ## APA2:Genotype  3    8.8    2.94   0.318 0.81215   
    ## Residuals     42  387.8    9.23                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = PathNumStats, Path1stEntr ~ APA2 * Genotype), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: Path1stEntr
    ##               Sum Sq Df F value  Pr(>F)  
    ## (Intercept)     0.32  1  0.0352 0.85211  
    ## APA2          100.78  3  3.6377 0.02022 *
    ## Genotype        1.41  1  0.1524 0.69822  
    ## APA2:Genotype   8.81  3  0.3182 0.81215  
    ## Residuals     387.84 42                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = PathNumStats, NumEntrances ~ APA2 * Genotype ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: NumEntrances
    ##                Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   1849.00  1 48.9485 1.471e-08 ***
    ## APA2           682.96  3  6.0266  0.001647 ** 
    ## Genotype         3.75  1  0.0994  0.754161    
    ## APA2:Genotype   37.44  3  0.3304  0.803363    
    ## Residuals     1586.53 42                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Anova(lm(data = PathNumStats, pTimeTarget ~ APA2 * Genotype ), type = 3)

    ## Anova Table (Type III tests)
    ## 
    ## Response: pTimeTarget
    ##                Sum Sq Df F value    Pr(>F)    
    ## (Intercept)   0.38057  1 42.5068 7.099e-08 ***
    ## APA2          0.14885  3  5.5420  0.002685 ** 
    ## Genotype      0.00038  1  0.0430  0.836796    
    ## APA2:Genotype 0.00291  3  0.1083  0.954798    
    ## Residuals     0.37603 42                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    TukeyHSD(aov(data = PathNumStats, NumEntrances ~ Genotype * APA2))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = NumEntrances ~ Genotype * APA2, data = PathNumStats)
    ## 
    ## $Genotype
    ##               diff       lwr      upr     p adj
    ## FMR1KO-WT 1.634615 -1.876381 5.145612 0.3528163
    ## 
    ## $APA2
    ##                                       diff        lwr       upr     p adj
    ## consistent-yoked-consistent     -12.862608 -19.224320 -6.500896 0.0000163
    ## yoked-conflict-yoked-consistent  -3.629152 -11.268411  4.010107 0.5862522
    ## conflict-yoked-consistent       -12.602023 -19.226101 -5.977945 0.0000460
    ## yoked-conflict-consistent         9.233456   2.184638 16.282274 0.0058512
    ## conflict-consistent               0.260585  -5.672877  6.194047 0.9994106
    ## conflict-yoked-conflict          -8.972871 -16.259356 -1.686386 0.0104313
    ## 
    ## $`Genotype:APA2`
    ##                                                      diff        lwr
    ## FMR1KO:yoked-consistent-WT:yoked-consistent     1.2142857 -11.068332
    ## WT:consistent-WT:yoked-consistent             -12.8750000 -24.875221
    ## FMR1KO:consistent-WT:yoked-consistent         -11.7222222 -23.498121
    ## WT:yoked-conflict-WT:yoked-consistent          -1.1666667 -16.133572
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      -3.9000000 -17.045584
    ## WT:conflict-WT:yoked-consistent               -11.5000000 -23.275898
    ## FMR1KO:conflict-WT:yoked-consistent           -13.7000000 -26.845584
    ## WT:consistent-FMR1KO:yoked-consistent         -14.0892857 -24.231324
    ## FMR1KO:consistent-FMR1KO:yoked-consistent     -12.9365079 -22.812104
    ## WT:yoked-conflict-FMR1KO:yoked-consistent      -2.3809524 -15.903670
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent  -5.1142857 -16.588692
    ## WT:conflict-FMR1KO:yoked-consistent           -12.7142857 -22.589882
    ## FMR1KO:conflict-FMR1KO:yoked-consistent       -14.9142857 -26.388692
    ## FMR1KO:consistent-WT:consistent                 1.1527778  -8.369302
    ## WT:yoked-conflict-WT:consistent                11.7083333  -1.558410
    ## FMR1KO:yoked-conflict-WT:consistent             8.9750000  -2.196598
    ## WT:conflict-WT:consistent                       1.3750000  -8.147080
    ## FMR1KO:conflict-WT:consistent                  -0.8250000 -11.996598
    ## WT:yoked-conflict-FMR1KO:consistent            10.5555556  -2.508631
    ## FMR1KO:yoked-conflict-FMR1KO:consistent         7.8222222  -3.108060
    ## WT:conflict-FMR1KO:consistent                   0.2222222  -9.015552
    ## FMR1KO:conflict-FMR1KO:consistent              -1.9777778 -12.908060
    ## FMR1KO:yoked-conflict-WT:yoked-conflict        -2.7333333 -17.044432
    ## WT:conflict-WT:yoked-conflict                 -10.3333333 -23.397520
    ## FMR1KO:conflict-WT:yoked-conflict             -12.5333333 -26.844432
    ## WT:conflict-FMR1KO:yoked-conflict              -7.6000000 -18.530282
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          -9.8000000 -22.193775
    ## FMR1KO:conflict-WT:conflict                    -2.2000000 -13.130282
    ##                                                       upr     p adj
    ## FMR1KO:yoked-consistent-WT:yoked-consistent   13.49690378 0.9999821
    ## WT:consistent-WT:yoked-consistent             -0.87477872 0.0277839
    ## FMR1KO:consistent-WT:yoked-consistent          0.05367608 0.0518092
    ## WT:yoked-conflict-WT:yoked-consistent         13.80023886 0.9999965
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      9.24558378 0.9793693
    ## WT:conflict-WT:yoked-consistent                0.27589830 0.0599273
    ## FMR1KO:conflict-WT:yoked-consistent           -0.55441622 0.0357422
    ## WT:consistent-FMR1KO:yoked-consistent         -3.94724764 0.0015734
    ## FMR1KO:consistent-FMR1KO:yoked-consistent     -3.06091144 0.0033589
    ## WT:yoked-conflict-FMR1KO:yoked-consistent     11.14176505 0.9991519
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent  6.36012052 0.8423750
    ## WT:conflict-FMR1KO:yoked-consistent           -2.83868921 0.0041494
    ## FMR1KO:conflict-FMR1KO:yoked-consistent       -3.43987948 0.0036961
    ## FMR1KO:consistent-WT:consistent               10.67485793 0.9999293
    ## WT:yoked-conflict-WT:consistent               24.97507713 0.1187790
    ## FMR1KO:yoked-conflict-WT:consistent           20.14659804 0.1989740
    ## WT:conflict-WT:consistent                     10.89708015 0.9997695
    ## FMR1KO:conflict-WT:consistent                 10.34659804 0.9999976
    ## WT:yoked-conflict-FMR1KO:consistent           23.61974175 0.1934166
    ## FMR1KO:yoked-conflict-FMR1KO:consistent       18.75250459 0.3272519
    ## WT:conflict-FMR1KO:consistent                  9.45999687 1.0000000
    ## FMR1KO:conflict-FMR1KO:consistent              8.95250459 0.9989881
    ## FMR1KO:yoked-conflict-WT:yoked-conflict       11.57776561 0.9985679
    ## WT:conflict-WT:yoked-conflict                  2.73085286 0.2145471
    ## FMR1KO:conflict-WT:yoked-conflict              1.77776561 0.1244075
    ## WT:conflict-FMR1KO:yoked-conflict              3.33028237 0.3627775
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          2.59377524 0.2148657
    ## FMR1KO:conflict-WT:conflict                    8.73028237 0.9980007

    TukeyHSD(aov(data = PathNumStats, Path1stEntr ~ Genotype * APA2))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Path1stEntr ~ Genotype * APA2, data = PathNumStats)
    ## 
    ## $Genotype
    ##                 diff       lwr      upr    p adj
    ## FMR1KO-WT -0.5939103 -2.329839 1.142019 0.493714
    ## 
    ## $APA2
    ##                                       diff        lwr        upr     p adj
    ## consistent-yoked-consistent      3.9906513  0.8452532  7.1360494 0.0079464
    ## yoked-conflict-yoked-consistent -0.3586808 -4.1357320  3.4183704 0.9941610
    ## conflict-yoked-consistent        2.5188432 -0.7562755  5.7939620 0.1839459
    ## yoked-conflict-consistent       -4.3493321 -7.8344538 -0.8642104 0.0092514
    ## conflict-consistent             -1.4718081 -4.4054680  1.4618519 0.5420192
    ## conflict-yoked-conflict          2.8775240 -0.7251066  6.4801546 0.1583323
    ## 
    ## $`Genotype:APA2`
    ##                                                     diff         lwr
    ## FMR1KO:yoked-consistent-WT:yoked-consistent    0.7435714  -5.3292790
    ## WT:consistent-WT:yoked-consistent              5.2000000  -0.7332260
    ## FMR1KO:consistent-WT:yoked-consistent          3.9294444  -1.8928704
    ## WT:yoked-conflict-WT:yoked-consistent          0.0250000  -7.3750330
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      0.1790000  -6.3205234
    ## WT:conflict-WT:yoked-consistent                2.9850000  -2.8373148
    ## FMR1KO:conflict-WT:yoked-consistent            3.4690000  -3.0305234
    ## WT:consistent-FMR1KO:yoked-consistent          4.4564286  -0.5580626
    ## FMR1KO:consistent-FMR1KO:yoked-consistent      3.1858730  -1.6968824
    ## WT:yoked-conflict-FMR1KO:yoked-consistent     -0.7185714  -7.4045597
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent -0.5645714  -6.2378206
    ## WT:conflict-FMR1KO:yoked-consistent            2.2414286  -2.6413269
    ## FMR1KO:conflict-FMR1KO:yoked-consistent        2.7254286  -2.9478206
    ## FMR1KO:consistent-WT:consistent               -1.2705556  -5.9785232
    ## WT:yoked-conflict-WT:consistent               -5.1750000 -11.7344282
    ## FMR1KO:yoked-conflict-WT:consistent           -5.0210000 -10.5445328
    ## WT:conflict-WT:consistent                     -2.2150000  -6.9229677
    ## FMR1KO:conflict-WT:consistent                 -1.7310000  -7.2545328
    ## WT:yoked-conflict-FMR1KO:consistent           -3.9044444 -10.3637228
    ## FMR1KO:yoked-conflict-FMR1KO:consistent       -3.7504444  -9.1546644
    ## WT:conflict-FMR1KO:consistent                 -0.9444444  -5.5118440
    ## FMR1KO:conflict-FMR1KO:consistent             -0.4604444  -5.8646644
    ## FMR1KO:yoked-conflict-WT:yoked-conflict        0.1540000  -6.9217849
    ## WT:conflict-WT:yoked-conflict                  2.9600000  -3.4992783
    ## FMR1KO:conflict-WT:yoked-conflict              3.4440000  -3.6317849
    ## WT:conflict-FMR1KO:yoked-conflict              2.8060000  -2.5982200
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          3.2900000  -2.8378095
    ## FMR1KO:conflict-WT:conflict                    0.4840000  -4.9202200
    ##                                                      upr     p adj
    ## FMR1KO:yoked-consistent-WT:yoked-consistent    6.8164219 0.9999237
    ## WT:consistent-WT:yoked-consistent             11.1332260 0.1238624
    ## FMR1KO:consistent-WT:yoked-consistent          9.7517593 0.4003735
    ## WT:yoked-conflict-WT:yoked-consistent          7.4250330 1.0000000
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      6.6785234 1.0000000
    ## WT:conflict-WT:yoked-consistent                8.8073148 0.7271300
    ## FMR1KO:conflict-WT:yoked-consistent            9.9685234 0.6860643
    ## WT:consistent-FMR1KO:yoked-consistent          9.4709198 0.1138153
    ## FMR1KO:consistent-FMR1KO:yoked-consistent      8.0686285 0.4435615
    ## WT:yoked-conflict-FMR1KO:yoked-consistent      5.9674168 0.9999684
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent  5.1086777 0.9999813
    ## WT:conflict-FMR1KO:yoked-consistent            7.1241840 0.8217228
    ## FMR1KO:conflict-FMR1KO:yoked-consistent        8.3986777 0.7861109
    ## FMR1KO:consistent-WT:consistent                3.4374121 0.9880135
    ## WT:yoked-conflict-WT:consistent                1.3844282 0.2171700
    ## FMR1KO:yoked-conflict-WT:consistent            0.5025328 0.0986822
    ## WT:conflict-WT:consistent                      2.4929677 0.8030318
    ## FMR1KO:conflict-WT:consistent                  3.7925328 0.9720339
    ## WT:yoked-conflict-FMR1KO:consistent            2.5548339 0.5408114
    ## FMR1KO:yoked-conflict-FMR1KO:consistent        1.6537755 0.3651727
    ## WT:conflict-FMR1KO:consistent                  3.6229551 0.9976294
    ## FMR1KO:conflict-FMR1KO:consistent              4.9437755 0.9999936
    ## FMR1KO:yoked-conflict-WT:yoked-conflict        7.2297849 1.0000000
    ## WT:conflict-WT:yoked-conflict                  9.4192783 0.8229886
    ## FMR1KO:conflict-WT:yoked-conflict             10.5197849 0.7749960
    ## WT:conflict-FMR1KO:yoked-conflict              8.2102200 0.7145546
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          9.4178095 0.6797519
    ## FMR1KO:conflict-WT:conflict                    5.8882200 0.9999909

    TukeyHSD(aov(data = PathNumStats, pTimeTarget ~  Genotype * APA2))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = pTimeTarget ~ Genotype * APA2, data = PathNumStats)
    ## 
    ## $Genotype
    ##                 diff        lwr        upr     p adj
    ## FMR1KO-WT 0.01413397 -0.0399187 0.06818665 0.6004856
    ## 
    ## $APA2
    ##                                        diff         lwr          upr
    ## consistent-yoked-consistent     -0.18194503 -0.27988519 -0.084004868
    ## yoked-conflict-yoked-consistent -0.10866666 -0.22627499  0.008941666
    ## conflict-yoked-consistent       -0.20241649 -0.30439584 -0.100437136
    ## yoked-conflict-consistent        0.07327837 -0.03523997  0.181796713
    ## conflict-consistent             -0.02047146 -0.11181860  0.070875685
    ## conflict-yoked-conflict         -0.09374983 -0.20592712  0.018427459
    ##                                     p adj
    ## consistent-yoked-consistent     0.0000676
    ## yoked-conflict-yoked-consistent 0.0793153
    ## conflict-yoked-consistent       0.0000225
    ## yoked-conflict-consistent       0.2847497
    ## conflict-consistent             0.9316486
    ## conflict-yoked-conflict         0.1303159
    ## 
    ## $`Genotype:APA2`
    ##                                                        diff         lwr
    ## FMR1KO:yoked-consistent-WT:yoked-consistent   -0.0122928571 -0.20138687
    ## WT:consistent-WT:yoked-consistent             -0.1846375000 -0.36938395
    ## FMR1KO:consistent-WT:yoked-consistent         -0.1971833333 -0.37847627
    ## WT:yoked-conflict-WT:yoked-consistent         -0.0864166667 -0.31683597
    ## FMR1KO:yoked-conflict-WT:yoked-consistent     -0.1347900000 -0.33716959
    ## WT:conflict-WT:yoked-consistent               -0.2143833333 -0.39567627
    ## FMR1KO:conflict-WT:yoked-consistent           -0.2138300000 -0.41620959
    ## WT:consistent-FMR1KO:yoked-consistent         -0.1723446429 -0.32848389
    ## FMR1KO:consistent-FMR1KO:yoked-consistent     -0.1848904762 -0.33692779
    ## WT:yoked-conflict-FMR1KO:yoked-consistent     -0.0741238095 -0.28230947
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent -0.1224971429 -0.29914853
    ## WT:conflict-FMR1KO:yoked-consistent           -0.2020904762 -0.35412779
    ## FMR1KO:conflict-FMR1KO:yoked-consistent       -0.2015371429 -0.37818853
    ## FMR1KO:consistent-WT:consistent               -0.0125458333 -0.15914067
    ## WT:yoked-conflict-WT:consistent                0.0982208333 -0.10602405
    ## FMR1KO:yoked-conflict-WT:consistent            0.0498475000 -0.12214208
    ## WT:conflict-WT:consistent                     -0.0297458333 -0.17634067
    ## FMR1KO:conflict-WT:consistent                 -0.0291925000 -0.20118208
    ## WT:yoked-conflict-FMR1KO:consistent            0.1107666667 -0.09035979
    ## FMR1KO:yoked-conflict-FMR1KO:consistent        0.0623933333 -0.10588113
    ## WT:conflict-FMR1KO:consistent                 -0.0172000000 -0.15941788
    ## FMR1KO:conflict-FMR1KO:consistent             -0.0166466667 -0.18492113
    ## FMR1KO:yoked-conflict-WT:yoked-conflict       -0.0483733333 -0.26869633
    ## WT:conflict-WT:yoked-conflict                 -0.1279666667 -0.32909312
    ## FMR1KO:conflict-WT:yoked-conflict             -0.1274133333 -0.34773633
    ## WT:conflict-FMR1KO:yoked-conflict             -0.0795933333 -0.24786780
    ## FMR1KO:conflict-FMR1KO:yoked-conflict         -0.0790400000 -0.26984531
    ## FMR1KO:conflict-WT:conflict                    0.0005533333 -0.16772113
    ##                                                         upr     p adj
    ## FMR1KO:yoked-consistent-WT:yoked-consistent    0.1768011584 0.9999990
    ## WT:consistent-WT:yoked-consistent              0.0001089455 0.0502309
    ## FMR1KO:consistent-WT:yoked-consistent         -0.0158903969 0.0245675
    ## WT:yoked-conflict-WT:yoked-consistent          0.1440026342 0.9286751
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      0.0675895913 0.4172367
    ## WT:conflict-WT:yoked-consistent               -0.0330903969 0.0108157
    ## FMR1KO:conflict-WT:yoked-consistent           -0.0114504087 0.0317847
    ## WT:consistent-FMR1KO:yoked-consistent         -0.0162053984 0.0214423
    ## FMR1KO:consistent-FMR1KO:yoked-consistent     -0.0328531672 0.0080029
    ## WT:yoked-conflict-FMR1KO:yoked-consistent      0.1340618498 0.9450292
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent  0.0541542469 0.3661544
    ## WT:conflict-FMR1KO:yoked-consistent           -0.0500531672 0.0027987
    ## FMR1KO:conflict-FMR1KO:yoked-consistent       -0.0248857531 0.0155974
    ## FMR1KO:consistent-WT:consistent                0.1340490020 0.9999934
    ## WT:yoked-conflict-WT:consistent                0.3024657137 0.7852462
    ## FMR1KO:yoked-conflict-WT:consistent            0.2218370808 0.9819202
    ## WT:conflict-WT:consistent                      0.1168490020 0.9978956
    ## FMR1KO:conflict-WT:consistent                  0.1427970808 0.9993317
    ## WT:yoked-conflict-FMR1KO:consistent            0.3118931215 0.6518284
    ## FMR1KO:yoked-conflict-FMR1KO:consistent        0.2306677984 0.9325880
    ## WT:conflict-FMR1KO:consistent                  0.1250178801 0.9999298
    ## FMR1KO:conflict-FMR1KO:consistent              0.1516277984 0.9999821
    ## FMR1KO:yoked-conflict-WT:yoked-conflict        0.1719496591 0.9965509
    ## WT:conflict-WT:yoked-conflict                  0.0731597882 0.4758458
    ## FMR1KO:conflict-WT:yoked-conflict              0.0929096591 0.5950808
    ## WT:conflict-FMR1KO:yoked-conflict              0.0886811317 0.7987965
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          0.1117653085 0.8857553
    ## FMR1KO:conflict-WT:conflict                    0.1688277984 1.0000000

    PathNumStats <- behavior  %>% 
     filter(TrainSessionComboNum %in% c("9"))  %>% 
      filter(APA2 %in% c("yoked-consistent", "yoked-conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 20.73684

    mean(PathNumStats$Path1stEntr)

    ## [1] 0.61

    mean(PathNumStats$pTimeTarget)

    ## [1] 0.2548053

    PathNumStats <- behavior  %>% 
       filter(TrainSessionComboNum %in% c("9"))  %>%
      filter(APA2 %in% c("consistent", "conflict")) 
    mean(PathNumStats$NumEntrances)

    ## [1] 9.225806

    mean(PathNumStats$Path1stEntr)

    ## [1] 4.193871

    mean(PathNumStats$pTimeTarget)

    ## [1] 0.1068258

    PathNumStats <- behavior  %>% 
       filter(TrainSessionComboNum %in% c("9"))  
    mean(PathNumStats$Path1stEntr)

    ## [1] 2.832

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum  %in% c("7")) 
    TukeyHSD(aov(data = PathNumStats, Path1stEntr ~ Genotype * APA2))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Path1stEntr ~ Genotype * APA2, data = PathNumStats)
    ## 
    ## $Genotype
    ##                diff       lwr      upr     p adj
    ## FMR1KO-WT 0.5198627 -2.216535 3.256261 0.7018376
    ## 
    ## $APA2
    ##                                       diff       lwr      upr     p adj
    ## consistent-yoked-consistent      0.5187127 -4.493054 5.530479 0.9922339
    ## yoked-conflict-yoked-consistent -1.4355403 -7.617995 4.746914 0.9226271
    ## conflict-yoked-consistent       -2.1617434 -7.248379 2.924892 0.6630445
    ## yoked-conflict-consistent       -1.9542530 -7.678094 3.769588 0.7932404
    ## conflict-consistent             -2.6804562 -7.198583 1.837670 0.3908923
    ## conflict-yoked-conflict         -0.7262031 -6.515712 5.063305 0.9863786
    ## 
    ## $`Genotype:APA2`
    ##                                                     diff        lwr
    ## FMR1KO:yoked-consistent-WT:yoked-consistent    4.5710000  -4.830079
    ## WT:consistent-WT:yoked-consistent              4.2875000  -4.294472
    ## FMR1KO:consistent-WT:yoked-consistent          1.2650000  -7.781193
    ## WT:yoked-conflict-WT:yoked-consistent         -1.0750000 -13.211741
    ## FMR1KO:yoked-conflict-WT:yoked-consistent      2.2800000  -7.629607
    ## WT:conflict-WT:yoked-consistent                0.3872222  -8.034325
    ## FMR1KO:conflict-WT:yoked-consistent           -0.0625000  -9.972107
    ## WT:consistent-FMR1KO:yoked-consistent         -0.2835000  -8.272881
    ## FMR1KO:consistent-FMR1KO:yoked-consistent     -3.3060000 -11.792081
    ## WT:yoked-conflict-FMR1KO:yoked-consistent     -5.6460000 -17.371206
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent -2.2910000 -11.692079
    ## WT:conflict-FMR1KO:yoked-consistent           -4.1837778 -12.000582
    ## FMR1KO:conflict-FMR1KO:yoked-consistent       -4.6335000 -14.034579
    ## FMR1KO:consistent-WT:consistent               -3.0225000 -10.591088
    ## WT:yoked-conflict-WT:consistent               -5.3625000 -16.441778
    ## FMR1KO:yoked-conflict-WT:consistent           -2.0075000 -10.589472
    ## WT:conflict-WT:consistent                     -3.9002778 -10.710004
    ## FMR1KO:conflict-WT:consistent                 -4.3500000 -12.931972
    ## WT:yoked-conflict-FMR1KO:consistent           -2.3400000 -13.782629
    ## FMR1KO:yoked-conflict-FMR1KO:consistent        1.0150000  -8.031193
    ## WT:conflict-FMR1KO:consistent                 -0.8777778  -8.263963
    ## FMR1KO:conflict-FMR1KO:consistent             -1.3275000 -10.373693
    ## FMR1KO:yoked-conflict-WT:yoked-conflict        3.3550000  -8.781741
    ## WT:conflict-WT:yoked-conflict                  1.4622222  -9.493261
    ## FMR1KO:conflict-WT:yoked-conflict              1.0125000 -11.124241
    ## WT:conflict-FMR1KO:yoked-conflict             -1.8927778 -10.314325
    ## FMR1KO:conflict-FMR1KO:yoked-conflict         -2.3425000 -12.252107
    ## FMR1KO:conflict-WT:conflict                   -0.4497222  -8.871269
    ##                                                     upr     p adj
    ## FMR1KO:yoked-consistent-WT:yoked-consistent   13.972079 0.7647289
    ## WT:consistent-WT:yoked-consistent             12.869472 0.7400240
    ## FMR1KO:consistent-WT:yoked-consistent         10.311193 0.9997905
    ## WT:yoked-conflict-WT:yoked-consistent         11.061741 0.9999905
    ## FMR1KO:yoked-conflict-WT:yoked-consistent     12.189607 0.9948924
    ## WT:conflict-WT:yoked-consistent                8.808769 0.9999999
    ## FMR1KO:conflict-WT:yoked-consistent            9.847107 1.0000000
    ## WT:consistent-FMR1KO:yoked-consistent          7.705881 1.0000000
    ## FMR1KO:consistent-FMR1KO:yoked-consistent      5.180081 0.9080559
    ## WT:yoked-conflict-FMR1KO:yoked-consistent      6.079206 0.7731749
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent  7.110079 0.9927700
    ## WT:conflict-FMR1KO:yoked-consistent            3.633026 0.6704196
    ## FMR1KO:conflict-FMR1KO:yoked-consistent        4.767579 0.7525618
    ## FMR1KO:consistent-WT:consistent                4.546088 0.8968335
    ## WT:yoked-conflict-WT:consistent                5.716778 0.7687191
    ## FMR1KO:yoked-conflict-WT:consistent            6.574472 0.9943514
    ## WT:conflict-WT:consistent                      2.909449 0.5935498
    ## FMR1KO:conflict-WT:consistent                  4.231972 0.7262044
    ## WT:yoked-conflict-FMR1KO:consistent            9.102629 0.9975369
    ## FMR1KO:yoked-conflict-FMR1KO:consistent       10.061193 0.9999523
    ## WT:conflict-FMR1KO:consistent                  6.508408 0.9999296
    ## FMR1KO:conflict-FMR1KO:consistent              7.718693 0.9997116
    ## FMR1KO:yoked-conflict-WT:yoked-conflict       15.491741 0.9848553
    ## WT:conflict-WT:yoked-conflict                 12.417705 0.9998465
    ## FMR1KO:conflict-WT:yoked-conflict             13.149241 0.9999937
    ## WT:conflict-FMR1KO:yoked-conflict              6.528769 0.9955737
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          7.567107 0.9939810
    ## FMR1KO:conflict-WT:conflict                    7.971825 0.9999997

    PathNumStats <- behavior  %>% 
      filter(TrainSessionComboNum  %in% c("6")) 
    TukeyHSD(aov(data = PathNumStats, NumEntrances ~ Genotype * APA2))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = NumEntrances ~ Genotype * APA2, data = PathNumStats)
    ## 
    ## $Genotype
    ##                diff       lwr      upr     p adj
    ## FMR1KO-WT 0.7583333 -2.444905 3.961572 0.6340395
    ## 
    ## $APA2
    ##                                      diff        lwr        upr     p adj
    ## consistent-yoked-consistent     -9.354815 -15.278708 -3.4309219 0.0007929
    ## yoked-conflict-yoked-consistent  4.099074  -2.981332 11.1794804 0.4141129
    ## conflict-yoked-consistent       -2.162464  -8.254843  3.9299141 0.7749428
    ## yoked-conflict-consistent       13.453889   7.022790 19.8849873 0.0000124
    ## conflict-consistent              7.192350   1.868447 12.5162539 0.0045329
    ## conflict-yoked-conflict         -6.261538 -12.848161  0.3250839 0.0674296
    ## 
    ## $`Genotype:APA2`
    ##                                                      diff         lwr
    ## FMR1KO:yoked-consistent-WT:yoked-consistent     2.0500000  -9.2023630
    ## WT:consistent-WT:yoked-consistent              -9.0000000 -19.2719551
    ## FMR1KO:consistent-WT:yoked-consistent          -7.4642857 -17.9779669
    ## WT:yoked-conflict-WT:yoked-consistent           5.2500000  -7.5613788
    ## FMR1KO:yoked-conflict-WT:yoked-consistent       5.2500000  -6.6110320
    ## WT:conflict-WT:yoked-consistent                -0.3055556 -10.3854945
    ## FMR1KO:conflict-WT:yoked-consistent            -3.2500000 -15.1110320
    ## WT:consistent-FMR1KO:yoked-consistent         -11.0500000 -20.6126697
    ## FMR1KO:consistent-FMR1KO:yoked-consistent      -9.5142857 -19.3361534
    ## WT:yoked-conflict-FMR1KO:yoked-consistent       3.2000000  -9.0500212
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent   3.2000000  -8.0523630
    ## WT:conflict-FMR1KO:yoked-consistent            -2.3555556 -11.7116638
    ## FMR1KO:conflict-FMR1KO:yoked-consistent        -5.3000000 -16.5523630
    ## FMR1KO:consistent-WT:consistent                 1.5357143  -7.1456722
    ## WT:yoked-conflict-WT:consistent                14.2500000   2.8939264
    ## FMR1KO:yoked-conflict-WT:consistent            14.2500000   3.9780449
    ## WT:conflict-WT:consistent                       8.6944444   0.5437298
    ## FMR1KO:conflict-WT:consistent                   5.7500000  -4.5219551
    ## WT:yoked-conflict-FMR1KO:consistent            12.7142857   1.1391037
    ## FMR1KO:yoked-conflict-FMR1KO:consistent        12.7142857   2.2006045
    ## WT:conflict-FMR1KO:consistent                   7.1587302  -1.2945876
    ## FMR1KO:conflict-FMR1KO:consistent               4.2142857  -6.2993955
    ## FMR1KO:yoked-conflict-WT:yoked-conflict         0.0000000 -12.8113788
    ## WT:conflict-WT:yoked-conflict                  -5.5555556 -16.7382438
    ## FMR1KO:conflict-WT:yoked-conflict              -8.5000000 -21.3113788
    ## WT:conflict-FMR1KO:yoked-conflict              -5.5555556 -15.6354945
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          -8.5000000 -20.3610320
    ## FMR1KO:conflict-WT:conflict                    -2.9444444 -13.0243834
    ##                                                      upr     p adj
    ## FMR1KO:yoked-consistent-WT:yoked-consistent   13.3023630 0.9988551
    ## WT:consistent-WT:yoked-consistent              1.2719551 0.1222755
    ## FMR1KO:consistent-WT:yoked-consistent          3.0493955 0.3302891
    ## WT:yoked-conflict-WT:yoked-consistent         18.0613788 0.8861219
    ## FMR1KO:yoked-conflict-WT:yoked-consistent     17.1110320 0.8405486
    ## WT:conflict-WT:yoked-consistent                9.7743834 1.0000000
    ## FMR1KO:conflict-WT:yoked-consistent            8.6110320 0.9859926
    ## WT:consistent-FMR1KO:yoked-consistent         -1.4873303 0.0141784
    ## FMR1KO:consistent-FMR1KO:yoked-consistent      0.3075819 0.0633444
    ## WT:yoked-conflict-FMR1KO:yoked-consistent     15.4500212 0.9893925
    ## FMR1KO:yoked-conflict-FMR1KO:yoked-consistent 14.4523630 0.9826704
    ## WT:conflict-FMR1KO:yoked-consistent            7.0005527 0.9914691
    ## FMR1KO:conflict-FMR1KO:yoked-consistent        5.9523630 0.7947108
    ## FMR1KO:consistent-WT:consistent               10.2171008 0.9990529
    ## WT:yoked-conflict-WT:consistent               25.6060736 0.0059740
    ## FMR1KO:yoked-conflict-WT:consistent           24.5219551 0.0017907
    ## WT:conflict-WT:consistent                     16.8451591 0.0295954
    ## FMR1KO:conflict-WT:consistent                 16.0219551 0.6241249
    ## WT:yoked-conflict-FMR1KO:consistent           24.2894677 0.0228639
    ## FMR1KO:yoked-conflict-FMR1KO:consistent       23.2279669 0.0089219
    ## WT:conflict-FMR1KO:consistent                 15.6120479 0.1484802
    ## FMR1KO:conflict-FMR1KO:consistent             14.7279669 0.8970577
    ## FMR1KO:yoked-conflict-WT:yoked-conflict       12.8113788 1.0000000
    ## WT:conflict-WT:yoked-conflict                  5.6271327 0.7486472
    ## FMR1KO:conflict-WT:yoked-conflict              4.3113788 0.4141170
    ## WT:conflict-FMR1KO:yoked-conflict              4.5243834 0.6418261
    ## FMR1KO:conflict-FMR1KO:yoked-conflict          3.3610320 0.3190938
    ## FMR1KO:conflict-WT:conflict                    7.1354945 0.9798377

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
      #geom_hline(yintercept=c(9.21), color="red" , linetype = "dashed") + 
       # geom_hline(yintercept=c(18.65), color="black", linetype = "dashed" ) + 
      theme_cowplot(font_size = 8, line_size = 0.25) + 
      theme(legend.position="none") +
        background_grid(major = "y", minor = "none") 

    path9 <- behavior %>%
        filter(TrainSessionComboNum %in% c("9")) %>% 
      ggplot(aes(x = as.numeric(TrainSessionComboNum), y = Path1stEntr, fill=APA2)) +
      geom_boxplot(outlier.size=0.8, lwd=0.5) +
      facet_wrap(~ Genotype ) +
      scale_fill_manual(values = colorvalAPA00) +  
     scale_x_continuous(name=NULL, 
                           breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                           labels = NULL) +
       scale_y_continuous(name = "Retention path to 1st entrance") +
       geom_hline(yintercept=c(2.832), color="black" , linetype = "dashed") + 
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
