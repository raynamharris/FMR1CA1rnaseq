This script is use to import, clean, reformat, subset, and summarize
data

This first chuck of code loads and wrangles the data.

    ## read intermediate data (raw data from video tracker program analyzed in matlab)
    behavior <- read.csv("../data/fmr1.csv", header = T)
    behavior$APA <- as.factor(behavior$APA)
    behavior$APA2 <- as.factor(behavior$APA2)

    ## relevel then rename factors treatment
    behavior$APA2 <- factor(behavior$APA2, levels = c("controlconsistent", "consistent", "controlconflict", "conflict"))
    levels(behavior$APA2) <- c("yoked-consistent","consistent", "yoked-conflict","conflict")

    levels(behavior$Genotype) <- c("WT","FMR1KO") ## relevel genotype

    ## relevel conflict
    behavior$conflict <- ifelse(grepl("conflict", behavior$APA2), "conflict", "consistent") # for splitting
    levels(behavior$conflict) <- c("consistent","conflict")

    behavior$Time1stEntrLog <- log(behavior$Time1stEntr)  ## log transformation

    behavior <- behavior[c(1,3,7,8,10,60,14:59)] # subset data
    behavior <- subset(behavior, !is.na(behavior$NumEntrances)) # remove nas

    write.csv(behavior, "../results/behaviordata.csv", row.names = F)

The videotracking software captures ~ 40 measures about the animal moves
around the arena. I also have a handful of columns of meta data that
range in their utitlity.

    names(behavior)

    ##  [1] "ID"                   "Genotype"             "Day"                 
    ##  [4] "TrainSession"         "PairedPartner"        "Time1stEntrLog"      
    ##  [7] "APA"                  "APA2"                 "TrainSessionCombo"   
    ## [10] "pair1"                "pair2"                "TrainSessionComboNum"
    ## [13] "SdevSpeedArena"       "Linearity.Arena."     "NumEntrances"        
    ## [16] "Time1stEntr"          "Path1stEntr"          "Speed1stEntr.cm.s."  
    ## [19] "Dist1stEntr.m."       "NumShock"             "MaxTimeAvoid"        
    ## [22] "Time2ndEntr"          "Path2ndEntr"          "Speed2ndEntr"        
    ## [25] "TimeTarget"           "pTimeTarget"          "pTimeCCW"            
    ## [28] "pTimeOPP"             "pTimeCW"              "RayleigLength"       
    ## [31] "RayleigAngle"         "PolarAvgVal"          "PolarSdVal"          
    ## [34] "PolarMinVal"          "PolarMinBin"          "Min50.RngLoBin"      
    ## [37] "Min50.RngHiBin"       "PolarMaxVal"          "PolarMaxBin"         
    ## [40] "Max50.RngLoBin"       "Max50.RngHiBin"       "AnnularMinVal"       
    ## [43] "AnnularMinBin"        "AnnularMaxVal"        "AnnularMaxBin"       
    ## [46] "AnnularAvg"           "AnnularSd"            "AnnularSkewnes"      
    ## [49] "AnnularKurtosis"      "Speed1"               "Speed2"              
    ## [52] "conflict"

I focus on these 6 "behavioral" measures with a few descriptive columns
about the data. The `ID` identified each unique mouse. I use
`TrainSessionCombo` and `TrainSessionCombo` to indicate the order of
active place avoiance training sessions. `Genotype` and `APA2` are
catagorical variables under investigation. The behaioral measures are
explored in detail in the subesent scripts named Fig-Fig7.

    avoidance <- behavior[,c(1,2,8,4,9,12,26:29,15,16)]
    summary(avoidance)

    ##        ID        Genotype                 APA2        TrainSession
    ##  16-116A:  9   WT    :215   yoked-consistent: 76   Retention: 50  
    ##  16-116B:  9   FMR1KO:172   consistent      :133   T2       : 44  
    ##  16-116C:  9                yoked-conflict  : 61   Hab      : 43  
    ##  16-116D:  9                conflict        :117   T1       : 42  
    ##  16-117A:  9                                       T3       : 42  
    ##  16-117B:  9                                       Retest   : 38  
    ##  (Other):333                                       (Other)  :128  
    ##  TrainSessionCombo TrainSessionComboNum  pTimeTarget        pTimeCCW     
    ##  Retention: 50     Min.   :1.000        Min.   :0.0000   Min.   :0.0001  
    ##  T2       : 44     1st Qu.:3.000        1st Qu.:0.0148   1st Qu.:0.2087  
    ##  T4_C1    : 44     Median :5.000        Median :0.0713   Median :0.2776  
    ##  Hab      : 43     Mean   :5.067        Mean   :0.1203   Mean   :0.3058  
    ##  T1       : 42     3rd Qu.:7.000        3rd Qu.:0.2210   3rd Qu.:0.3763  
    ##  T3       : 42     Max.   :9.000        Max.   :0.5258   Max.   :0.8816  
    ##  (Other)  :122                                                           
    ##     pTimeOPP         pTimeCW        NumEntrances    Time1stEntr    
    ##  Min.   :0.0515   Min.   :0.0000   Min.   : 0.00   Min.   :  0.50  
    ##  1st Qu.:0.2301   1st Qu.:0.1027   1st Qu.: 5.00   1st Qu.:  8.23  
    ##  Median :0.3337   Median :0.2143   Median :11.00   Median : 24.80  
    ##  Mean   :0.3661   Mean   :0.2077   Mean   :12.24   Mean   : 99.13  
    ##  3rd Qu.:0.4755   3rd Qu.:0.2884   3rd Qu.:18.00   3rd Qu.: 89.77  
    ##  Max.   :0.8500   Max.   :0.7525   Max.   :41.00   Max.   :600.00  
    ## 

These group sizes are also shown in Fig 1.

    avoidance %>% 
      filter(TrainSessionCombo == "Retention", Genotype == "WT") %>%
      select(APA2, Genotype)  %>%  summary()

    ##                APA2     Genotype 
    ##  yoked-consistent:4   WT    :24  
    ##  consistent      :8   FMR1KO: 0  
    ##  yoked-conflict  :3              
    ##  conflict        :9

    avoidance %>% 
      filter(TrainSessionCombo == "Retention", Genotype == "FMR1KO") %>%
      select(APA2, Genotype)  %>%  summary()

    ##                APA2     Genotype 
    ##  yoked-consistent:7   WT    : 0  
    ##  consistent      :9   FMR1KO:26  
    ##  yoked-conflict  :5              
    ##  conflict        :5

This is a long dataframe where `variable` has four levels (shock zone,
clockwise, opposite, or counter-clockwise) and `value` has the
propportion of time spent in each of those locaitons.

    ## proptime
    proptime <- behavior[,c(1,2,4,8,9,12,26:29)]
    proptime <- melt(proptime, id.vars = c("ID", "Genotype", "TrainSession",
                                           "APA2", "TrainSessionCombo", "TrainSessionComboNum")) 

    levels(proptime$variable)

    ## [1] "pTimeTarget" "pTimeCCW"    "pTimeOPP"    "pTimeCW"

    write.csv(proptime, "../results/behaviorproptime.csv", row.names = F)

This chunck of code is for summarizing the data to plot mean +/- SEM
because box plots are too unwieldly for this type of data. I add a
`measure` column for facetting or subsetting the data.

    numentr <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum, conflict), 
                                m = mean(NumEntrances), 
                                se = sd(NumEntrances)/sqrt(length(NumEntrances)), 
                                len = length(NumEntrances))
    fivenum(numentr$m)

    ## [1]  2.750000  7.166667 13.422222 16.900000 31.428571

    pathentr <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum, conflict), 
                                 m = mean(Path1stEntr), 
                                 se = sd(Path1stEntr)/sqrt(length(Path1stEntr)), 
                                 len = length(Path1stEntr))
    fivenum(pathentr$m)

    ## [1]  0.000000  0.472000  1.051786  3.865750 12.023333

    numentr$measure <- "Number of Entrances"
    pathentr$measure <- "Path to the 1st Entrance"
    PathNum <- rbind(pathentr,numentr)
    write.csv(PathNum, "../results/behaviordatasummary.csv", row.names = F)
