This script is use to import, clean, reformat, subset, and summarize
data

This first chuck of code loads and wrangles the data.

    ## [1] "yoked-consistent" "consistent"       "yoked-conflict"  
    ## [4] "conflict"

This is a long dataframe where `variable` has four levels (shock zone,
clockwise, opposite, or counter-clockwise) and `value` has the
propportion of time spent in each of those locaitons.

    ## proptime
    proptime <- behavior[,c(1,2,4,8,9,12,26:29)]
    proptime <- melt(proptime, id.vars = c("ID", "Genotype", "TrainSession",
                                           "APA2", "TrainSessionCombo", "TrainSessionComboNum")) 
    write.csv(proptime, "../results/behaviorproptime.csv", row.names = F)

This chunck of code is for summarizing the data to plot mean +/- SEM because box plots are too unwieldly for this type of data.
===============================================================================================================================

    numentr <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum, conflict), m = mean(NumEntrances), se = sd(NumEntrances)/sqrt(length(NumEntrances)), len = length(NumEntrances))

    pathentr <- dplyr::summarise(group_by(behavior, Genotype, APA2, TrainSessionComboNum, conflict), m = mean(Path1stEntr), se = sd(Path1stEntr)/sqrt(length(Path1stEntr)), len = length(Path1stEntr))

    levels(numentr$APA2) <- c("yoked-consistent","consistent", "yoked-conflict","conflict")
    levels(pathentr$APA2) <- c("yoked-consistent","consistent", "yoked-conflict","conflict")

    numentr$conflict = factor(numentr$conflict, levels = c("consistent","conflict"))
    pathentr$conflict = factor(numentr$conflict, levels = c("consistent","conflict"))
    numentr$measure <- "Number of Entrances"
    pathentr$measure <- "Path to the 1st Entrance"

    PathNum <- rbind(pathentr,numentr)
    PathNum$measure <- as.factor(PathNum$measure)

    write.csv(PathNum, "../results/behaviordatasummary.csv", row.names = F)
