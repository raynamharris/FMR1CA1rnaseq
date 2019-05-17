CA1 setup for deseq
-------------------

    # data from other rnaseq study
    # fiture to keep only ca1 samples
    int.colData <- read.csv("~/GitHub/IntegrativeProjectWT2015/data/02a_colData.csv", check.names = F)
    int.countData <- read.csv("~/GitHub/IntegrativeProjectWT2015/data/02a_countData.csv", check.names = F)

    int.colData <- int.colData %>%
          dplyr::filter(Punch == "CA1") %>%
          droplevels()

    names(int.countData)[1]<-"geneID"
    row.names(int.countData) <- int.countData$geneID

    savecols <- as.character(int.colData$RNAseqID) 
    savecols <- as.vector(savecols) 

    int.countData <- int.countData %>% dplyr::select(one_of(savecols)) 


    # data from this fmr1 study
    fmr.colData <- read.csv("../data/fmr1ColData.csv", header = T)
    fmr.countData <- read.csv("../data/fmr1CountData.csv", header = T, check.names = F, row.names = 1)

    # confirm proper subsetting
    print(ncol(fmr.countData) == nrow(fmr.colData))

    ## [1] TRUE

    print(ncol(int.countData) == nrow(int.colData))

    ## [1] TRUE

    # fill in details to make datasets mergable
    int.colData$Genotype <- "WT"
    fmr.colData$APA2 <- "yoked_consistent"
    fmr.colData$Punch <- "CA1"

    fmr.colData$treatment <- paste(fmr.colData$Genotype, fmr.colData$APA, sep = "_")
    int.colData$treatment <- paste(int.colData$Genotype, int.colData$APA, sep = "_")

    int.colData <- int.colData %>% select(RNAseqID, Mouse, Punch, Genotype, APA2, treatment)
    fmr.colData <- fmr.colData %>% select(RNAseqID, Mouse, Punch, Genotype, APA2, treatment)

    colData <- rbind(fmr.colData, int.colData)


    fmr.countData$geneID <- row.names(fmr.countData)
    int.countData$geneID <- row.names(int.countData)

    countData <- inner_join(fmr.countData, int.countData)

    ## Joining, by = "geneID"

    colData$Punch <- as.factor(colData$Punch)
    colData$treatment <- as.factor(colData$treatment)
    row.names(countData) <- countData$geneID
    countData$geneID <- NULL
    row.names(colData) <- colData$RNAseqID
    head(colData)

    ##         RNAseqID   Mouse Punch Genotype             APA2  treatment
    ## 16-116B  16-116B 16-116B   CA1     FMR1 yoked_consistent FMR1_Yoked
    ## 16-117D  16-117D 16-117D   CA1     FMR1 yoked_consistent FMR1_Yoked
    ## 16-118B  16-118B 16-118B   CA1     FMR1 yoked_consistent FMR1_Yoked
    ## 16-118D  16-118D 16-118D   CA1     FMR1 yoked_consistent FMR1_Yoked
    ## 16-119B  16-119B 16-119B   CA1     FMR1 yoked_consistent FMR1_Yoked
    ## 16-119D  16-119D 16-119D   CA1     FMR1 yoked_consistent FMR1_Yoked

     # check that row and col lenghts are equal
    print(ncol(countData) == nrow(colData))

    ## [1] TRUE

    dds <- DESeqDataSetFromMatrix(countData = countData,
                                  colData = colData,
                                  design = ~ treatment )
    dds <- dds[ rowSums(counts(dds)) > 2, ] ## pre-filter genes 
    dds <- DESeq(dds) # Differential expression analysis

    ## estimating size factors

    ## estimating dispersions

    ## gene-wise dispersion estimates

    ## mean-dispersion relationship

    ## final dispersion estimates

    ## fitting model and testing

    ## -- replacing outliers and refitting for 116 genes
    ## -- DESeq argument 'minReplicatesForReplace' = 7 
    ## -- original counts are preserved in counts(dds)

    ## estimating dispersions

    ## fitting model and testing

    vsd <- vst(dds, blind=FALSE) # variance stabilized 

    #create list of groups
    a <- levels(colData$treatment)
    b <- a

    numDEGs <- function(group1, group2){
      res <- results(dds, contrast = c("treatment", group1, group2), independentFiltering = T)
      sumpadj <- sum(res$padj < 0.1, na.rm = TRUE)
      return(sumpadj)
    }

    # comapre all contrasts, save to datafrmes
    dat=data.frame()
    for (i in a){
      for (j in b){
        if (i != j) {
          k <- paste(i,j, sep = "") #assigns usique rownames
          dat[k,1]<-i               
          dat[k,2]<-j
          dat[k,3]<- numDEGs(i,j) #caluculates number of DEGs
        }
      }
    }

    head(dat)

    ##                                        V1                  V2   V3
    ## FMR1_YokedWT_conflict          FMR1_Yoked         WT_conflict 2206
    ## FMR1_YokedWT_consistent        FMR1_Yoked       WT_consistent  134
    ## FMR1_YokedWT_Yoked             FMR1_Yoked            WT_Yoked    6
    ## FMR1_YokedWT_yoked_conflict    FMR1_Yoked   WT_yoked_conflict 1224
    ## FMR1_YokedWT_yoked_consistent  FMR1_Yoked WT_yoked_consistent 3103
    ## WT_conflictFMR1_Yoked         WT_conflict          FMR1_Yoked 2206

    # widen data to create table of degs
    rownames(dat) <- NULL #remove row names
    data_wide <- spread(dat, V2, V3)
    print(data_wide) 

    ##                    V1 FMR1_Yoked WT_conflict WT_consistent WT_Yoked
    ## 1          FMR1_Yoked         NA        2206           134        6
    ## 2         WT_conflict       2206          NA             0     1339
    ## 3       WT_consistent        134           0            NA        4
    ## 4            WT_Yoked          6        1339             4       NA
    ## 5   WT_yoked_conflict       1224           2             1      353
    ## 6 WT_yoked_consistent       3103        1108          1733     2645
    ##   WT_yoked_conflict WT_yoked_consistent
    ## 1              1224                3103
    ## 2                 2                1108
    ## 3                 1                1733
    ## 4               353                2645
    ## 5                NA                1738
    ## 6              1738                  NA

    # set factors
    allcontrasts <- dat %>%
      ggplot( aes(V1, V2)) +
        geom_tile(aes(fill = V3)) +
        scale_fill_viridis(na.value="#FFFFFF00") + 
        xlab(" ") + ylab("Timepoint") +
        labs(fill = "# of DEGs",
             subtitle = "CA1")
    plot(allcontrasts)

![](../figures/02_RNAseqCA1integrative/unnamed-chunk-1-1.png)

    # create the dataframe using my function pcadataframe
    pcadataframe <- function (object, intgroup = "condition", ntop = 500, returnData = FALSE) 
    {
      rv <- rowVars(assay(object))
      select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
      pca <- prcomp(t(assay(object)[select, ]))
      percentVar <- pca$sdev^2/sum(pca$sdev^2)
      if (!all(intgroup %in% names(colData(object)))) {
        stop("the argument 'intgroup' should specify columns of colData(dds)")
      }
      intgroup.df <- as.data.frame(colData(object)[, intgroup, 
                                                   drop = FALSE])
      group <- if (length(intgroup) > 1) {
        factor(apply(intgroup.df, 1, paste, collapse = " : "))
      }
      else {
        colData(object)[[intgroup]]
      }
      d <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], PC3 = pca$x[, 3], PC4 = pca$x[, 4],PC5 = pca$x[, 5],PC6 = pca$x[, 6],group = group, 
                      intgroup.df, name = colnames(object))
      if (returnData) {
        attr(d, "percentVar") <- percentVar[1:6]
        return(d)
      }
    }


    pcadata <- pcadataframe(vsd, intgroup=c("treatment"), returnData=TRUE)
    percentVar <- round(100 * attr(pcadata, "percentVar"))
    percentVar

    ## [1] 34 22 11  8  4  3

    pca12 <- ggplot(pcadata, aes(PC1, PC2,color = treatment)) + 
      geom_point(size = 2, alpha = 1) +
      stat_ellipse(type = "t") +
      xlab(paste0("PC1: ", percentVar[1],"% variance")) +
      ylab(paste0("PC2: ", percentVar[2],"% variance")) +
      theme_cowplot(font_size = 8, line_size = 0.25) +
      labs(subtitle = "CA1")
    print(pca12)

    ## Too few points to calculate an ellipse

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](../figures/02_RNAseqCA1integrative/unnamed-chunk-1-2.png)

    print(summary(aov(PC1 ~ treatment, data=pcadata)))

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## treatment    5  282.5   56.50   1.272  0.307
    ## Residuals   25 1110.4   44.42

    print(TukeyHSD(aov(PC1 ~ treatment, data=pcadata), which = "treatment"))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = PC1 ~ treatment, data = pcadata)
    ## 
    ## $treatment
    ##                                             diff        lwr       upr
    ## WT_conflict-FMR1_Yoked                 5.7389442  -6.838506 18.316395
    ## WT_consistent-FMR1_Yoked               2.8417994  -9.735651 15.419250
    ## WT_Yoked-FMR1_Yoked                   -3.1781556 -13.447601  7.091290
    ## WT_yoked_conflict-FMR1_Yoked          -0.8933379 -12.602307 10.815631
    ## WT_yoked_consistent-FMR1_Yoked        -4.1323124 -20.369731 12.105106
    ## WT_consistent-WT_conflict             -2.8971448 -17.420333 11.626044
    ## WT_Yoked-WT_conflict                  -8.9170999 -21.494550  3.660350
    ## WT_yoked_conflict-WT_conflict         -6.6322821 -20.410189  7.145624
    ## WT_yoked_consistent-WT_conflict       -9.8712566 -27.658457  7.915944
    ## WT_Yoked-WT_consistent                -6.0199550 -18.597405  6.557495
    ## WT_yoked_conflict-WT_consistent       -3.7351373 -17.513044 10.042769
    ## WT_yoked_consistent-WT_consistent     -6.9741118 -24.761313 10.813089
    ## WT_yoked_conflict-WT_Yoked             2.2848178  -9.424151 13.993787
    ## WT_yoked_consistent-WT_Yoked          -0.9541567 -17.191575 15.283262
    ## WT_yoked_consistent-WT_yoked_conflict -3.2389745 -20.423043 13.945094
    ##                                           p adj
    ## WT_conflict-FMR1_Yoked                0.7230715
    ## WT_consistent-FMR1_Yoked              0.9806742
    ## WT_Yoked-FMR1_Yoked                   0.9280668
    ## WT_yoked_conflict-FMR1_Yoked          0.9998881
    ## WT_yoked_consistent-FMR1_Yoked        0.9676843
    ## WT_consistent-WT_conflict             0.9889102
    ## WT_Yoked-WT_conflict                  0.2795551
    ## WT_yoked_conflict-WT_conflict         0.6773533
    ## WT_yoked_consistent-WT_conflict       0.5380068
    ## WT_Yoked-WT_consistent                0.6824286
    ## WT_yoked_conflict-WT_consistent       0.9578672
    ## WT_yoked_consistent-WT_consistent     0.8287155
    ## WT_yoked_conflict-WT_Yoked            0.9899644
    ## WT_yoked_consistent-WT_Yoked          0.9999692
    ## WT_yoked_consistent-WT_yoked_conflict 0.9914299

    print(summary(aov(PC2 ~ treatment, data=pcadata))) 

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## treatment    5  653.7  130.75   13.23 2.35e-06 ***
    ## Residuals   25  247.1    9.88                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    print(TukeyHSD(aov(PC2 ~ treatment, data=pcadata), which = "treatment")) 

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = PC2 ~ treatment, data = pcadata)
    ## 
    ## $treatment
    ##                                              diff        lwr       upr
    ## WT_conflict-FMR1_Yoked                 -9.2330594 -15.165964 -3.300155
    ## WT_consistent-FMR1_Yoked               -4.7437616 -10.676666  1.189143
    ## WT_Yoked-FMR1_Yoked                    -2.0744991  -6.918695  2.769697
    ## WT_yoked_conflict-FMR1_Yoked           -9.4059896 -14.929223 -3.882756
    ## WT_yoked_consistent-FMR1_Yoked        -15.4154373 -23.074784 -7.756091
    ## WT_consistent-WT_conflict               4.4892978  -2.361430 11.340025
    ## WT_Yoked-WT_conflict                    7.1585603   1.225656 13.091464
    ## WT_yoked_conflict-WT_conflict          -0.1729301  -6.672101  6.326241
    ## WT_yoked_consistent-WT_conflict        -6.1823779 -14.572771  2.208016
    ## WT_Yoked-WT_consistent                  2.6692625  -3.263642  8.602167
    ## WT_yoked_conflict-WT_consistent        -4.6622280 -11.161399  1.836943
    ## WT_yoked_consistent-WT_consistent     -10.6716757 -19.062069 -2.281282
    ## WT_yoked_conflict-WT_Yoked             -7.3314905 -12.854724 -1.808257
    ## WT_yoked_consistent-WT_Yoked          -13.3409382 -21.000285 -5.681592
    ## WT_yoked_consistent-WT_yoked_conflict  -6.0094477 -14.115338  2.096442
    ##                                           p adj
    ## WT_conflict-FMR1_Yoked                0.0008062
    ## WT_consistent-FMR1_Yoked              0.1732402
    ## WT_Yoked-FMR1_Yoked                   0.7716268
    ## WT_yoked_conflict-FMR1_Yoked          0.0002571
    ## WT_yoked_consistent-FMR1_Yoked        0.0000236
    ## WT_consistent-WT_conflict             0.3599386
    ## WT_Yoked-WT_conflict                  0.0116109
    ## WT_yoked_conflict-WT_conflict         0.9999994
    ## WT_yoked_consistent-WT_conflict       0.2428901
    ## WT_Yoked-WT_consistent                0.7343893
    ## WT_yoked_conflict-WT_consistent       0.2681528
    ## WT_yoked_consistent-WT_consistent     0.0071454
    ## WT_yoked_conflict-WT_Yoked            0.0047009
    ## WT_yoked_consistent-WT_Yoked          0.0001901
    ## WT_yoked_consistent-WT_yoked_conflict 0.2372677

    pca34 <- ggplot(pcadata, aes(PC3, PC4,color = treatment)) + 
      geom_point(size = 2, alpha = 1) +
      stat_ellipse(type = "t") +
      xlab(paste0("PC3: ", percentVar[3],"% variance")) +
      ylab(paste0("PC4: ", percentVar[4],"% variance")) +
      theme_cowplot(font_size = 8, line_size = 0.25) +
      labs(subtitle = "CA1")
    print(pca34)

    ## Too few points to calculate an ellipse

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](../figures/02_RNAseqCA1integrative/unnamed-chunk-1-3.png)

    print(summary(aov(PC4 ~ treatment, data=pcadata))) 

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## treatment    5  133.2  26.632   3.519 0.0152 *
    ## Residuals   25  189.2   7.569                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    print(TukeyHSD(aov(PC4 ~ treatment, data=pcadata), which = "treatment")) 

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = PC4 ~ treatment, data = pcadata)
    ## 
    ## $treatment
    ##                                             diff        lwr        upr
    ## WT_conflict-FMR1_Yoked                -4.4555726 -9.6474796  0.7363344
    ## WT_consistent-FMR1_Yoked              -3.6110621 -8.8029691  1.5808449
    ## WT_Yoked-FMR1_Yoked                   -2.2101259 -6.4493002  2.0290485
    ## WT_yoked_conflict-FMR1_Yoked          -3.7397524 -8.5731548  1.0936500
    ## WT_yoked_consistent-FMR1_Yoked         3.0587207 -3.6440025  9.7614439
    ## WT_consistent-WT_conflict              0.8445105 -5.1505874  6.8396083
    ## WT_Yoked-WT_conflict                   2.2454467 -2.9464603  7.4373538
    ## WT_yoked_conflict-WT_conflict          0.7158202 -4.9716290  6.4032694
    ## WT_yoked_consistent-WT_conflict        7.5142933  0.1718280 14.8567586
    ## WT_Yoked-WT_consistent                 1.4009363 -3.7909708  6.5928433
    ## WT_yoked_conflict-WT_consistent       -0.1286902 -5.8161394  5.5587589
    ## WT_yoked_consistent-WT_consistent      6.6697828 -0.6726825 14.0122482
    ## WT_yoked_conflict-WT_Yoked            -1.5296265 -6.3630289  3.3037759
    ## WT_yoked_consistent-WT_Yoked           5.2688466 -1.4338766 11.9715697
    ## WT_yoked_consistent-WT_yoked_conflict  6.7984731 -0.2950224 13.8919685
    ##                                           p adj
    ## WT_conflict-FMR1_Yoked                0.1233086
    ## WT_consistent-FMR1_Yoked              0.2985265
    ## WT_Yoked-FMR1_Yoked                   0.6019172
    ## WT_yoked_conflict-FMR1_Yoked          0.1997996
    ## WT_yoked_consistent-FMR1_Yoked        0.7229794
    ## WT_consistent-WT_conflict             0.9978007
    ## WT_Yoked-WT_conflict                  0.7644876
    ## WT_yoked_conflict-WT_conflict         0.9987164
    ## WT_yoked_consistent-WT_conflict       0.0426918
    ## WT_Yoked-WT_consistent                0.9586781
    ## WT_yoked_conflict-WT_consistent       0.9999997
    ## WT_yoked_consistent-WT_consistent     0.0906478
    ## WT_yoked_conflict-WT_Yoked            0.9215169
    ## WT_yoked_consistent-WT_Yoked          0.1867457
    ## WT_yoked_consistent-WT_yoked_conflict 0.0658282
