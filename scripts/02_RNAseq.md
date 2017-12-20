RNAseq gene expression analysis with DESeq2
-------------------------------------------

This workflow was modified from the DESeq2 tutorial found at: <https://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.pdf>

First I load a handful of packages for data wrangling, gene expression analysis, data visualization, and statistics.

``` r
library(dplyr) ## for filtering and selecting rows
library(DESeq2) ## for gene expression analysis
library(edgeR)  ## for basic read counts status
library(genefilter)  ## for PCA fuction
library(magrittr) ## to use the weird pipe
library(ggplot2) ## for aweseom plots
library(cowplot) ## for some easy to use themes
library(car) ## stats
library(pheatmap) ## awesome heatmaps
library(viridis) # for awesome color pallette

## load functions 
source("functions_RNAseq.R")

## set output file for figures 
knitr::opts_chunk$set(fig.path = '../figures/02_RNAseq/')
```

We are ready to calculate differential gene expression using the DESeq package. For simplicity, I will use the standard nameing of "countData" and "colData" for the gene counts and gene information, respectively.

``` r
colData <- read.csv("../data/fmr1ColData.csv", header = T)
countData <- read.csv("../data/fmr1CountData.csv", header = T, check.names = F, row.names = 1)

## remove outliers
#colData <- colData %>% 
#  filter(RNAseqID != "16-123B")  %>% 
#  filter(RNAseqID != "16-125B") %>% 
#  droplevels()

savecols <- as.character(colData$RNAseqID) 
savecols <- as.vector(savecols) 
countData <- countData %>% dplyr::select(one_of(savecols)) 

# colData must be factors
cols = c(1:6)
colData[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

# daytime
colData$daytime3 <- as.character(colData$daytime)
colData$daytime3 <- ifelse(grepl("beforenoon", colData$daytime3), "daytime", 
                           ifelse(grepl("afternoon", colData$daytime3), "daytime", "nighttime"))
colData$daytime3 <- as.factor(colData$daytime3)

# summary data
colData %>% select(Genotype, APA, daytime3)  %>%  summary()
```

    ##  Genotype    APA          daytime3
    ##  FMR1:8   Yoked:16   daytime  :8  
    ##  WT  :8              nighttime:8

Total Gene Counts Per Sample
----------------------------

this could say something about data before normalization

``` r
dim(countData)
```

    ## [1] 22485    16

``` r
counts <- countData
dim( counts )
```

    ## [1] 22485    16

``` r
colSums( counts ) / 1e06  # in millions of reads
```

    ##  16-116B  16-117D  16-118B  16-118D  16-119B  16-119D  16-120B  16-120D 
    ## 2.082858 1.437951 2.903268 2.191553 2.619744 2.593812 2.869718 2.194511 
    ##  16-122B  16-122D  16-123B  16-123D  16-124D  16-125B  16-125D  16-126B 
    ## 2.778324 3.203040 0.846916 2.551592 2.595799 0.485904 2.054411 2.700689

``` r
table( rowSums( counts ) )[ 1:30 ] # Number of genes with low counts
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
    ## 5081  425  334  312  223  183  179  162  135  130  137  105  103   95   93 
    ##   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29 
    ##   78  103   77   75   67   82   68   59   65   68   57   52   61   51   53

``` r
rowsum <- as.data.frame(colSums( counts ) / 1e06 )
names(rowsum)[1] <- "millioncounts"
rowsum$sample <- row.names(rowsum)

ggplot(rowsum, aes(x=millioncounts)) + 
  geom_histogram(binwidth = 1, colour = "black", fill = "darkgrey") +
  theme_classic() +
  scale_x_continuous(name = "Millions of Gene Counts per Sample") +
  scale_y_continuous(name = "Number of Samples")
```

![](../figures/02_RNAseq/totalRNAseqcounts-1.png)

``` r
hist(rowsum$millioncounts)
```

![](../figures/02_RNAseq/totalRNAseqcounts-2.png)

DeSeq2
------

``` r
## create DESeq object using the factor Genotyp
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ Genotype)

dds$Genotype <- factor(dds$Genotype, levels=c("WT", "FMR1")) ## specify the factor levels 

dds # view the DESeq object - note numnber of genes
```

    ## class: DESeqDataSet 
    ## dim: 22485 16 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(22485): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(16): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(7): RNAseqID Mouse ... daytime daytime3

``` r
dds <- dds[ rowSums(counts(dds)) > 1, ]  # Pre-filtering genes with 0 counts
dds # view number of genes afternormalization and the number of samples
```

    ## class: DESeqDataSet 
    ## dim: 16979 16 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(16979): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(16): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(7): RNAseqID Mouse ... daytime daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data
rlddf <- assay(rld)
vsd <- getVarianceStabilizedData(dds)
```

PCA
---

``` r
# create the dataframe using my function pcadataframe
pcadata <- pcadataframe(rld, intgroup=c("Genotype"), returnData=TRUE)
write.csv(pcadata, "../results/rnaseqpcadata.csv", row.names = T)

percentVar <- round(100 * attr(pcadata, "percentVar"))
percentVar
```

    ## [1] 54 16  9  5  3  2  2  2  2

``` r
summary(aov(PC1 ~ Genotype, data=pcadata)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   54.8   54.82   1.163  0.299
    ## Residuals   14  660.0   47.14

``` r
summary(aov(PC2 ~ Genotype, data=pcadata)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   28.2   28.20   2.197   0.16
    ## Residuals   14  179.7   12.84

``` r
summary(aov(PC3 ~ Genotype, data=pcadata)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1  14.66  14.661   1.992   0.18
    ## Residuals   14 103.07   7.362

``` r
summary(aov(PC4 ~ Genotype, data=pcadata)) 
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Genotype     1   0.02   0.021   0.004  0.948
    ## Residuals   14  66.49   4.749

``` r
pcadata$Genotype <- factor(pcadata$Genotype, levels=c("WT", "FMR1"))

PCA12 <- ggplot(pcadata, aes(PC1, PC2, shape = Genotype, color = Genotype)) + 
  geom_point(size = 3, alpha = 1) +
    xlab(paste0("PC1: ", percentVar[1],"% variance")) +
    ylab(paste0("PC2: ", percentVar[2],"% variance")) +
    scale_color_manual(values =c("#404040", "#404040")) +
    theme_cowplot(font_size = 8, line_size = 0.25)  +
    theme(legend.position="none") +
    scale_shape_manual(values=c(16, 1)) 
PCA12
```

![](../figures/02_RNAseq/pca-1.png)

``` r
PCA14 <- ggplot(pcadata, aes(PC1, PC4, shape = Genotype, color = Genotype)) + 
  geom_point(size = 3, alpha = 1) +
    xlab(paste0("PC1: ", percentVar[1],"% variance")) +
    ylab(paste0("PC2: ", percentVar[4],"% variance")) +
    scale_color_manual(values =c("#404040", "#404040")) +
    theme_cowplot(font_size = 8, line_size = 0.25)  +
    theme(legend.position="none") +
    scale_shape_manual(values=c(16, 1)) 
PCA14
```

![](../figures/02_RNAseq/pca-2.png)

``` r
# pdf the same pca plots descripbed above of the above
pdf(file="../figures/02_RNAseq/PCA12.pdf", width=1.75, height=2)
plot(PCA12)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
pdf(file="../figures/02_RNAseq/PCA14.pdf", width=1.75, height=2)
plot(PCA14)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

Number of differentially expressed genes per two-way contrast
=============================================================

``` r
#calculate significance of all two way comparisions
contrast1 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 11
```

    ## [1] 29

``` r
# gene list
res <- results(dds, contrast =c("Genotype", "FMR1", "WT"), independentFiltering = T, alpha = 0.1)
summary(res)
```

    ## 
    ## out of 16917 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)     : 13, 0.077% 
    ## LFC < 0 (down)   : 16, 0.095% 
    ## outliers [1]     : 0, 0% 
    ## low counts [2]   : 6293, 37% 
    ## (mean count < 10)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

``` r
resOrdered <- res[order(res$padj),]
head(resOrdered, 10)
```

    ## log2 fold change (MAP): Genotype FMR1 vs WT 
    ## Wald test p-value: Genotype FMR1 vs WT 
    ## DataFrame with 10 rows and 6 columns
    ##          baseMean log2FoldChange      lfcSE       stat       pvalue
    ##         <numeric>      <numeric>  <numeric>  <numeric>    <numeric>
    ## Ccnd2    43.99689     -1.8572662 0.16016256 -11.596132 4.311281e-31
    ## Fmr1     77.54058     -1.0999523 0.15694873  -7.008354 2.411375e-12
    ## Kcnt1    69.17765     -0.8121867 0.16122731  -5.037525 4.715890e-07
    ## Arel1   718.10450      0.3544819 0.07386369   4.799135 1.593521e-06
    ## Slc29a4  18.12739     -0.6881522 0.16806055  -4.094668 4.227738e-05
    ## Efcab6   30.24328     -0.6767416 0.16835420  -4.019749 5.826023e-05
    ## Sstr3    26.99639     -0.6729482 0.16849586  -3.993856 6.500740e-05
    ## Apc2    494.35939      0.3624314 0.09169804   3.952444 7.735696e-05
    ## Brf1    100.46734      0.4607804 0.12597128   3.657821 2.543685e-04
    ## Cacna1g 154.16646     -0.5563997 0.15070957  -3.691867 2.226140e-04
    ##                 padj
    ##            <numeric>
    ## Ccnd2   4.607035e-27
    ## Fmr1    1.288398e-08
    ## Kcnt1   1.679800e-03
    ## Arel1   4.257091e-03
    ## Slc29a4 9.035521e-02
    ## Efcab6  9.923844e-02
    ## Sstr3   9.923844e-02
    ## Apc2    9.933341e-02
    ## Brf1    9.933341e-02
    ## Cacna1g 9.933341e-02

``` r
data <- data.frame(gene = row.names(res), padj = (res$padj), lfc = res$log2FoldChange)
data <- na.omit(data)
data <- filter(data, padj < 0.1)
data[order(data$padj),]
```

    ##         gene         padj        lfc
    ## 6      Ccnd2 4.607035e-27 -1.8572662
    ## 13      Fmr1 1.288398e-08 -1.0999523
    ## 15     Kcnt1 1.679800e-03 -0.8121867
    ## 2      Arel1 4.257091e-03  0.3544819
    ## 24   Slc29a4 9.035521e-02 -0.6881522
    ## 10    Efcab6 9.923844e-02 -0.6767416
    ## 26     Sstr3 9.923844e-02 -0.6729482
    ## 1       Apc2 9.933341e-02  0.3624314
    ## 3       Brf1 9.933341e-02  0.4607804
    ## 4    Cacna1g 9.933341e-02 -0.5563997
    ## 5       Car4 9.933341e-02 -0.6530049
    ## 7      Cpne7 9.933341e-02 -0.5195620
    ## 8       Cry2 9.933341e-02  0.3672937
    ## 9       Dlx1 9.933341e-02 -0.6151019
    ## 11     Fgfr1 9.933341e-02 -0.3824715
    ## 12    Fibcd1 9.933341e-02  0.2002234
    ## 14     Grin1 9.933341e-02  0.2661368
    ## 16   Laptm4a 9.933341e-02 -0.4293355
    ## 17     Mtus1 9.933341e-02 -0.6174893
    ## 18      Ncdn 9.933341e-02  0.1707820
    ## 19      Plat 9.933341e-02 -0.5396718
    ## 20    Pnmal2 9.933341e-02  0.3144891
    ## 21     Prpf8 9.933341e-02  0.3478673
    ## 22 Serpina3n 9.933341e-02 -0.4769215
    ## 23     Sidt1 9.933341e-02  0.3978328
    ## 25    Slc8a2 9.933341e-02  0.2202670
    ## 27      Tnik 9.933341e-02  0.3904730
    ## 28     Wipf3 9.933341e-02  0.2778451
    ## 29      Xbp1 9.933341e-02 -0.5057252

``` r
topGene <- rownames(res)[which.min(res$padj)]
plotCounts(dds, gene = topGene, intgroup=c("Genotype"))
```

![](../figures/02_RNAseq/Twowaycontrasts3-1.png)

``` r
data <- data.frame(gene = row.names(res),
                   pvalue = -log10(res$padj), 
                   lfc = res$log2FoldChange)
data <- na.omit(data)
data <- data %>%
  mutate(color = ifelse(data$lfc > 0 & data$pvalue > 1, 
                        yes = "FMR1", 
                        no = ifelse(data$lfc < 0 & data$pvalue > 1, 
                                    yes = "WT", no = "none")))

FMR1volcano <- ggplot(data, aes(x = lfc, y = pvalue)) + 
  geom_point(aes(color = factor(color), shape = factor(color)), size = 1, alpha = 0.8, na.rm = T) + # add gene points
  theme_cowplot(font_size = 8, line_size = 0.25) +
  geom_hline(yintercept = 1,  size = 0.25, linetype = 2) + 
  scale_color_manual(values = c("black", "grey", "black"))  + 
  scale_shape_manual(values = c(1,16,16))  + 
  xlab(paste0("Log Fold Change")) +       
  ylab(paste0("-log(p-value)")) + 
    scale_x_continuous( limits=c(-2, 2)) +
  theme(panel.grid.minor=element_blank(),
        legend.position = "none", # remove legend 
        panel.grid.major=element_blank())
FMR1volcano
```

![](../figures/02_RNAseq/Twowaycontrasts3-2.png)

``` r
pdf(file="../figures/02_RNAseq/FMR1volcano.pdf", width=2, height=2)
plot(FMR1volcano)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

GO setup
========

``` r
#calculate significance of all two way comparisions
contrast1 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 11
```

    FALSE [1] 29

``` r
GOpvals <- assay(rld)
GOpvals <- cbind(GOpvals, contrast1)
GOpvals <- as.data.frame(GOpvals)
GOpvals <- GOpvals[ , grepl( "padj|pval" , names( GOpvals ) ) ]

GOpvals$gene<-rownames(GOpvals)

GOpvals <- GOpvals %>%
  select(gene, pvalGenotypeFMR1WT)
GOpvals$logP <- log(GOpvals$pvalGenotypeFMR1WT)
GOpvals <- GOpvals %>%
  select(gene, logP)

write.csv(GOpvals, "./06_GO_MWU/GenotypeFMR1KOWT_GOpvals.csv", row.names = F)
```

Heatmap
=======

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- DEGes$padjGenotypeFMR1WT

write.csv(as.data.frame(DEGes), "../results/02_DEGes.csv", row.names = F)

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("Genotype")]) ## matrix to df
rownames(df) <- names(countData)
colnames(df) <- "Genotype"


ann_colors <- list(Genotype =  
                     c('FMR1' = (values=c("white")), 
                       'WT' = (values=c("#404040"))))

# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = T,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 11, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation" 
         )
```

![](../figures/02_RNAseq/heatmap-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = T,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 10, treeheight_col = 10,
         legend=T,
         fontsize = 4.5, 
         width=3.5, height=3.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         cellheight = 4.5, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/pheatmap.pdf"
         )

pheatmap(DEGes, show_colnames=F, show_rownames = T,
         annotation_col=df, annotation_colors = ann_colors, 
         annotation_row = NA, 
         annotation_legend = FALSE,
         annotation_names_row = FALSE, annotation_names_col = FALSE,
         treeheight_row = 10, treeheight_col = 10,
         fontsize = 7, 
         border_color = "grey60" ,
         color = viridis(30),
         width=2.5, height=3.25,
         #cellwidth = 10,
         #cellheight = 7,
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation", 
         filename = "../figures/02_RNAseq/pheatmap_minimal.pdf"
         )
```

Venn Diagram of both study's DEGS
---------------------------------

``` r
contrast1 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 11
```

    ## [1] 29

``` r
#create a new DF with the gene counts
rldpvals <- assay(rld)
rldpvals <- cbind(rldpvals, contrast1)
rldpvals <- as.data.frame(rldpvals)
rldpvals <- rldpvals[ , grepl( "padj|pval" , names( rldpvals ) ) ]
names(rldpvals)
```

    ## [1] "pvalGenotypeFMR1WT" "padjGenotypeFMR1WT"

``` r
# venn with padj values
venn1 <- row.names(rldpvals[rldpvals[2] <0.1 & !is.na(rldpvals[2]),])
```

``` r
multiqc01 <- read.csv(file = "../data/multiqc_report_01.csv")
summary(multiqc01)
```

    ##                   Sample.Name      Dups            X..GC       
    ##  16_116B_S3_L002_R1_001 : 1   Min.   :0.3600   Min.   :0.4400  
    ##  16_116B_S3_L002_R2_001 : 1   1st Qu.:0.4725   1st Qu.:0.4800  
    ##  16_116D_S5_L002_R1_001 : 1   Median :0.5250   Median :0.4900  
    ##  16_116D_S5_L002_R2_001 : 1   Mean   :0.5459   Mean   :0.4841  
    ##  16_117D_S15_L002_R1_001: 1   3rd Qu.:0.6175   3rd Qu.:0.4900  
    ##  16_117D_S15_L002_R2_001: 1   Max.   :0.7900   Max.   :0.5000  
    ##  (Other)                :28                                    
    ##   MillionReads  
    ##  Min.   :3.300  
    ##  1st Qu.:3.600  
    ##  Median :3.800  
    ##  Mean   :3.918  
    ##  3rd Qu.:4.200  
    ##  Max.   :5.100  
    ## 

``` r
# mean 3.9 million reads per sample. Min 3.3 max 5.1
hist(multiqc01$MillionReads)
```

![](../figures/02_RNAseq/multiqc-1.png)

``` r
boxplot(multiqc01$MillionReads,
        xlab = "All samples",
        ylab = "Million reads per sample")
```

![](../figures/02_RNAseq/multiqc-2.png)

``` r
multiqc03 <- read.csv(file = "../data/multiqc_report_03.csv")
summary(multiqc03)
```

    ##                   Sample.Name   FragLength    FractionAligned 
    ##  16_116B_S3_L002_R1_001 : 1   Min.   :210.0   Min.   :0.1000  
    ##  16_116D_S5_L002_R1_001 : 1   1st Qu.:224.9   1st Qu.:0.5700  
    ##  16_117D_S15_L002_R1_001: 1   Median :230.4   Median :0.6500  
    ##  16_118B_S16_L002_R1_001: 1   Mean   :229.5   Mean   :0.5735  
    ##  16_118D_S17_L002_R1_001: 1   3rd Qu.:236.0   3rd Qu.:0.7000  
    ##  16_119B_S8_L002_R1_001 : 1   Max.   :237.1   Max.   :0.7700  
    ##  (Other)                :11                                   
    ##  MillionAligned                        R1        R1_Dups      
    ##  Min.   :0.400   16_116B_S3_L002_R1_001 : 1   Min.   :0.4300  
    ##  1st Qu.:2.100   16_116D_S5_L002_R1_001 : 1   1st Qu.:0.5300  
    ##  Median :2.600   16_117D_S15_L002_R1_001: 1   Median :0.5900  
    ##  Mean   :2.212   16_118B_S16_L002_R1_001: 1   Mean   :0.5971  
    ##  3rd Qu.:2.800   16_118D_S17_L002_R1_001: 1   3rd Qu.:0.6300  
    ##  Max.   :3.300   16_119B_S8_L002_R1_001 : 1   Max.   :0.7900  
    ##                  (Other)                :11                   
    ##      R1_GC        MillionReads_R1                       R2    
    ##  Min.   :0.4400   Min.   :3.300   16_116B_S3_L002_R2_001 : 1  
    ##  1st Qu.:0.4800   1st Qu.:3.600   16_116D_S5_L002_R2_001 : 1  
    ##  Median :0.4900   Median :3.800   16_117D_S15_L002_R2_001: 1  
    ##  Mean   :0.4829   Mean   :3.918   16_118B_S16_L002_R2_001: 1  
    ##  3rd Qu.:0.4900   3rd Qu.:4.200   16_118D_S17_L002_R2_001: 1  
    ##  Max.   :0.5000   Max.   :5.100   16_119B_S8_L002_R2_001 : 1  
    ##                                   (Other)                :11  
    ##     R2_Dups           R2_GC        MillionReads_R2
    ##  Min.   :0.3600   Min.   :0.4400   Min.   :3.300  
    ##  1st Qu.:0.4300   1st Qu.:0.4800   1st Qu.:3.600  
    ##  Median :0.4900   Median :0.4900   Median :3.800  
    ##  Mean   :0.4947   Mean   :0.4853   Mean   :3.918  
    ##  3rd Qu.:0.5200   3rd Qu.:0.5000   3rd Qu.:4.200  
    ##  Max.   :0.6500   Max.   :0.5000   Max.   :5.100  
    ## 

``` r
library(stats) # for standard deviation
sd(multiqc03$FragLength)
```

    ## [1] 6.943167

``` r
# mean 2.2 million reads alligned per sample. max 3.3 min 0.4
hist(multiqc03$MillionAligned)
```

![](../figures/02_RNAseq/multiqc-3.png)

``` r
boxplot(multiqc03$MillionAligned,
        xlab = "All samples",
        ylab = "Million reads aligned per sample")
```

![](../figures/02_RNAseq/multiqc-4.png)

``` r
boxplot(multiqc03$FractionAligned,
        xlab = "All samples",
        ylab = "Fraction aligned")
```

![](../figures/02_RNAseq/multiqc-5.png)

Write the files
---------------

``` r
write.csv(vsd, file = "../results/02_vsd.csv", row.names = T)
write.csv(rlddf, file = "../results/02_rlddf.csv", row.names = T)
write.csv(colData, file = "../results/02_colData.csv", row.names = T)
write.csv(data, file = "../results/FMR1_CA1_rnaseq.csv", row.names = F)
write.csv(venn1, file = "../results/FMR1_CA1_venn1.csv", row.names = F)
```
