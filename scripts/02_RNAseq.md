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
source("figureoptions.R")
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
colData$daytime2 <- as.character(colData$daytime)
colData$daytime2 <- ifelse(grepl("beforenoon", colData$daytime2), "beforenoon", "afternoon")
colData$daytime2 <- as.factor(colData$daytime2)

colData$daytime3 <- as.character(colData$daytime)
colData$daytime3 <- ifelse(grepl("beforenoon", colData$daytime3), "daytime", 
                           ifelse(grepl("afternoon", colData$daytime3), "daytime", "nighttime"))
colData$daytime3 <- as.factor(colData$daytime3)

# summary data
colData %>% select(Genotype, APA, daytime, daytime2, daytime3)  %>%  summary()
```

    ##  Genotype    APA           daytime        daytime2       daytime3
    ##  FMR1:8   Yoked:16   afternoon :2   afternoon :10   daytime  :8  
    ##  WT  :8              beforenoon:6   beforenoon: 6   nighttime:8  
    ##                      evening   :5                                
    ##                      nighttime :3

Total Gene Counts Per Sample
----------------------------

this could say something about data before normalization

``` r
## stats
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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

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
head(resOrdered, 20)
```

    ## log2 fold change (MAP): Genotype FMR1 vs WT 
    ## Wald test p-value: Genotype FMR1 vs WT 
    ## DataFrame with 20 rows and 6 columns
    ##           baseMean log2FoldChange      lfcSE       stat       pvalue
    ##          <numeric>      <numeric>  <numeric>  <numeric>    <numeric>
    ## Ccnd2     43.99689     -1.8572662 0.16016256 -11.596132 4.311281e-31
    ## Fmr1      77.54058     -1.0999523 0.15694873  -7.008354 2.411375e-12
    ## Kcnt1     69.17765     -0.8121867 0.16122731  -5.037525 4.715890e-07
    ## Arel1    718.10450      0.3544819 0.07386369   4.799135 1.593521e-06
    ## Slc29a4   18.12739     -0.6881522 0.16806055  -4.094668 4.227738e-05
    ## ...            ...            ...        ...        ...          ...
    ## Fibcd1  1486.45413      0.2002234 0.05452206   3.672338 0.0002403411
    ## Grin1   2880.63646      0.2661368 0.07221541   3.685319 0.0002284160
    ## Laptm4a  235.32818     -0.4293355 0.11382405  -3.771923 0.0001619944
    ## Mtus1     15.70662     -0.6174893 0.16296253  -3.789149 0.0001511643
    ## Ncdn    8100.27182      0.1707820 0.04683268   3.646641 0.0002656902
    ##                 padj
    ##            <numeric>
    ## Ccnd2   4.607035e-27
    ## Fmr1    1.288398e-08
    ## Kcnt1   1.679800e-03
    ## Arel1   4.257091e-03
    ## Slc29a4 9.035521e-02
    ## ...              ...
    ## Fibcd1    0.09933341
    ## Grin1     0.09933341
    ## Laptm4a   0.09933341
    ## Mtus1     0.09933341
    ## Ncdn      0.09933341

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

ggplot(data, aes(x = lfc, y = pvalue)) + 
  geom_point(aes(color = factor(color), shape = factor(color)), size = 8, alpha = 0.8, na.rm = T) + # add gene points
  theme_cowplot(font_size = 8, line_size = 0.25) +
  geom_hline(yintercept = 1,  size = 0.25, linetype = 2) + 
  scale_color_manual(values = c("black", "grey", "black"))  + 
  scale_shape_manual(values = c(1,16,16))  + 
  xlab(paste0("Log Fold Change")) +       
  ylab(paste0("-log(p-value)")) +       
  theme(panel.grid.minor=element_blank(),
        legend.position = "none", # remove legend 
        panel.grid.major=element_blank())
```

![](../figures/02_RNAseq/Twowaycontrasts3-2.png)

``` r
FMR1volcano <- ggplot(data, aes(x = lfc, y = pvalue)) + 
  geom_point(aes(color = factor(color), shape = factor(color)), size = 1, alpha = 0.8, na.rm = T) + # add gene points
  theme_cowplot(font_size = 8, line_size = 0.25) +
  geom_hline(yintercept = 1,  size = 0.25, linetype = 2) + 
  scale_color_manual(values = c("black", "grey", "black"))  + 
  scale_shape_manual(values = c(1,16,16))  + 
  xlab(paste0("Log Fold Change")) +       
  ylab(paste0("-log(p-value)")) +       
  theme(panel.grid.minor=element_blank(),
        legend.position = "none", # remove legend 
        panel.grid.major=element_blank())
FMR1volcano
```

![](../figures/02_RNAseq/Twowaycontrasts3-3.png)

``` r
pdf(file="../figures/02_RNAseq/FMR1volcano.pdf", width=1.5, height=2)
plot(FMR1volcano)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- DEGes$padjGenotypeFMR1WT

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
ann_colors <- ann_colorsGenotype

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
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
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
         treeheight_row = 20, treeheight_col = 20,
         fontsize = 8, 
         border_color = "grey60" ,
         color = viridis(30),
         width=3.5, height=2.25,
         cellwidth = 10,
         #cellheight = 10,
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation", 
         filename = "../figures/02_RNAseq/pheatmap_minimal.pdf"
         )
```

Daytime 2
---------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ daytime2)
dds # view the DESeq object - note numnber of genes
```

    ## class: DESeqDataSet 
    ## dim: 22485 16 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(22485): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(16): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime2", "afternoon", "beforenoon"), mypval = 0.1) # 793 (337 with outliers)
```

    ## [1] 337

``` r
res <- results(dds, contrast =c("daytime2", "afternoon", "beforenoon"), independentFiltering = T, alpha = 0.1)
resOrdered <- res[order(res$padj),]
head(resOrdered, 10)
```

    ## log2 fold change (MAP): daytime2 afternoon vs beforenoon 
    ## Wald test p-value: daytime2 afternoon vs beforenoon 
    ## DataFrame with 10 rows and 6 columns
    ##           baseMean log2FoldChange      lfcSE      stat       pvalue
    ##          <numeric>      <numeric>  <numeric> <numeric>    <numeric>
    ## Cck      588.80246     -0.6350744 0.13009251 -4.881714 1.051678e-06
    ## Srrm2   1882.70766      0.2807543 0.05746930  4.885292 1.032759e-06
    ## Ctsb     867.49785     -0.3369583 0.07345542 -4.587248 4.491263e-06
    ## Irs2     376.36499      0.6326652 0.13580481  4.658636 3.183107e-06
    ## Laptm4a  235.32818     -0.4971941 0.10927261 -4.550034 5.363717e-06
    ## Ndufb7    38.50238     -0.8602495 0.18544286 -4.638892 3.502814e-06
    ## Nrgn    3194.30034     -0.3990034 0.08614294 -4.631876 3.623679e-06
    ## Ube2m    161.30997     -0.4559715 0.09981091 -4.568353 4.915722e-06
    ## Scand1    55.77737     -0.7230971 0.15987947 -4.522764 6.103730e-06
    ## Sepw1    764.67921     -0.5183379 0.11528367 -4.496195 6.918027e-06
    ##                padj
    ##           <numeric>
    ## Cck     0.004417049
    ## Srrm2   0.004417049
    ## Ctsb    0.005631902
    ## Irs2    0.005631902
    ## Laptm4a 0.005631902
    ## Ndufb7  0.005631902
    ## Nrgn    0.005631902
    ## Ube2m   0.005631902
    ## Scand1  0.005696815
    ## Sepw1   0.005811142

``` r
summary(res)
```

    ## 
    ## out of 16936 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)     : 146, 0.86% 
    ## LFC < 0 (down)   : 191, 1.1% 
    ## outliers [1]     : 33, 0.19% 
    ## low counts [2]   : 8546, 50% 
    ## (mean count < 26)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- DEGes$padjdaytime2afternoonbeforenoon

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime2")]) ## matrix to df
rownames(df) <- names(countData)
colnames(df) <- "daytime2"
ann_colors <- ann_colorsdaytime2

# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-1-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = F,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/daytimeheat.pdf"
         )
```

Daytime 3 - daytime nighttime
-----------------------------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ daytime3)
dds # view the DESeq object - note numnber of genes
```

    ## class: DESeqDataSet 
    ## dim: 22485 16 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(22485): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(16): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime3", "nighttime", "daytime"), mypval = 0.1) # 978 (530 with outliers included)
```

    ## [1] 530

``` r
res <- results(dds, contrast =c("daytime3", "nighttime", "daytime"), independentFiltering = T, alpha = 0.1)
resOrdered <- res[order(res$padj),]
head(resOrdered, 20)
```

    ## log2 fold change (MAP): daytime3 nighttime vs daytime 
    ## Wald test p-value: daytime3 nighttime vs daytime 
    ## DataFrame with 20 rows and 6 columns
    ##          baseMean log2FoldChange      lfcSE      stat       pvalue
    ##         <numeric>      <numeric>  <numeric> <numeric>    <numeric>
    ## Efna3    181.3672     -0.5476312 0.09766881 -5.607023 2.058368e-08
    ## Mif      111.3487     -0.7186259 0.12918912 -5.562588 2.658023e-08
    ## Fkbp4    143.0442     -0.6114104 0.11215853 -5.451306 5.000125e-08
    ## Cox4i1   350.4879     -0.6230585 0.12034535 -5.177254 2.251752e-07
    ## Nedd4l   534.9398      0.3429340 0.06696366  5.121195 3.036055e-07
    ## ...           ...            ...        ...       ...          ...
    ## Laptm4a  235.3282     -0.5076397 0.10820731 -4.691362 2.713917e-06
    ## Lsamp   1186.4206      0.3987008 0.08594989  4.638759 3.505070e-06
    ## Kbtbd11  721.2242      0.3339773 0.07250264  4.606416 4.096679e-06
    ## Rbfox2   751.8005      0.2934002 0.06397542  4.586139 4.515182e-06
    ## Rps25     94.3567     -0.6672838 0.14618214 -4.564742 5.001082e-06
    ##                 padj
    ##            <numeric>
    ## Efna3   0.0000853757
    ## Mif     0.0000853757
    ## Fkbp4   0.0001070694
    ## Cox4i1  0.0003250603
    ## Nedd4l  0.0003250603
    ## ...              ...
    ## Laptm4a  0.001089638
    ## Lsamp    0.001324504
    ## Kbtbd11  0.001462059
    ## Rbfox2   0.001526607
    ## Rps25    0.001606348

``` r
summary(res)
```

    ## 
    ## out of 16917 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)     : 287, 1.7% 
    ## LFC < 0 (down)   : 243, 1.4% 
    ## outliers [1]     : 0, 0% 
    ## low counts [2]   : 10555, 62% 
    ## (mean count < 49)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- DEGes$padjdaytime3nighttimedaytime
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime3")]) ## matrix to df
rownames(df) <- names(countData)
colnames(df) <- "daytime3"
ann_colors <- ann_colorsdaytime3

# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-2-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = F,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/daynightheat.pdf"
         )
```

Daytime - all four options
--------------------------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ daytime)
dds # view the DESeq object - note numnber of genes
```

    ## class: DESeqDataSet 
    ## dim: 22485 16 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(22485): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(16): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime", "evening", "beforenoon"), mypval = 0.1) # 897 (388 with outliers included)
```

    ## [1] 388

``` r
contrast2 <- resvals(contrastvector = c("daytime", "nighttime", "beforenoon"), mypval = 0.1) # 151 (14 with outliers included)
```

    ## [1] 14

``` r
contrast3 <- resvals(contrastvector = c("daytime", "nighttime", "evening"), mypval = 0.1) # 0 (0 with outliers included)
```

    ## [1] 0

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1, contrast2)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- with(DEGes, pmin(padjdaytimeeveningbeforenoon, padjdaytimenighttimebeforenoon)) 

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime")]) ## matrix to df
rownames(df) <- names(countData)
colnames(df) <- "daytime"
ann_colors <- ann_colorsdaytime

# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=T, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-3-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = F,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/alltimes.pdf"
         )
```

Daytime3 and FMR1 - all data points
-----------------------------------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ Genotype * daytime )
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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime", "evening", "beforenoon"), mypval = 0.1) # 281 (281 with outliers included)
```

    ## [1] 281

``` r
contrast2 <- resvals(contrastvector = c("daytime", "nighttime", "beforenoon"), mypval = 0.1) # 51 (51 with outliers included)
```

    ## [1] 51

``` r
contrast3 <- resvals(contrastvector = c("daytime", "nighttime", "evening"), mypval = 0.1) # 5  (5 with outliers included)
```

    ## [1] 5

``` r
contrast4 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 1923 (1923 with outliers removed)
```

    ## [1] 1923

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1, contrast2, contrast3, contrast4)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- with(DEGes, pmin(padjdaytimeeveningbeforenoon, padjdaytimenighttimebeforenoon, padjdaytimenighttimeevening, padjGenotypeFMR1WT)) 

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime", "Genotype")]) ## matrix to df
rownames(df) <- names(countData)
ann_colors <- ann_colorsall


# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-4-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = F,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/alltimesGenotype.pdf"
         )
```

Daytime3 and FMR1 - no outliers removed
---------------------------------------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ Genotype * daytime3 )
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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime3", "nighttime", "daytime"), mypval = 0.1) #    (11 with outliers included)
```

    ## [1] 11

``` r
contrast2 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) #    (2 with outliers removed)
```

    ## [1] 2

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1, contrast2)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- with(DEGes, pmin(padjdaytime3nighttimedaytime, padjGenotypeFMR1WT)) 

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime3", "Genotype")]) ## matrix to df
rownames(df) <- names(countData)
ann_colors <- ann_colorsdaytime3frm1

# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-5-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = F,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/daytime3Genotype.pdf"
         )
```

Daytime 3 and genotype - 2 samples removed
------------------------------------------

``` r
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ Genotype * daytime )
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
    ## colData names(8): RNAseqID Mouse ... daytime2 daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime", "evening", "beforenoon"), mypval = 0.1) # 281 (281 with outliers included)
```

    ## [1] 281

``` r
contrast2 <- resvals(contrastvector = c("daytime", "nighttime", "beforenoon"), mypval = 0.1) # 51 (51 with outliers included)
```

    ## [1] 51

``` r
contrast3 <- resvals(contrastvector = c("daytime", "nighttime", "evening"), mypval = 0.1) # 5  (5 with outliers included)
```

    ## [1] 5

``` r
contrast4 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 1923 (1923 with outliers removed)
```

    ## [1] 1923

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1, contrast2, contrast3, contrast4)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- with(DEGes, pmin(padjdaytimeeveningbeforenoon, padjdaytimenighttimebeforenoon, padjdaytimenighttimeevening, padjGenotypeFMR1WT)) 

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime", "Genotype")]) ## matrix to df
rownames(df) <- names(countData)
ann_colors <- ann_colorsall


# set color breaks
paletteLength <- 30
myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))

pheatmap(DEGes, show_colnames=F, show_rownames = F,
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

![](../figures/02_RNAseq/unnamed-chunk-6-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = T,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/alltimesGenotype.pdf"
         )
```

Daytime3 and FMR1 - no outliers removed
---------------------------------------

``` r
colData <- read.csv("../data/fmr1ColData.csv", header = T)
countData <- read.csv("../data/fmr1CountData.csv", header = T, check.names = F, row.names = 1)

## remove outliers
colData <- colData %>% 
  filter(RNAseqID != "16-123B")  %>% 
  filter(RNAseqID != "16-125B") %>% 
  droplevels()

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


dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ Genotype * daytime3 )
dds <- dds[ rowSums(counts(dds)) > 1, ]  # Pre-filtering genes with 0 counts
dds # view number of genes afternormalization and the number of samples
```

    ## class: DESeqDataSet 
    ## dim: 16894 14 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(16894): 0610007P14Rik 0610009B22Rik ... Zzef1 Zzz3
    ## rowData names(0):
    ## colnames(14): 16-116B 16-117D ... 16-125D 16-126B
    ## colData names(7): RNAseqID Mouse ... daytime daytime3

``` r
dds <- DESeq(dds) # Differential expression analysis
rld <- rlog(dds, blind=FALSE) ## log transformed data

contrast1 <- resvals(contrastvector = c("daytime3", "nighttime", "daytime"), mypval = 0.1) # 11 with or without outliers included)
```

    ## [1] 243

``` r
contrast2 <- resvals(contrastvector = c("Genotype", "FMR1", "WT"), mypval = 0.1) # 2 with or without outliers removed)
```

    ## [1] 2

``` r
DEGes <- assay(rld)
DEGes <- cbind(DEGes, contrast1, contrast2)
DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
DEGes$padjmin <- with(DEGes, pmin(padjdaytime3nighttimedaytime, padjGenotypeFMR1WT)) 

# create new col with min padj
DEGes <- DEGes %>% filter(padjmin < 0.1)
rownames(DEGes) <- DEGes$rownames
drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
DEGes <- DEGes %>% dplyr::select(-one_of(drop.cols))
DEGes <- as.matrix(DEGes)
DEGes <- DEGes - rowMeans(DEGes)
DEGes <- as.matrix(DEGes) 

## the heatmap annotation file
df <- as.data.frame(colData(dds)[,c("daytime3", "Genotype")]) ## matrix to df
rownames(df) <- names(countData)
ann_colors <- ann_colorsdaytime3frm1

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

![](../figures/02_RNAseq/unnamed-chunk-7-1.png)

``` r
# for adobe
pheatmap(DEGes, show_colnames=F, show_rownames = T,
         annotation_col=df, annotation_colors = ann_colors,
         treeheight_row = 25, treeheight_col = 25,
         fontsize = 10, 
         width=4.5, height=2.25,
         border_color = "grey60" ,
         color = viridis(30),
         cellwidth = 8, 
         clustering_method="average",
         breaks=myBreaks,
         clustering_distance_cols="correlation",
         filename = "../figures/02_RNAseq/daytime3Genotype2.pdf"
         )
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
# write.csv(vsd, file = "../data/02_vsd.csv", row.names = T)
# write.csv(rlddf, file = "../data/02_rlddf.csv", row.names = T)
# write.csv(colData, file = "../data/02_colData.csv", row.names = T)
```
