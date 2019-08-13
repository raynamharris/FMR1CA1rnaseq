Reproduction of and comparison to the Ceolin et al. 2017 study.
---------------------------------------------------------------

From [Ceolin L, Bouquier N, Vitre-Boubaker J, Rialle S et al. Cell
Type-Specific mRNA Dysregulation in Hippocampal CA1 Pyramidal Neurons of
the Fragile X Syndrome Mouse Model. Front Mol Neurosci 2017;10:340.
PMID:
29104533](https://www.frontiersin.org/articles/10.3389/fnmol.2017.00340/full)

This data was made available here [open source
data](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE94559).

The parts of their paper that I reproduced are visuzalized in Figure 2
and 3 of Ceolin et al 2017. Ceolin’s fluoresence staining of the CA1
provided the inspriation for the color palette. I reproduced the heatmap
and part of the GO anlsysis but not that of FMRP binding. Then, I
compared this “reproduced analysis of the Ceolin data” to my primary
results to identify genes that robustly change expression in CA1
following *Fmr1* gene knock out.

![](../figures/fig3-02.png)

I reproduced the data from the Ceolin et al. 2017 which used
fluorescence labeling to selectively sequence pyramidal neurons in the
CA1 subfield of the hippocampus from WT and FMR1-KO mice (Fig 2A). My
reproduction of their data produced a very similar pattern of gene
expression and list of differentially expressed genes with roughly equal
up and downregulation of expression. Then, I asked how many of their
differentially expressed genes are differentially expressed in my study.
I found found that downregulation of expression of Cacna1g, Efcab6,
Serpina3n, and Sstr3 was consistent in both the Coelin data and in my
data (Fig 2B). Next, I check to see if the genes that I calculated to be
significantly different als identified by Coelin and described as
significantly different. I determined that 39 of top 45 most significant
(p &lt; 0.01) genes in my analysis make up over half of the most
significant (p &lt; 0.05) genes of from the Ceolin study (Fig 2C). Of my
list of “replicated” 39 differentially expressed genes, two genes
(Serpina3a and Efcab6) were also identified in my analysis of
differential expression (Fig 2D). My GO analysis highlighted different
but also overlapping patterns. The Ceolin study highlights the molecular
function enriched pathways in FMR1-KO mice, but my analysis provided
stronger evidence for a deletion of calcium receptor-related functions
(Fig 2E). This suggests a role for dysregulation of calcium signaling in
the hippocampus of Fragile X Syndrome patients and is consistent with my
research findings.

![](../figures/fig3-01.png) Reproducing the Ceolin study for direct
comparison of results. A) Graphical representation of the samples for
the Ceolin et al. 2017 study examining CA1 expression in WT and FMR1-KO
mice. B) Reproduction: This volcano plot shows that my analysis of the
Ceolin et al count data identified 88 genes that are up-regulated in
FMR1-KO mice and the 146 genes that are up-regulated in WT mice a p &lt;
0.05. Comparison: The gene expression and significance values from the
Ceolin data are color-coded by the levels of significance from my
results described in Fig 2.8. Four genes that are upregulated in WT in
my study were also upregulated in my reproduction of the Ceolin data. C)
Analysis showing that 39 of top 45 most significant (p &lt; 0.01) genes
in my reproduction of the analysis, make up over half of the top most
significant (p &lt; 0.05) genes of from the Ceolin study. D)
Hierarchical clustering shows the names and expression patterns of those
same significant genes. D) GO analysis showing a very similar pattern of
depletion of calcium channel activity as was shown in Fig. 2.8). In
contrast, Ceolin detected enrichment of ribosomal processes in response
to FMR1-KO in CA1 pyramidal neurons. Legend) Teal: Enriched in FMR1-KO,
pink: enriched in WT, grey: genes with insignificant expression, black:
genes whos expression was not calculated in my original analysis

Here is the analysis,

    #source("http://www.bioconductor.org/biocLite.R")
    #biocLite("DESeq2")
    library(DESeq2)
    library(magrittr)
    library(tidyverse)
    library(plyr)
    library(reshape2)
    library(VennDiagram)
    library(genefilter)
    library(pheatmap)
    library(edgeR)
    #library(colorRamps) # for a matlab like color scheme
    library(viridis)
    library(genefilter)  ## for PCA fuction
    library(ggrepel) ## for labeling volcano plot
    library(cowplot)
    library(topconfects)

    source("functions_RNAseq.R")

    # set output file for figures 
    knitr::opts_chunk$set(fig.path = '../figures/05_Ceolin/')

    # contains a file with the gene name and transcript id
    geneids <- read.table("../data/geneids.tsv", header=T)

    #read count data 
    count <- read.table("../data/GSE94559_Raw_Counts_RNA-Seq_Ceolin.txt", header = T)

    # strip gene length from data
    count$Gene <- sapply(strsplit(as.character(count$Gene),'\\|'), "[", 1)
    count$gene <- sapply(strsplit(as.character(count$Gene),'\\|'), "[", 1)

    ## join with geneids so we can look at gene level stuff
    countbygene <- full_join(geneids, count)
    countbygene <- countbygene %>% 
      filter(gene != "-")
    countbygene <- countbygene[-c(1,3)] ## keep gene name and counts for samples)


    ## lengthen the dataframe, then wide with gene level sums, then make gene the row name, then round the value to nearest integer
    countbygene <- melt(countbygene, id=c("gene")) 
    countbygene  <- dcast(countbygene, gene ~ variable, value.var= "value", fun.aggregate=sum)
    row.names(countbygene) <- countbygene$gene
    countbygene[1] <- NULL
    countbygene <- round(countbygene)

    # getting ready for DESeq2
    countData <- countbygene 


    # meta data
    sample=c("KO1", "KO2", "KO3", "KO4","KO5", "KO6", 
               "WT1", "WT2","WT3", "WT4", "WT5", "WT6") 
    genotype=c("FMR1", "FMR1", "FMR1", "FMR1", "FMR1", "FMR1", 
               "WT", "WT","WT", "WT", "WT", "WT") 

    colData <- data.frame(sample,genotype)
    rownames(colData) <- colData$sample


    # keep only data with >2 counts
    countData[countData < 2] <- 0

    # replace nas with 0
    countData[is.na(countData)] <- 0

The first thing I notice is that they have waay more reads per sample
and thus gene counts per sample than I do. They have a mean gene counts
per sample around 400 million counts per gene. My data had 5 million
counts per gene.

    counts <- countData
    dim( counts )

    FALSE [1] 37167    12

    colSums( counts ) / 1e06  # in millions of reads

    FALSE      KO1      KO2      KO3      KO4      KO5      KO6      WT1      WT2 
    FALSE 1418.315 1350.637 1138.349 1227.207 1713.639 1262.710 1462.382 1096.985 
    FALSE      WT3      WT4      WT5      WT6 
    FALSE 1328.221 1041.819 1051.388 1079.573

    table( rowSums( counts ) )[ 1:30 ] # Number of genes with low counts

    FALSE 
    FALSE     0     2     3     4     5     6     7     8     9    10    11    12 
    FALSE 17743    63    46    48    28    29    24    21    17    18    13    10 
    FALSE    13    14    15    16    17    18    19    20    21    22    23    24 
    FALSE    14    12     4    11    10    12    10     7     6     5    11     5 
    FALSE    25    26    27    28    29    30 
    FALSE    10     6     7   183     2     8

    rowsum <- as.data.frame(colSums( counts ) / 1e06 )
    names(rowsum)[1] <- "millioncounts"
    rowsum$sample <- row.names(rowsum)

    ggplot(rowsum, aes(x=millioncounts)) + 
      geom_histogram(bins = 20, colour = "black", fill = "darkgrey") +
      theme_classic() +
      scale_x_continuous(name = "Millions of Gene Counts per Sample") +
      scale_y_continuous(name = "Number of Samples")

![](../figures/05_Ceolin/edgeR-1.png)

Then I conducted differential gene experssion with DESeq2 modeling the
effect genotype.

    dds <- DESeqDataSetFromMatrix(countData = countData,
                                  colData = colData,
                                  design = ~genotype )

    ## converting counts to integer mode

    dds <- dds[ rowSums(counts(dds)) > 2, ] ## filter genes with 0 counts
    dds <- DESeq(dds) # Differential expression analysis

    ## estimating size factors

    ## estimating dispersions

    ## gene-wise dispersion estimates

    ## mean-dispersion relationship

    ## final dispersion estimates

    ## fitting model and testing

    dds

    ## class: DESeqDataSet 
    ## dim: 19361 12 
    ## metadata(1): version
    ## assays(4): counts mu H cooks
    ## rownames(19361): 0610005C13Rik 0610007P14Rik ... Zzef1 Zzz3
    ## rowData names(22): baseMean baseVar ... deviance maxCooks
    ## colnames(12): KO1 KO2 ... WT5 WT6
    ## colData names(3): sample genotype sizeFactor

    ## for variance stablized gene expression and log transformed data
    rld <- rlog(dds, blind=FALSE)


    results(dds, contrast =c("genotype", "FMR1", "WT"), independentFiltering = T)

    ## log2 fold change (MLE): genotype FMR1 vs WT 
    ## Wald test p-value: genotype FMR1 vs WT 
    ## DataFrame with 19361 rows and 6 columns
    ##                       baseMean      log2FoldChange              lfcSE
    ##                      <numeric>           <numeric>          <numeric>
    ## 0610005C13Rik 12.3836842245309   -21.4822551988674    3.0056900130607
    ## 0610007P14Rik 48797.8320853426  0.0446162068928321 0.0751658671078423
    ## 0610009B22Rik 25691.5427060416 -0.0778405035952667 0.0989335868660761
    ## 0610009L18Rik  1143.8574766414  -0.230146360527051  0.302476615797001
    ## 0610009O20Rik 33655.7846363426  0.0599534817184228 0.0898967784882136
    ## ...                        ...                 ...                ...
    ## Zyg11a        6.19184211226545   -6.18744023476566   3.00819992966199
    ## Zyg11b        216840.408890062  -0.145100372359937 0.0516948553773774
    ## Zyx           24625.8079783001   0.023352506672673  0.122089301513339
    ## Zzef1         52687.2816154765  0.0131355363928746  0.065231600692671
    ## Zzz3           75770.606886356 -0.0461123319262117 0.0445234785954328
    ##                             stat              pvalue              padj
    ##                        <numeric>           <numeric>         <numeric>
    ## 0610005C13Rik  -7.14719585370417                  NA                NA
    ## 0610007P14Rik  0.593570041955615   0.552799723133589 0.999781696584314
    ## 0610009B22Rik -0.786795526787454   0.431401572499944 0.989611193172537
    ## 0610009L18Rik -0.760873232863421    0.44673278769446  0.99684247778883
    ## 0610009O20Rik  0.666914685116144    0.50482663019688 0.999781696584314
    ## ...                          ...                 ...               ...
    ## Zyg11a         -2.05685804781629                  NA                NA
    ## Zyg11b         -2.80686291316012 0.00500265261376567 0.178075192078467
    ## Zyx            0.191273980465206   0.848310945306944 0.999781696584314
    ## Zzef1          0.201367684579147   0.840411081611035 0.999781696584314
    ## Zzz3           -1.03568574111686   0.300348776641202 0.941411306522267

    summary(results(dds, contrast =c("genotype", "FMR1", "WT"), independentFiltering = T)) #original use

    ## 
    ## out of 19361 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)       : 140, 0.72%
    ## LFC < 0 (down)     : 216, 1.1%
    ## outliers [1]       : 851, 4.4%
    ## low counts [2]     : 0, 0%
    ## (mean count < 0)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

    results(dds, contrast =c("genotype", "WT", "FMR1"))

    ## log2 fold change (MLE): genotype WT vs FMR1 
    ## Wald test p-value: genotype WT vs FMR1 
    ## DataFrame with 19361 rows and 6 columns
    ##                       baseMean      log2FoldChange              lfcSE
    ##                      <numeric>           <numeric>          <numeric>
    ## 0610005C13Rik 12.3836842245309    21.4822551988674    3.0056900130607
    ## 0610007P14Rik 48797.8320853426 -0.0446162068928321 0.0751658671078423
    ## 0610009B22Rik 25691.5427060416  0.0778405035952667 0.0989335868660761
    ## 0610009L18Rik  1143.8574766414   0.230146360527051  0.302476615797001
    ## 0610009O20Rik 33655.7846363426 -0.0599534817184228 0.0898967784882136
    ## ...                        ...                 ...                ...
    ## Zyg11a        6.19184211226545    6.18744023476566   3.00819992966199
    ## Zyg11b        216840.408890062   0.145100372359937 0.0516948553773774
    ## Zyx           24625.8079783001  -0.023352506672673  0.122089301513339
    ## Zzef1         52687.2816154765 -0.0131355363928746  0.065231600692671
    ## Zzz3           75770.606886356  0.0461123319262117 0.0445234785954328
    ##                             stat              pvalue              padj
    ##                        <numeric>           <numeric>         <numeric>
    ## 0610005C13Rik   7.14719585370417                  NA                NA
    ## 0610007P14Rik -0.593570041955615   0.552799723133589 0.999781696584314
    ## 0610009B22Rik  0.786795526787454   0.431401572499944 0.989611193172537
    ## 0610009L18Rik  0.760873232863421    0.44673278769446  0.99684247778883
    ## 0610009O20Rik -0.666914685116144    0.50482663019688 0.999781696584314
    ## ...                          ...                 ...               ...
    ## Zyg11a          2.05685804781629                  NA                NA
    ## Zyg11b          2.80686291316012 0.00500265261376567 0.178075192078467
    ## Zyx           -0.191273980465206   0.848310945306944 0.999781696584314
    ## Zzef1         -0.201367684579147   0.840411081611035 0.999781696584314
    ## Zzz3            1.03568574111686   0.300348776641202 0.941411306522267

    summary(results(dds, contrast =c("genotype", "WT", "FMR1"))) # added for comparison to deseq2_confects results

    ## 
    ## out of 19361 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)       : 216, 1.1%
    ## LFC < 0 (down)     : 140, 0.72%
    ## outliers [1]       : 851, 4.4%
    ## low counts [2]     : 0, 0%
    ## (mean count < 0)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

There are 12 samples (6 WT and 6 FMR1-KO) and 37,167 genes were included
in the analysis, but most were discarded during the normalizaiton and
analysis iwth DESeq such that the expression of only 19,361 genes were
analysed.

**Differentially expressed genes**

-   out of 19361 with nonzero total read count
-   adjusted p-value &lt; 0.1
-   LFC &gt; 0 (up) : 88, 0.45%
-   LFC &lt; 0 (down) : 146, 0.75%

<!-- -->

    colData$genotype <- as.factor(colData$genotype)
    colData %>% select(genotype)  %>%  summary()

    FALSE  genotype
    FALSE  FMR1:6  
    FALSE  WT  :6

    dim(countData)

    FALSE [1] 37167    12

    dim(rld)

    FALSE [1] 19361    12

Then I did my favorite principle component analysis. The clustering of
points shows clear separation of samples by PC1 and PC2 together. PC2 is
signfificant.

    # create the dataframe using my function pcadataframe
    pcadata <- pcadataframe(rld, intgroup=c("genotype"), returnData=TRUE)
    percentVar <- round(100 * attr(pcadata, "percentVar"))

    ## PC1 vs PC2

    PCA12 <- ggplot(pcadata, aes(PC1, PC2, shape = genotype, color = genotype)) + 
      geom_point(size = 3, alpha = 1) +
        xlab(paste0("PC1: ", percentVar[1],"% variance")) +
        ylab(paste0("PC2: ", percentVar[2],"% variance")) +
        scale_color_manual(values =c("#41b6c4", "#e7298a")) +
        theme_cowplot(font_size = 8, line_size = 0.25)  +
        #theme(legend.position="none") +
        scale_shape_manual(values=c(16, 16)) 
    PCA12

![](../figures/05_Ceolin/PCA-1.png)

    summary(aov(PC1 ~ genotype, data=pcadata))

    FALSE             Df Sum Sq Mean Sq F value Pr(>F)
    FALSE genotype     1   21.8   21.80   1.905  0.198
    FALSE Residuals   10  114.4   11.44

    summary(aov(PC2 ~ genotype, data=pcadata))

    FALSE             Df Sum Sq Mean Sq F value  Pr(>F)   
    FALSE genotype     1  41.04   41.04   10.53 0.00879 **
    FALSE Residuals   10  38.97    3.90                   
    FALSE ---
    FALSE Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summary(aov(PC3 ~ genotype, data=pcadata))

    FALSE             Df Sum Sq Mean Sq F value Pr(>F)
    FALSE genotype     1   0.82   0.819     0.2  0.664
    FALSE Residuals   10  41.00   4.100

The heatmap shows a similar pattern as the volcano plot and PCA analysis
and allows us to visualize patterns of expression with gene names.

    contrast1 <- resvals(contrastvector = c('genotype', 'FMR1', 'WT'), mypval = 0.1)

    FALSE [1] 356

    ## Any padj <0.1
    DEGes <- assay(rld)
    DEGes <- cbind(DEGes, contrast1)
    DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
    DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe

    coelinDEGs <-  DEGes %>%
      select("pvalgenotypeFMR1WT", "padjgenotypeFMR1WT","log2FoldChange", "rownames")

    coelinDEGs <- na.omit(coelinDEGs)
    head(coelinDEGs)

    FALSE               pvalgenotypeFMR1WT padjgenotypeFMR1WT log2FoldChange
    FALSE 0610007P14Rik          0.5527997          0.9997817     0.04461621
    FALSE 0610009B22Rik          0.4314016          0.9896112    -0.07784050
    FALSE 0610009L18Rik          0.4467328          0.9968425    -0.23014636
    FALSE 0610009O20Rik          0.5048266          0.9997817     0.05995348
    FALSE 0610010B08Rik          0.7847458          0.9997817     0.23562423
    FALSE 0610010F05Rik          0.6637219          0.9997817    -0.02690120
    FALSE                    rownames
    FALSE 0610007P14Rik 0610007P14Rik
    FALSE 0610009B22Rik 0610009B22Rik
    FALSE 0610009L18Rik 0610009L18Rik
    FALSE 0610009O20Rik 0610009O20Rik
    FALSE 0610010B08Rik 0610010B08Rik
    FALSE 0610010F05Rik 0610010F05Rik

    write.csv(coelinDEGs, "../data/coelinDEGs.csv", row.names = T)

    DEGes <- DEGes %>% filter(padjgenotypeFMR1WT < 0.01)

    rownames(DEGes) <- DEGes$rownames
    drop.cols <-colnames(DEGes[,grep("padj|pval|rownames|log2FoldChange", colnames(DEGes))])
    DEGes <- DEGes %>% select(-one_of(drop.cols))
    DEGes <- as.matrix(DEGes)
    DEGes <- DEGes - rowMeans(DEGes)

    # setting color options
    ann_colors <- list(genotype =  c('FMR1' = (values=c("#41b6c4")), 
                'WT' = (values=c("#e7298a"))))

    df <- as.data.frame(colData(dds)[,c( "genotype")])
    rownames(df) <- names(countData)
    colnames(df) <- "genotype"

    paletteLength <- 40
    myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
                  seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))


    pheatmap(DEGes, show_colnames=T, show_rownames = T,
             annotation_col=df, 
             annotation_colors = ann_colors,
             treeheight_row = 0, treeheight_col = 10,
             border_color = "grey60" ,
             color = viridis(40), breaks=myBreaks,
             clustering_distance_cols="correlation" ,
             clustering_method="average"
             )

![](../figures/05_Ceolin/heatmap-1.png)

    # for adobe
    pheatmap(DEGes, show_colnames=F, show_rownames = T,
             annotation_col=df, annotation_colors = ann_colors,
             treeheight_row = 0, treeheight_col = 10,
             fontsize = 4, width=2, height=3.4, cellwidth = 5, 
             border_color = "grey60" ,
             color = viridis(40), breaks=myBreaks,
             clustering_distance_cols="correlation" ,
             clustering_method="average",
             filename = "../figures/05_Ceolin/HeatmapPadj-1.pdf"
             )

Create list of p-values for all genes
-------------------------------------

    #create a new DF with the gene counts
    GOpvals <- assay(rld)
    GOpvals <- cbind(GOpvals, contrast1)
    GOpvals <- as.data.frame(GOpvals)
    GOpvals <- GOpvals[ , grepl( "padj|pval" , names( GOpvals ) ) ]

    GOpvals$gene<-rownames(GOpvals)

    GOpvals <- GOpvals %>%
      select(gene, padjgenotypeFMR1WT)
    GOpvals$logP <- log(GOpvals$padjgenotypeFMR1WT)
    GOpvals <- GOpvals %>%
      select(gene, logP)

    write.csv(GOpvals, "./06_GO_MWU/05_Ceolin_GOpvals.csv", row.names = F)

Volcanos
--------

    res <- results(dds, contrast =c("genotype", "FMR1", "WT"), independentFiltering = T, alpha = 0.1)
    summary(res)

    FALSE 
    FALSE out of 19361 with nonzero total read count
    FALSE adjusted p-value < 0.1
    FALSE LFC > 0 (up)       : 140, 0.72%
    FALSE LFC < 0 (down)     : 216, 1.1%
    FALSE outliers [1]       : 851, 4.4%
    FALSE low counts [2]     : 0, 0%
    FALSE (mean count < 0)
    FALSE [1] see 'cooksCutoff' argument of ?results
    FALSE [2] see 'independentFiltering' argument of ?results

    resOrdered <- res[order(res$padj),]
    head(resOrdered, 10)

    FALSE log2 fold change (MLE): genotype FMR1 vs WT 
    FALSE Wald test p-value: genotype FMR1 vs WT 
    FALSE DataFrame with 10 rows and 6 columns
    FALSE                       baseMean    log2FoldChange            lfcSE
    FALSE                      <numeric>         <numeric>        <numeric>
    FALSE Fmr1          32772.3736216097 -13.8780823627799 1.50954803281915
    FALSE Gm15056        37.088831880183  23.5463717195844 3.00390182339074
    FALSE Hgfac         35.2667058916136  23.4831347528134 3.00393931914501
    FALSE Mir341        39.3195918348791  23.6238991840141 3.00386056217713
    FALSE Vmn1r206      37.8521985741489  23.5791719004966 3.00388722049682
    FALSE 1600014K23Rik 22.4583516643602  22.8686479287732 3.00437439887387
    FALSE 1700029M20Rik 22.2916222642752  22.8596217205454 3.00438357971532
    FALSE Ace3          24.6025053141726  22.9927191636575 3.00426999636628
    FALSE Gm14743       28.9885563235712  22.9329312300403 3.00410445828891
    FALSE Klk13         24.5529634037135  22.9900141250764 3.00427237590613
    FALSE                            stat               pvalue                 padj
    FALSE                       <numeric>            <numeric>            <numeric>
    FALSE Fmr1          -9.19353479389585 3.80139736956176e-20 7.03638653105882e-16
    FALSE Gm15056        7.83859563459557 4.55613158747969e-15 1.99558718853911e-11
    FALSE Hgfac          7.81744644545524 5.39056506898733e-15 1.99558718853911e-11
    FALSE Mir341         7.86451258140028 3.70537347388698e-15 1.99558718853911e-11
    FALSE Vmn1r206       7.84955298574652 4.17522753710331e-15 1.99558718853911e-11
    FALSE 1600014K23Rik  7.61178365031503 2.70338823276925e-14 3.65898480952533e-11
    FALSE 1700029M20Rik  7.60875604396409 2.76746555015422e-14 3.65898480952533e-11
    FALSE Ace3           7.65334646735068 1.95815396446219e-14 3.65898480952533e-11
    FALSE Gm14743        7.63386611499606 2.27816503040613e-14 3.65898480952533e-11
    FALSE Klk13          7.65244000825403 1.97201116640638e-14 3.65898480952533e-11

    data <- data.frame(gene = row.names(res), pvalue = -log10(res$pvalue), lfc = res$log2FoldChange)
    data <- na.omit(data)

    data$wrap <- "Reproduction"
    data <- data %>%
      mutate(color = ifelse(data$lfc > 1.5 & data$pvalue > 2, 
                            yes = "FMR1", 
                            no = ifelse(data$lfc < -1.5 & data$pvalue > 2, 
                                        yes = "WT", 
                                        no = "none")))

    # Color corresponds to fold change directionality

    volcano <- ggplot(data, aes(x = lfc, y = pvalue)) + 
      geom_point(aes(color = factor(color), shape = factor(color), size = factor(color)), alpha = 0.5, na.rm = T) + # add gene points
      geom_hline(yintercept = 2,  size = 0.25, linetype = 2) + 
      scale_color_manual(values = c("FMR1" = "#41b6c4",
                                    "WT" = "#e7298a", 
                                    "none" = "grey"),
                         name = NULL) + 
      #scale_y_continuous(limits=c(0, 8)) +
        scale_size_manual(values=c(2, 0.5, 2)) +
        scale_shape_manual(values=c(16, 16, 16)) +
      scale_x_continuous(name="Log fold change")+
      ylab(paste0("-log10(p-value)")) +       
        theme_cowplot(font_size = 7, line_size = 0.25) +  theme(legend.position = "none") + 
      facet_wrap(~wrap)
    volcano

![](../figures/05_Ceolin/volcanos-1.png)

    #pdf(file="../figures/05_Ceolin/volcano.pdf", width=1.25, height=1.7)
    #plot(volcano)
    #dev.off()

Suzy-like volcano plot

    res <- results(dds, contrast =c("genotype", "FMR1", "WT"), independentFiltering = T, alpha = 0.1)
    summary(res)

    ## 
    ## out of 19361 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)       : 140, 0.72%
    ## LFC < 0 (down)     : 216, 1.1%
    ## outliers [1]       : 851, 4.4%
    ## low counts [2]     : 0, 0%
    ## (mean count < 0)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

    resOrdered <- res[order(res$padj),]
    head(resOrdered, 10)

    ## log2 fold change (MLE): genotype FMR1 vs WT 
    ## Wald test p-value: genotype FMR1 vs WT 
    ## DataFrame with 10 rows and 6 columns
    ##                       baseMean    log2FoldChange            lfcSE
    ##                      <numeric>         <numeric>        <numeric>
    ## Fmr1          32772.3736216097 -13.8780823627799 1.50954803281915
    ## Gm15056        37.088831880183  23.5463717195844 3.00390182339074
    ## Hgfac         35.2667058916136  23.4831347528134 3.00393931914501
    ## Mir341        39.3195918348791  23.6238991840141 3.00386056217713
    ## Vmn1r206      37.8521985741489  23.5791719004966 3.00388722049682
    ## 1600014K23Rik 22.4583516643602  22.8686479287732 3.00437439887387
    ## 1700029M20Rik 22.2916222642752  22.8596217205454 3.00438357971532
    ## Ace3          24.6025053141726  22.9927191636575 3.00426999636628
    ## Gm14743       28.9885563235712  22.9329312300403 3.00410445828891
    ## Klk13         24.5529634037135  22.9900141250764 3.00427237590613
    ##                            stat               pvalue                 padj
    ##                       <numeric>            <numeric>            <numeric>
    ## Fmr1          -9.19353479389585 3.80139736956176e-20 7.03638653105882e-16
    ## Gm15056        7.83859563459557 4.55613158747969e-15 1.99558718853911e-11
    ## Hgfac          7.81744644545524 5.39056506898733e-15 1.99558718853911e-11
    ## Mir341         7.86451258140028 3.70537347388698e-15 1.99558718853911e-11
    ## Vmn1r206       7.84955298574652 4.17522753710331e-15 1.99558718853911e-11
    ## 1600014K23Rik  7.61178365031503 2.70338823276925e-14 3.65898480952533e-11
    ## 1700029M20Rik  7.60875604396409 2.76746555015422e-14 3.65898480952533e-11
    ## Ace3           7.65334646735068 1.95815396446219e-14 3.65898480952533e-11
    ## Gm14743        7.63386611499606 2.27816503040613e-14 3.65898480952533e-11
    ## Klk13          7.65244000825403 1.97201116640638e-14 3.65898480952533e-11

    data <- data.frame(gene = row.names(res), pvalue = -log10(res$padj), lfc = res$log2FoldChange)
    data <- na.omit(data)

    pointcolor <- read.csv("../results/FMR1_CA1_rnaseq.csv")
    tempdata <- left_join(data, pointcolor, by = "gene")

    ## Warning: Column `gene` joining factors with different levels, coercing to
    ## character vector

    tempdata$color <- as.character(tempdata$color)
    tempdata$color[is.na(tempdata$color)] <- "absent"
    tempdata$color <- as.factor(tempdata$color)
    levels(tempdata$color) <- list(FMR1KO="FMR1", WT="WT", NS = "none",absent="absent")

    tempdata$wrap <- "Comparison"

    levels(tempdata$color) 

    ## [1] "FMR1KO" "WT"     "NS"     "absent"

    summary(tempdata$color)

    ## FMR1KO     WT     NS absent 
    ##     13     16  10342   8139

    # Color corresponds to fold change directionality

    suzyvolcano <- ggplot(tempdata, aes(x = lfc.x, y = pvalue.x)) + 
      geom_point(aes(color = color, shape = color, size = color), alpha = 0.5, na.rm = T) + # add gene points
      theme_cowplot(font_size = 7, line_size = 0.25) +
        #scale_y_continuous(limits=c(0, 8)) +
      scale_x_continuous(name="Log fold change")+
      scale_shape_manual(values=c(16, 16, 16, 16)) +
      scale_size_manual(values=c(2,2, 0.5, 0.5)) +

      geom_hline(yintercept = 2,  size = 0.25, linetype = 2) + 
      scale_color_manual(values = c("NS" = "grey",
                                    "absent" = "black",
                                    "WT" = "#e7298a",
                                    "FMR1KO" = "#41b6c4")) + 
     scale_y_continuous(name=NULL,
                        labels = NULL)+      
      theme(panel.grid.minor=element_blank(),
            legend.position = "none", # remove legend 
            panel.grid.major=element_blank()) +
      facet_wrap(~wrap)
    suzyvolcano

![](../figures/05_Ceolin/suzyvolcano-1.png)

    pdf(file="../figures/05_Ceolin/suzyvolcano.pdf", width=1, height=1.7)
    plot(suzyvolcano)
    dev.off()

    ## quartz_off_screen 
    ##                 2

    legendvolcano <- ggplot(tempdata, aes(x = lfc.x, y = pvalue.x)) + 
      geom_point(aes(color = color, shape = color, size = color), alpha = 0.5, na.rm = T) + # add gene points
      theme_cowplot(font_size = 7, line_size = 0.25) +
      geom_hline(yintercept = 2,  size = 0.25, linetype = 2) + 
      scale_color_manual(values = c("NS" = "grey",
                                    "absent" = "black",
                                    "FMR1KO" = "#41b6c4",
                                    "WT" = "#e7298a")) + 
      #scale_y_continuous(limits=c(0, 8)) +
      scale_x_continuous(name="Log fold change")+
      scale_shape_manual(values=c(16, 16, 16, 16)) +
      scale_size_manual(values=c(2,2, 2, 2)) +
     scale_y_continuous(name=NULL,
                        labels = NULL)+      
      theme(legend.position = "top")
    legendvolcano

![](../figures/05_Ceolin/suzyvolcano-2.png)

    pdf(file="../figures/05_Ceolin/legendvolcano.pdf", width=3, height=1.5)
    plot(legendvolcano)
    dev.off()

    ## quartz_off_screen 
    ##                 2

    # list top deges
    mytopgenes <- tempdata %>%
      filter(color %in% c("WT", "FMR1"), pvalue.x > 2)
    head(mytopgenes)

    ##        gene  pvalue.x       lfc.x pvalue.y      lfc.y color       wrap
    ## 1   Cacna1g  2.190358  -0.3435248 1.004657 -0.7686247    WT Comparison
    ## 2    Efcab6  6.283394  -0.8090983 1.057545 -1.4464714    WT Comparison
    ## 3      Fmr1 15.152650 -13.8780824 7.831595 -1.6165142    WT Comparison
    ## 4 Serpina3n  9.746152  -0.9013798 1.004657 -0.5682942    WT Comparison

    mytopgenes$gene

    ## [1] "Cacna1g"   "Efcab6"    "Fmr1"      "Serpina3n"

Venn Diagram of both study’s DEGS
---------------------------------

    contrast1 <- resvals(contrastvector = c("genotype", "FMR1", "WT"), mypval = 0.01)

    ## [1] 145

    #create a new DF with the gene counts
    rldpvals <- assay(rld)
    rldpvals <- cbind(rldpvals, contrast1)
    rldpvals <- as.data.frame(rldpvals)
    rldpvals <- rldpvals[ , grepl( "padj|pval" , names( rldpvals ) ) ]

    # venn with padj values
    venn1 <- row.names(rldpvals[rldpvals[2] <0.01 & !is.na(rldpvals[2]),])
    venn2 <- read.csv("../data/GSE94559_Ceolin_DEGS.csv", header = F)
    venn2 <- as.matrix(venn2)
    venn3 <- read.csv("../results/FMR1_CA1_venn1.csv", skip = 1)
    venn3 <- as.matrix(venn3)

    candidates <- list("Published" = venn2, "Reproduced" = venn1, "Compared" = venn3)

    prettyvenn <- venn.diagram(scaled=T,
      x = candidates, filename=NULL, 
      col = "black",
      fill = c( "white", "white", "white"),
      alpha = 0.5,
      cex = 1, fontfamily = "sans", #fontface = "bold",
      cat.default.pos = "text",
      #cat.dist = c(0.1, 0.1, 0.1), cat.pos = 1,
      cat.cex = 1, 
      cat.fontfamily = "sans")
    #dev.off()
    grid.draw(prettyvenn)

![](../figures/05_Ceolin/venn-1.png)

    venn12 <- intersect(venn1,venn2)
    write(venn12, "./06_GO_MWU/CeolinHarrisOverlap.csv")

    2+1+25

    ## [1] 28

    2+1+37+34

    ## [1] 74

    6+37+2

    ## [1] 45

Over reproducible differentially expressed genes
================================================

I took their list of differentially expressed genes at p &lt; 0.05 and
my list at 0.01 and identified the overlap. Then I made a heatmap.

    DEGes <- assay(rld)
    DEGes <- cbind(DEGes, contrast1)
    DEGes <- as.data.frame(DEGes) # convert matrix to dataframe
    DEGes$rownames <- rownames(DEGes)  # add the rownames to the dataframe
    head(DEGes)

    ##                     KO1       KO2       KO3       KO4       KO5       KO6
    ## 0610005C13Rik  3.611345  3.611346  3.611347  3.611346  3.611344  3.611346
    ## 0610007P14Rik 15.600213 15.608739 15.561463 15.529979 15.600742 15.613098
    ## 0610009B22Rik 14.753542 14.669214 14.593776 14.502330 14.593874 14.620354
    ## 0610009L18Rik 10.138022 10.170858 10.134683 10.130807 10.183998 10.067714
    ## 0610009O20Rik 15.040722 14.965714 15.171640 15.123312 14.924288 15.068769
    ## 0610010B08Rik  3.895081  3.892674  3.894827  3.893908  3.894546  3.892744
    ##                     WT1       WT2       WT3       WT4       WT5       WT6
    ## 0610005C13Rik  3.631150  3.611347  3.611346  3.611348  3.611348  3.611348
    ## 0610007P14Rik 15.669142 15.449074 15.564393 15.494937 15.499091 15.661841
    ## 0610009B22Rik 14.755996 14.574368 14.694065 14.597266 14.584117 14.771452
    ## 0610009L18Rik 10.207004 10.110643 10.214312 10.113629 10.140719 10.165465
    ## 0610009O20Rik 15.012408 15.062111 14.909219 15.112654 14.987760 15.020742
    ## 0610010B08Rik  3.895251  3.893922  3.893433  3.893360  3.893246  3.892588
    ##               pvalgenotypeFMR1WT padjgenotypeFMR1WT log2FoldChange
    ## 0610005C13Rik                 NA                 NA   -21.48225520
    ## 0610007P14Rik          0.5527997          0.9997817     0.04461621
    ## 0610009B22Rik          0.4314016          0.9896112    -0.07784050
    ## 0610009L18Rik          0.4467328          0.9968425    -0.23014636
    ## 0610009O20Rik          0.5048266          0.9997817     0.05995348
    ## 0610010B08Rik          0.7847458          0.9997817     0.23562423
    ##                    rownames
    ## 0610005C13Rik 0610005C13Rik
    ## 0610007P14Rik 0610007P14Rik
    ## 0610009B22Rik 0610009B22Rik
    ## 0610009L18Rik 0610009L18Rik
    ## 0610009O20Rik 0610009O20Rik
    ## 0610010B08Rik 0610010B08Rik

    Coelin <- read.csv("../data/GSE94559_Ceolin_DEGS.csv", header = F)
    colnames(Coelin)<- c("rownames")
    DEGes <- inner_join(DEGes, Coelin)

    ## Joining, by = "rownames"

    ## Warning: Column `rownames` joining character vector and factor, coercing
    ## into character vector

    DEGes <- DEGes %>% filter(rownames != "Col1a1")

    DEGes <- DEGes %>% filter(padjgenotypeFMR1WT < 0.01)

    rownames(DEGes) <- DEGes$rownames
    drop.cols <-colnames(DEGes[,grep("padj|pval|rownames", colnames(DEGes))])
    DEGes <- DEGes %>% select(-one_of(drop.cols))
    DEGes <- as.matrix(DEGes)
    DEGes <- DEGes - rowMeans(DEGes)
    head(DEGes)

    ##                KO1       KO2       KO3       KO4       KO5      KO6
    ## Arhgap35 0.9670412 0.9855426 1.0095585 1.0495598 0.9938179 1.020148
    ## Arhgef6  0.9433074 1.0867743 1.1213901 1.0395654 0.9086475 1.058869
    ## Bcam     1.4596690 1.0691261 1.1252908 1.2939927 1.1230020 1.122630
    ## Bst2     1.3545676 1.0299623 0.9909878 1.1929428 1.1810450 1.168412
    ## Cacna1g  1.0133649 1.0974250 1.1293466 0.9702868 1.2447774 1.098128
    ## Ccnd1    1.2003823 1.3446105 1.3060284 1.1214162 1.2151638 1.249613
    ##                WT1       WT2       WT3       WT4       WT5       WT6
    ## Arhgap35 1.0752523 1.1223722 1.1405482 1.1016655 1.1273890 1.0676906
    ## Arhgef6  1.2249791 1.2951014 1.3005655 1.3185998 1.2446879 1.1827272
    ## Bcam     0.8990085 0.8720735 0.8905232 1.0520668 0.9638842 0.8553603
    ## Bst2     0.9415398 0.8451348 0.8579976 0.8334812 0.8929926 0.9671201
    ## Cacna1g  1.2898566 1.2823522 1.2245829 1.3471607 1.3325728 1.2548744
    ## Ccnd1    1.6516073 1.4885153 1.4273316 1.4816069 1.4934833 1.4842805
    ##          log2FoldChange
    ## Arhgap35      -12.66059
    ## Arhgef6       -13.72521
    ## Bcam          -12.72663
    ## Bst2          -12.25618
    ## Cacna1g       -14.28473
    ## Ccnd1         -16.46404

    # setting color options
    ann_colors <- list(genotype =  c('FMR1' = (values=c("#41b6c4")), 
                'WT' = (values=c("#e7298a"))))

    df <- as.data.frame(colData(dds)[,c( "genotype")])
    rownames(df) <- names(countData)
    colnames(df) <- "genotype"

    paletteLength <- 40
    myBreaks <- c(seq(min(DEGes), 0, length.out=ceiling(paletteLength/2) + 1), 
                  seq(max(DEGes)/paletteLength, max(DEGes), length.out=floor(paletteLength/2)))


    pheatmap(DEGes, show_colnames=T, show_rownames = T,
             annotation_col=df, 
             annotation_colors = ann_colors,
             treeheight_row = 0, treeheight_col = 10,
             border_color = "grey60" ,
             color = viridis(40), breaks=myBreaks,
             clustering_distance_cols="correlation" ,
             clustering_method="average"
             )

![](../figures/05_Ceolin/heatmapoverlap-1.png)

    pheatmap(DEGes, show_colnames=F, show_rownames = T,
             annotation_col=df, annotation_colors = ann_colors,
             treeheight_row = 0, treeheight_col = 0,
             fontsize = 5, width=2, height=3.4, cellwidth = 5, 
             border_color = "grey60" ,
             color = viridis(40), breaks=myBreaks,
             clustering_distance_cols="correlation" ,
             clustering_method="average",
             filename = "../figures/05_Ceolin/HeatmapOverlap.pdf"
             )
