resvals <- function(contrastvector, mypval){
  res <- results(dds, contrast = c(contrastvector[1],contrastvector[2],contrastvector[3]), independentFiltering = T)
  sumpvalue <- sum(res$pvalue < mypval, na.rm = TRUE)
  #print(sumpvalue)
  sumpadj <- sum(res$padj < mypval, na.rm = TRUE)
  print(sumpadj)
  vals <- cbind(res$pvalue, res$padj, res$log2FoldChange)
  pvalcolname <- as.character(paste("pval",contrastvector[1],contrastvector[2],contrastvector[3], sep=""))
  padjcolname <- as.character(paste("padj",contrastvector[1],contrastvector[2],contrastvector[3], sep=""))
  colnames(vals) <- c(pvalcolname, padjcolname, "log2FoldChange")
  return(vals)
}


pcadataframe <- function (object, intgroup = "condition", ntop = 500, returnData = FALSE) 
{
  rv <- rowVars(assay(object))
  select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, 
                                                     length(rv)))]
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
  d <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], PC3 = pca$x[, 3], 
                  PC4 = pca$x[, 4], PC5 = pca$x[, 5], PC6 = pca$x[, 6], 
                  PC7 = pca$x[, 7], PC8 = pca$x[, 8], PC9 = pca$x[, 9],
                  group = group, 
                  intgroup.df, name = colnames(object))
  if (returnData) {
    attr(d, "percentVar") <- percentVar[1:9]
    return(d)
  }
}
