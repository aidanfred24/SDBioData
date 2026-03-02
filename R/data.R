#' Sample of Zebra Finch Gene Counts
#'
#' A subset of Zebra Finch (Taeniopygia guttata) gene count readings of 21,407
#' genes. The subset contains 100 randomly sampled genes from 19 individuals
#' from different experimental groups.
#'
#' @format ## `finch_sample`
#' A data frame with 100 rows and 19 columns
#' \describe{
#'   \item{Row Names}{Gene Name/Label}
#'   \item{Columns}{Gene count readings for sepearate groups}
#'   \item{Experimental Groups}{
#'   * Male vs. Female
#'   * Heat vs. Control}
#' }
#' @md
"finch_sample"

#' Hypoxia Data Differential Expression Analysis Results
#'
#' Results of performing differential expression analysis (DESeq2) on gene
#' counts gathered in the following experiment: RNAseq transcriptomic profile of
#' glioblastoma stem-like cells derived from U87MG cell line treated with a
#' selective A3 adenosine receptor antagonist (MRS1220) under hypoxia.
#'
#' @format ## `hypox_deseq`
#' A data frame with 13,818 rows and 6 columns:
#' \describe{
#'   \item{baseMean}{Mean of normalized counts for all samples}
#'   \item{log2FoldChange}{Log2 fold change between treated and control}
#'   \item{lfcSE}{Standard error estimate for the log2 fold change estimate}
#'   \item{stat}{Wald statistic}
#'   \item{pvalue}{Wald test p-value}
#'   \item{padj}{Benjamini-Hochberg adjusted p-value}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE100146}
#' @md
"hypox_deseq"

#' Example KEGG Pathway Mapping for Hypoxia
#'
#' An example TERM2GENE mapping tailored for the `hypox_deseq` dataset,
#' demonstrating the `T2G_prep` function's output.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{description}{Pathway ID or description}
#'   \item{gene}{Ensembl Gene ID}
#' }
"hypox_T2G"
