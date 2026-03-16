#' Hypoxia Data Gene Counts
#'
#' Gene counts gathered in the following experiment: RNAseq transcriptomic profile of
#' glioblastoma stem-like cells derived from U87MG cell line treated with a
#' selective A3 adenosine receptor antagonist (MRS1220) under hypoxia.
#'
#' @format ## `hypoxia_reads`
#' A data frame with 35,238 rows and 4 columns:
#' \describe{
#'   \item{MRS1220_hypoxia_rep1}{Treatment replication 1 counts}
#'   \item{MRS1220_hypoxia_rep2}{Treatment replication 2 counts}
#'   \item{vehicle_hypoxia_rep1}{Control replication 1 counts}
#'   \item{vehicle_hypoxia_rep2}{Control replication 2 counts}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE100146}
#' @md
"hypoxia_reads"

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
"hypoxia_deseq"

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
"hypoxia_T2G"
