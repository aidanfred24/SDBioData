# Hypoxia Data Differential Expression Analysis Results

Results of performing differential expression analysis (DESeq2) on gene
counts gathered in the following experiment: RNAseq transcriptomic
profile of glioblastoma stem-like cells derived from U87MG cell line
treated with a selective A3 adenosine receptor antagonist (MRS1220)
under hypoxia.

## Usage

``` r
hypoxia_deseq
```

## Format

### `hypox_deseq`

A data frame with 13,818 rows and 6 columns:

- baseMean:

  Mean of normalized counts for all samples

- log2FoldChange:

  Log2 fold change between treated and control

- lfcSE:

  Standard error estimate for the log2 fold change estimate

- stat:

  Wald statistic

- pvalue:

  Wald test p-value

- padj:

  Benjamini-Hochberg adjusted p-value

## Source

<https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE100146>
