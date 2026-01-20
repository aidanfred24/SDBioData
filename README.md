# SDBioData

**SDBioData** is an R package designed to facilitate access to the South Dakota State University (SDSU) bioinformatics database (used in [iDEP](https://bioinformatics.sdstate.edu/idep/)) and perform essential data preparation tasks for gene expression analysis.

It allows users to seamlessly retrieve species-specific gene and pathway information and process RNA-Seq data for downstream analysis using standard Bioconductor workflows.

## Installation

You can install the development version of SDBioData from GitHub:

```r
# install.packages("devtools")
devtools::install_github("aidanfred24/SDBioData")
```

## Core Functionality

*   **Database Access**: Connect to and query the centralized SDSU bioinformatics database.
*   **Species Search**: Easily find if your organism of interest is supported using common names, taxonomic names, or IDs.
*   **ID Conversion**: Convert gene identifiers to standard formats (Ensembl/STRINGdb).
*   **Data Processing**: Pre-process RNA-Seq count data (filtering, imputation, transformation) for tools like DESeq2 and edgeR.

## Usage Examples

Here is a quick start guide to using `SDBioData`.

### 1. Searching for a Species

Before analyzing data, check if your species is available in the database using `srch_species`.

```r
library(SDBioData)

# Search for "Zebra" to find Zebra Finch
srch_results <- srch_species(query = "Zebra", name_type = "all")
print(srch_results)
# Returns matches like: Zebrafish, Zebra Finch, etc.
```

### 2. ID Conversion

You can convert gene IDs in your dataset to standard Ensembl IDs using `convert_id`.

```r
# Example using the built-in 'finch_sample' dataset
data(finch_sample)

# Convert IDs for a vector of genes (Species ID 210 = Zebra Finch)
conv_table <- convert_id(genes = rownames(finch_sample), species_id = 210)
head(conv_table)

# OR convert the entire data matrix relative to the species
finch_conv <- convert_id(genes = rownames(finch_sample),
                         data = finch_sample,
                         species_id = 210)
```

### 3. Connect to Database

If you need direct access to the underlying SQLite database for a species:

```r
# Connect to the Zebra Finch database (ID 210)
# This handles downloading the necessary database files automatically
conn <- connect_database(species_id = 210)

# Use standard DBI queries
db_tables <- DBI::dbListTables(conn)
print(db_tables)

DBI::dbDisconnect(conn)
```

### 4. Data Processing

Use `process_data` to clean and transform raw count matrices for analysis. This includes filtering low-count genes, imputing missing values, and transforming counts (e.g., VST, rlog, or logCPM).

```r
# Example processing workflow
processed_list <- process_data(
  data = finch_sample,
  missing_value = "geneMedian", # Impute strategy
  min_counts = 10,              # Filter threshold
  n_min_samples_count = 2,      # Min samples meeting threshold
  counts_transform = 1,         # 1=log2(CPM), 2=VST, 3=rlog
  counts_log_start = 0.75       # Pseudo-count
)

processed_data <- processed_list$data
head(processed_data)
```

## Requirements

*   R >= 3.5
*   **Imports**: `RSQLite`, `DESeq2`, `edgeR`, `dplyr`, `SummarizedExperiment`, `BiocGenerics`, `R.utils`
