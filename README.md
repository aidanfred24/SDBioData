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

# Search for "Human" to find Human
srch_results <- srch_species(query = "Human", name_type = "all")
print(srch_results)
# Returns matches like: Human, Pediculus humanus, etc.
```

### 2. ID Conversion

You can convert gene IDs in your dataset to standard Ensembl IDs using `convert_id`.

```r
# Example using the built-in 'hypoxia_reads' dataset
data(hypoxia_reads)

# Convert IDs for a vector of genes (Species ID 96 = Human)
conv_table <- convert_id(genes = rownames(hypoxia_reads), species_id = 96)
head(conv_table)

# OR convert the entire data matrix relative to the species
hypox_conv <- convert_id(genes = rownames(hypoxia_reads),
                         data = hypoxia_reads,
                         species_id = 96)
```

### 3. Connect to Database

If you need direct access to the underlying SQLite database for a species:

```r
# Connect to the Human database (ID 96)
# This handles downloading the necessary database files automatically
conn <- connect_database(species_id = 96)

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
  data = hypoxia_reads,
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

*   R >= 4.1.0
*   **Imports**: `RSQLite`, `DESeq2`, `edgeR`, `dplyr`, `SummarizedExperiment`, `BiocGenerics`, `R.utils`
