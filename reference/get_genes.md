# Get Gene Information

Retrieves gene information (e.g., Ensembl IDs, positions) for a specific
species, optionally filtered by a list of user-provided gene
identifiers.

## Usage

``` r
get_genes(species_id, genes = NULL)
```

## Arguments

- species_id:

  Numeric. The ID of the desired species.

- genes:

  A vector or list of gene identifiers to filter by. If `NULL`, returns
  the full gene table.

## Value

A data frame containing gene information (from the `geneInfo` table). If
`genes` are provided, the result is filtered to match the converted
Ensembl IDs.
