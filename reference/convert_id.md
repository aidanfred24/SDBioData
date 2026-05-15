# Convert Gene IDs to Ensembl

Queries the database to map user-provided gene identifiers to Ensembl
IDs.

## Usage

``` r
convert_id(genes, data = NULL, species_id, max_sample_ids = 100)
```

## Arguments

- genes:

  A vector or character string of gene identifiers to convert.

- data:

  Optional data frame or matrix. If provided, the function attempts to
  match `genes` to the row names or a column in `data` and merges the
  conversion results with the original data.

- species_id:

  Numeric. The ID of the species for the database connection.

- max_sample_ids:

  Integer. The maximum number of gene IDs to use when guessing the ID
  type (optimization parameter). Defaults to 100.

## Value

A data frame.

- If `data` is `NULL`: Returns a mapping table with original IDs and
  Ensembl IDs.

- If `data` is provided: Returns `data` merged with the Ensembl IDs.
  Returns `NULL` if `species_id` is missing or no matches are found.
