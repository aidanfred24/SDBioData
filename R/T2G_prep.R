#' TERM2GENE Data Prep for iDEP Database
#'
#' Prepares background genes for enrichment analysis functions in the
#' format of TERM2GENE data, using pathway information from various databases.
#' Requires ID for a species, and can filter for specific vector of genes.
#'
#' @param species_id Numeric. The ID of a desired species from database, found
#' using `srch_species()`
#' @param category Character. A vector or character constant of pathway
#' categories/databases (e.g. KEGG, GOBP, GOCC, etc.). It is not recommended to
#' use all categories, as some species have many, leading to performance issues
#' @param genes Character. A character vector of genes to add to query
#'
#' @returns A data frame containing TERM2GENE Data (pathways to genes)
#'
#' @md
#' @export
T2G_prep <- function(species_id = NULL,
                     category = "GOBP",
                     genes = NULL) {

    # Check for species
    if (is.null(species_id)) {
        stop("species_id is required")
    }

    # Connect to database
    conn <- connect_database(species_id = species_id)

    # Convert genes if provided
    if (!is.null(genes)) {

        # Convert to ensembl IDs
        gene_map <- convert_id(genes = genes, species_id = species_id)

        if (is.null(gene_map) || nrow(gene_map) == 0) {
           stop("Could not map any genes to Ensembl IDs.")
        }

        # Build query string for genes
        ens_ids <- unique(gene_map$ens)
        genes_sql <- paste0("'", ens_ids, "'", collapse = ",")
    }

    # Construct Path Map Query

    # Build query string for categories
    category_sql <- paste0("'", category, "'", collapse = ",")
    query <- paste0("select * from pathway where category IN (",
                    category_sql,
                    ")")

    # Append gene query
    if (!is.null(genes)) {
        query <- paste0(query,
                        " AND gene IN (",
                        genes_sql,
                        ")")
    }
    query <- paste0(query, ";")

    # Run query
    path_map <- DBI::dbGetQuery(conn, statement = query)

    if (nrow(path_map) == 0) {
        DBI::dbDisconnect(conn)
        warning("No pathways found for the given criteria")
        return(NULL)
    }

    # Retrieve Pathway Info
    # Query unique pathway information
    pathway_ids <- unique(path_map$pathwayID)
    pathways_sql <- paste0("select * from pathwayInfo where id IN (",
                           paste0("'", pathway_ids, "'", collapse = ","),
                           ");")

    pathways <- DBI::dbGetQuery(conn, statement = pathways_sql)

    DBI::dbDisconnect(conn)

    # Ensure same data type between tables
    path_map$pathwayID <- as.character(path_map$pathwayID)
    pathways$id <- as.character(pathways$id)

    DB <- dplyr::left_join(x = path_map,
                           y = pathways,
                           by = c("pathwayID" = "id"))

    if ("description" %in% names(DB) && "gene" %in% names(DB)) {
        return(DB[, c("description", "gene")])
    } else {
        warning("Pathway or gene columns not found in final data")
        return(NULL)
    }
}

