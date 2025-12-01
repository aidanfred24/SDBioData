#'  Convert gene IDs to ensembl data.
#'
#' @param max_sample_ids The number of gene ids used to determine species
#' @param species_id
#' @param genes
#' @param data
#'
#' @export
#' @return A large list of the conversion ID information that was gathered
#'  from querying the database with the original IDs.
convert_id <- function(genes,
                       data = NULL,
                       species_id,
                       max_sample_ids = 100) {

    if (is.null(species_id)) {
        return(NULL)
    }

    genes <- gsub(pattern = "\"|\'", "", x = genes)
    # remove " in gene ids, mess up SQL query
    # remove ' in gene ids
    # |\\.[0-9] remove anything after A35244.1 -> A35244
    #  some gene ids are like Glyma.01G002100

    query_set <- unlist(
        strsplit(
            x = toupper(genes),
            split = "\t| |\n|\\,"
        )
    )

    # remove duplicate; upper case; remove special characters
    query_set <- unique(toupper(gsub("\n| ", "", query_set)))
    # genes should have at least two characters
    query_set <- query_set[which(nchar(query_set) > 1)]

    query_string <- paste0("('", paste(query_set, collapse = "', '"), "')")

    # use a small set of genes to guess species and idType; to improve speed
    # use a small set of gene ids, with the max #
    # when the query is small, use the quary

    # acutal number of samples, for calculating % later
    n_sample_ids <- length(query_set)
    test_query_string <- query_string
    if (length(query_set) > max_sample_ids) {
        n_sample_ids <- max_sample_ids
        test_query_set <- sample(query_set, max_sample_ids)
        test_query_string <- paste0(
            "('",
            paste(test_query_set, collapse = "', '"),
            "')"
        )
    }

    conn_db <- connect_database(species_id = species_id)

    # if database connection error
    # see ? try
    if(inherits(conn_db, "try-error")) {
        return(NULL)
    }


    # if species is selected ---------------------------------------------------
    query_statement <- paste0(
        "select id,ens,idType from mapping where id IN ",
        query_string
    )

    result <- DBI::dbGetQuery(conn_db, query_statement)

    DBI::dbDisconnect(conn_db)
    if (nrow(result) == 0) {
        return(NULL)
    }

    # resolve multiple ID types, get the most matched
    best_id_type <- as.integer(
        names(
            sort(
                table(result$idType),
                decreasing = TRUE
            )
        )[1]
    )
    result <- result[result$idType == best_id_type, ]

    # Needs review---------------------------------------
    # one to many, keep one ensembl id, randomly
    # remove duplicates in query gene ids
    result <- result[which(!duplicated(result[, 1])), ]

    if (is.null(data) || is.null(dim(data))) {

        genes <- as.data.frame(genes)
        colnames(genes)[1] <- "id"
        result <- dplyr::left_join(x = result[,1:2],
                                   y = genes,
                                   by = "id")

    } else {

        match_idx <- which(colSums(data == genes) == nrow(data))
        gene_col <- colnames(data)[match_idx]

        if (is.null(gene_col)) {
            if (sum(rownames(data) == genes) == nrow(data)) {
                gene_col <- "row.names"
            } else {
                stop("Genes not found in provided dataset")
            }
        }

        # many user id to one ensembl id mapping, keep all
        # remove duplicates in ensembl_gene_id
        # result <- result[which(!duplicated(result[, 2])), ]

        conversion_table <- result[, 1:2]
        colnames(conversion_table) <- c(gene_col, "ensembl_gene_id")

        result <- dplyr::left_join(x = conversion_table,
                                   y = data,
                                   by = gene_col)
    }

    message(paste(nrow(result), "genes found with Ensembl IDs"))
    return(result)
}
