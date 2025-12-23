#' Search for Species by Name
#'
#' Searches the organism database for species matching a query string.
#'
#' @param query Character. The species name, partial name, or ID to search for.
#' @param name_type Character. The type of name to search against. Options:
#'   - `"academic"`: Scientific name (default).
#'   - `"common"`: Common name (note: not available for all species).
#'   - `"id"`: Exact species ID match.
#'
#' @returns A data frame containing information for all matching species.
#'   Throws an error if no species are found.
#' @md
#' @export
#'
srch_species <- function(query,
                         name_type = "academic"){

    types <- setNames(c("name2", "academicName", "id"),
                      c("common", "academic", "id"))
    org <- get_table()

    if (name_type == "id") {
        results <- dplyr::filter(org, id == query)
    } else {
        result_ind <- sapply(org[, unname(types[[name_type]])], function(x){
            grepl(toupper(query), toupper(x))
        })

        results <- org[result_ind, ]
    }

    if (nrow(results) == 0) {
        results <- "Species Not Found"
        stop(results)
    }

    return(results)
}
