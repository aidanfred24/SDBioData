#' Search for Species by Name
#'
#' @param query Species name or partial name
#' @param name_type Name type for species ("common", "academic", "id"); Be aware
#'  that common names are not available for all species
#'
#' @returns Data frame of all species info with names that match the query
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
