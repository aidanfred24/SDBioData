#' Get Species Pathways
#'
#' @param genes Vector or column of data frame containing genes of interest
#' @param species_id ID of desired species
#'
#' @returns Data frame of pathways that contain the provided genes (returns
#'  all pathways if genes = NULL)
#' @export
#'
get_pathways <- function(species_id,
                         genes = NULL) {

    path_map <- get_table(species_id = species_id,
                          table = "pathway")

    pathways <- get_table(species_id = species_id,
                          table = "pathwayInfo")

    if (!is.null(genes)) {
        genes <- convert_id(genes = genes,
                            species_id = species_id)

        ix <- match(path_map$gene, genes[, 2])

        path_map <- path_map[ix[which(!is.na(ix))],]
        path_map$pathwayID <- as.character(path_map$pathwayID)

        by <- dplyr::join_by(pathwayID == id)

        pathways <- dplyr::left_join(x = path_map,
                                     y = pathways,
                                     by)
        pathways <- pathways[!duplicated(pathways[, -1]), -1]
    }

    if (nrow(pathways) == 0) {
        stop(
            paste("No pathways found. Check that the genes submitted are a",
                  "vector/column of known gene IDS.")
        )
    }

    return(pathways)
}
