#' Filter Gene Data
#'
#' @param data Data frame containing gene IDs and information
#' @param chrs Vector of chromosome names/numbers to keep in data
#' @param min_gc_content Minimum percentage GC content for genes
#' @param max_gc_content Maximum percentage GC content for genes
#'
#' @returns Data frame of filtered gene information
#' @export
#'
gene_filter <- function(data,
                        chrs = NULL,
                        min_gc_content = NULL,
                        max_gc_content = NULL){

    if(!is.null(chrs)){
        data <- data |>
            dplyr::filter(as.character(chromosome_name) %in% as.character(chrs))
    }

    if(!is.null(min_gc_content)){
        data <- data |>
            dplyr::filter(percentage_gc_content >= min_gc_content)
    }

    if(!is.null(max_gc_content)){
        data <- data |>
            dplyr::filter(percentage_gc_content <= max_gc_content)
    }

    return(as.data.frame(data))
}
