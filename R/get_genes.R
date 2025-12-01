#' Get Genes
#'
#' @param species_id
#' @param genes
#'
#' @returns
#' @export
#'
#' @examples
get_genes <- function(species_id,
                      genes) {

    gene_table <- get_table(species_id = species_id,
                            table = "geneInfo")

    if (!is.null(genes)) {
        genes <- convert_id(genes = genes,
                            species_id = species_id)

        ix <- match(genes[, 2], gene_table$ensembl_gene_id)

        gene_table <- gene_table[ix, ]
    }

    return(gene_table)
}
