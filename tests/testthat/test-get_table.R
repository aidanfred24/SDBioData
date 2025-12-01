test_that("orgInfo returned", {
    expect_equal(class(get_table()), "data.frame")
    expect_in(c("name", "name2", "idType", "id"), colnames(get_table()))
})

test_that("geneInfo returned", {
    expect_equal(class(get_table(species_id = 1)), "data.frame")
    expect_in(c("ensembl_gene_id", "band", "chromosome_name", "entrezgene_id"),
              colnames(get_table(species_id = 1)))
})
