#' Process Gene Expression Data
#'
#' Performs pre-processing, missing value imputation, filtering, and
#' transformation on gene expression count data.
#'
#' @param data A numeric matrix or data frame of gene expression counts.
#' @param missing_value Character. Method to handle missing values. Options:
#'   * `"geneMedian"`: Impute using the median expression of the gene across
#'   samples.
#'   * `"treatAsZero"`: Replace NAs with 0.
#'   * `"groupMedian"`: Impute using the median of the sample group
#'   (detected from colnames).
#' @param min_cpm Numeric. Minimum counts per million threshold for filtering
#' genes.
#' @param n_min_samples_count Numeric. Minimum number of samples that must meet
#'   the `min_cpm` threshold for a gene to be retained.
#' @param counts_transform Integer. Method for data transformation:
#'   0. None (Raw Counts).
#'   1. log2(CPM + `counts_log_start`)
#'   2. Variance Stabilizing Transformation (VST) via `DESeq2`.
#'   3. Regularized Log (rlog) via `DESeq2`.
#' @param counts_log_start Numeric. Constant added to counts before log
#' transformation
#'   (used when `counts_transform = 1`).
#'
#' @returns The processed and transformed data matrix.
#'
#' @md
#' @export
process_data <- function(data,
                         missing_value,
                         min_cpm,
                         n_min_samples_count,
                         counts_transform,
                         counts_log_start) {
    # Sort by standard deviation -----------
    data <- data[order(-apply(
        data[, 1:dim(data)[2]],
        1,
        function(x) sd(x, na.rm = TRUE)
    )), ]

    # Missng values in expression data ----------
    if (sum(is.na(data)) > 0) {
        if (missing_value == "geneMedian") {
            row_medians <- apply(data, 1, function(y) median(y, na.rm = T))
            for (i in 1:ncol(data)) {
                val_miss_row <- which(is.na(data[, i]))
                data[val_miss_row, i] <- row_medians[val_miss_row]
            }
        } else if (missing_value == "treatAsZero") {
            data[is.na(data)] <- 0
        } else if (missing_value == "groupMedian") {
            sample_groups <- detect_groups(colnames(data))
            for (group in unique(sample_groups)) {
                samples <- which(sample_groups == group)
                row_medians <- apply(
                    data[, samples, drop = F],
                    1,
                    function(y) median(y, na.rm = T)
                )
                for (i in samples) {
                    missing <- which(is.na(data[, i]))
                    if (length(missing) > 0) {
                        data[missing, i] <- row_medians[missing]
                    }
                }
            }
            if (sum(is.na(data)) > 0) {
                row_medians <- apply(
                    data,
                    1,
                    function(y) median(y, na.rm = T)
                )
                for (i in 1:ncol(data)) {
                    missing <- which(is.na(data[, i]))
                    data[missing, i] <- row_medians[missing]
                }
            }
        }
    }

    data <- round(data, 0)
    # Check if any columns have all zeros
    if (any(apply(data, 2, function(col) all(col == 0)))) {
        return(as.matrix(data))
    }


    data <- data[which(apply(
        edgeR::cpm(edgeR::DGEList(counts = data)),
        1,
        function(y) sum(y >= min_cpm)
    ) >= n_min_samples_count), ]

    # R cannot handle integers larger than 3 billion, which can impact popular
    # packages such as DESeq2. R still uses 32-bit integers, and the largest
    # allowable integer is 2^32 −1. In an unusual case, a user's RNA-Seq counts
    # matrix included a count of 4 billion for a single gene, which was converted
    # to NA, leading to an error in DESeq2. This issue also caused the iDEP app to crash.
    if (max(data) > 2e9) {
        scale_factor <- max(data) / (2^32 - 1)
        # round up scale_factor to the nearest integer
        scale_factor <- ceiling(scale_factor / 10 + 1) * 10 #  just to be safe.
        # divide by scale factor and round to the nearest integer, for the entire matrix, data
        data <- round(data / scale_factor)
    }


    # Construct DESeqExpression Object ----------
    if (counts_transform != 0) {
        tem <- rep("A", dim(data)[2])
        tem[1] <- "B"
        col_data <- cbind(colnames(data), tem)
        colnames(col_data) <- c("sample", "groups")

        dds <- DESeq2::DESeqDataSetFromMatrix(
            countData = data,
            colData = col_data,
            design = ~groups
        )
        dds <- DESeq2::estimateSizeFactors(dds)

        # Counts Transformation ------------
        if (counts_transform == 3) {
            data <- DESeq2::rlog(dds, blind = TRUE)
            data <- SummarizedExperiment::assay(data)
        } else if (counts_transform == 2) {
            data <- DESeq2::vst(dds, blind = TRUE)
            data <- SummarizedExperiment::assay(data)
        } else {
            data <- log2(BiocGenerics::counts(
                dds,
                normalized = TRUE
            ) + counts_log_start)
        }
    }


    data <- data[order(-apply(
        data[, 1:dim(data)[2]],
        1,
        sd
    )), ]

    return(as.matrix(data))
}

#' Detect Groups by Sample Names
#'
#' Detects experimental groups based on column names in the data or an
#' optional sample info matrix.
#'
#' @param sample_names Vector of column names (sample IDs) from the data.
#' @param sample_info Optional matrix or data frame of experiment design information.
#'   If `NULL`, groups are inferred by stripping numeric suffixes and "Rep" labels
#'   from `sample_names`.
#'
#' @returns A character vector representing the group assignment for each sample.
#' @note This function is mainly called internally by other processing functions.
#' @export
detect_groups <- function(sample_names, sample_info = NULL) {
    # sample_names are col names parsing samples by either the name
    # or using a data frame of sample infos.
    # Note that each row of the sample_info data frame represents a sample.
    sample_group <- NULL
    if (is.null(sample_info)) {
        # Remove all numbers from end
        # remove "_" from end
        # remove "_Rep" from end
        # remove "_rep" from end
        # remove "_REP" from end
        sample_group <- gsub(
            "[0-9]*$", "",
            sample_names
        )
        sample_group <- gsub("_$", "", sample_group)
        sample_group <- gsub("_Rep$", "", sample_group)
        sample_group <- gsub("_rep$", "", sample_group)
        sample_group <- gsub("_REP$", "", sample_group)
    } else {
        # the orders of samples might not be the same.
        # The total number of samples might also differ
        match_sample <- match(sample_names, row.names(sample_info))
        sample_info2 <- sample_info[match_sample, , drop = FALSE]
        if (ncol(sample_info2) == 1) {
            # if there's only one factor
            sample_group <- sample_info2[, 1]
        } else {
            # multiple columns/factors
            foo <- function(y) paste(y, collapse = "_")
            sample_group <- unlist(apply(
                X = sample_info2,
                MARGIN = 1,
                FUN = foo
            ))
            names(sample_group) <- row.names(sample_info2)
            if (min(table(sample_group)) == 1) { # no replicates?
                sample_group <- sample_info2[, 1]
            }
        }
    }
    return(as.character(sample_group))
}
