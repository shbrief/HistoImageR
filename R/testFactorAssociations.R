#' Test factor associations with sample groups
#' 
#' This function tests whether MOFA factor scores are significantly associated 
#' with different sample groups using the Kruskal-Wallis test. It can 
#' automatically select appropriate grouping variables or test user-specified 
#' groups.
#' 
#' @importFrom stats kruskal.test p.adjust
#' 
#' @param object A trained MOFA object containing factor scores and sample 
#' metadata
#' @param factor Character or numeric (1). The MOFA factor to test (required).
#' @param groups Character vector. Names of metadata columns to use as grouping 
#'   variables. If `NULL` (default), the function automatically selects columns 
#'   with 2-10 unique non-NA values
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{group}{Name of the grouping variable tested}
#'     \item{statistic}{Kruskal-Wallis H statistic (chi-square)}
#'     \item{p_value}{Raw p-value from the test}
#'     \item{df}{Degrees of freedom}
#'     \item{p_adjusted}{Benjamini-Hochberg adjusted p-value}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts factor scores for the specified factor
#'   \item Merges factor scores with sample metadata
#'   \item Selects grouping variables (automatically or user-specified)
#'   \item Runs Kruskal-Wallis tests for each grouping variable
#'   \item Applies Benjamini-Hochberg correction for multiple testing
#' }
#'
#' When \code{groups = NULL}, the function automatically selects metadata 
#' columns that have between 2 and 10 unique non-NA values, which are typically
#' suitable for group comparisons.
#'
#' The Kruskal-Wallis test is used because it doesn't assume normality and is
#' appropriate for comparing factor scores across multiple groups.
#' 
#' @examples
#' \dontrun{
#' # Test associations for Factor 1 with auto-selected groups
#' results <- test_factor_associations(mofa_object, factor = 1)
#' 
#' # Test specific grouping variables
#' results <- test_factor_associations(mofa_object, 
#'                                     factor = "Factor1",
#'                                     groups = c("treatment", "tissue_type"))
#' 
#' # View significant associations (p < 0.05)
#' significant <- results[results$p_adjusted < 0.05, ]
#' }
#'
#' @seealso
#' \code{\link[stats]{kruskal.test}}, \code{\link[stats]{p.adjust}}
#'
#' 
testFactorAssociations <- function(object,
                                   factor = NULL,
                                   groups = NULL) {
    
    ## Check the input
    if (is.null(factor)) {
        stop("Specify the MOFA Factor to run the test.")
    }
    
    ## Extract factor scores (Z matrix)
    factor_scores <- get_factors(object, 
                                 factors = factor,        
                                 groups = "all",
                                 as.data.frame = TRUE)
    
    ## Get sample metadata (should include your grouping variable)
    metadata <- samples_metadata(object)
    
    ## Merge factor scores with metadata
    df_factors <- merge(factor_scores, metadata, by = "sample")
    
    ## Sanity check 
    if (any(!groups %in% colnames(metadata))) {
        stop("The selected group(s) are not available.")
    }
    
    ## Choose the groups
    if (is.null(groups)) {
        ## Select groups with more then 2 categories
        numLv <- apply(metadata, 2, function(x) {length(unique(na.omit(x)))}) # integer vector: the number of unique values
        
        # We can expose these (n and m) as parameters 
        n <- 2 # the minimum number of unique values per attribute
        m <- 10 # the maximum number of unique values per attribute
        nLv <- numLv[numLv >= n & numLv <= m] # two levels most likely include `NA`
        
        groups <- names(nLv)
    }
    
    ## Initialize the result object
    kw_results <- vector(mode = "list", length = length(groups))
    
    ## Run Kruskal-Wallis test
    for (group in groups) {
        formula_str <- paste("value ~", group)
        kw_test <- kruskal.test(as.formula(formula_str), data = df_factors)
        
        kw_results[[group]] <- list(
            group = group,
            statistic = kw_test$statistic,
            p_value = kw_test$p.value,
            df = kw_test$parameter
        )
    }
    
    ## Convert results to data frame
    kw_results_df <- do.call(rbind, lapply(kw_results, data.frame))
    kw_results_df$p_adjusted <- p.adjust(kw_results_df$p_value, method = "BH")
    
    rownames(kw_results_df) <- NULL
    
    return(kw_results_df)
}
