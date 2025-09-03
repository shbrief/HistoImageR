#' Select and expand Prov-GigaPath embeddings
#' 
#' @param pg A data frame. Prov-GigaPath output with the dimension 1x15.
#' @param layer The layer of embedding. Available 0 to 13. If it is set to 
#' `NULL`, all the layers will be returned as 'samples x feature_dimensions'
#' matrix.
#' 
#' @return A data frame (1 x 768) of a chosen embedding layer
#' 
#' @export
getEmbedding <- function(pg, layer = NULL) {
    
    ## extract the target layer
    layer_ind <- layer + 1
    tensor_string <- pg[[1, layer_ind]] 
    
    ## Remove the tensor wrapper and brackets
    clean_string <- gsub("tensor\\(\\[\\[|\\]\\]\\)", "", tensor_string)
    
    ## Split by comma and convert to numeric
    values <- as.numeric(unlist(strsplit(clean_string, ",\\s*")))
    return(values)
}