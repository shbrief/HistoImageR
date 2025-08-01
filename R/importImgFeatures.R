#' Import nucleu features for TCGA samples as SpatialExperiment
#' 
#' This function imports HoVerNet outputs (JSON) or Squidpy outputs (h5ad) as R list.
#' 
#' @importFrom stringr str_extract
#' @importFrom MultiAssayExperiment ExperimentList
#' 
#' @param x A character vector containing file paths to image feature files.
#' @param subsample_rate A numeric value >= 1. `1/subsample_rate` amount of 
#' cells will be collected. If you want to import all the cells, set this as `1`.
#' 
#' @return An ExperimentList object containing SpatialExperiment objects (one per slide/sample)
#' 
#' @export
importImgFeatures <- function(fnames, subsample_rate = 10, seed = NULL) {
  
    patientIDs <- regmatches(basename(fnames), regexpr("TCGA-[0-9A-Z]{2}-[0-9A-Z]{4}", basename(fnames)))
    
    ## Keep only one slide per patient for now <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    dupInd <- which(duplicated(patientIDs))
    message(paste("Only one slide for", patientIDs[dupInd], "is imported."))
    patientIDs <- patientIDs[-dupInd]
    fnames <- fnames[-dupInd]
    
    # patientIDs <- stringr::str_extract(fnames, "TCGA-\\d{2}-\\d{4}")
    spelist <- vector(mode = "list", length = length(fnames))
    names(spelist) <- patientIDs
  
    for (i in seq_along(fnames)) {
        fname <- fnames[i]
    
        if (grepl(".json$", fname)) {
            spe <- jsonToSe(fname, subsample_rate = subsample_rate, seed = seed)
        } else if (grepl(".h5ad$", fname)) {
            spe <- h5adToSe(fname, subsample_rate = subsample_rate, seed = seed)
        }

        message(paste(i, "out of", length(fnames), "image has imported"))
    
        # ## Create SotredSpatialImage object (from local file)
        # image_file <- file.path(pathToFiles, paste0(fnames[i], ".png"))
        # spe <- addImg(spe, 
        #               imageSource = image_file,
        #               sample_id = "sample01",
        #               image_id = "h&e",
        #               scaleFactor = 1)
    
        ## Combine all results as ExperimentList
        spelist[i] <- spe
    }
  
    spes <- MultiAssayExperiment::ExperimentList(spelist)
    return(spes)
}