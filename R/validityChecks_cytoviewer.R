# Validity checks for cytoviewer

#' @importFrom cytomapper channelNames
#' @importFrom SingleCellExperiment colData
#' @importFrom SummarizedExperiment assayNames
#' @importFrom methods is
#' @importFrom S4Vectors mcols
#' @importFrom EBImage numberOfFrames

.valid.cytoviewer.shiny <- function(image, mask, object, cell_id, img_id){
  
  # General general validity 
  
  if (is.null(image) && is.null(mask) && is.null(object)){
    stop("Please provide 'image', 'mask' and/or 'object' data.\n", 
         "See ?cytoviewer() for details.")
  }
  
  if (!is.null(image) && 
      !is(image, "CytoImageList")) {
    stop("Please provide the image(s) in form of a 'CytoImageList' object")
    }

  if (!is.null(mask) && 
      !is(mask, "CytoImageList")) {
    stop("Please provide the mask(s) in form of a 'CytoImageList' object")
    }
  
  if (!is.null(object) && 
      !is(object, "SingleCellExperiment")) {
    stop("Please provide the 'object' in form of a 'SingleCellExperiment' or 
         'SpatialExperiment' object")
    }
 
  if (!is.null(mask) && 
      is.null(img_id)) {
    stop("Please provide an 'img_id' argument.")
    }
  
  if (!is.null(object) &&
      is.null(cell_id)) {
    stop("Please provide a 'cell_id' argument.")
    }
  
  if (!is.null(cell_id) && 
      !is.character(cell_id) || length(cell_id) > 1) {
    stop("Invalid argument for 'cell_id'.")
  }
  
  if (!is.null(img_id) && 
      !is.character(img_id) || length(img_id) > 1) {
    stop("Invalid argument for 'img_id'.")
  }
  
  if(!is.null(object)){
    if ((!is.null(img_id) && !(img_id %in% colnames(colData(object)))) ||
      (!is.null(cell_id) && !(cell_id %in% colnames(colData(object))))){
    stop("'img_id' and/or 'cell_id' not in 'colData(object)'.")
      }
    }
  
  if (!is.null(image) && 
      is.null(channelNames(image))){
    stop("Please specify the 'channelNames' of the 'image' object.")
  }
  
  if (!is.null(image) && 
      is.null(names(image))){
    stop("Please specify the 'names' of the 'image' object.")
  }
  
  if (!is.null(mask) && 
      is.null(names(mask))){
    stop("Please specify the 'names' of the 'mask' object.")
  }
  
  if (!is.null(image) && !is.null(object)){
    
    if (!identical(channelNames(image), rownames(object))) {
      stop("The 'channelNames' of the images need to match the rownames of the 
           'object'.")
    }
    
    if (length(image) != length(unique(colData(object)[[img_id]]))) {
      stop("Please provide a unique 'image' for every sample stored in 'object'.")
    }
  }
  
  if (!is.null(mask) && !is.null(object)){
    if (length(mask) != length(unique(colData(object)[[img_id]]))) {
      stop("Please provide a unique 'mask' for every sample stored in 'object'.")
    }
  }
  
  # Check image valididty
  if (!is.null(image)){
    
    if(!is.null(img_id) && !(img_id %in% colnames(mcols(image)))){
      stop("'img_id' not in 'mcols(image)'.")
    }
    
    if(!is.null(img_id)){
      l_unique <- length(unique(mcols(image)[,img_id]))
      l_all <- length(mcols(image)[,img_id])
      
      if(l_unique < l_all){
        stop("Entries in the 'mcols(image)[,img_id]' slot are not unique.")
      }
    }
  }
  
  # Check mask validity 
  if (!is.null(mask)){
    
    if(!all(unlist(lapply(mask, numberOfFrames)) == 1L)){
      stop("Segmentation masks must only contain one channel.")
    }  
    
    cur_out <- lapply(mask, function(x){all(x == floor(x))})
    if(!all(unlist(cur_out))){
      stop("Segmentation masks must only contain integer values.")
    }
    
    if(!is.null(img_id) && !(img_id %in% colnames(mcols(mask)))){
      stop("'img_id' not in 'mcols(mask)'.")
    }
    
    l_unique <- length(unique(mcols(mask)[,img_id]))
    l_all <- length(mcols(mask)[,img_id])
    if(l_unique < l_all){
      stop("Entries in the 'mcols(mask)[,img_id]' slot are not unique.")
    }
  }
  
  # Check sce validity
  if (!is.null(object)){
    
    if (!all(is.numeric(colData(object)[,cell_id]))) {
      stop("Cell ids should only contain numeric integer values.")
    }
    
    if(!all(colData(object)[,cell_id] == floor(colData(object)[,cell_id]))){
      stop("Cell ids should only contain numeric integer values.")
    }
  }
}