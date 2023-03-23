#'cytoviewer - Shiny application to interactively browse multi-channel images
#'
#'This shiny application allows users to interactively visualize multi-channel 
#'images and masks. The cytoviewer package is divided into image-level (Composite and Channels) 
#'and cell-level visualization (Masks). It allows users to overlay individual images 
#'with masks and integrates well with \code{SingleCellExperiment} and \code{SpatialExperiment} 
#'objects for metadata visualization. 
#'
#'@param image (optional) a \code{CytoImageList} object containing 
#'    single or multi-channel \code{Image} objects.
#'@param mask (optional) a \code{CytoImageList} containing
#'    single-channel \code{Image} objects.
#'@param object (optional) a \code{SingleCellExperiment} or \code{SpatialExperiment} object.
#'@param cell_id character specifying the \code{colData(object)} entry, in which
#'    the integer cell IDs are stored. These IDs should match the integer pixel
#'    values in the segmentation mask object (\code{mask}).
#'@param img_id character specifying the \code{colData(object)} and
#'    \code{mcols(mask)} and/or \code{mcols(image)} entry, 
#'    in which the image IDs are stored.
#'    
#'@section The input objects
#'TODO - explain what happens when you use different input objects
#'    
#'@return A Shiny app object for interactive multi-channel image visualization 
#'and exploration
#'
#'@examples
#'## Only run this example in interactive R sessions
#'if (interactive()) {
#'
#' library(cytoviewer)
#' 
#' # Load example datasets 
#' data("pancreasImages")
#' data("pancreasMasks")
#' data("pancreasSCE")
#'
#' # Use shiny with images
#' app <- cytoviewer(image = pancreasImages, img_id = "ImageNb")
#' shiny::runApp(app, launch.browser = TRUE)
#' 
#' # Use shiny with images and masks 
#' app <- cytoviewer(image = pancreasImages, masks = pancreasMasks, img_id = "ImageNb", cell_id = "CellNb")
#' shiny::runApp(app, launch.browser = TRUE)
#' 
#' # Use shiny with masks and SCE objects
#' app <- cytoviewer(masks = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb")
#' shiny::runApp(app, launch.browser = TRUE)
#' 
#' # Use shiny with images, masks and SCE object
#' app <- cytoviewer(image = pancreasImages, masks = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb")
#' shiny::runApp(app, launch.browser = TRUE)
#' 
#'}
#'
#'@export
#'
#' @import shiny
#' @import shinydashboard

cytoviewer <- function(image,
                       mask = NULL,
                       object = NULL,
                       cell_id = NULL,
                       img_id = NULL) {
    
    # Validity checks - TO DO 

    shiny_ui <- dashboardPage(
        header = .cytoviewer_header(),
        sidebar = .cytoviewer_sidebar(),
        body = .cytoviewer_body() 
    )

    shiny_server <- function(input, output, session) {
        .cytoviewer_server(object, mask, image, cell_id, img_id,
                            input, output, session)
    }
    
    shinyApp(ui = shiny_ui, server = shiny_server)
}


