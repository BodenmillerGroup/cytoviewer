#'Shiny application to visualise multi-channel images
#'
#'TODO
#'
#'@param object a \code{\linkS4class{SingleCellExperiment}} object.
#'@param mask (optional) a \code{\linkS4class{CytoImageList}} containing
#'    single-channel \code{\linkS4class{Image}} objects.
#'@param image (optional) a \code{\linkS4class{CytoImageList}} object containing
#'    single or multi-channel \code{\linkS4class{Image}} objects.
#'@param cell_id character specifying the \code{colData(object)} entry, in which
#'    the integer cell IDs are stored. These IDs should match the integer pixel
#'    values in the segmentation mask object (\code{mask}).
#'@param img_id character specifying the \code{colData(object)} and
#'    \code{mcols(mask)} and/or \code{mcols(image)} entry, 
#'    in which the image IDs are stored.
#'@param ... parameters passed to the \code{\link{plotCells}} or
#'    \code{\link{plotPixels}} function.
#'
#'
#'@return A Shiny app object for multi-channel image visualization
#'
#'@examples
#'TODO
#'
#'
#'@export
#'
#'@import shiny
#'@import shinydashboard
cytoviewer <- function(image,
                        mask = NULL,
                        object = NULL,
                        cell_id = NULL,
                        img_id = NULL,
                        ...) {
    # Object checks
    #.valid.sce.shiny(image, mask, object, cell_id, img_id)
    #.valid.image(image, img_id)

    #if (!is.null(mask)) {
    #    .valid.sce(object, img_id, cell_id, exprs_values = NULL)
    #    .valid.mask(mask, img_id)
    #    .valid.matchObjects.plotCells(object, mask, img_id)
    #}

    #if (!is.null(image) & !is.null(mask)) {
    #    .valid.matchObjects.plotPixels(object, mask, image, img_id)
    #}

    shiny_ui <- dashboardPage(
        header = .imageBrowser_header(),
        sidebar = .imageBrowser_sidebar(),
        body = .imageBrowser_body()
        
    )

    shiny_server <- function(input, output, session) {
        .imageBrowser_server(object, mask, image, cell_id, img_id,
                            input, output, session, ...)
    }

    shinyApp(ui = shiny_ui, server = shiny_server)
}


