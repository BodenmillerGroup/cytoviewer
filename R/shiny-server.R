# -----------------------------------------------------------------------------
# Definition of the shiny server
# -----------------------------------------------------------------------------

#' @importFrom SummarizedExperiment assay
.imageBrowser_server <- function(object, mask, image, cell_id, img_id,
                                input, output, session, ...)
{
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    .create_updateSelectizeInput(image, input, session)
    
    output$Outline_controls <- .create_outline_controls(object, mask, input, session)
    .populate_outline_controls(object, input, session)
    
    output$Basic_color_outline <- .create_basic_color_outline(object, mask, input, session)
    output$Advanced_color_outline <- .create_advanced_color_outline(object, mask, input, session)
    output$Outline_thickness <- .create_thickness_control(object, mask, input, session)
      
    .downloadSelection_1(input, object, mask, image, img_id, cell_id, ...)
    # Dynamically create image plot
    output$imagePlot <- .imagePlot(input, object, mask, image, img_id, cell_id)
    
    #output$downloadData <- .downloadSelection(input, object, mask, image, img_id, cell_id, ...)
    
}