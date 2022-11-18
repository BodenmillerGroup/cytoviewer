# -----------------------------------------------------------------------------
# Definition of the shiny server
# -----------------------------------------------------------------------------

#' @importFrom SummarizedExperiment assay
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                                input, output, session, ...)
{
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    .create_updateSelectizeInput(image, input, session)

    # Dynamically create image plot
    output$imagePlot <- .imagePlot(input, object, mask, image, cell_id, img_id)
    
}