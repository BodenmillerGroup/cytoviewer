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
    #.populate_advanced_color_outline(object, mask, input, session)
    output$Outline_thickness <- .create_thickness_control(object, mask, input, session)
    
    output$tiles_tab <- .add_tiles_tab(input, object, mask, image, img_id, cell_id)
    
    observe({
      cur_markers <- .select_markers(input)
      cur_markers <- cur_markers[cur_markers != ""]
      lapply(seq_along(cur_markers), function(cur_plot){
        
        # Ideas to add tiles functionality 
        # Idea 1: Would need writable input 
        # seq <- seq_along(cur_markers)
        # seq <- seq[seq != cur_plot]
        # 
        # for(x in seq){
        #   input[[paste0("marker",x)]] <- ""
        # }
        
        # seq <- seq_along(cur_markers)
        # cur_markers[seq != cur_plot] <- ""
        # 
        # Idea 2: Would need java-scripting
        # session$sendCustomMessage("marker1", cur_markers[1])
        # session$sendCustomMessage("marker2", cur_markers[2])
        # session$sendCustomMessage("marker3", cur_markers[3])
        # session$sendCustomMessage("marker4", cur_markers[4])
        # session$sendCustomMessage("marker5", cur_markers[5])
        # session$sendCustomMessage("marker6", cur_markers[6])
        
        output[[paste0("tile", cur_plot)]] <- .imagePlot(input, object, mask, image, img_id, cell_id)
        })
      })
    
    .downloadSelection_1(input, object, mask, image, img_id, cell_id, ...)
    
    # Dynamically create image plot
    output$imagePlot <- .imagePlot(input, object, mask, image, img_id, cell_id)
    
    #output$downloadData <- .downloadSelection(input, object, mask, image, img_id, cell_id, ...)
    
}