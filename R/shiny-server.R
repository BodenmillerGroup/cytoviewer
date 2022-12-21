# -----------------------------------------------------------------------------
# Definition of the shiny server
# -----------------------------------------------------------------------------

#' @importFrom SummarizedExperiment assay
.imageBrowser_server <- function(object, mask, image, cell_id, img_id,
                                input, output, session, ...)
{
    ## Session Info
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    ## Dynamic user inputs
    # Sample selection 
    .create_interactive_observer(image, input, session)
    
    # Marker inputs
    .create_updateSelectizeInput(image, input, session)
    
    # Outline inputs
    output$Outline_controls <- .create_outline_controls(object, mask, input, session)
    .populate_outline_controls(object, input, session)
    output$Basic_color_outline <- .create_basic_color_outline(object, mask, input, session)
    output$Advanced_color_outline <- .create_advanced_color_outline(object, mask, input, session)
    #.populate_advanced_color_outline(object, mask, input, session)
    output$Outline_thickness <- .create_thickness_control(object, mask, input, session)
    
    ## Image-level
    # Dynamically create image plot
    output$imagePlot <- .imagePlot(input, object, mask, image, img_id, cell_id)
    
    # Dynamically create image tiles 
    output$tiles_tab <- .add_tiles_tab(input, object, mask, image, img_id, cell_id)
    
    observe({
    
      cur_markers <- .select_markers(input)
      cur_markers <- cur_markers[cur_markers != ""]
  
        lapply(seq_along(cur_markers), function(cur_plot){
          output[[paste0("tile", cur_plot)]] <- renderPlot(.create_image_tiles(input, object, mask, image, img_id, cell_id)[[cur_plot]]$plot)
        })
      })
    
    
    ## Cell-level 
    output$Colorby_controls <- .create_colorby_controls(object, mask, input, session)
    .populate_colorby_controls(object, input, session)
    output$Colorby_colors <- .create_colorby_color(object, mask, input, session, ...)
    
    output$cells_tab <- .add_cells_tab(input, object, mask, image, img_id, cell_id)
    output$cellsPlot <- .cellsPlot(input, object, mask, image, img_id, cell_id, ...)
    
    
    ## Download
    .downloadSelection_1(input, object, mask, image, img_id, cell_id, ...)
    #output$downloadData <- .downloadSelection(input, object, mask, image, img_id, cell_id, ...)
    
}