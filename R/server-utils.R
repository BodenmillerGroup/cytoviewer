# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------


# Generate help text - TO BE UPDATED
.general_help <- function(){
    tagList(
        h3("Using the Shiny application"),
        p("This help page provides a recommended workflow on how to most ",
        "efficiently use the app. The workflow is solely a recommendation - ",
        "the app provides full flexibility to change settings during each ",
        "step. To see the full documentation, please refer to the help page ",
        "found at", em("?cytomapperShiny")),
        h3("1. Select the number of plots"),
        p("The slider under ", em("General controls"), 
        " can be used to specify ",
        "the number of plots on which to perform gating. Up to two markers ",
        "can be visualized per plot."),
        h3("2. Select the sample"),
        p("The ", em("assay"), " dropdown selection under ", 
        em("General controls"), " allows the user to specify on",
        "which assay entry to perform gating. In most cases, a log- or ",
        "arcsinh-transformation can help to distinguish between 'positive' ",
        "and 'negative' populations."),
        h3("3. Select the markers"),
        p("For each plot, up to two markers can be specified. If selecting ",
        "a single marker, please specify this marker in the first of the ",
        "two dropdown menus. A violin plot is used to visualize the ",
        "expression of a single marker while a scatter plot is used to ",
        "visualize the expression of two markers."),
        h3("4. Gate cells"),
        p("When selecting cells in one plot, only those cells are visualized ",
        "on the following plot. Once markers, the assay or the number of ",
        "plots are changed, gates are cleared."),
        h3("5. Observe the selected cells"),
        p("After gating, the selected cells are visualized on the ",
        "corresponding images by switching to the ",
        em("Images"), " tab. By default, the first marker is selected. ",
        "The user can change the displayed marker or press reset marker ",
        "to switch to the markers used for gating. If a multi-channel ",
        "image object is provided, the contrast of the image can be ",
        "changed. The right panel visualizes the selected cells either ",
        "by filling in the segmentation masks or by outlining the ",
        "cells on the images."),
        h3("6. Change samples"),
        p("Samples can now be iteratively changed using the dropdown ",
        "menu under ", em("General controls"), ". The gates will remain ",
        "on the plots and can be adjusted for each sample."),
        h3("7. Save the selected cells"),
        p("Finally, the selected cells can be saved by clicking the download ",
        "button next to the '?' symbol. The selected cells will be stored ",
        "as a ", em("SingleCellExperiment"), " object in .rds format.",
        "Per selection, the user can provide a ", em("Cell label"), 
        " that will be stored in the ", em("colData"), " under the ", 
        em("cytomapper_CellLabel"), " entry of the downloaded object.")
    )
}

# Create general observers for header
#' @importFrom utils capture.output
.create_general_observer <- function(input, si){


    # Return session info
    observeEvent(input$SessionInfo, {
        showModal(modalDialog(
            pre(paste(capture.output(si), collapse = "\n")),
            size = "l",fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Session Info",
        ))
    })

    # Return helptext
    observeEvent(input$Help, {
        showModal(modalDialog(
            .general_help(),
            size = "l",fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Help",
        ))
    })
}

# Create interactive observers
.create_interactive_observer <- function(image, input, session){

    # Next Image Observer
    observeEvent(input$next.sample, {
        img_IDs <- names(image)
        cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == length(img_IDs), 1, cur_index + 1)
    
        # return updated img_id 
        updated_sample <- img_IDs[updated_index]
    
        updateSelectInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        selected = updated_sample)
    
        }, ignoreInit = TRUE)    

    # Previous Image Observer
    observeEvent(input$previous.sample, {
        img_IDs <- names(image)
        cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == 1,  length(img_IDs), cur_index - 1)
    
        # return updated img_id
        updated_sample <- img_IDs[updated_index]
    
        updateSelectInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        selected = updated_sample)

    }, ignoreInit = TRUE)    
}

# Create updateSelectizeInput objects
.create_updateSelectizeInput <- function(image, input, session){
    # Store image IDs and marker names
    img_IDs <- names(image)
    markers <- channelNames(image)

    updateSelectizeInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        selected = unique(img_IDs)[1])
    updateSelectizeInput(session, inputId = "marker1",
                        choices = markers,
                        server = TRUE,
                        selected = markers[1])
    updateSelectizeInput(session, inputId = "marker2",
                         choices = markers,
                         server = TRUE,
                         selected = "")
    updateSelectizeInput(session, inputId = "marker3",
                         choices = markers,
                         server = TRUE,
                         selected = "")
    updateSelectizeInput(session, inputId = "marker4",
                         choices = markers,
                         server = TRUE,
                         selected = "")
    updateSelectizeInput(session, inputId = "marker5",
                         choices = markers,
                         server = TRUE,
                         selected = "")
    updateSelectizeInput(session, inputId = "marker6",
                         choices = markers,
                         server = TRUE,
                         selected = "")
}

# Helper function to select markers
.select_markers <- function(input, exprs_marker_update = TRUE){
    cur_markers <- c(input$marker1, input$marker2, input$marker3, 
                     input$marker4, input$marker5, input$marker6)
    
    cur_views <- c(input$view1, input$view2, input$view3, 
                   input$view4, input$view5, input$view6)
    
    names(cur_markers) <- cur_views
    
    cur_markers[names(cur_markers) == "FALSE"] <- ""
    
    return(cur_markers)
}

# Helper function to select colors
.select_colors <- function(input, exprs_marker_update = TRUE){
  cur_colors <- list(c("black", input$color1),
                     c("black", input$color2),
                     c("black", input$color3),
                     c("black", input$color4),
                     c("black", input$color5),
                     c("black", input$color6))
  
  cur_markers <- .select_markers(input)
  names(cur_colors) <- cur_markers

  return(cur_colors)
}


# Helper function to select outline colors
.select_outline_colors <- function(input, object, mask, session, exprs_marker_update = TRUE){
  
  browser()
  input[["color_outline1"]]
  cur_vec <- input[grepl("color_outline_", names(input))]
  names(cur_vec) <- input$select_outline
  cur_vec <- unlist(cur_vec, use.names = TRUE)
  return(cur_vec)

}


# Helper function to define bcg parameter when using plotPixels()
.select_contrast <- function(input){
    cur_markers <- .select_markers(input)
    
    cur_bcg <- list(c(input$brightness1, input$contrast1, input$gamma1),
                    c(input$brightness2, input$contrast2, input$gamma2),
                    c(input$brightness3, input$contrast3, input$gamma3),
                    c(input$brightness4, input$contrast4, input$gamma4),
                    c(input$brightness5, input$contrast5, input$gamma5),
                    c(input$brightness6, input$contrast6, input$gamma6))
    names(cur_bcg) <- cur_markers

    return(cur_bcg)
}


# Helper function for legend construction 

.show_legend <- function(input){
  legend_param <- list(margin = 3) #use default options from cytomapper
  
  if(input$show_legend){cur_legend <- legend_param}else{cur_legend <- NULL}
  return(cur_legend)
}

# Helper function for image title 

.show_title <- function(input){
  imagetitle_param <- list(margin = c(10,2)) #use default options from cytomapper
  
  if(input$show_title){cur_imagetitle <- imagetitle_param}else{cur_imagetitle <- NULL}
  return(cur_imagetitle)
}

#  Helper function to construct image 

.create_image <- function(input, object, mask,
                          image, img_id, cell_id, cur_markers, cur_bcg, cur_color,
                          ...){
    # Marker and color control
    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    cur_color <- .select_colors(input)
    cur_color <- cur_color[names(cur_color) != ""]
    
    cur_basic_outline <- input$basic_color_outline
    cur_scale <- input$scalebar
    cur_thick <- input$thick
    cur_image <- image[input$sample]
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)

    if (input$outline && input$outline_by == "") {
        cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
        
        plotPixels(image = cur_image,
                   mask = cur_mask,
                   img_id = img_id,
                   colour_by = cur_markers,
                   colour = cur_color,
                   missing_colour = cur_basic_outline, 
                   bcg = cur_bcg,
                   legend = cur_legend,
                   image_title = cur_imagetitle,
                   thick = cur_thick,
                   scale_bar = list(length = cur_scale),
                   ...)
        
    } else if (input$outline && input$outline_by != "" && !is.null(input$select_outline)) {
        cur_object <- object[,colData(object)[[input$outline_by]] %in% input$select_outline]
        cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
        #cur_advanced_outline <- .select_outline_colors(input)
        #cur_color[[input$outline_by]] <- cur_advanced_outline
        
        plotPixels(image = cur_image,
                   mask = cur_mask,
                   object = cur_object,
                   img_id = img_id,
                   cell_id = cell_id,
                   colour_by = cur_markers,
                   colour = cur_color,
                   bcg = cur_bcg,
                   outline_by = input$outline_by,
                   legend = cur_legend,
                   image_title = cur_imagetitle,
                   thick = cur_thick,
                   scale_bar = list(length = cur_scale),
                   ...)
        
    } else {
        plotPixels(image = cur_image,
                   colour_by = cur_markers,
                   colour = cur_color,
                   bcg = cur_bcg,
                   legend = cur_legend,
                   image_title = cur_imagetitle,
                   scale_bar = list(length = cur_scale),
                   ...)   
    }
}

# Visualize marker expression on images
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom svglite stringSVG
.imagePlot <- function(input, object, mask,
                       image, img_id, cell_id, ...){
    renderSvgPanZoom({

        suppressMessages(svgPanZoom(stringSVG(
            .create_image(input, object, mask,
                          image, img_id, cell_id, cur_markers, cur_bcg,
                          ...)
            ),
            zoomScaleSensitivity = 0.4, 
            maxZoom = 20,
            controlIconsEnabled = TRUE, 
            viewBox = FALSE))
    })
}

# Optional: Image function for not-zoomable images - maybe relevant for tiles
.basic_imagePlot <- function(input, object, mask, 
                       image, img_id, cell_id, ...){
  
  renderPlot({
    .create_image(input, object, mask,
                            image, img_id, cell_id, cur_markers, cur_bcg,
                            ...)
    })
}


## Image tiles function draft 
.create_image_tiles <- function(input, object, mask, image, img_id, cell_id, ...){
  
  #browser()
  cur_markers <- .select_markers(input)
  cur_markers <- cur_markers[cur_markers != ""]
  
  plot_list <- list()
  plot_list <- lapply(seq_along(cur_markers), function(i){ 
    
    seq <- seq_along(cur_markers)
    markers <- cur_markers
    markers[seq != i] <- ""
    markers <- markers[markers != ""]
    
    # Coloring
    cur_color <- .select_colors(input)
    cur_color <- cur_color[names(cur_color) != ""]
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    
    cur_basic_outline <- input$basic_color_outline
    cur_scale <- input$scalebar
    cur_thick <- input$thick
    cur_image <- image[input$sample]
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)
    
    if (input$outline && input$outline_by == "") {
      cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
      
      plot_list[[i]] <- plotPixels(image = cur_image,
                 mask = cur_mask,
                 img_id = img_id,
                 colour_by = markers,
                 colour = cur_color,
                 missing_colour = cur_basic_outline, 
                 bcg = cur_bcg,
                 legend = cur_legend,
                 image_title = cur_imagetitle,
                 thick = cur_thick,
                 scale_bar = list(length = cur_scale),
                 return_plot = TRUE,
                 ...)
      
    } else if (input$outline && input$outline_by != "" && !is.null(input$select_outline)) {
      cur_object <- object[,colData(object)[[input$outline_by]] %in% input$select_outline]
      cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
      #cur_advanced_outline <- .select_outline_colors(input)
      #cur_color[[input$outline_by]] <- cur_advanced_outline
      
      plot_list[[i]] <- plotPixels(image = cur_image,
                 mask = cur_mask,
                 object = cur_object,
                 img_id = img_id,
                 cell_id = cell_id,
                 colour_by = markers,
                 colour = cur_color,
                 bcg = cur_bcg,
                 outline_by = input$outline_by,
                 legend = cur_legend,
                 image_title = cur_imagetitle,
                 thick = cur_thick,
                 scale_bar = list(length = cur_scale),
                 return_plot = TRUE,
                 ...)
      
    } else {
      plot_list[[i]] <- plotPixels(image = cur_image,
                 colour_by = markers,
                 colour = cur_color,
                 bcg = cur_bcg,
                 legend = cur_legend,
                 image_title = cur_imagetitle,
                 scale_bar = list(length = cur_scale),
                 return_plot = TRUE,
                 ...)   
    }
    
    #return(plot_list)
  
    })
  
  return(plot_list)
}




# # Download the images - via downloadHandler
# .downloadSelection <- function(input, object, mask,
#                                image, img_id, cell_id, ...){
#   downloadHandler(
#     filename = function(){
#       paste0(input$filename1, ".",input$filename2)
#       },
#     content = function(file){
#       #browser()
#       if(input$filename2 == "pdf"){
#         pdf(file = file)
#         .create_image(input, object, mask,
#                       image, img_id, cell_id, cur_markers, cur_bcg,
#                       ...)
#         dev.off()
#       } else {
#         png(file = file)
#         .create_image(input, object, mask,
#                       image, img_id, cell_id, cur_markers, cur_bcg,
#                       ...)
#         dev.off()
#       }
#     }
#   )
# }
      

# Download the images - via ActionButton 
#' @importFrom cowplot ggdraw plot_grid
.downloadSelection_1 <- function(input, object, mask,
                               image, img_id, cell_id, ...){
  observeEvent(input$download_data, {
    if(input$fileselection == "Composite"){
      if(input$filename2 == "pdf"){
        pdf(file = paste0(input$filename1, ".",input$filename2))
        .create_image(input, object, mask,
                      image, img_id, cell_id, cur_markers, cur_bcg,
                      ...)
        dev.off()
      } else {
        png(file = paste0(input$filename1, ".",input$filename2))
        .create_image(input, object, mask,
                      image, img_id, cell_id, cur_markers, cur_bcg,
                      ...)
        dev.off()
      }}
    else if(input$fileselection == "Tiles"){
      browser()
      cur_markers <- .select_markers(input)
      cur_markers <- cur_markers[cur_markers != ""]
      plot_list <- .create_image_tiles(input, object, mask, image, img_id, cell_id)
      
      plot_out <- list()
      plot_out <- lapply(seq_along(cur_markers), function(i){ 
        plot_out[[i]] <- cowplot::ggdraw(plot_list[[i]]$plot, clip = "on")
      })

      cur_row <- ceiling(length(cur_markers) / 3) # number of rows
      cur_col <- ifelse(length(cur_markers) > 3, 3, length(cur_markers)) #number of columns
      
      if(input$filename2 == "pdf"){
        #browser()
        pdf(file = paste0(input$filename1, ".",input$filename2))
        cowplot::plot_grid(plotlist = plot_out, nrow = cur_row, ncol = cur_col, labels = cur_markers)
        dev.off()
      } else {
        png(file = paste0(input$filename1, ".",input$filename2))
        cowplot::plot_grid(plotlist = plot_out, nrow = cur_row, ncol = cur_col, labels = cur_markers)
        dev.off()
        }
    }#else to be added for plotCells implementation
  })}



## Advanced controls - Cell outlining

.create_outline_controls <- function(object, mask, input, session, ...){
  renderUI({
    if (input$outline){
        wellPanel(
          selectizeInput("outline_by", label = span("Outline by",style = "color: black; padding-top: 0px"), 
                         choices = NULL, options = NULL, list(placeholder = 'Outline by', maxItems = 1,maxOptions = 10)
          ),
          selectizeInput("select_outline",
                         label = span("Select outline",style = "color: black; padding-top: 0px"),
                         choices = NULL,
                         multiple = TRUE)
        )}})}

.populate_outline_controls <- function(object, input, session){
  observeEvent(input$outline, {
    if (input$outline) {
      updateSelectizeInput(session, inputId = "outline_by",
                           choices = names(colData(object)),
                           server = TRUE,
                           selected = "")
      observeEvent(input$outline_by, { 
        updateSelectizeInput(session, inputId = "select_outline",
                             choices = unique(colData(object)[[input$outline_by]]),
                             server = TRUE,
                             selected = unique(colData(object)[[input$outline_by]])[1])
      })
    }
  })
}

.create_basic_color_outline <- function(object, mask, input, session, ...){
  renderUI({
    if (input$outline && is.null(input$select_outline)){
      wellPanel(
        menuItem(span("Outline color control", style = "color: black;padding-top: 0px"), style = "color: black; padding-top: 0px",
        colourInput(inputId = "basic_color_outline",
                    label = "General outline color",
                    value = "white")
                    ))}})}

.create_advanced_color_outline <- function(object, mask, input, session, ...){
  renderUI({
  if(input$outline && !is.null(input$select_outline)){
    wellPanel(
     menuItem(span("Outline color control", style = "color: black;padding-top: 0px"), style = "color: black; padding-top: 0px",
     lapply(seq_along(input$select_outline), function (i){
        colourInput(inputId = paste0("color_outline",i),
                    label = input$select_outline[i],
                    value = brewer.pal(12, "Paired")[i])
      })))}})}


# .populate_advanced_color_outline <- function(object, mask, input, session, ...){
#   observeEvent(input$select_outline, {
#     for(i in seq_along(input$select_outline)){
#     updateColourInput(session, inputId = paste0(input$select_outline[i],"_color"),
#                       label = input$select_outline[i],
#                       value = brewer.pal(12, "Paired")[i]
#                       )
#   }
# })}


.create_thickness_control <- function(object, mask, input, session, ...){
  renderUI({
  if(input$outline){
    wellPanel(
      menuItem(span("Outline thickness control", style = "color: black;padding-top: 0px"), style = "color: black; padding-top: 0px",
      checkboxInput(inputId = "thick", "Thick", value = FALSE)
      ))}})}



# Tiles functionality 

.add_tiles_tab <- function(input, object, mask,
                           image, img_id, cell_id){
  renderUI({
    #browser()
    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    
    cur_row <- ceiling(length(cur_markers) / 3)

    # Generate separate boxes
    box_list <- lapply(seq_along(cur_markers), function(cur_plot) {

      cur_val <- (cur_plot * 2) - 1

      box(plotOutput(paste0("tile", cur_plot)), #can use svgPanZoomOutput() for zoom-able images?
          title = paste(cur_markers[cur_plot]),
          status = "primary",
          width = 4)
    })
    lapply(seq_len(cur_row), function(cr) {
      cur_val <- (cr * 3) - 2
      fluidRow(box_list[seq.int(cur_val, cur_val + 2)])
    })
    })
    }

# PlotCells functionality 

## Advanced controls - Cell outlining

.create_colorby_controls <- function(object, mask, input, session, ...){
  renderUI({
    if (input$plotcells){
      wellPanel(
        selectizeInput("color_by", label = span("Color by",style = "color: black; padding-top: 0px"), 
                       choices = NULL, options = NULL, list(placeholder = 'Color by', maxItems = 1,maxOptions = 10)
        ),
        selectizeInput("color_by_selection",
                       label = span("Color by selection",style = "color: black; padding-top: 0px"),
                       choices = NULL,
                       multiple = TRUE)
      )}})}

.populate_colorby_controls <- function(object, input, session){
  observeEvent(input$plotcells, {
    if (input$plotcells) {
      updateSelectizeInput(session, inputId = "color_by",
                           choices = names(colData(object)),
                           server = TRUE,
                           selected = "")
      observeEvent(input$color_by_selection, { 
        updateSelectizeInput(session, inputId = "color_by_selection",
                             choices = unique(colData(object)[[input$color_by]]),
                             server = TRUE,
                             selected = unique(colData(object)[[input$color_by]])[1])
      })
      }
  })}

.create_colorby_controls <- function(object, mask, input, session, ...){
  renderUI({
    if (input$plotcells){
      wellPanel(
        selectizeInput("color_by", label = span("Color by",style = "color: black; padding-top: 0px"), 
                       choices = NULL, options = NULL, list(placeholder = 'Color by', maxItems = 1,maxOptions = 10)
        ),
        selectizeInput("color_by_selection",
                       label = span("Select color by",style = "color: black; padding-top: 0px"),
                       choices = NULL,
                       multiple = TRUE)
      )}})}

.populate_colorby_controls <- function(object, input, session){
  observeEvent(input$plotcells, {
    if (input$plotcells) {
      updateSelectizeInput(session, inputId = "color_by",
                           choices = names(colData(object)),
                           server = TRUE,
                           selected = "")
      observeEvent(input$color_by, { 
        updateSelectizeInput(session, inputId = "color_by_selection",
                             choices = unique(colData(object)[[input$color_by]]),
                             server = TRUE,
                             selected = unique(colData(object)[[input$color_by]]))
      })
    }
  })
  }
  