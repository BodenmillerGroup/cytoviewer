# Helper functions to modify the server side of the shiny app

#' @importFrom cytomapper plotCells plotPixels channelNames CytoImageList
#' @importFrom SingleCellExperiment colData colPair colPairNames
#' @importFrom ggplot2 aes ggtitle scale_color_manual scale_color_gradientn scale_shape_manual scale_size_manual theme element_text element_blank scale_y_reverse
#' @importFrom ggraph ggraph create_layout geom_node_point geom_edge_fan0 geom_edge_link0 scale_edge_color_manual scale_edge_width_manual
#' @importFrom tidygraph tbl_graph
#' @importFrom rlang .data
#' @importFrom SpatialExperiment spatialCoords
#' @importFrom viridis viridis
#' @importFrom archive archive_write_files
#' @importFrom colourpicker colourInput
#' @importFrom grDevices dev.off pdf png replayPlot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shinycssloaders withSpinner
#' @importFrom svglite stringSVG
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom svgPanZoomOutput
#' @importFrom utils capture.output
#' @importFrom EBImage gblur
#' @importFrom methods as
#' @importFrom S4Vectors endoapply mcols mcols<- isRedundantHit


# Generate help text
.general_help <- function(){
    tagList(
        h3("Using the Shiny application"),
        p("This help page provides an overview on the main functionality",
        "that this app offers. For each step, user-defined adjustments are 
        possible.", "To see the full documentation and more details, 
        please refer to the help page found at", strong("?cytoviewer")," 
        and to the", strong("package vignette"), "."),
        h3("Interface"),
        p("The cytoviewer interface is divided into a", 
        strong("Header, Sidebar and Body"), "section.", 
        "The Header includes package version information, access to session 
        information, R code and this help page as well as a dropdown-menu for image 
        downloads.", "The Body features a Tabset-Panel layout allowing the user 
        to switch between four modes:", strong("Image-level (Composite and Channels), 
                                               Cell-level (Mask) and Points-level (Graph)")),
        h3("Image-level visualization"),
        p("Image visualization control is split into", 
          em("basic and advanced controls"),".",
        "Basic controls supports the selection of up to six markers/channels
        for image display. Each marker has color control settings that allow
        the user to set contrast, brightness, gamma and select a channel color.
        In the advanced controls part, the user can choose to overlay the
        displayed images with provided segmentation masks. Outline color and
        mask thickness can be adjusted by the user. Moreover, the masks can be
        outlined by cell-specific metadata provided in colData slot of the
        object."), 
        h3("Cell-level visualization"),
        p("Cell visualization has",em("basic controls."),"
        Here, the user can choose to display the provided segmentation masks.
        If an object is provided, the masks can be colored by cell-specific
        metadata."),
        h3("Points-level visualization"),
        p("Point visualization control is split into", 
          em("basic and advanced controls"),".",
          "Basic controls allow coloring, sizing and shaping of individual cell 
          centroids by cell-specific metadata from the colData slot of the object.
          Advanced controls allow the selection of a spatial graph stored in
          the object's colPair slot and control edge appearance (color, width, 
          direction)."),
        h3("General controls"),
        p("General controls is subdivided into an", em("Image/Cell appearance and 
        Image filters"), "part.", "In the Image/Cell appearance section, the user can 
        adjust the scale bar length, set the pixel resolution and include legend/image titles, 
        while the Image filters section allows to control pixel-wise interpolation 
        (default) and apply a Gaussian filter."),
        h3("Image download"),
        p("The cytoviewer package supports fast and uncomplicated image downloads.
        Download controls are part of the", em("Header"), ".", "The user can
        specify a file name, select the image of interest (Composite, Channels,
        Mask, Graph) and the file format (pdf, png). Upon clicking the download button,
        a pop-window should appear where the user can specify the download location.")
    )
}

# Create general observers for header
.create_general_observer <- function(input, si, image, mask, object,
                                     img_id, cell_id, coords){

    # Return session info
    observeEvent(input$SessionInfo, {
        showModal(modalDialog(
            pre(paste(capture.output(si), collapse = "\n")),
            size = "l", fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Session Info"
        ))
    })

    # Return helptext
    observeEvent(input$Help, {
        showModal(modalDialog(
            .general_help(),
            size = "l", fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Help"
        ))
    })

    # Return R code for current visualization
    observeEvent(input$ViewCode, {
        code <- .generate_r_code(input, image, mask, object, img_id, cell_id, coords)
        showModal(modalDialog(
            tagList(
                tags$details(
                    tags$summary(tags$b("Image-level")),
                    tags$pre(style = "margin-top:8px", code$image)
                ),
                if (isTRUE(input$plotcells)) tagList(
                    tags$hr(),
                    tags$details(
                        tags$summary(tags$b("Cell-level")),
                        tags$pre(style = "margin-top:8px", code$cells)
                    )
                ),
                if (isTRUE(input$plotpoints)) tagList(
                    tags$hr(),
                    tags$details(
                        tags$summary(tags$b("Points-level")),
                        tags$pre(style = "margin-top:8px", code$points)
                    )
                )
            ),
            size = "l", fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "R Code"
        ))
    })
}

.generate_r_code <- function(input, image, mask, object, img_id, cell_id, coords){

    cur_sample <- if (!is.null(input$sample) && input$sample != "") input$sample else "<sample>"

    # --- Image-level ---
    all_markers <- c(input$marker1, input$marker2, input$marker3,
                     input$marker4, input$marker5, input$marker6)
    all_views   <- c(isTRUE(input$view1), isTRUE(input$view2), isTRUE(input$view3),
                     isTRUE(input$view4), isTRUE(input$view5), isTRUE(input$view6))
    active_idx  <- which(nchar(all_markers) > 0 & all_views)
    if (length(active_idx) == 0) active_idx <- seq_along(all_markers)
    active_markers <- all_markers[active_idx]
    markers_str <- paste0('c("', paste(active_markers, collapse = '", "'), '")')

    colour_pairs <- vapply(active_idx, function(i) {
        col <- if (!is.null(input[[paste0("color", i)]])) input[[paste0("color", i)]] else "white"
        paste0('"', all_markers[i], '" = c("black", "', col, '")')
    }, character(1))
    colour_img_str <- paste0('list(', paste(colour_pairs, collapse = ", "), ')')

    bcg_parts <- Filter(Negate(is.null), lapply(active_idx, function(i) {
        b  <- if (!is.null(input[[paste0("brightness", i)]])) input[[paste0("brightness", i)]] else 1
        cc <- if (!is.null(input[[paste0("contrast",   i)]])) input[[paste0("contrast",   i)]] else 1
        g  <- if (!is.null(input[[paste0("gamma",      i)]])) input[[paste0("gamma",      i)]] else 1
        if (b == 1 && cc == 1 && g == 1) return(NULL)
        paste0('"', all_markers[i], '" = c(', b, ', ', cc, ', ', g, ')')
    }))
    bcg_str <- if (length(bcg_parts) > 0) {
        paste0('  bcg        = list(', paste(bcg_parts, collapse = ", "), '),\n')
    } else ""

    outline_str <- ""
    if (isTRUE(input$outline)) {
        thick <- if (!is.null(input$thick)) input$thick else 1
        if (!is.null(input$outline_by) && input$outline_by != "") {
            outline_str <- paste0(
                '  outline_by = "', input$outline_by, '",\n',
                '  thick      = ', thick, ',\n')
        } else {
            mc <- if (!is.null(input$basic_color_outline)) input$basic_color_outline else "white"
            outline_str <- paste0(
                '  missing_colour = "', mc, '",\n',
                '  thick          = ', thick, ',\n')
        }
    }

    image_code <- paste0(
        'library(cytomapper)\n\n',
        'plotPixels(\n',
        '  image      = image["', cur_sample, '"],\n',
        '  colour_by  = ', markers_str, ',\n',
        '  colour     = ', colour_img_str, ',\n',
        bcg_str,
        outline_str,
        '  ...\n',
        ')'
    )

    # --- Cell-level ---
    cur_colorby <- if (!is.null(input$color_by) && input$color_by != "") {
        paste0('"', input$color_by, '"')
    } else { "NULL" }

    cell_colour_str <- "NULL"
    if (!is.null(input$color_by) && input$color_by != "" &&
        !is.null(input$color_by_selection) && length(input$color_by_selection) > 0) {
        cell_colour_pairs <- Filter(Negate(is.null), lapply(
            seq_along(input$color_by_selection), function(i) {
                col <- input[[paste0("color_by", i)]]
                if (is.null(col)) return(NULL)
                paste0('"', input$color_by_selection[i], '" = "', col, '"')
            }))
        if (length(cell_colour_pairs) > 0) {
            cell_colour_str <- paste0(
                'list("', input$color_by, '" = c(',
                paste(cell_colour_pairs, collapse = ", "), '))')
        }
    }

    missing_col_cells <- if (!is.null(input$missing_colorby)) {
        paste0('"', input$missing_colorby, '"')
    } else '"white"'

    cells_code <- paste0(
        'library(cytomapper)\n\n',
        'plotCells(\n',
        '  mask           = mask["', cur_sample, '"],\n',
        if (!is.null(object)) '  object         = object,\n' else '',
        '  img_id         = "', if (!is.null(img_id)) img_id else "<img_id>", '",\n',
        '  cell_id        = "', if (!is.null(cell_id)) cell_id else "<cell_id>", '",\n',
        '  colour_by      = ', cur_colorby, ',\n',
        '  colour         = ', cell_colour_str, ',\n',
        '  missing_colour = ', missing_col_cells, ',\n',
        '  ...\n',
        ')'
    )

    # --- Points-level ---
    cur_color_by  <- .select_node_color_by(input)
    cur_color_arg <- if (!is.null(cur_color_by)) {
        paste0('  node_color_by  = "', cur_color_by, '",\n')
    } else {
        paste0('  node_color_fix = "', if (!is.null(input$node_color_fix)) input$node_color_fix else "black", '",\n')
    }
    cur_size_by  <- if (!is.null(input$node_size_by) && input$node_size_by != "") input$node_size_by else NULL
    cur_size_arg <- if (!is.null(cur_size_by)) {
        paste0('  node_size_by   = "', cur_size_by, '",\n')
    } else {
        paste0('  node_size_fix  = ', if (!is.null(input$node_size_fix)) input$node_size_fix else 1.5, ',\n')
    }
    cur_shape_by  <- if (!is.null(input$node_shape_by) && input$node_shape_by != "") input$node_shape_by else NULL
    cur_shape_arg <- if (!is.null(cur_shape_by)) {
        paste0('  node_shape_by  = "', cur_shape_by, '",\n')
    } else {
        paste0('  node_shape_fix = ', if (!is.null(input$node_shape_fix)) as.integer(input$node_shape_fix) else 19L, ',\n')
    }
    cur_graph <- if (!is.null(input$spatial_graph) && input$spatial_graph != "") {
        paste0('"', input$spatial_graph, '"')
    } else { "NULL" }
    cur_directed    <- if (!is.null(input$directed))    input$directed    else FALSE
    cur_nodes_first <- if (!is.null(input$nodes_first)) input$nodes_first else FALSE
    cur_edge_color  <- if (!is.null(input$edge_color_fix)) paste0('"', input$edge_color_fix, '"') else '"black"'
    cur_edge_width  <- if (!is.null(input$edge_width_fix)) input$edge_width_fix else 0.5

    coords_str <- if (!is.null(coords)) {
        paste0('c("', coords[1], '", "', coords[2], '")')
    } else '"<coords>"'

    points_code <- paste0(
        'library(imcRtools)\n\n',
        'plotSpatial(\n',
        '  object         = object[, colData(object)$',
            if (!is.null(img_id)) img_id else "<img_id>", ' == "', cur_sample, '"],\n',
        '  img_id         = "', if (!is.null(img_id)) img_id else "<img_id>", '",\n',
        '  coords         = ', coords_str, ',\n',
        cur_color_arg,
        cur_size_arg,
        cur_shape_arg,
        '  colPairName    = ', cur_graph, ',\n',
        '  draw_edges     = ', tolower(as.character(cur_graph != "NULL")), ',\n',
        '  directed       = ', tolower(as.character(cur_directed)), ',\n',
        '  nodes_first    = ', tolower(as.character(cur_nodes_first)), ',\n',
        '  edge_color_fix = ', cur_edge_color, ',\n',
        '  edge_width_fix = ', cur_edge_width, ',\n',
        '  ...\n',
        ')'
    )

    list(image = image_code, cells = cells_code, points = points_code)
}

# Create interactive observers
.create_interactive_observer <- function(image, mask, input, session){

    # Next Image Observer
    observeEvent(input$next.sample, {
      img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
      cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == length(img_IDs), 1, cur_index + 1)
    
        # return updated img_id 
        updated_sample <- img_IDs[updated_index]
    
        updateSelectizeInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        server = TRUE,
                        selected = updated_sample)
    
        }, ignoreInit = TRUE)    

    # Previous Image Observer
    observeEvent(input$previous.sample, {
      img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
      cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == 1,  length(img_IDs), cur_index - 1)
    
        # return updated img_id
        updated_sample <- img_IDs[updated_index]
    
        updateSelectizeInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        server = TRUE,
                        selected = updated_sample)

    }, ignoreInit = TRUE)    
}

# Create updateSelectizeInput objects
.create_updateSelectizeInput <- function(image, mask, input, session){
  
  img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
  
  # Store image IDs
    updateSelectizeInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        server = TRUE,
                        selected = unique(img_IDs)[1])
    
  # Store marker names 
    markers <- if(!is.null(names(image))) channelNames(image) else c("")
    
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
.select_outline_colors <- function(input, 
                                    object, 
                                    session, 
                                    exprs_marker_update = TRUE){
  cur_entries <- length(unique(colData(object)[[input$outline_by]]))
  if (is.numeric(colData(object)[[input$outline_by]]) && cur_entries > 23L) {
    req(input$numeric_color_outline)
    cur_vec <- viridis(100, option = input$numeric_color_outline)
  }else{
    cur_vec <- lapply(seq_along(input$select_outline), function (i){
      return(input[[paste0("color_outline", i)]])})
    cur_vec <- unlist(cur_vec)
    if(!is.null(cur_vec)){
      req(length(cur_vec) == length(input$select_outline))
      names(cur_vec) <- input$select_outline
    }
  }
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
  legend_param <- list(margin = 3) #use default from cytomapper
  
  if(input$show_legend) { cur_legend <- legend_param 
  } else { cur_legend <- NULL }
  
  return(cur_legend)
}

# Helper function for image title 
.show_title <- function(input){
  imagetitle_param <- list(margin = c(10,2)) #use default from cytomapper
  
  if (input$show_title) {
      cur_imagetitle <- imagetitle_param
  } else {
      cur_imagetitle <- NULL
  }
  return(cur_imagetitle)
}

# Helper function to get image into memory
.get_image <- function(input, image, ...){
  
  req(input$sample != "")
  
  cur_image <- reactive({
    cur_image <- image[input$sample]
    cur_image <- CytoImageList(cur_image, on_disk = FALSE)
    return(cur_image)
    })
  
  return(cur_image())
  
}

# Helper function to apply image filter
.filter_image <- function(input, image, ...){
  
  req(input$sample != "")
  
  if(!is.null(image)){
    cur_image <- .get_image(input, image)
    if(input$gaussian_blur){
      cur_image_fil <- endoapply(cur_image, function(x){
        gblur(x, sigma = input$gaussian_blur_sigma)
      })
      names(cur_image_fil) <- names(cur_image)
      mcols(cur_image_fil) <- mcols(cur_image)
      cur_image <- cur_image_fil
    }
    return(cur_image)
  }
}

# Helper function to get mask into memory
.get_mask <- function(input, mask, img_id, cur_image){
  
  cur_mask <- reactive({
    cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
    cur_mask <- CytoImageList(cur_mask, on_disk = FALSE)
    return(cur_mask)
  })
  
  return(cur_mask())
  
}

#  Helper function to construct image 
.create_image <- function(input, object, mask,
                          image, img_id, cell_id,...){
    
  req(input$sample != "")
  req(!is.null(input$scalebar))
    
  # Marker and color control
    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    
    if(length(cur_markers) > 1){
    validate(
      need(!any(duplicated(cur_markers)), 
           "NOTE: Please only select unique markers.")
    )}
    
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    cur_color <- .select_colors(input)
    cur_color <- cur_color[names(cur_color) != ""]
    
    cur_basic_outline <- input$basic_color_outline
    cur_scale <- .get_scalebar(input)
    cur_resolution <- .get_resolution(input)
    cur_thick <- input$thick
    cur_interpolate <- input$interpolate
    
    cur_image <- .filter_image(input, image)
    
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)
    
    if (input$outline && !is.null(input$outline_by)){
      if (input$outline_by == "") {
        
        req(img_id, cur_markers)
        
        cur_mask <- .get_mask(input, mask, img_id, cur_image)
        
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
                   scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                   interpolate = cur_interpolate,
                   ...)
      
        } else if (input$outline_by != "") { 
        
        req(img_id, cell_id, cur_markers) 
          
        validate(
          need(is.null(dim(colData(object)[[input$outline_by]])), 
               "NOTE: The current [Outline by] choice can not be visualized 
               because it has more than one dimension in 
               colData(object)[[Outline by]].")
        )
        
        cur_entries <- length(unique(colData(object)[[input$outline_by]]))
        if (is.numeric(colData(object)[[input$outline_by]]) && cur_entries > 23L) {
        cur_object <- object
        } else {
          cur_object <- object[,colData(object)[[input$outline_by]] 
                               %in% input$select_outline]
        }
        
        cur_mask <- .get_mask(input, mask, img_id, cur_image)
        cur_advanced_outline <- .select_outline_colors(input, object)
        cur_color[[input$outline_by]] <- cur_advanced_outline

          if (is.logical(colData(object)[[input$outline_by]])) {
            cur_object <- object[,as.numeric(colData(object)[[input$outline_by]]) 
                                 %in% input$select_outline]
            
            req(!is.null(cur_color[[input$outline_by]]))
            req(any(as.numeric(colData(cur_object)[[input$outline_by]]) %in% input$select_outline))
            names(cur_color[[input$outline_by]]) <- as.logical(as.numeric(input$select_outline))
            }

          req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
          req(!identical(unique(colData(cur_object)[,img_id]), character(0)))
          
      validate(
        need(mcols(cur_image)[[img_id]] %in% cur_object[[img_id]], 
             "NOTE: Your [Select outline] choices are not featured 
             in the current image.")
      )
      
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
                   scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                   interpolate = cur_interpolate,
                   ...)
        
    }} else {
      req(length(cur_markers) != 0)
      plotPixels(image = cur_image,
                 colour_by = cur_markers,
                 colour = cur_color,
                 bcg = cur_bcg,
                 legend = cur_legend,
                 image_title = cur_imagetitle,
                 scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                 interpolate = cur_interpolate,
                 ...)   
    }
}

# Visualize marker expression on images
.imagePlot <- function(input, object, mask,
                       image, img_id, cell_id, ...){
    renderSvgPanZoom({
        suppressMessages(svgPanZoom(stringSVG(
            .create_image(input, object, mask,image, img_id, cell_id, ...)
            ),
            zoomScaleSensitivity = 0.4, 
            maxZoom = 20,
            controlIconsEnabled = TRUE, 
            viewBox = FALSE))
    })
}


## Image tiles function
.create_image_tiles <- function(input, object, mask, image, channels,
                                img_id, cell_id, ...){
  req(input$sample != "")
  req(!is.null(input$scalebar))
  
  cur_markers <- .select_markers(input)
  cur_markers <- cur_markers[cur_markers != ""]
  
  if (!is.null(channels)) {
    req(channels$length_output == length(cur_markers))
    }
  
  if(length(cur_markers) > 1){
    validate(
      need(!any(duplicated(cur_markers)), 
           "NOTE: Please only select unique markers.")
    )}
  
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
    cur_scale <- .get_scalebar(input)
    cur_resolution <- .get_resolution(input)
    cur_thick <- input$thick
    cur_interpolate <- input$interpolate
    
    cur_image <- .filter_image(input, image)
    
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)
    
    if (input$outline && !is.null(input$outline_by)){
      if(input$outline_by == "") {
        
        req(img_id, markers)
        
        cur_mask <- .get_mask(input, mask, img_id, cur_image)
        
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
                 scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                 interpolate = cur_interpolate,
                 return_plot = TRUE,
                 ...)
      
    } else if (input$outline_by != "") {
      
      req(img_id, cell_id, markers) 
      
      cur_entries <- length(unique(colData(object)[[input$outline_by]]))
      
      if (is.numeric(colData(object)[[input$outline_by]]) && cur_entries > 23L) {
        cur_object <- object
      } else {
        cur_object <- object[,colData(object)[[input$outline_by]] 
                             %in% input$select_outline]
      }
      
      cur_mask <- .get_mask(input, mask, img_id, cur_image)
      cur_advanced_outline <- .select_outline_colors(input, object)
      cur_color[[input$outline_by]] <- cur_advanced_outline
      
      if (is.logical(colData(object)[[input$outline_by]])) {
        cur_object <- object[,as.numeric(colData(object)[[input$outline_by]]) 
                             %in% input$select_outline]
        
        req(!is.null(cur_color[[input$outline_by]]))
        req(any(as.numeric(colData(object)[[input$outline_by]]) %in% input$select_outline))
        names(cur_color[[input$outline_by]]) <- as.logical(as.numeric(input$select_outline))
      }
      
      req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
      req(!identical(unique(colData(cur_object)[,img_id]), character(0)))
      
      validate(
        need(mcols(cur_image)[[img_id]] %in% cur_object[[img_id]], 
             "NOTE: Your [Select outline] choices are not featured 
             in the current image.")
      )
      
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
                 scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                 interpolate = cur_interpolate,
                 return_plot = TRUE,
                 ...)
      
    }} else {
      req(length(cur_markers) != 0)
      plot_list[[i]] <- plotPixels(image = cur_image,
                 colour_by = markers,
                 colour = cur_color,
                 bcg = cur_bcg,
                 legend = cur_legend,
                 image_title = cur_imagetitle,
                 scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
                 interpolate = cur_interpolate,
                 return_plot = TRUE,
                 ...)   
    }
    })
  
  return(plot_list)
}


# Download the images - via downloadHandler
.downloadSelection <- function(input, object, mask,
                               image, img_id, cell_id, ...){
    downloadHandler(
    filename = function(){
      if(input$fileselection %in% c("Composite","Mask","Graph")){
        paste0(input$filename1, ".",input$filename2)
      } else {
        paste0(input$filename1,".zip")
      }},
    content = function(file){
      if(input$fileselection == "Composite"){
        if(input$filename2 == "pdf"){
          pdf(file = file)
          .create_image(input, object, mask,
                        image, img_id, cell_id)
          dev.off()
          } else {
            png(filename = file)
            .create_image(input, object, mask,
                          image, img_id, cell_id)
            dev.off()
          }
        } else if(input$fileselection == "Mask"){
          if(input$filename2 == "pdf"){
            pdf(file = file)
            .create_cells(input, object, mask, image, img_id, cell_id)
            dev.off()
          } else {
            png(filename = file)
            .create_cells(input, object, mask, image, img_id, cell_id)
            dev.off()
          }
        } else if (input$fileselection == "Graph") {
          if (input$filename2 == "pdf") {
            pdf(file = file)
            print(.create_graph(input, image, mask, object, img_id, ...))
            dev.off()
          } else {
            png(filename = file)
            print(.create_graph(input, image, mask, object, img_id, ...))
            dev.off()
          }
        } else {
          cur_markers <- .select_markers(input)
          cur_markers <- cur_markers[cur_markers != ""]
          plot_list <- .create_image_tiles(input, object, mask, image, 
                                           channels = NULL, img_id, cell_id)
          
          # save files into temporary directory
          twd <- setwd(tempdir())
          on.exit(setwd(twd))
          
          files <- NULL 
          
          if(input$filename2 == "pdf"){
            for(i in seq_along(cur_markers)){
            filename <- paste0(input$filename1,"_",cur_markers[i],".pdf")
            
            pdf(file = filename)
            replayPlot(plot_list[[i]]$plot)
            dev.off()
            
            files <- c(files, filename)
            }
            
          } else {
             for(i in seq_along(cur_markers)){
              filename <- paste0(input$filename1,"_",cur_markers[i],".png")
              
              png(filename = filename)
              replayPlot(plot_list[[i]]$plot)
              dev.off()
              
              files <- c(files, filename)
             }
            }
          
          #create archive from written files (here zip)
          archive_write_files(file, files)
          }
      })
    } 


## Advanced controls - Cell outlining
.create_outline_controls <- function(object, mask, input, session, ...){
  renderUI({
    if (input$outline){
        wellPanel(
          selectizeInput("outline_by", label = span("Outline by",
                                    style = "color: black; padding-top: 0px"), 
                         choices = NULL, options = NULL, 
                list(placeholder = 'Outline by', maxItems = 1,maxOptions = 10)
          ),
          selectizeInput("select_outline",
                         label = span("Select outline",
                                      style = "color: black; padding-top: 0px"),
                         choices = NULL,
                         multiple = TRUE)
        )}})}


.populate_outline_controls <- function(object, input, session){
  observeEvent(input$outline, {
    
    if (input$outline && is.null(object)) {
      updateSelectizeInput(session, inputId = "outline_by",
                           choices = c(""),
                           server = TRUE,
                           selected = "")
    }
    
    if (input$outline && !is.null(object)) {
      updateSelectizeInput(session, inputId = "outline_by",
                           choices = names(colData(object)),
                           server = TRUE,
                           selected = "")
      observeEvent(input$outline_by, { 
        validate(
          need(is.null(dim(colData(object)[[input$outline_by]])), 
               "NOTE: The current [Outline by] choice can not be visualized 
               because it has more than one dimension in 
               colData(object)[[Outline by]].")
        )
        
        cur_entries <- length(unique(colData(object)[[input$outline_by]]))
        if(is.numeric(colData(object)[[input$outline_by]]) && cur_entries > 23L){
          updateSelectizeInput(session, inputId = "select_outline",
                               choices = input$outline_by,
                               server = TRUE,
                               selected = input$outline_by) 
        }else{
          updateSelectizeInput(session, inputId = "select_outline",
                               choices = unique(colData(object)[[input$outline_by]]),
                               server = TRUE,
                               selected = unique(colData(object)[[input$outline_by]][1])) 
          
        }
      })
    }
  })
}

.create_basic_color_outline <- function(object, mask, input, session, ...){
  renderUI({
    if (input$outline && is.null(input$select_outline)){
      wellPanel(
        menuItem(span("Outline color control", 
                      style = "color: black;padding-top: 0px"), 
                 style = "color: black; padding-top: 0px",
        colourInput(inputId = "basic_color_outline",
                    label = "General outline color",
                    value = "white")
                    ))}})}

.create_advanced_color_outline <- function(object, mask, input, session, ...){
  renderUI({
  if(input$outline && !is.null(input$select_outline)){
    cur_entries <- length(unique(colData(object)[[input$outline_by]]))
    wellPanel(
      if(is.numeric(colData(object)[[input$outline_by]]) && cur_entries > 23L){ 
        menuItem(span("Outline color control", 
                      style = "color: black;padding-top: 0px"), 
                 style = "color: black; padding-top: 0px",
                 radioButtons(inputId = "numeric_color_outline", 
                              label = "Color palettes",
                              choices = list("viridis","inferno","plasma"), 
                              selected = "viridis"))
      }else{
        menuItem(span("Outline color control", 
                      style = "color: black;padding-top: 0px"), 
                 style = "color: black; padding-top: 0px",
                 lapply(seq_along(input$select_outline), function (i){
                   cur_col <- c(brewer.pal(9, "Set1"),
                                brewer.pal(8, "Pastel2"),
                                brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                   colourInput(inputId = paste0("color_outline",i),
                               label = if (is.logical(colData(object)[[input$outline_by]])) {
                                 req(any(as.numeric(colData(object)[[input$outline_by]]) %in% input$select_outline))
                                 as.logical(as.numeric(input$select_outline[i]))
                                 } else { input$select_outline[i] },
                               value = cur_col[i])
                 }))
      }
      )}})}


.create_thickness_control <- function(input, session, ...){
  renderUI({
  if(input$outline){
    wellPanel(
      menuItem(span("Outline thickness control", 
                    style = "color: black;padding-top: 0px"), 
               style = "color: black; padding-top: 0px",
      checkboxInput(inputId = "thick", "Thick", value = FALSE)
      ))}})}



# Tiles functionality 
.add_tiles_tab <- function(input, object, mask,
                           image, img_id, cell_id){
  renderUI({
    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    
    req(length(cur_markers) == length(unique(cur_markers)))
    
    cur_row <- ceiling(length(cur_markers) / 3)
    
    # Generate separate boxes
    box_list <- lapply(seq_along(cur_markers), function(cur_plot) {
      cur_val <- (cur_plot * 2) - 1

      box(withSpinner(plotOutput(paste0("tile", cur_plot)), type = 6),
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
        selectizeInput("color_by", label = span("Color by",
                                    style = "color: black; padding-top: 0px"), 
                       choices = NULL, options = NULL, 
                     list(placeholder = 'Color by', maxItems = 1,
                          maxOptions = 10)
        ),
        selectizeInput("color_by_selection",
                       label = span("Select color by",
                            style = "color: black; padding-top: 0px"),
                       choices = NULL,
                       multiple = TRUE)
      )}})}

.populate_colorby_controls <- function(object, input, session){
  observeEvent(input$plotcells, {
    
    if (input$plotcells && is.null(object)) {
      updateSelectizeInput(session, inputId = "color_by",
                           choices = c(""),
                           server = TRUE,
                           selected = "")
    }
    
    if (input$plotcells && !is.null(object)) {
      updateSelectizeInput(session, inputId = "color_by",
                           choices = names(colData(object)),
                           server = TRUE,
                           selected = "")
      observeEvent(input$color_by, { 
        
        validate(
          need(is.null(dim(colData(object)[[input$color_by]])), 
               "NOTE: The current [Color by] choice can not be visualized 
               because it has more than one dimension in 
               colData(object)[[Color by]].")
        )
        
        cur_entries <- length(unique(colData(object)[[input$color_by]]))
        if(is.numeric(colData(object)[[input$color_by]]) && cur_entries > 23L){
          updateSelectizeInput(session, inputId = "color_by_selection",
                               choices = input$color_by,
                               server = TRUE,
                               selected = input$color_by)
        }else{
        updateSelectizeInput(session, inputId = "color_by_selection",
                             choices = unique(colData(object)[[input$color_by]]),
                             server = TRUE,
                             selected = unique(colData(object)[[input$color_by]][1]))
      }})
    }
  })
}

.create_colorby_color <- function(object, mask, input, session, ...){
  renderUI({
    if(input$plotcells && is.null(input$color_by_selection)){
      wellPanel(
        menuItem(span("Color control", 
                      style = "color: black;padding-top: 0px"), 
                 style = "color: black; padding-top: 0px",
                 colourInput(inputId = "missing_colorby", 
                             label = "Missing color",
                             value = "gray")))}
    else if(input$plotcells && !is.null(input$color_by_selection)){
      cur_entries <- length(unique(colData(object)[[input$color_by]]))
      wellPanel(
        if(is.numeric(colData(object)[[input$color_by]]) && cur_entries > 23L){
          menuItem(span("Color control", 
                        style = "color: black;padding-top: 0px"), 
                   style = "color: black; padding-top: 0px",
                   radioButtons(inputId = "numeric_colorby", 
                                label = "Color palettes",
                                choices = list("viridis","inferno","plasma"), 
                                selected = "viridis"))
        }else{
          menuItem(span("Color control", 
                        style = "color: black;padding-top: 0px"), 
                   style = "color: black; padding-top: 0px",
                   lapply(seq_along(input$color_by_selection), function (i){
                     cur_col <- c(brewer.pal(9, "Set1"),
                                  brewer.pal(8, "Pastel2"),
                                  brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                     colourInput(inputId = paste0("color_by",i),
                                 label = if (is.logical(colData(object)[[input$color_by]])) {
                                   req(any(as.numeric(colData(object)[[input$color_by]]) 
                                           %in% input$color_by_selection))
                                   as.logical(as.numeric(input$color_by_selection[i]))
                                 } else { input$color_by_selection[i] },
                                 value = cur_col[i])}),
                   colourInput(inputId = "missing_colorby", 
                               label = "Missing color",
                               value = "gray"))
        }
      )}})}


# Helper function to retrieve color by colors
.select_colorby_color <- function(input, object, session, 
                                  exprs_marker_update = TRUE){
  
  cur_list <- list()
  
  if (input$color_by != "" && !is.null(input$color_by_selection)) {
    
    cur_entries <- length(unique(colData(object)[[input$color_by]]))
    
    if (is.numeric(colData(object)[[input$color_by]]) && cur_entries > 23L) {
      req(input$numeric_colorby)
      cur_list[[input$color_by]] <- viridis(100, option = input$numeric_colorby)
      } else {
        cur_vec <- lapply(seq_along(input$color_by_selection), function (i){
        return(input[[paste0("color_by", i)]])})
        cur_vec <- unlist(cur_vec)
      if(!is.null(cur_vec)){
        req(length(cur_vec) == length(input$color_by_selection))
        names(cur_vec) <- input$color_by_selection
        cur_list[[input$color_by]] <- cur_vec
      } else { cur_list <- NULL }
      }} else {
    cur_list <- NULL
    }
  
  return(cur_list)
  
}

# Helper function to retrieve color_by
.select_colorby <- function(input){
  
  if (input$color_by != "") {
    cur_colorby <- input$color_by
  } else { 
    cur_colorby <- NULL 
    }
  
  return(cur_colorby)
}

# Helper function to subset object 
.subset_object <- function(input, object){
  if (!is.null(object)) {
  cur_entries <- length(unique(colData(object)[[input$color_by]]))
  if (input$color_by != "" && !is.numeric(colData(object)[[input$color_by]]) 
     && cur_entries <= 23L) {
    req(input$color_by_selection)
    object <- object[, colData(object)[[input$color_by]] %in% 
                       input$color_by_selection]
  }} else {
    object <- object
    }
  return(object)
}

#  Helper function to construct image 

.create_cells <- function(input, object, mask,
                          image, img_id, cell_id, ...){
  
  req(img_id)

  cur_scale <- .get_scalebar(input)
  cur_resolution <- .get_resolution(input)
  cur_legend <- .show_legend(input)
  cur_imagetitle <- .show_title(input)
  cur_missingcolor <- input$missing_colorby
  
  if(!is.null(object)){
  validate(
    need(is.null(dim(colData(object)[[input$color_by]])), 
         "NOTE: The current [Color by] choice can not be visualized 
               because it has more than one dimension in 
               colData(object)[[Color by]]."))
    }
  
  cur_colorby <- .select_colorby(input)
  cur_color <- .select_colorby_color(input, object)
  cur_object <- .subset_object(input, object)
  
  if(!is.null(image)){
    cur_image <- image[input$sample]
    cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
  }else{
    cur_mask <- mask[input$sample]
  }
  
  if(!is.null(object)){
    
  cur_object <- cur_object[, colData(cur_object)[[img_id]] %in% mcols(cur_mask)[,img_id]]

  if (is.logical(colData(object)[[input$color_by]])) {
    cur_object <- object[,as.numeric(colData(object)[[input$color_by]]) 
                         %in% input$color_by_selection]
    req(!is.null(cur_color[[input$color_by]]))
    req(any(as.numeric(colData(object)[[input$color_by]]) %in% input$color_by_selection))
    names(cur_color[[input$color_by]]) <- as.logical(as.numeric(input$color_by_selection))
    
    validate(
      need(input$color_by_selection %in% as.numeric(colData(cur_object)[[input$color_by]]), 
           "NOTE: Your [Select color by] choices are not featured 
             in the current image."))                                                 
  }  
  
  validate(
    need(mcols(cur_mask)[[img_id]] %in% cur_object[[img_id]], 
         "NOTE: Your [Select color by] choices are not featured 
             in the current image.")
  )
  
  cur_entries <- length(unique(colData(object)[[input$color_by]]))
  
  if (input$color_by != ""){
    if (is.numeric(colData(object)[[input$color_by]]) && cur_entries > 23L) {
      cur_object <- cur_object
    } else if (is.logical(colData(object)[[input$color_by]])) { 
      cur_object <- cur_object 
    } else {
      cur_object <- cur_object[,colData(cur_object)[[input$color_by]] 
                         %in% input$color_by_selection]
      
      validate(
        need(input$color_by_selection %in% colData(cur_object)[[input$color_by]], 
             "NOTE: Your [Select color by] choices are not featured 
             in the current image."))
      
    }
  }
  
  req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
  req(!identical(unique(colData(cur_object)[,img_id]), character(0)))
  
  } else {
    cell_id <- "placeholder"
  }
  
  req(cell_id)
  
  plotCells(mask = cur_mask,
            img_id = img_id,
            object = cur_object,
            cell_id = cell_id,
            colour_by = cur_colorby,
            colour = cur_color,
            missing_colour = cur_missingcolor, 
            legend = cur_legend,
            image_title = cur_imagetitle,
            scale_bar = list(length = cur_scale, label = cur_scale*cur_resolution),
            ...)
    
}

# Visualize plotCells
.cellsPlot <- function(input, object, mask,
                       image, img_id, cell_id, ...){
  renderSvgPanZoom({
    
    suppressMessages(svgPanZoom(stringSVG(
      .create_cells(input, object, mask, image, img_id, cell_id, ...)
    ),
    zoomScaleSensitivity = 0.4, 
    maxZoom = 20,
    controlIconsEnabled = TRUE, 
    viewBox = FALSE))
  })
}

## Add plotCells tab
.add_cells_tab <- function(input, object, mask,
                           image, img_id, cell_id){
  renderUI({
    if(input$plotcells){
    box(withSpinner(svgPanZoomOutput("cellsPlot", width = "100%",height = "75vh"),type = 6), 
          title = NULL, 
          id = "expression",
          status = "primary",
          width = 12)
    }
    })
  }


# Add scalebar tab
.add_scalebar <- function(input, object, mask,
                           image, img_id, cell_id){
  renderUI({
    if(!is.null(image)){
    cur_image <- .filter_image(input, image)
    cur_value <- round(dim(cur_image[[1]])[1]/4, digits=-1)
    }else{ 
      cur_value <- round(dim(mask[[1]])[1]/4, digits=-1)
      }
    
    numericInput(inputId = "scalebar", label = "Scale bar length [Pixels]", 
                 value = cur_value, min = 0, max = 1000, step = 5)
  })
}

.get_scalebar <- function(input){
  cur_scale <- input$scalebar
  
  validate(
    need(!is.na(cur_scale) && cur_scale > 0, "NOTE: Please specify a [Scale bar length [Pixels]] value."),
  )
  
  return(cur_scale)
}

# Add resolution tab
.add_resolution <- function(input){
  renderUI({
    numericInput(inputId = "resolution", label = "Pixel resolution [um]", 
                 value = 1, min = 0, max = 100, step = 1)
  })
}

.get_resolution <- function(input){
  cur_resolution <- input$resolution
  
  validate(
    need(!is.na(cur_resolution) && cur_resolution > 0, "NOTE: Please specify a [Pixel resolution [um]] value."),
    )
  
  return(cur_resolution)
}





# plotSpatial functionality 

## Add plotSpatial tab

.add_graph_tab <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if(input$plotpoints){
      box(withSpinner(
        svgPanZoomOutput("graphPlot", width = "100%", height = "75vh"), type = 6),
          title = NULL,
          id = "graph",
          status = "primary",
          width = 12)
    }
  })
}

## Visualize graphs
.graphPlot <- function(input, image, mask, object, img_id, ...){
  renderSvgPanZoom({
    suppressMessages(svgPanZoom(
      stringSVG(print(.create_graph(input, image, mask, object, img_id, ...))),
      zoomScaleSensitivity = 0.4,
      maxZoom = 20,
      controlIconsEnabled = TRUE,
      viewBox = FALSE))
  })
}

## Internal plotSpatial implementation (mimics imcRtools function)
.makeNodes_cytoviewer <- function(object, img_id, node_color_by,
                                   node_shape_by, node_size_by) {
    cols  <- unique(c(img_id, node_color_by, node_shape_by, node_size_by))
    nodes <- colData(object)[, cols, drop = FALSE]
    if (!is.null(node_shape_by))
        nodes[, node_shape_by] <- as.character(nodes[, node_shape_by])
    nodes
}

.generateGraph_cytoviewer <- function(object, nodes, colPairName,
                                       draw_edges, directed) {
    if (draw_edges) {
        cur_SH <- colPair(object, colPairName)
        if (!directed) cur_SH <- cur_SH[!isRedundantHit(cur_SH)]
        edges <- as.data.frame(as(cur_SH, "DataFrame"))
        tbl_graph(nodes = as.data.frame(nodes), edges = edges,
                  directed = directed)
    } else {
        tbl_graph(nodes = as.data.frame(nodes), directed = directed)
    }
}

.generatePlot_cytoviewer <- function(layout, draw_edges, directed,
                                      node_color_by, node_size_by, node_shape_by,
                                      node_color_fix, node_size_fix, node_shape_fix,
                                      edge_color_fix, edge_width_fix, nodes_first) {
    node_mapping <- aes(colour = .data[[node_color_by]],
                        size   = .data[[node_size_by]],
                        shape  = .data[[node_shape_by]])
    if (is.null(node_color_by))   node_mapping$colour <- NULL
    if (is.null(node_size_by))    node_mapping$size   <- NULL
    if (is.null(node_shape_by))   node_mapping$shape  <- NULL
    if (!is.null(node_color_fix)) node_mapping$colour <- as.character(node_color_fix)
    if (!is.null(node_size_fix))  node_mapping$size   <- as.character(node_size_fix)
    if (!is.null(node_shape_fix)) node_mapping$shape  <- as.character(node_shape_fix)

    if (draw_edges) {
        edge_mapping <- aes()
        if (!is.null(edge_color_fix))
            edge_mapping$edge_colour <- as.character(edge_color_fix)
        if (!is.null(edge_width_fix))
            edge_mapping$edge_width  <- as.character(edge_width_fix)

        cur_geom_edge <- if (directed) geom_edge_fan0(edge_mapping) else
                                       geom_edge_link0(edge_mapping)

        if (nodes_first) ggraph(layout) + geom_node_point(node_mapping) + cur_geom_edge
        else             ggraph(layout) + cur_geom_edge + geom_node_point(node_mapping)
    } else {
        ggraph(layout) + geom_node_point(node_mapping)
    }
}

.postProcessPlot_cytoviewer <- function(p, node_color_fix, node_shape_fix,
                                         node_size_fix, edge_color_fix,
                                         edge_width_fix) {
    if (!is.null(node_color_fix)) {
        names(node_color_fix) <- as.character(node_color_fix)
        p <- p + scale_color_manual(values = node_color_fix, guide = "none")
    }
    if (!is.null(node_shape_fix)) {
        names(node_shape_fix) <- as.character(node_shape_fix)
        p <- p + scale_shape_manual(values = node_shape_fix, guide = "none")
    }
    if (!is.null(node_size_fix)) {
        names(node_size_fix) <- as.character(node_size_fix)
        p <- p + scale_size_manual(values = node_size_fix, guide = "none")
    }
    if (!is.null(edge_color_fix)) {
        names(edge_color_fix) <- as.character(edge_color_fix)
        p <- p + scale_edge_color_manual(values = edge_color_fix, guide = "none")
    }
    if (!is.null(edge_width_fix)) {
        names(edge_width_fix) <- as.character(edge_width_fix)
        p <- p + scale_edge_width_manual(values = edge_width_fix, guide = "none")
    }
    p + theme(axis.text = element_text(), panel.background = element_blank()) +
        scale_y_reverse()
}

.plotSpatial_cytoviewer <- function(object, img_id, coords,
                                    node_color_by  = NULL,
                                    node_shape_by  = NULL,
                                    node_size_by   = NULL,
                                    node_color_fix = NULL,
                                    node_shape_fix = NULL,
                                    node_size_fix  = NULL,
                                    draw_edges     = FALSE,
                                    directed       = TRUE,
                                    edge_color_fix = NULL,
                                    edge_width_fix = NULL,
                                    colPairName    = NULL,
                                    nodes_first    = TRUE) {

    nodes     <- .makeNodes_cytoviewer(object, img_id, node_color_by,
                                        node_shape_by, node_size_by)
    cur_graph <- .generateGraph_cytoviewer(object, nodes, colPairName,
                                            draw_edges, directed)

    if (is(object, "SpatialExperiment")) {
        layout <- create_layout(cur_graph, layout = "manual",
                                x = spatialCoords(object)[, coords[1]],
                                y = spatialCoords(object)[, coords[2]])
    } else {
        layout <- create_layout(cur_graph, layout = "manual",
                                x = colData(object)[[coords[1]]],
                                y = colData(object)[[coords[2]]])
    }

    p <- .generatePlot_cytoviewer(layout, draw_edges, directed,
                                   node_color_by, node_size_by, node_shape_by,
                                   node_color_fix, node_size_fix, node_shape_fix,
                                   edge_color_fix, edge_width_fix, nodes_first)

    .postProcessPlot_cytoviewer(p, node_color_fix, node_shape_fix,
                                 node_size_fix, edge_color_fix, edge_width_fix)
}


## Create graph plot
.create_graph <- function(input, image, mask, object, img_id, ...){

  dots <- list(...)
  cur_coords <- dots$coords

  req(img_id, !is.null(object))
  req(!is.null(input$sample), input$sample != "")
  if (!is.null(image)) {
    req(input$sample %in% names(image))
  } else {
    req(input$sample %in% names(mask))
  }

  if (!is.null(image)) {
    cur_image <- image[input$sample]
    cur_mask  <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
  } else {
    cur_mask <- mask[input$sample]
  }
  cur_object <- object[, colData(object)[[img_id]] %in% mcols(cur_mask)[, img_id]]

  cur_node_color_by <- .select_node_color_by(input)
  if (!is.null(cur_node_color_by)) {
    validate(
      need(is.null(dim(colData(object)[[cur_node_color_by]])),
           "NOTE: The current [Node color by] choice can not be visualized
           because it has more than one dimension in
           colData(object)[[Node color by]].")
    )
  }

  validate(
    need(mcols(cur_mask)[[img_id]] %in% cur_object[[img_id]],
         "NOTE: Your [Node color by] choices are not featured
         in the current image.")
  )

  if (!is.null(cur_node_color_by) && !is.null(input$node_color_by_selection)) {
    cur_entries <- length(unique(colData(object)[[cur_node_color_by]]))
    if (!is.numeric(colData(object)[[cur_node_color_by]]) || cur_entries <= 23L) {
      if (is.logical(colData(object)[[cur_node_color_by]])) {
        cur_object <- cur_object[, as.numeric(colData(cur_object)[[cur_node_color_by]]) %in%
                                   input$node_color_by_selection]
        req(any(as.numeric(colData(cur_object)[[cur_node_color_by]]) %in%
                  input$node_color_by_selection))
      } else {
        cur_object <- cur_object[, colData(cur_object)[[cur_node_color_by]] %in%
                                   input$node_color_by_selection]
        validate(
          need(input$node_color_by_selection %in% colData(cur_object)[[cur_node_color_by]],
               "NOTE: Your [Node color by] choices are not featured
               in the current image.")
        )
      }
    }
  }

  req(!identical(unique(colData(cur_object)[, img_id]), integer(0)))
  req(!identical(unique(colData(cur_object)[, img_id]), character(0)))

  if (!is.null(input$spatial_graph) && input$spatial_graph != "") {
    cur_graph       <- input$spatial_graph
    cur_edges       <- TRUE
    cur_directed    <- if (!is.null(input$directed))    input$directed    else FALSE
    cur_nodes_first <- if (!is.null(input$nodes_first)) input$nodes_first else FALSE
    cur_edge_color_fix <- if (!is.null(input$edge_color_fix)) input$edge_color_fix else "black"
    cur_edge_width_fix <- if (!is.null(input$edge_width_fix)) input$edge_width_fix else 0.5
  } else {
    cur_graph <- cur_edge_color_fix <- cur_edge_width_fix <- NULL
    cur_edges <- cur_nodes_first <- cur_directed <- FALSE
  }

  if (!is.null(cur_node_color_by) &&
      (is.null(input$node_color_by_selection) || length(input$node_color_by_selection) == 0)) {
    cur_node_color_by <- NULL
  }
  cur_node_color_fix <- if (is.null(cur_node_color_by)) {
    if (!is.null(input$node_color_fix)) input$node_color_fix else "black"
  } else NULL
  cur_node_colors <- if (!is.null(cur_node_color_by)) .select_node_color(input, object) else NULL

  cur_node_size_by  <- if (!is.null(input$node_size_by) && input$node_size_by != "") input$node_size_by else NULL
  cur_node_size_fix <- if (is.null(cur_node_size_by)) {
    if (!is.null(input$node_size_fix)) input$node_size_fix else 1.5
  } else NULL

  cur_node_shape_by  <- if (!is.null(input$node_shape_by) && input$node_shape_by != "") input$node_shape_by else NULL
  if (!is.null(cur_node_shape_by) && !is.null(input$node_shape_by_selection)) {
    validate(
      need(length(input$node_shape_by_selection) <= 6L,
           "NOTE: Your [Node shape by] selection has more than 6 entries.
           Please deselect entries to use shape mapping.")
    )
  }
  if (!is.null(cur_node_shape_by) && !is.null(input$node_shape_by_selection)) {
    if (is.logical(colData(object)[[cur_node_shape_by]])) {
      cur_object <- cur_object[, as.numeric(colData(cur_object)[[cur_node_shape_by]]) %in%
                                 input$node_shape_by_selection]
      req(any(as.numeric(colData(cur_object)[[cur_node_shape_by]]) %in%
                input$node_shape_by_selection))
    } else {
      cur_object <- cur_object[, colData(cur_object)[[cur_node_shape_by]] %in%
                                 input$node_shape_by_selection]
      validate(
        need(input$node_shape_by_selection %in% colData(cur_object)[[cur_node_shape_by]],
             "NOTE: Your [Node shape by] choices are not featured in the current image.")
      )
    }
  }
  if (!is.null(cur_node_shape_by) &&
      (is.null(input$node_shape_by_selection) || length(input$node_shape_by_selection) == 0)) {
    cur_node_shape_by <- NULL
  }
  cur_node_shapes    <- if (!is.null(cur_node_shape_by)) .select_node_shape(input, object) else NULL
  cur_node_shape_fix <- if (is.null(cur_node_shape_by)) {
    if (!is.null(input$node_shape_fix)) as.integer(input$node_shape_fix) else NULL
  } else NULL

  p <- .plotSpatial_cytoviewer(cur_object,
                              img_id         = img_id,
                              coords         = cur_coords,
                              colPairName    = cur_graph,
                              draw_edges     = cur_edges,
                              directed       = cur_directed,
                              nodes_first    = cur_nodes_first,
                              node_color_by  = cur_node_color_by,
                              node_color_fix = cur_node_color_fix,
                              node_size_by   = cur_node_size_by,
                              node_size_fix  = cur_node_size_fix,
                              node_shape_by  = cur_node_shape_by,
                              node_shape_fix = cur_node_shape_fix,
                              edge_color_fix = cur_edge_color_fix,
                              edge_width_fix = cur_edge_width_fix) +
    ggtitle("")

  if (!is.null(cur_node_color_by) && !is.null(cur_node_colors)) {
    cur_entries <- length(unique(colData(object)[[cur_node_color_by]]))
    if (is.numeric(colData(object)[[cur_node_color_by]]) && cur_entries > 23L) {
      p <- p + scale_color_gradientn(colors = cur_node_colors)
    } else {
      p <- p + scale_color_manual(values = cur_node_colors)
    }
  }

  if (!is.null(cur_node_shape_by) && !is.null(cur_node_shapes)) {
    p <- p + scale_shape_manual(values = cur_node_shapes)
  }

  p
}

.select_node_color_by <- function(input){
  if (!is.null(input$node_color_by) && input$node_color_by != "") {
    input$node_color_by
  } else {
    NULL
  }
}

.select_node_color <- function(input, object){
  cur_color_by <- .select_node_color_by(input)
  if (is.null(cur_color_by) || is.null(input$node_color_by_selection)) return(NULL)

  cur_entries <- length(unique(colData(object)[[cur_color_by]]))

  if (is.numeric(colData(object)[[cur_color_by]]) && cur_entries > 23L) {
    req(input$numeric_node_color)
    return(viridis(100, option = input$numeric_node_color))
  }

  cur_vec <- vapply(seq_along(input$node_color_by_selection), function(i) {
    val <- input[[paste0("node_color_advanced", i)]]
    if (is.null(val)) "" else val
  }, character(1))

  if (!any(cur_vec == "")) {
    req(length(cur_vec) == length(input$node_color_by_selection))
    names(cur_vec) <- if (is.logical(colData(object)[[cur_color_by]])) {
      as.logical(as.numeric(input$node_color_by_selection))
    } else {
      input$node_color_by_selection
    }
    return(cur_vec)
  }
  NULL
}

.select_node_shape <- function(input, object){
  if (is.null(input$node_shape_by_selection)) return(NULL)
  cur_shape_by <- if (!is.null(input$node_shape_by) && input$node_shape_by != "") input$node_shape_by else NULL
  cur_vec <- vapply(seq_along(input$node_shape_by_selection), function(i) {
    val <- input[[paste0("node_shape_advanced", i)]]
    if (is.null(val)) return(NA_integer_)
    as.integer(val)
  }, integer(1))
  if (any(is.na(cur_vec))) return(NULL)
  names(cur_vec) <- if (!is.null(cur_shape_by) && is.logical(colData(object)[[cur_shape_by]])) {
    as.logical(as.numeric(input$node_shape_by_selection))
  } else {
    as.character(input$node_shape_by_selection)
  }
  cur_vec
}


.create_node_color_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints){
      wellPanel(
        menuItem(span("Node color control", 
                      style = "color: black;padding-top: 0px"), 
                 style = "color: black; padding-top: 0px",
        selectizeInput("node_color_by", label = span("Color by",
                                                     style = "color: black; padding-top: 0px"), 
                       choices = NULL, options = NULL, 
                       list(placeholder = 'Color by', maxItems = 1,
                            maxOptions = 10)
        ),
        selectizeInput("node_color_by_selection",
                       label = span("Select color by",
                                    style = "color: black; padding-top: 0px"),
                       choices = NULL,
                       multiple = TRUE),
        uiOutput("basic_node_color_controls"),
        uiOutput("advanced_node_color_controls"))
      )}})}


.create_basic_node_color <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints && is.null(input$node_color_by_selection)){
      wellPanel(colourInput(inputId = "node_color_fix",
                             label = "Basic color",
                             value = "black"),
                class = "wellpanel_node"
        )}})}

.create_advanced_node_color <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if(input$plotpoints && !is.null(input$node_color_by_selection)){
      cur_entries <- length(unique(colData(object)[[input$node_color_by]]))
      wellPanel(
        if(is.numeric(colData(object)[[input$node_color_by]]) && cur_entries > 23L){
                   radioButtons(inputId = "numeric_node_color",
                                label = "Color palettes",
                                choices = list("viridis","inferno","plasma"),
                                selected = "viridis")
        }else{
          lapply(seq_along(input$node_color_by_selection), function (i){
            cur_col <- c(brewer.pal(9, "Set1"),
                         brewer.pal(8, "Pastel2"),
                         brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                     colourInput(inputId = paste0("node_color_advanced", i),
                                 label = if (is.logical(colData(object)[[input$node_color_by]])) {
                                   as.logical(as.numeric(input$node_color_by_selection[i]))
                                 } else { input$node_color_by_selection[i] },
                                 value = cur_col[i])
                   })
        }, class = "wellpanel_node"
      )
      }})}


.create_node_size_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints){
      wellPanel(
        menuItem(span("Node size control",
                      style = "color: black;padding-top: 0px"),
                 style = "color: black; padding-top: 0px",
                 selectizeInput("node_size_by", label = span("Size by",
                                                             style = "color: black; padding-top: 0px"),
                                choices = NULL, options = NULL,
                                list(placeholder = 'Size by', maxItems = 1,
                                     maxOptions = 10)),
                 uiOutput("basic_node_size_controls"))
      )}})}

.create_basic_node_size <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints && (is.null(input$node_size_by) || input$node_size_by == "")){
      wellPanel(sliderInput(inputId = "node_size_fix",
                            label = "Basic size",
                            min = 0.5, max = 5, step = 0.5,
                            value = 1.5),
                class = "wellpanel_node")
    }
  })
}

.create_node_shape_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints){
      wellPanel(
        menuItem(span("Node shape control",
                      style = "color: black;padding-top: 0px"),
                 style = "color: black; padding-top: 0px",
                 selectizeInput("node_shape_by",
                                label = span("Shape by",
                                             style = "color: black; padding-top: 0px"),
                                choices = NULL, options = NULL,
                                list(placeholder = 'Shape by', maxItems = 1,
                                     maxOptions = 10)),
                 selectizeInput("node_shape_by_selection",
                                label = span("Select shape by",
                                             style = "color: black; padding-top: 0px"),
                                choices = NULL,
                                multiple = TRUE,
                                options = list(maxItems = 6)),
                 uiOutput("basic_node_shape_controls"),
                 uiOutput("advanced_node_shape_controls"))
      )
    }
  })}

.create_basic_node_shape <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints && is.null(input$node_shape_by_selection)){
      wellPanel(
        selectInput("node_shape_fix",
                    label = span("Basic shape", style = "color: black"),
                    choices = c("Circle" = 19, "Square" = 15, "Triangle" = 17,
                                "Diamond" = 18, "Plus" = 3, "Cross" = 4),
                    selected = 19),
        class = "wellpanel_node"
      )
    }
  })}

.create_advanced_node_shape <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints && !is.null(input$node_shape_by_selection)) {
      default_shapes <- c(19, 15, 17, 18, 3, 4)
      wellPanel(
        lapply(seq_along(input$node_shape_by_selection), function(i) {
          selectInput(inputId = paste0("node_shape_advanced", i),
                      label = span(
                        if (is.logical(colData(object)[[input$node_shape_by]])) {
                          as.character(as.logical(as.numeric(input$node_shape_by_selection[i])))
                        } else {
                          as.character(input$node_shape_by_selection[i])
                        },
                        style = "color: black"),
                      choices = c("Circle" = 19, "Square" = 15, "Triangle" = 17,
                                  "Diamond" = 18, "Plus" = 3, "Cross" = 4),
                      selected = default_shapes[min(i, 6)])
        }),
        class = "wellpanel_node"
      )
    }
  })}

.populate_node_shape_controls <- function(session, object, input){
  observeEvent(input$plotpoints, {
    if (input$plotpoints && !is.null(object)) {
      cur_choices <- names(colData(object))[vapply(seq_along(colData(object)),
        function(i) !is.numeric(colData(object)[[i]]), logical(1))]
      updateSelectizeInput(session, inputId = "node_shape_by",
                           choices = cur_choices, server = TRUE, selected = "")
    } else if (input$plotpoints) {
      updateSelectizeInput(session, inputId = "node_shape_by",
                           choices = c(""), server = TRUE, selected = "")
    }
  })

  observeEvent(input$node_shape_by, {
    req(input$plotpoints, !is.null(object))

    if (is.null(input$node_shape_by) || input$node_shape_by == "") {
      updateSelectizeInput(session, inputId = "node_shape_by_selection",
                           choices = c(""), server = TRUE, selected = "")
      return()
    }

    updateSelectizeInput(session, inputId = "node_shape_by_selection",
                         choices = unique(colData(object)[[input$node_shape_by]]),
                         server = TRUE,
                         selected = unique(colData(object)[[input$node_shape_by]][1]))
  })
}

.create_spatial_graph_control <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints){
      selectizeInput("spatial_graph",
                     label = "Spatial graph",
                     choices = NULL,
                     options = list(placeholder = 'Spatial graph',
                                    maxItems = 1, maxOptions = 10))
    }
  })}

.create_edge_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (input$plotpoints){
      tagList(
        uiOutput("edge_color_controls"),
        uiOutput("edge_width_controls"),
        uiOutput("fine_graph_controls"))
}})}



.populate_graph_controls <- function(session, object, input){
  observeEvent(input$plotpoints, {
    if (input$plotpoints && !is.null(object)) {
      updateSelectizeInput(session, inputId = "spatial_graph",
                           choices = colPairNames(object),
                           server = TRUE,
                           selected = "")
    }})}

.create_fine_graph_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if(!is.null(input$spatial_graph) && input$spatial_graph != ""){
        wellPanel(
          menuItem(span("Other edge control", 
                        style = "color: black;padding-top: 0px"),
          checkboxInput("directed",
                        span("Directed layout", style = "color: black"),
                        value = FALSE, width = NULL),
          checkboxInput("nodes_first",
                        span("Nodes first", style = "color: black"),
                        value = FALSE, width = NULL),
          class = "wellpanel_node"))
    }})}


.create_edge_color_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (!is.null(input$spatial_graph) && input$spatial_graph != "") {
      wellPanel(
        colourInput(inputId = "edge_color_fix",
                    label = span("Edge color control", style = "color: black"),
                    value = "black"),
        class = "wellpanel_node"
      )
    }
  })}

.create_edge_width_controls <- function(input, image, mask, object, img_id, ...){
  renderUI({
    if (!is.null(input$spatial_graph) && input$spatial_graph != "") {
      wellPanel(
        sliderInput(inputId = "edge_width_fix",
                    label = span("Edge width control", style = "color: black"),
                    min = 0.5, max = 5, step = 0.5,
                    value = 0.5),
        class = "wellpanel_node"
      )
    }
  })}

.populate_node_color_controls <- function(session, object, input){
  observeEvent(input$plotpoints, {
    if (input$plotpoints && !is.null(object)) {
      updateSelectizeInput(session, inputId = "node_color_by",
                           choices = names(colData(object)),
                           server = TRUE, selected = "")
    } else if (input$plotpoints) {
      updateSelectizeInput(session, inputId = "node_color_by",
                           choices = c(""), server = TRUE, selected = "")
    }
  })

  observeEvent(input$node_color_by, {
    req(input$plotpoints, !is.null(object))

    if (is.null(input$node_color_by) || input$node_color_by == "") {
      updateSelectizeInput(session, inputId = "node_color_by_selection",
                           choices = c(""), server = TRUE, selected = "")
      return()
    }

    validate(
      need(is.null(dim(colData(object)[[input$node_color_by]])),
           "NOTE: The current [Node color by] choice can not be visualized
           because it has more than one dimension in
           colData(object)[[Node color by]].")
    )

    cur_entries <- length(unique(colData(object)[[input$node_color_by]]))
    if (is.numeric(colData(object)[[input$node_color_by]]) && cur_entries > 23L) {
      updateSelectizeInput(session, inputId = "node_color_by_selection",
                           choices = input$node_color_by,
                           server = TRUE, selected = input$node_color_by)
    } else {
      updateSelectizeInput(session, inputId = "node_color_by_selection",
                           choices = unique(colData(object)[[input$node_color_by]]),
                           server = TRUE,
                           selected = unique(colData(object)[[input$node_color_by]][1]))
    }
  })
}


.populate_node_size_controls <- function(session, object, input){
  observeEvent(input$plotpoints, {
    if (input$plotpoints && !is.null(object)) {
      cur_choices <- names(colData(object))[vapply(seq_along(colData(object)),
        function(i) is.numeric(colData(object)[[i]]), logical(1))]
      updateSelectizeInput(session, inputId = "node_size_by",
                           choices = cur_choices, server = TRUE, selected = "")
    } else if (input$plotpoints) {
      updateSelectizeInput(session, inputId = "node_size_by",
                           choices = c(""), server = TRUE, selected = "")
    }
  })
}

