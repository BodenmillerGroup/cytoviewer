# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------

#' @importFrom cytomapper plotCells plotPixels channelNames CytoImageList
#' @importFrom SingleCellExperiment colData
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
#' @importFrom S4Vectors endoapply mcols mcols<-


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
        information and this help page as well as a dropdown-menu for image 
        downloads.", "The Body features a Tabset-Panel layout allowing the user 
        to switch between three image modes:", strong("Image-level 
                                                      (Composite and Channels) 
                                                      and Cell-level (Mask).")),
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
        h3("General controls"),
        p("General controls is subdivided into an", em("Image appearance and 
        Image filters"), "part.", "In the Image appearance section, the user can 
        adjust the scale bar length and include legend/image titles, 
        while the Image filters section allows to control pixel-wise interpolation 
        (default) and apply a Gaussian filter."),
        h3("Image download"), 
        p("The cytoviewer package supports fast and uncomplicated image downloads. 
        Download controls are part of the", em("Header"), ".", "The user can 
        specify a file name, select the image of interest (Composite, Channels, 
        Mask) and the file format (pdf, png). Upon clicking the download button, 
        a pop-window should appear where the user can specify the download location.")
    )
}

# Create general observers for header
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
.create_interactive_observer <- function(image, mask, input, session){

    # Next Image Observer
    observeEvent(input$next.sample, {
      #browser()
      img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
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
      img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
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
.create_updateSelectizeInput <- function(image, mask, input, session){
  
  img_IDs <- if(!is.null(names(image))) names(image) else names(mask)
  
  # Store image IDs
    updateSelectizeInput(session, inputId = "sample",
                        choices = unique(img_IDs),
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
  if (is.numeric(colData(object)[[input$outline_by]])) {
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
  
  if(input$show_legend){cur_legend <- legend_param}else{cur_legend <- NULL}
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
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    cur_color <- .select_colors(input)
    cur_color <- cur_color[names(cur_color) != ""]
    
    cur_basic_outline <- input$basic_color_outline
    cur_scale <- input$scalebar
    cur_thick <- input$thick
    cur_interpolate <- input$interpolate
    
    cur_image <- .filter_image(input, image)
    
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)
    
    if (input$outline && !is.null(input$outline_by)){
      if(input$outline_by == "") {
        
        req(img_id)
        
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
                   scale_bar = list(length = cur_scale),
                   interpolate = cur_interpolate,
                   ...)
      
        } else if (input$outline_by != "") { 
        
          req(img_id, cell_id)
          
        if (is.numeric(colData(object)[[input$outline_by]])) {
        cur_object <- object
        } else {
        cur_object <- object[,colData(object)[[input$outline_by]] %in% input$select_outline]
        }
          
      cur_mask <- .get_mask(input, mask, img_id, cur_image)
      cur_advanced_outline <- .select_outline_colors(input, object)
      cur_color[[input$outline_by]] <- cur_advanced_outline
      
      req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
      req(!identical(unique(colData(cur_object)[,img_id]), character(0)))
      
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
                 scale_bar = list(length = cur_scale),
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


## Image tiles function draft 
.create_image_tiles <- function(input, object, mask, image, channels,
                                img_id, cell_id, ...){
  
  req(input$sample != "")
  req(!is.null(input$scalebar))
  
  cur_markers <- .select_markers(input)
  cur_markers <- cur_markers[cur_markers != ""]
  
  req(channels$length_output == length(cur_markers))

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
    cur_interpolate <- input$interpolate
    
    cur_image <- .filter_image(input, image)
    
    cur_legend <- .show_legend(input)
    cur_imagetitle <- .show_title(input)
    
    if (input$outline && !is.null(input$outline_by)){
      if(input$outline_by == "") {
        
        req(img_id)
        
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
                 scale_bar = list(length = cur_scale),
                 interpolate = cur_interpolate,
                 return_plot = TRUE,
                 ...)
      
    } else if (input$outline_by != "") {
      
      req(img_id, cell_id)
      
      if(is.numeric(colData(object)[[input$outline_by]])){
        cur_object <- object
      }else{
        cur_object <- object[,colData(object)[[input$outline_by]] %in% 
                               input$select_outline]
      }
      
      cur_mask <- .get_mask(input, mask, img_id, cur_image)
      cur_advanced_outline <- .select_outline_colors(input, object)
      cur_color[[input$outline_by]] <- cur_advanced_outline
      
      req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
      
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
                 scale_bar = list(length = cur_scale),
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
      if(input$fileselection %in% c("Composite","Masks")){
        paste0(input$filename1, ".",input$filename2)
      } else {
        paste0(input$filename1,".zip")
      }},
    content = function(file){
      if(input$fileselection == "Composite"){
        if(input$filename2 == "pdf"){
          pdf(file = file)
          .create_image(input, object, mask,
                        image, img_id, cell_id,
                        ...)
          dev.off()
          } else {
            png(filename = file)
            .create_image(input, object, mask,
                          image, img_id, cell_id,
                          ...)
            dev.off()
          }
        } else if(input$fileselection == "Masks"){
          if(input$filename2 == "pdf"){
            pdf(file = file)
            .create_cells(input, object, mask, image, img_id, cell_id, ...)
            dev.off()
          } else {
            png(filename = file)
            .create_cells(input, object, mask, image, img_id, cell_id, ...)
            dev.off()
          }
        } else {
          cur_markers <- .select_markers(input)
          cur_markers <- cur_markers[cur_markers != ""]
          plot_list <- .create_image_tiles(input, object, mask, image, 
                                           img_id, cell_id)
          
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
        if(is.numeric(colData(object)[[input$outline_by]])){
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
    wellPanel(
      if(is.numeric(colData(object)[[input$outline_by]])){ 
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
                   cur_col <- c(brewer.pal(12, "Paired"),
                                brewer.pal(8, "Pastel2")[-c(3,5,8)],
                                brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                   colourInput(inputId = paste0("color_outline",i),
                               label = input$select_outline[i],
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
        if(is.numeric(colData(object)[[input$color_by]])){
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
      wellPanel(
        if(is.numeric(colData(object)[[input$color_by]])){
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
                     cur_col <- c(brewer.pal(12, "Paired"),
                                  brewer.pal(8, "Pastel2")[-c(3,5,8)],
                                  brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                     colourInput(inputId = paste0("color_by",i),
                                 label = input$color_by_selection[i],
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
    if (is.numeric(colData(object)[[input$color_by]])) {
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
  if(input$color_by != "" && !is.numeric(colData(object)[[input$color_by]])){
    req(input$color_by_selection)
    object <- object[, colData(object)[[input$color_by]] %in% input$color_by_selection]
  }else{object <- object}
  
  return(object)
}

#  Helper function to construct image 

.create_cells <- function(input, object, mask,
                          image, img_id, cell_id, ...){
  
  req(img_id)

  cur_scale <- input$scalebar
  cur_legend <- .show_legend(input)
  cur_imagetitle <- .show_title(input)
  cur_missingcolor <- input$missing_colorby
  
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
  req(!identical(unique(colData(cur_object)[,img_id]), integer(0)))
  req(!identical(unique(colData(cur_object)[,img_id]), character(0)))
  }else{
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
            scale_bar = list(length = cur_scale),
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
    box(withSpinner(svgPanZoomOutput("cellsPlot"),type = 6), 
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
    
    numericInput(inputId = "scalebar", label = "Scale bar length", 
                 value = cur_value, min = 0, max = 1000, step = 5)
  })
}
