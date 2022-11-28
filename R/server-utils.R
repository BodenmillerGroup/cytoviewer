# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------


# Generate help text
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
        img_IDs <- names(images)
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
        img_IDs <- names(images)
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

    return(cur_markers)
}

# Helper function to define bcg parameter when using plotPixels()
.select_contrast <- function(input){
    cur_markers <- .select_markers(input)
    
    cur_bcg <- list(c(0, input$contrast1, 1),
                    c(0, input$contrast2, 1),
                    c(0, input$contrast3, 1),
                    c(0, input$contrast4, 1),
                    c(0, input$contrast5, 1),
                    c(0, input$contrast6, 1))
    names(cur_bcg) <- cur_markers

    return(cur_bcg)
}

.create_advanced_controls <- function(object, mask, input, session){
    renderUI({
        if (input$outline) {
            wellPanel(
            h3("Outline by", style = "color: black"),
            selectizeInput("outline_by", label = span("Outline by",
                                                      style = "color: black; padding-top: 0px"), 
                           choices = NULL, options =
                               list(placeholder = 'Outline by', maxItems = 1,
                                    maxOptions = 10)),
            h3("Select outline", style = "color: black"),
            selectInput("select_outline",
                           label = span("Select outline",
                                        style = "color: black; padding-top: 0px"),
                           choices = NULL,
                           multiple = TRUE)
            )
        }
    })
}

.populate_advanced_controls <- function(object, input, session){
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

.create_image <- function(input, object, mask,
                          image, img_id, cell_id, cur_markers, cur_bcg,
                          ...){
    
    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    
    cur_image <- image[input$sample]
    
    if (input$outline && input$outline_by == "") {
        cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
        
        plotPixels(image = cur_image,
                   mask = cur_mask,
                   img_id = img_id,
                   colour_by = cur_markers,
                   bcg = cur_bcg,
                   legend = NULL,
                   image_title = NULL,
                   ...)
    } else if (input$outline && input$outline_by != "") {
        cur_object <- object[,colData(object)[[input$outline_by]] %in% input$select_outline]
        cur_mask <- mask[mcols(mask)[[img_id]] == mcols(cur_image)[[img_id]]]
        
        plotPixels(image = cur_image,
                   mask = cur_mask,
                   object = cur_object,
                   img_id = img_id,
                   cell_id = cell_id,
                   colour_by = cur_markers,
                   bcg = cur_bcg,
                   outline_by = input$outline_by,
                   legend = NULL,
                   image_title = NULL,
                   ...)
    } else {
        plotPixels(image = cur_image,
                   colour_by = cur_markers,
                   bcg = cur_bcg,
                   legend = NULL,
                   image_title = NULL,
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