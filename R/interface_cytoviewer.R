# -----------------------------------------------------------------------------
# Helper functions to modify the visual appearance of the shiny app
# -----------------------------------------------------------------------------

#' @importFrom utils packageVersion
#' @importFrom colourpicker colourInput
#' @importFrom shinycssloaders withSpinner
#' @importFrom svgPanZoom svgPanZoomOutput 

# Create the application header
.cytoviewer_header <- function(){
    cm_head <- dashboardHeader(
        title = paste0("cytoviewer v", packageVersion("cytoviewer")),
        titleWidth = 250,
        dropdownMenu(
            notificationItem(
                text = actionButton(
                    inputId = "SessionInfo",
                    label = "Session Info",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                    "border-color: #3C8DBC"),
                ),
                status = "info", 
                icon = icon(NULL)
            ),
            notificationItem(
                text = actionButton(
                    inputId = "Help",
                    label = "Help",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                   "border-color: #3C8DBC"),
                ),
                status = "info", 
                icon(NULL)
            ),
            headerText = "",
            type = "notifications",
            icon = icon("question"),
            badgeStatus = NULL),
        dropdownMenu(
          notificationItem(
            text = textInput(inputId = "filename1",
                             label = "File name",
                             value = ""),
            icon = icon(NULL),
            status = "danger"
          ),
          notificationItem(
            text = radioButtons(inputId = "fileselection", 
                                label = "Select image",
                                choices = list("Composite","Channels","Mask"), 
                                selected = "Composite"), 
            icon = icon(NULL),
            status = "info"
          ),
          notificationItem(
            text = radioButtons(inputId = "filename2", 
                                label = "File format",
                                choices = list("pdf","png"), 
                                selected = "pdf"), 
            icon = icon(NULL),
            status = "info"
          ),
          notificationItem(
            text = downloadButton(
              outputId = "downloadData",
              label = "Download",
              style = paste0("background-color: #3C8DBC; color: white; ",
                            "border-color: #7EA6F8")
            ),
            icon = icon(NULL),
            status = "info"
          ),
          type = "notification",
          icon = icon("fas fa-download"),
          badgeStatus = NULL,
          headerText = "")
    )
    return(cm_head)
    }

# Create the side bar layout
.cytoviewer_sidebar <- function(){
    cm_side <- dashboardSidebar(
        sidebarMenu(
          fluidRow(column(2, 
                          actionButton("previous.sample", label = NULL,
                                       icon = icon("angle-left", class="fa-2x"),
                                       style = paste0("background-color: ",
                                                      "transparent; border-color: transparent",
                                                      "; color:white; margin-left: 0px;margin-top: 30px"))),
                   column(8, style="padding-left:0px;padding-right:0px",
                          selectizeInput("sample", label = "Sample selection",
                                         width = "100%",
                                         choices = NULL,
                                         options = list(
                                           placeholder = 'Select a sample', 
                                           maxItems = 1))),
                   column(2,style="padding-left:0px;",
                          actionButton("next.sample", label = NULL,
                                       icon = icon(name = "angle-right", 
                                                   class="fa-2x"),
                                       style = paste0("background-color: ",
                                                      "transparent; border-color: transparent",
                                                      "; color: white; margin-left: 0px; ",
                                                      "padding-left: 0px;margin-top: 30px")))),
          
          menuItem("Image-level", startExpanded = TRUE, icon = icon("camera"),
            menuItem("Basic controls",
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view1", label = NULL, value = TRUE, width = "1000px")), 
                      column(10, 
                             style= "margin-left:0px", 
                             selectizeInput("marker1", label = "Marker 1", choices = NULL))),
                    menuItem("Color control", 
                      sliderInput(inputId = "contrast1", label = "Contrast",  
                                  min = 1, max = 10, value = 1, step = 0.5),
                      sliderInput(inputId = "brightness1", label = "Brightness",  
                                  min = 1, max = 10, value = 1),
                      sliderInput(inputId = "gamma1", label = "Gamma",  
                                  min = 1, max = 3, value = 1, step = 0.1),
                      colourInput(inputId = "color1", label = "Color", 
                                  value = "magenta")),
                    hr(),
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view2", label = NULL, value = TRUE, width = "1000px")), 
                             column(10, 
                                    style= "margin-left:0px",
                                    selectizeInput("marker2", label = "Marker 2", choices = NULL))),
                    menuItem("Color control", 
                             sliderInput(inputId = "contrast2", label = "Contrast",  
                                         min = 1, max = 10, value = 1, step = 0.5),
                             sliderInput(inputId = "brightness2", label = "Brightness",  
                                         min = 1, max = 10, value = 1),
                             sliderInput(inputId = "gamma2", label = "Gamma",  
                                         min = 1, max = 3, value = 1, step = 0.1),
                             colourInput(inputId = "color2", label = "Color", 
                                         value = "cyan")),
                    hr(),
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view3", label = NULL, value = TRUE, width = "1000px")), 
                             column(10, 
                                    style= "margin-left:0px",
                                    selectizeInput("marker3", label = "Marker 3", choices = NULL))),
                    menuItem("Color control", 
                             sliderInput(inputId = "contrast3", label = "Contrast",  
                                         min = 1, max = 10, value = 1, step = 0.5),
                             sliderInput(inputId = "brightness3", label = "Brightness",  
                                         min = 1, max = 10, value = 1),
                             sliderInput(inputId = "gamma3", label = "Gamma",  
                                         min = 1, max = 3, value = 1, step = 0.1),
                             colourInput(inputId = "color3", label = "Color", 
                                         value = "yellow")),
                    hr(),
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view4", label = NULL, value = TRUE, width = "1000px")), 
                             column(10, 
                                    style= "margin-left:0px",
                                    selectizeInput("marker4", label = "Marker 4", choices = NULL))),
                    menuItem("Color control", 
                             sliderInput(inputId = "contrast4", label = "Contrast",  
                                         min = 1, max = 10, value = 1, step = 0.5),
                             sliderInput(inputId = "brightness4", label = "Brightness",  
                                         min = 1, max = 10, value = 1),
                             sliderInput(inputId = "gamma4", label = "Gamma",  
                                         min = 1, max = 3, value = 1, step = 0.1),
                             colourInput(inputId = "color4", label = "Color", 
                                         value = "red")),
                    hr(),
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view5", label = NULL, value = TRUE, width = "1000px")), 
                             column(10, 
                                    style= "margin-left:0px",
                                    selectizeInput("marker5", label = "Marker 5", choices = NULL))),
                    menuItem("Color control", 
                             sliderInput(inputId = "contrast5", label = "Contrast",  
                                         min = 1, max = 10, value = 1, step = 0.5),
                             sliderInput(inputId = "brightness5", label = "Brightness", 
                                         min = 1, max = 10, value = 1),
                             sliderInput(inputId = "gamma5", label = "Gamma",  
                                         min = 1, max = 3, value = 1, step = 0.1),
                             colourInput(inputId = "color5", label = "Color", 
                                         value = "green")),
                    hr(),
                    fluidRow(column(2, 
                                    style=c("margin-top:25px;margin-right:0px"), 
                                    checkboxInput(inputId = "view6", label = NULL, value = TRUE, width = "1000px")), 
                             column(10, 
                                    style= "margin-left:0px",
                                    selectizeInput("marker6", label = "Marker 6", choices = NULL))),
                    menuItem("Color control", 
                             sliderInput(inputId = "contrast6", label = "Contrast", 
                                         min = 1, max = 10, value = 1, step = 0.5),
                             sliderInput(inputId = "brightness6", label = "Brightness",  
                                         min = 1, max = 10, value = 1),
                             sliderInput(inputId = "gamma6", label = "Gamma",  
                                         min = 1, max = 3, value = 1, step = 0.1),
                             colourInput(inputId = "color6", label = "Color", 
                                         value = "blue")),
                    hr(),
                    #icon = icon("fas fa-sliders-h"), 
                    startExpanded = FALSE),
            menuItem("Advanced controls",
                     checkboxInput("outline", "Show cell outlines", 
                                   value = FALSE, width = NULL),
                     uiOutput("Outline_controls"),
                     uiOutput("Basic_color_outline"),
                     uiOutput("Advanced_color_outline"),
                     uiOutput("Outline_thickness"),
                     startExpanded = FALSE),
            id = "sidebar"
            ),
        menuItem("Cell-level", 
                 menuItem("Basic controls",
                          checkboxInput("plotcells", "Show cell-level plot", 
                                        value = FALSE, width = NULL),
                          uiOutput("Colorby_controls"),
                          uiOutput("Colorby_colors")), 
                 startExpanded = TRUE, icon = icon("shapes")
                 ),
        menuItem("General",
                 menuItem("Basic controls", 
                          menuItem("Image appearance",
                          uiOutput("scalebar_controls"),
                          checkboxInput(inputId = "show_legend","Show Legend", 
                                        value = FALSE),
                          checkboxInput(inputId = "show_title","Show Title", 
                                        value = FALSE), 
                          startExpanded = FALSE),
                          menuItem("Image filters",
                                   checkboxInput(inputId = "interpolate",
                                                 "Pixel-wise Interpolation", 
                                                 value = TRUE),
                                   fluidRow(column(2, 
                                                   style=c("margin-top:25px;margin-right:0px"), 
                                                   checkboxInput(inputId = "gaussian_blur", 
                                                                 label = NULL, value = FALSE, width = "1000px")), 
                                   column(10, 
                                          style= "margin-left:0px",
                                          sliderInput(inputId = "gaussian_blur_sigma", label = "Gaussian filter",  
                                                      min = 1, max = 3, value = 1, step = 0.1))),
                          startExpanded = FALSE)
                          ),
                 startExpanded = TRUE, icon = icon("sliders-h")
                          )
        ),
        width = 250,
        tags$style(
          "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh}")
        )
    return(cm_side)
}

# Create the main body
.cytoviewer_body <- function(){
    cm_body <- dashboardBody(
      tabsetPanel(
        tabPanel("Image-level",
                 tabsetPanel(
                   tabPanel("Composite",box(
                     withSpinner(
                       fullscreen_this(
                         svgPanZoomOutput("imagePlot")), type = 6), 
                     title = NULL, id = "expression", status = "primary", 
                     width = NULL, height = NULL)),
                   tabPanel("Channels", value = "tiles_tab", width = 12, 
                            withSpinner(uiOutput("tiles_tab"), type = 6)))),
        tabPanel("Cell-level", 
                 tabsetPanel(
                   tabPanel("Mask", value = "cells_tab", width = 12, 
                            withSpinner(uiOutput("cells_tab"), type = 6))))
      ))
    return(cm_body)
    }
