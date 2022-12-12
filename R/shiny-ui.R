# -----------------------------------------------------------------------------
# Helper functions to modify the visual appearance of the shiny app
# -----------------------------------------------------------------------------

# Create the application header
#' @importFrom utils packageVersion
.imageBrowser_header <- function(){
    cm_head <- dashboardHeader(
        title = paste0("imageBrowser v",
                        packageVersion("imageBrowser")),
        dropdownMenu(
            notificationItem(
                text = actionButton(
                    inputId = "SessionInfo",
                    label = "Session Info",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                    "border-color: #3C8DBC")
                ),
                status = "info"
            ),
            notificationItem(
                text = actionButton(
                    inputId = "Help",
                    label = "Help",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                    "border-color: #3C8DBC")
                ),
                status = "info"
            ),
            headerText = "Options",
            type = "notifications",
            icon = icon("circle-question"),
            badgeStatus = NULL)
        )
    return(cm_head)
    }

# Create the side bar layout
.imageBrowser_sidebar <- function(){
    cm_side <- dashboardSidebar(
        sidebarMenu(
            menuItem("General controls",
                    fluidRow(column(2, 
                        actionButton("previous.sample", label = NULL,
                                    icon = icon("angle-left", class="fa-2x"),
                                    style = paste0("background-color: ",
                                    "transparent; border-color: transparent",
                                    "; color:white; margin-left: 0px;margin-top: 30px"))),
                        column(8, style="padding-left:0px;padding-right:0px;",
                        selectizeInput("sample", label = "Image selection",
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
                    hr(),
                    fluidRow(column(2, style=c("margin-top:25px;margin-right:0px"), checkboxInput(inputId = "view1", label = NULL, value = TRUE, width = "1000px")), 
                      column(10, style= "margin-left:0px",selectizeInput("marker1", label = "Marker 1", choices = NULL))),
                    sliderInput(inputId = "contrast1", label = "Contrast",  min = 1, max = 10, value = 1, step = 0.5),
                    sliderInput(inputId = "brightness1", label = "Brightness",  min = 1, max = 10, value = 1),
                    sliderInput(inputId = "gamma1", label = "Gamma",  min = 1, max = 3, value = 1, step = 0.1),
                    colourInput(inputId = "color1", label = "Color", value = "red"),
                    hr(),
                    fluidRow(column(2, style=c("margin-top:25px;margin-right:0px"), checkboxInput(inputId = "view2", label = NULL, value = FALSE, width = "1000px")), 
                             column(10, style= "margin-left:0px",selectizeInput("marker2", label = "Marker 2", choices = NULL))),
                    sliderInput(inputId = "contrast2", label = NULL, min = 1, max = 100, value = 1),
                    colourInput(inputId = "color2", label = NULL, value = "green"),
                    hr(),
                    selectizeInput("marker3", label = "Marker 3", choices = NULL),
                    sliderInput(inputId = "contrast3", label = NULL, min = 1, max = 100, value = 1),
                    colourInput(inputId = "color3", label = NULL, value = "blue"),
                    hr(),
                    selectizeInput("marker4", label = "Marker 4", choices = NULL),
                    sliderInput(inputId = "contrast4", label = NULL, min = 1, max = 100, value = 1),
                    colourInput(inputId = "color4", label = NULL, value = "cyan"),
                    hr(),
                    selectizeInput("marker5", label = "Marker 5", choices = NULL),
                    sliderInput(inputId = "contrast5", label = NULL, min = 1, max = 100, value = 1),
                    colourInput(inputId = "color5", label = NULL, value = "magenta"),
                    hr(),
                    selectizeInput("marker6", label = "Marker 6", choices = NULL),
                    sliderInput(inputId = "contrast6", label = NULL, min = 1, max = 100, value = 1),
                    colourInput(inputId = "color6", label = NULL, value = "yellow"),
                    icon = icon("fas fa-sliders-h"), 
                    startExpanded = TRUE),
            menuItem("Advanced controls",
                     checkboxInput("outline", "Cell outline options", value = FALSE, width = NULL),
                     uiOutput("Advanced_controls"),
                     hr(),
                     numericInput(inputId = "scalebar", label = "Scale bar length", value = 20,
                                  min = 0, max = 100, step = 5),
                     icon = icon("far fa-chart-bar"), startExpanded = TRUE),
            id = "sidebar"
            ),
        tags$style(
          "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh}")
        )
    return(cm_side)
}

# Create the main body
#' @importFrom svgPanZoom svgPanZoomOutput
.imageBrowser_body <- function(){
    cm_body <- dashboardBody(
      tabsetPanel(
        tabPanel("Composite", 
                 box(svgPanZoomOutput("imagePlot"),
                     title = NULL, id = "expression", status = "primary",
                     width = 12, height = NULL)),
        tabPanel("Tiles", 
                 box("Images tiles functionality to be added"))
      ))
    return(cm_body)
    }


