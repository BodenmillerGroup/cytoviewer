# -----------------------------------------------------------------------------
# Helper functions to modify the visual appearance of the shiny app
# -----------------------------------------------------------------------------

# Create the application header
#' @importFrom utils packageVersion
.cytomapper_header <- function(){
    cm_head <- dashboardHeader(
        title = paste0("imageBrowser v",
                        packageVersion("imageBrowser")),,
        dropdownMenu(
            notificationItem(
                text = actionButton(
                    inputId = "SessionInfo",
                    label = "Session Info",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                    "border-color: #3C8DBC")
                ),
                icon = icon(""),
                status = "info"
            ),
            notificationItem(
                text = actionButton(
                    inputId = "Help",
                    label = "Help",
                    style = paste0("background-color: #3C8DBC; color: white; ",
                                    "border-color: #3C8DBC")
                ),
                icon = icon(""),
                status = "info"
            ),
            type = "tasks",
            icon = icon("fas fa-question"),
            badgeStatus = NULL,
            headerText = "")
        )
    return(cm_head)
    }

# Create the side bar layout
.cytomapper_sidebar <- function(){
    cm_side <- dashboardSidebar(
        sidebarMenu(
            menuItem("General controls",
                    fluidRow(column(2, 
                        actionButton("previous.sample", label = NULL,
                                    icon = icon("angle-left", class="fa-2x"),
                                    style = paste0("background-color: ",
                                    "transparent; border-color: transparent",
                                    "; color:white; margin-left: 0px;"))),
                        column(8, style="padding-left:0px;padding-right:0px;",
                        selectizeInput("sample", label = NULL,
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
                                    "padding-left: 0px;")))),
                    selectizeInput("marker1", choices = NULL),
                    sliderInput(inputId = "contrast1",  min = 1, max = 100, value = 1),
                    selectizeInput("marker2", choices = NULL),
                    sliderInput(inputId = "contrast2",  min = 1, max = 100, value = 1),
                    selectizeInput("marker3", choices = NULL),
                    sliderInput(inputId = "contrast3",  min = 1, max = 100, value = 1),
                    selectizeInput("marker4", choices = NULL),
                    sliderInput(inputId = "contrast4",  min = 1, max = 100, value = 1),
                    selectizeInput("marker5", choices = NULL),
                    sliderInput(inputId = "contrast5",  min = 1, max = 100, value = 1),
                    selectizeInput("marker6", choices = NULL),
                    sliderInput(inputId = "contrast6",  min = 1, max = 100, value = 1),
                    icon = icon("fas fa-sliders-h"), 
                    startExpanded = TRUE),
            menuItem("Adavanced controls",
                sliderInput(inputId = "test", min = 1, max = 12, value = 1),
                icon = icon("far fa-chart-bar"), startExpanded = TRUE),
            id = "sidebar"
            )
        )
    
    return(cm_side)

}

# Create the main body
#' @importFrom svgPanZoom svgPanZoomOutput
.cytomapper_body <- function(){
    cm_body <- dashboardBody(
        column(width = 12,
               svgPanZoomOutput("image_expression", height = "300px"))
        )
    return(cm_body)

    }


