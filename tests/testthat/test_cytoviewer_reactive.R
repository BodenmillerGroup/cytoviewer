test_that("cytoviewer: input testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  # Input testing 
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH",
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", resolution = 1)
    
    expect_equal(input$sample, "E34_imc")
    expect_equal(input$marker1, "H3")
    expect_equal(input$marker2, "CD99")
    expect_equal(input$marker3, "PIN")
    expect_equal(input$marker4, "CD8a")
    expect_equal(input$marker5, "CDH")
    expect_equal(input$contrast1, 1)
    expect_equal(input$contrast2, 1)
    expect_equal(input$contrast3, 1)
    expect_equal(input$contrast4, 1)
    expect_equal(input$contrast5, 1)
    expect_equal(input$brightness1, 1)
    expect_equal(input$brightness2, 1)
    expect_equal(input$brightness3, 1)
    expect_equal(input$brightness4, 1)
    expect_equal(input$brightness5, 1)
    expect_equal(input$gamma1, 1)
    expect_equal(input$gamma2, 1)
    expect_equal(input$gamma3, 1)
    expect_equal(input$gamma4, 1)
    expect_equal(input$gamma5, 1)
    expect_equal(input$color1, "#FF00FF")
    expect_equal(input$color2, "#00FFFF")
    expect_equal(input$color3, "#FFFF00")
    expect_equal(input$color4, "#FF0000")
    expect_equal(input$color5, "#00FF00")
  })
 
})

test_that("cytoviewer: plot input testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
    
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = FALSE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = FALSE, 
                      outline_by = NULL, 
                      resolution = 1
)
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"

    cur_markers <- .select_markers(input)
    cur_markers <- cur_markers[cur_markers != ""]
    cur_markers_test <- c("H3")
    names(cur_markers_test) <- TRUE
    expect_equal(cur_markers, cur_markers_test)
    
    cur_bcg <- .select_contrast(input)
    cur_bcg <- cur_bcg[names(cur_bcg) != ""]
    cur_bcg_test <- list()
    cur_bcg_test[[1]]<- c(1,1,1)
    names(cur_bcg_test) <- "H3"
    expect_equal(cur_bcg, cur_bcg_test)

    cur_color <- .select_colors(input)
    cur_color <- cur_color[names(cur_color) != ""]
    cur_color_test <- list()
    cur_color_test[[1]]<- c("black","#FF00FF")
    names(cur_color_test) <- "H3"
    expect_equal(cur_color, cur_color_test)
    
    cur_image <- .filter_image(input, image)
    expect_s4_class(cur_image, "CytoImageList")
    expect_equal(names(cur_image), "E34_imc")
    expect_equal(channelNames(cur_image), c("H3", "CD99", "PIN", "CD8a", "CDH"))

    cur_legend <- .show_legend(input)
    expect_null(cur_legend)

    cur_imagetitle <- .show_title(input)
    expect_null(cur_legend)
    
    # Plot pixel output (Images only)
    expect_silent(.create_image(input, object, mask, image, img_id, cell_id))

    })
  
})

test_that("cytoviewer: plot input 2 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = FALSE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = TRUE, 
                      outline_by = "", 
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"
    
    # Plot pixel output (Images + Masks)
    expect_silent(.create_image(input, object, mask, image, img_id, cell_id))
    
  })
  
})  

test_that("cytoviewer: plot input 3 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = FALSE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = TRUE, 
                      outline_by = "CellType",
                      select_outline = "celltype_C",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"
    
    # Plot pixel output (Images + Masks + Object - Categorical)
    expect_silent(.create_image(input, object, mask, image, img_id, cell_id))
    
  })
  
}) 

test_that("cytoviewer: plot input 4 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = FALSE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = TRUE, 
                      show_title = TRUE,
                      gaussian_blur = TRUE, 
                      gaussian_blur_sigma = 2,
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = TRUE, 
                      outline_by = "Area",
                      numeric_color_outline = "viridis",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"

    # Plot pixel output (Images + Masks + Object - Numeric + Title + Legend + Gaussian Blur)
    expect_silent(.create_image(input, object, mask, image, img_id, cell_id))
    
  })
  
})  

test_that("cytoviewer: plot input 5 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "CD99", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = FALSE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = TRUE,
                      outline_by = "",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- NULL
    img_id <- "ImageNb"
    cell_id <- "CellNb"

    # Plot pixel output (Images + Masks (- Object) - outline is TRUE but no object)
    expect_silent(.create_image(input, object, mask, image, img_id, cell_id))
    
  })
  
})  

test_that("cytoviewer: plot input 6 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE, 
                      color_by = "CellType",
                      color_by_selection = "celltype_C",
                      color_by1 = "blue",
                      missing_colorby = "white",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"

    # Plot cells output (Images + Masks + Object - Categorical)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))

  })

})

test_that("cytoviewer: plot input 7 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE, 
                      color_by = "Pattern",
                      color_by_selection = "1",
                      color_by1 = "blue",
                      missing_colorby = "white",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"
    
    # Plot cells output (Images + Masks + Object - Logical)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))
    
  })
  
})  

test_that("cytoviewer: plot input 8 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_mask", 
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE, 
                      color_by = "Area",
                      color_by_selection = "Area",
                      numeric_colorby = "viridis",
                      resolution = 1
    )
    image <- NULL
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"

    # Plot cells output (Masks + Object - No Image - Numeric without image)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))
    
  })
  
})  

test_that("cytoviewer: plot input 9 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE,
                      color_by = "",
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- NULL
    img_id <- "ImageNb"
    
    # Plot cells output (Images + Masks but no object)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))
    
  })
})  

test_that("cytoviewer: plot input 10 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_mask", 
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE,
                      color_by = "",
                      resolution = 1
    )
    
    image <- NULL
    mask <- pancreasMasks[1]
    object <- NULL
    img_id <- "ImageNb"
    
    # Plot cells output (Masks but no images and object)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))
    
  })
})  

test_that("cytoviewer: plot input 11 testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc", 
                      marker1 = "H3", marker2 = "H3", marker3 = "PIN", marker4 = "CD8a", marker5 = "CDH", marker6 = "",
                      view1 = TRUE, view2 = TRUE, view3 = FALSE, view4 = FALSE, view5 = FALSE, view6 = FALSE,
                      contrast1 = 1, contrast2 = 1, contrast3 = 1, contrast4 = 1, contrast5 = 1, contrast6 = 1,
                      brightness1 = 1, brightness2 = 1, brightness3 = 1, brightness4 = 1, brightness5 = 1, brightness6 = 1,
                      gamma1 = 1, gamma2 = 1, gamma3 = 1, gamma4 = 1, gamma5 = 1, gamma6 = 1,
                      color1 = "#FF00FF", color2 = "#00FFFF", color3 = "#FFFF00", color4 = "#FF0000", color5 = "#00FF00", color6 = "black",
                      show_legend = FALSE, 
                      show_title = FALSE,
                      gaussian_blur = FALSE, 
                      scalebar = 20,
                      thick = FALSE, 
                      interpolate = TRUE,
                      outline = FALSE, 
                      outline_by = NULL,
                      resolution = 1
    )
    
    image <- pancreasImages[1]
    mask <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"
    cell_id <- "CellNb"
    
    # Plot output (Selecting the same channel)
    expect_error(.create_image(input, object, mask, image, img_id, cell_id),
                 regexp = "NOTE: Please only select unique markers.",
                 fixed = TRUE)
    
  })
  
})  

test_that("cytoviewer: plot input 12 testing works", {

  # Load datasets
  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                              img_id = "ImageNb", cell_id = "CellNb",
                              coords = c("Pos_X", "Pos_Y")), {

    session$setInputs(sample = "E34_mask",
                      show_legend = FALSE,
                      show_title = FALSE,
                      gaussian_blur = FALSE,
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE,
                      color_by = "",
                      resolution = 1)

    image <- NULL
    mask <- pancreasMasks[1]
    object <- NULL
    img_id <- "ImageNb"

    # Plot cells output (Masks but no images and object)
    expect_silent(.create_cells(input, object, mask, image, img_id, cell_id))

  })
})

test_that("cytoviewer: plot input 13 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    session$setInputs(
        sample         = "E34_mask",
        plotpoints     = TRUE,
        node_color_by  = "",
        node_color_fix = "black",
        node_size_by   = "",
        node_size_fix  = 1.5,
        node_shape_by  = "",
        node_shape_fix = "19",
        spatial_graph  = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    # Basic graph: all fixed params, no edges
    p <- .create_graph(input, image, mask, object, img_id,
                       coords = c("Pos_X", "Pos_Y"))
    expect_s3_class(p, "gg")
  })
})

test_that("cytoviewer: plot input 14 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_color_by categorical + per-category colors
    session$setInputs(
        sample                  = "E34_mask",
        plotpoints               = TRUE,
        node_color_by           = "CellType",
        node_color_by_selection = c("celltype_A", "celltype_B",
                                    "celltype_C"),
        node_color_advanced1    = "yellow",
        node_color_advanced2    = "blue",
        node_color_advanced3    = "red",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "",
        node_shape_fix          = "19",
        spatial_graph           = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 15 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_color_by logical (Pattern) — selection uses "0"/"1"
    session$setInputs(
        sample                  = "E34_mask",
        plotpoints               = TRUE,
        node_color_by           = "Pattern",
        node_color_by_selection = c("0", "1"),
        node_color_advanced1    = "grey80",
        node_color_advanced2    = "orange",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "",
        node_shape_fix          = "19",
        spatial_graph           = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 16 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_color_by numeric (Area) + viridis colormap
    session$setInputs(
        sample                  = "E34_mask",
        plotpoints               = TRUE,
        node_color_by           = "Area",
        node_color_by_selection = "Area",
        numeric_node_color      = "A",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "",
        node_shape_fix          = "19",
        spatial_graph           = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 17 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_size_by (Area)
    session$setInputs(
        sample         = "E34_mask",
        plotpoints     = TRUE,
        node_color_by  = "",
        node_color_fix = "black",
        node_size_by   = "Area",
        node_shape_by  = "",
        node_shape_fix = "19",
        spatial_graph  = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 18 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_shape_by categorical + per-category shapes
    session$setInputs(
        sample                  = "E34_mask",
        plotpoints               = TRUE,
        node_color_by           = "",
        node_color_fix          = "black",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "CellType",
        node_shape_by_selection = c("celltype_A", "celltype_B",
                                    "celltype_C"),
        node_shape_advanced1    = "19",
        node_shape_advanced2    = "17",
        node_shape_advanced3    = "15",
        spatial_graph           = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 19 testing works", {

  library(cytomapper)
  library(S4Vectors)
  data("pancreasMasks")
  data("pancreasSCE")

  # Build bidirectional chain graph across all cells
  n <- ncol(pancreasSCE)
  test_hits <- SelfHits(
      from  = c(seq_len(n - 1L), seq_len(n - 1L) + 1L),
      to    = c(seq_len(n - 1L) + 1L, seq_len(n - 1L)),
      nnode = n)
  colPair(pancreasSCE, "test_graph") <- test_hits

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # Directed edges + color_by + all edge controls
    session$setInputs(
        sample                  = "E34_mask",
        node_color_by           = "CellType",
        node_color_by_selection = c("celltype_A", "celltype_B",
                                    "celltype_C"),
        node_color_advanced1    = "yellow",
        node_color_advanced2    = "blue",
        node_color_advanced3    = "red",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "",
        node_shape_fix          = "19",
        spatial_graph           = "test_graph",
        directed                = TRUE,
        nodes_first             = TRUE,
        edge_color_fix          = "grey50",
        edge_width_fix          = 0.5)

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 20 testing works", {

  library(cytomapper)
  library(S4Vectors)
  data("pancreasMasks")
  data("pancreasSCE")

  # Build bidirectional chain graph across all cells
  n <- ncol(pancreasSCE)
  test_hits <- SelfHits(
      from  = c(seq_len(n - 1L), seq_len(n - 1L) + 1L),
      to    = c(seq_len(n - 1L) + 1L, seq_len(n - 1L)),
      nnode = n)
  colPair(pancreasSCE, "test_graph") <- test_hits

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # Undirected edges + nodes_first = FALSE
    session$setInputs(
        sample         = "E34_mask",
        node_color_by  = "",
        node_color_fix = "black",
        node_size_by   = "",
        node_size_fix  = 1.5,
        node_shape_by  = "",
        node_shape_fix = "19",
        spatial_graph  = "test_graph",
        directed       = FALSE,
        nodes_first    = FALSE,
        edge_color_fix = "grey70",
        edge_width_fix = 0.3)

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    # Undirected edges, nodes drawn last
    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 21 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_color_by set but node_color_by_selection left NULL (never set) ->
    # cur_node_color_by nulled, graph renders with fixed color
    session$setInputs(
        sample         = "E34_mask",
        plotpoints     = TRUE,
        node_color_by  = "CellType",
        node_color_fix = "black",
        node_size_by   = "",
        node_size_fix  = 1.5,
        node_shape_by  = "",
        node_shape_fix = "19",
        spatial_graph  = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 22 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = pancreasSCE,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # node_shape_by set but node_shape_by_selection left NULL (never set) ->
    # cur_node_shape_by nulled, graph renders with fixed shape
    session$setInputs(
        sample         = "E34_mask",
        plotpoints     = TRUE,
        node_color_by  = "",
        node_color_fix = "black",
        node_size_by   = "",
        node_size_fix  = 1.5,
        node_shape_by  = "CellType",
        node_shape_fix = "19",
        spatial_graph  = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- pancreasSCE
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
  })
})

test_that("cytoviewer: plot input 23 testing works", {

  library(cytomapper)
  data("pancreasMasks")
  data("pancreasSCE")

  # Add logical column to cover the logical node_shape_by branch
  cur_sce_logical <- pancreasSCE
  cur_sce_logical$IsActivated <- as.logical(seq_len(ncol(pancreasSCE)) %% 2)

  testServer(
    app = cytoviewer(mask = pancreasMasks, object = cur_sce_logical,
                     img_id = "ImageNb", cell_id = "CellNb",
                     coords = c("Pos_X", "Pos_Y")), {

    # logical node_shape_by: subsetting uses numeric string selection
    # covers lines in .create_graph and in .create_advanced_node_shape 
    # (logical label rendering)
    session$setInputs(
        sample                  = "E34_mask",
        plotpoints              = TRUE,
        node_color_by           = "",
        node_color_fix          = "black",
        node_size_by            = "",
        node_size_fix           = 1.5,
        node_shape_by           = "IsActivated",
        node_shape_by_selection = c("0", "1"),
        node_shape_advanced1    = "19",
        node_shape_advanced2    = "15",
        spatial_graph           = "")

    image  <- NULL
    mask   <- pancreasMasks[1]
    object <- cur_sce_logical
    img_id <- "ImageNb"

    expect_silent(.create_graph(input, image, mask, object, img_id,
                                coords = c("Pos_X", "Pos_Y")))
    expect_false(is.null(output$advanced_node_shape_controls))
  })
})

test_that("cytoviewer: plot input 24 testing works", {
  
  # Load datasets
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  # Regression test for the reactive output$cellsPlot wiring - 'coords' argument threaded through
  # server(...) for points-level rendering must not leak into plotCells().
  testServer(app = cytoviewer(image = pancreasImages, mask = pancreasMasks, object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", coords = c("Pos_X", "Pos_Y")), {
    
    session$setInputs(sample = "E34_imc",
                      show_legend = FALSE,
                      show_title = FALSE,
                      gaussian_blur = FALSE,
                      scalebar = 20,
                      interpolate = TRUE,
                      plotcells = TRUE,
                      color_by = "CellType",
                      color_by_selection = "celltype_C",
                      color_by1 = "blue",
                      missing_colorby = "white",
                      resolution = 1
    )
    
    expect_silent(output$cellsPlot)
    
  })
  
})