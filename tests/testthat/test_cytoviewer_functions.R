test_that("cytoviewer: validity check testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  # Fail in validity checks 
  expect_error(cytoviewer(), 
               regexp = "Please provide 'image', 'mask' and/or 'object' data.\nSee ?cytoviewer() for details.", 
               fixed = TRUE)
  
  expect_error(cytoviewer(image = "test"), 
               regexp = "Please provide the image(s) in form of a 'CytoImageList' object",
               fixed = TRUE)
  
  expect_error(cytoviewer(mask = "test"), 
               regexp = "Please provide the mask(s) in form of a 'CytoImageList' object",
               fixed = TRUE)
  
  expect_error(cytoviewer(object = "test"), 
               regexp = "Please provide the 'object' in form of a 'SingleCellExperiment' or 
         'SpatialExperiment' object",
               fixed = TRUE)
  
  expect_error(cytoviewer(mask = pancreasMasks, img_id = NULL), 
               regexp = "Please provide an 'img_id' argument.",
               fixed = TRUE)
  
  expect_error(cytoviewer(object = pancreasSCE, cell_id = NULL), 
               regexp = "Please provide a 'cell_id' argument.",
               fixed = TRUE)
  
  expect_error(cytoviewer(object = pancreasSCE, cell_id = c("cell","ID")), 
               regexp = "Invalid argument for 'cell_id'.",
               fixed = TRUE)
  
  expect_error(cytoviewer(mask = pancreasMasks, img_id = c("img","ID")), 
               regexp = "Invalid argument for 'img_id'.",
               fixed = TRUE)
  
  cur_object <- pancreasSCE
  colData(cur_object)[["ImageNb"]] <- NULL
  
  expect_error(cytoviewer(image = pancreasImages, 
                          object = cur_object, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fixed = TRUE)
  
  colData(cur_object)[["ImageNb"]] <- colData(pancreasSCE)[["ImageNb"]]
  colData(cur_object)[["CellNb"]] <- NULL
  
  expect_error(cytoviewer(image = pancreasImages, 
                          object = cur_object, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fixed = TRUE)
  
  cur_image <- pancreasImages
  channelNames(cur_image) <- NULL 
  expect_error(cytoviewer(image = cur_image), 
               regexp = "Please specify the 'channelNames' of the 'image' object.",
               fixed = TRUE)
  
  cur_object_2 <- pancreasSCE
  rownames(cur_object_2)[1] <- "HistoneH3"
  
  expect_error(cytoviewer(image = pancreasImages, 
                          object = cur_object_2, 
                          cell_id = "CellNb"), 
               regexp = "The 'channelNames' of the images need to match the rownames of the 
           'object'.",
               fixed = TRUE)

  expect_error(cytoviewer(image = pancreasImages, 
                          object = cur_object_2, 
                          cell_id = "CellNb"), 
               regexp = "The 'channelNames' of the images need to match the rownames of the 
           'object'.",
               fixed = TRUE)
  
  cur_object_3 <- pancreasSCE
  colData(cur_object_3)[["ImageNb"]][1] <- 4
  
  expect_error(cytoviewer(image = pancreasImages, 
                          object = cur_object_3, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "Please provide a unique 'image' for every sample stored in 'object'.",
               fixed = TRUE)
  
  expect_error(cytoviewer(mask = pancreasMasks, 
                          object = cur_object_3, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "Please provide a unique 'mask' for every sample stored in 'object'.",
               fixed = TRUE)
  
  cur_image_2 <- pancreasImages
  mcols(cur_image_2)[["ImageNb"]] <- c(1,1,2)
  
  expect_error(cytoviewer(image = cur_image_2, img_id = "ImageNb"), 
               regexp = "Entries in the 'mcols(image)[,img_id]' slot are not unique.",
               fixed = TRUE)
  
  mcols(cur_image_2)[["ImageNb"]] <- NULL
  
  expect_error(cytoviewer(image = cur_image_2, img_id = "ImageNb"), 
               regexp = "'img_id' not in 'mcols(image)'.",
               fixed = TRUE)
  
  
  expect_error(cytoviewer(image = pancreasImages, 
                          mask = pancreasImages, 
                          img_id = "ImageNb"), 
               regexp = "Segmentation masks must only contain one channel.",
               fixed = TRUE)
  
  cur_mask <- pancreasMasks
  imageData(cur_mask[[1]])[1,1] <- 8.24
  
  expect_error(cytoviewer(image = pancreasImages, 
                          mask = cur_mask, 
                          img_id = "ImageNb"), 
               regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)
  
  cur_mask_2 <- pancreasMasks
  mcols(cur_mask_2)[["ImageNb"]] <- c(1,1,2)
  
  expect_error(cytoviewer(image = pancreasImages, 
                          mask = cur_mask_2, 
                          img_id = "ImageNb"), 
               regexp = "Entries in the 'mcols(mask)[,img_id]' slot are not unique.",
               fixed = TRUE)
  
  mcols(cur_mask_2)[["ImageNb"]] <- NULL
  
  expect_error(cytoviewer(image = pancreasImages, 
                          mask = cur_mask_2, 
                          img_id = "ImageNb"), 
               regexp = "'img_id' not in 'mcols(mask)'.",
               fixed = TRUE)
  
  cur_object_3 <- pancreasSCE
  colData(cur_object_3)[["CellNb"]] <- as.character(colData(cur_object_3)[["CellNb"]])
  
  expect_error(cytoviewer(mask = pancreasMasks, 
                          object = cur_object_3, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "Cell ids should only contain numeric integer values.",
               fixed = TRUE)
  
  cur_object_4 <- pancreasSCE
  colData(cur_object_4)[["CellNb"]][1] <- 824.1
  
  expect_error(cytoviewer(mask = pancreasMasks, 
                          object = cur_object_4, 
                          img_id = "ImageNb", 
                          cell_id = "CellNb"), 
               regexp = "Cell ids should only contain numeric integer values.",
               fixed = TRUE)
  
  cur_image_3 <- pancreasImages
  names(cur_image_3) <- NULL
  expect_error(cytoviewer(image = cur_image_3), 
               regexp = "Please specify the 'names' of the 'image' object.",
               fixed = TRUE)
  
  cur_mask_3 <- pancreasMasks
  names(cur_mask_3) <- NULL
  expect_error(
      cytoviewer(image = pancreasImages,
                 mask = cur_mask_3,
                 img_id = "ImageNb"),
      regexp = "Please specify the 'names' of the 'mask' object.",
      fixed = TRUE)

  expect_error(
      cytoviewer(mask = pancreasMasks,
                 object = pancreasSCE,
                 img_id = "ImageNb",
                 cell_id = "CellNb"),
      regexp = "Please provide a 'coords' argument.",
      fixed = TRUE)

  expect_error(
      cytoviewer(mask = pancreasMasks,
                 object = pancreasSCE,
                 img_id = "ImageNb",
                 cell_id = "CellNb",
                 coords = 1),
      regexp = "'coords' must be a character vector of length 2.",
      fixed = TRUE)

  expect_error(
      cytoviewer(mask = pancreasMasks,
                 object = pancreasSCE,
                 img_id = "ImageNb",
                 cell_id = "CellNb",
                 coords = "Pos_X"),
      regexp = "'coords' must be a character vector of length 2.",
      fixed = TRUE)

  expect_error(
      cytoviewer(mask = pancreasMasks,
                 object = pancreasSCE,
                 img_id = "ImageNb",
                 cell_id = "CellNb",
                 coords = c("Pos_X", "notAColumn")),
      regexp = "'coords' entries not found in 'colData(object)'.",
      fixed = TRUE)

  library(SpatialExperiment)
  cur_spe_v <- SpatialExperiment(
      assays = list(counts = counts(pancreasSCE)),
      sample_id = pancreasSCE$ImageName)
  colData(cur_spe_v) <- colData(pancreasSCE)
  spatialCoords(cur_spe_v) <- as.matrix(
      as.data.frame(colData(pancreasSCE))[,
          c("Pos_X", "Pos_Y")])

  expect_error(
      cytoviewer(mask = pancreasMasks,
                 object = cur_spe_v,
                 img_id = "ImageNb",
                 cell_id = "CellNb",
                 coords = c("Pos_X", "notAColumn")),
      regexp =
          "'coords' entries not found in 'spatialCoordsNames(object)'.",
      fixed = TRUE)
})


test_that("cytoviewer: auxiliary functions works", {

  #General help
  cur_out <-.general_help()
  expect_length(cur_out, 14)
  expect_equal(unlist(cur_out[[1]]$children), "Using the Shiny application")
  expect_true(is.character(unlist(cur_out[[2]]$children)))
  expect_equal(unlist(cur_out[[3]]$children), "Interface")
  expect_true(is.character(unlist(cur_out[[4]]$children)))
  expect_equal(unlist(cur_out[[5]]$children), "Image-level visualization")
  expect_true(is.character(unlist(cur_out[[6]]$children)))
  expect_equal(unlist(cur_out[[7]]$children), "Cell-level visualization")
  expect_true(is.character(unlist(cur_out[[8]]$children)))
  expect_equal(unlist(cur_out[[9]]$children), "Points-level visualization")
  expect_true(is.character(unlist(cur_out[[10]]$children)))
  expect_equal(unlist(cur_out[[11]]$children), "General controls")
  expect_true(is.character(unlist(cur_out[[12]]$children)))
  expect_equal(unlist(cur_out[[13]]$children), "Image download")
  expect_true(is.character(unlist(cur_out[[14]]$children)))

})

test_that(".plotSpatial_cytoviewer function works", {

    library(cytomapper)
    library(SingleCellExperiment)
    library(S4Vectors)
    data("pancreasSCE")

    cur_sce <- pancreasSCE[, pancreasSCE$ImageNb == 1]

    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"))
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$x, cur_sce$Pos_X)
    expect_equal(p$data$y, cur_sce$Pos_Y)

    # NODES
    # node_color_by categorical
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "CellType")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$CellType, cur_sce$CellType)

    # node_color_by numeric
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "Area")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$Area, cur_sce$Area)

    # node_color_by logical
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "Pattern")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$Pattern, cur_sce$Pattern)

    # node_color_fix
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_fix = "red")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_fix = "black")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # node_color_by + node_color_fix (fix overrides mapping)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by  = "CellType",
        node_color_fix = "red")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # node_shape_by categorical
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_shape_by = "CellType")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$CellType,
                 as.character(cur_sce$CellType))

    # node_shape_by logical -> coerced to character
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_shape_by = "Pattern")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$Pattern,
                 as.character(cur_sce$Pattern))

    # node_shape_fix
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_shape_fix = 19L)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_shape_fix = 21L)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # node_shape_by + node_shape_fix (fix overrides mapping)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_shape_by  = "CellType",
        node_shape_fix = 15L)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # node_size_by - Numerical only
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_size_by = "Area")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$Area, cur_sce$Area)

    # node_size_fix
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_size_fix = 5)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # node_size_by + node_size_fix (fix overrides mapping)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_size_by  = "Area",
        node_size_fix = 5)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # combined color + shape + size
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "CellType",
        node_shape_by = "Pattern",
        node_size_by  = "Area")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$CellType, cur_sce$CellType)
    expect_equal(p$data$Pattern,
                 as.character(cur_sce$Pattern))
    expect_equal(p$data$Area, cur_sce$Area)

    # post-hoc scale addition (mirrors cytoviewer usage)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "CellType") +
        ggplot2::scale_color_manual(
            values = c(celltype_A = "yellow",
                       celltype_B = "blue",
                       celltype_C = "red"))
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # EDGES
    n <- ncol(cur_sce)
    test_hits <- SelfHits(
        from  = c(seq_len(n - 1L), seq_len(n - 1L) + 1L),
        to    = c(seq_len(n - 1L) + 1L, seq_len(n - 1L)),
        nnode = n)
    colPair(cur_sce, "test_graph") <- test_hits

    # directed edges
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges  = TRUE,
        colPairName = "test_graph",
        directed    = TRUE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))
    expect_equal(p$data$x, cur_sce$Pos_X)
    expect_equal(p$data$y, cur_sce$Pos_Y)

    # undirected 
    p2 <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges  = TRUE,
        colPairName = "test_graph",
        directed    = FALSE)
    expect_s3_class(p2, "ggraph")
    expect_silent(print(p2))

    cur_g1 <- igraph::as.igraph(attributes(p$data)$graph)
    cur_g2 <- igraph::as.igraph(attributes(p2$data)$graph)
    expect_gt(igraph::ecount(cur_g1),
              igraph::ecount(cur_g2))

    # edge_color_fix (directed)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_color_fix = "red")
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # edge_color_fix (undirected)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_color_fix = "red",
        directed       = FALSE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # edge_width_fix (directed)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_width_fix = 0.1)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # edge_width_fix (undirected)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_width_fix = 0.1,
        directed       = FALSE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # edge_color_fix + edge_width_fix (directed)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_color_fix = "blue",
        edge_width_fix = 0.5)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # edge_color_fix + edge_width_fix (undirected)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        edge_color_fix = "blue",
        edge_width_fix = 0.5,
        directed       = FALSE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # nodes_first = FALSE (directed)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges  = TRUE,
        colPairName = "test_graph",
        nodes_first = FALSE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # nodes_first = FALSE (undirected)
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges  = TRUE,
        colPairName = "test_graph",
        nodes_first = FALSE,
        directed    = FALSE)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    # combined: color + shape + size + edges + fixes
    p <- .plotSpatial_cytoviewer(
        cur_sce, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        node_color_by  = "CellType",
        node_shape_by  = "Pattern",
        node_size_by   = "Area",
        edge_color_fix = "grey50",
        edge_width_fix = 0.3)
    expect_s3_class(p, "ggraph")
    expect_silent(print(p))

    #Spatial Experiment
    library(SpatialExperiment)
    cur_spe <- SpatialExperiment(
        assays    = list(counts = counts(cur_sce)),
        sample_id = as.character(cur_sce$ImageNb))
    colData(cur_spe) <- colData(cur_sce)
    colData(cur_spe)[, c("Pos_X", "Pos_Y")] <- NULL
    spatialCoords(cur_spe) <- as.matrix(
        as.data.frame(colData(cur_sce))[, c("Pos_X", "Pos_Y")])
    colPairs(cur_spe) <- colPairs(cur_sce)

    p_spe <- .plotSpatial_cytoviewer(
        cur_spe, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        node_color_by = "CellType")
    expect_s3_class(p_spe, "ggraph")
    expect_silent(print(p_spe))
    expect_equal(unname(p_spe$data$x), cur_sce$Pos_X)
    expect_equal(unname(p_spe$data$y), cur_sce$Pos_Y)
    expect_equal(p_spe$data$CellType,
                 colData(cur_sce)$CellType)

    p_spe_e <- .plotSpatial_cytoviewer(
        cur_spe, img_id = "ImageNb",
        coords = c("Pos_X", "Pos_Y"),
        draw_edges     = TRUE,
        colPairName    = "test_graph",
        node_color_by  = "CellType",
        edge_color_fix = "grey50")
    expect_s3_class(p_spe_e, "ggraph")
    expect_silent(print(p_spe_e))
})

test_that("select_node_color helper works", {

    library(cytomapper)
    data("pancreasSCE")

    # early return: node_color_by is NULL
    mock_input <- list(node_color_by = NULL)
    expect_null(.select_node_color(mock_input, pancreasSCE))

    # early return: node_color_by_selection is NULL
    mock_input <- list(node_color_by           = "CellType",
                       node_color_by_selection = NULL)
    expect_null(.select_node_color(mock_input, pancreasSCE))

    # some node_color_advanced{i} NULL -> "" in vec -> final NULL
    mock_input <- list(node_color_by           = "CellType",
                       node_color_by_selection = c("celltype_A", "celltype_B"),
                       node_color_advanced1    = "#FF0000")
    expect_null(.select_node_color(mock_input, pancreasSCE))

    # all colors present, character column -> named character vector
    mock_input <- list(node_color_by           = "CellType",
                       node_color_by_selection = c("celltype_A", "celltype_B",
                                                   "celltype_C"),
                       node_color_advanced1    = "#FF0000",
                       node_color_advanced2    = "#00FF00",
                       node_color_advanced3    = "#0000FF")
    result <- .select_node_color(mock_input, pancreasSCE)
    expect_type(result, "character")
    expect_equal(names(result),
                 c("celltype_A", "celltype_B", "celltype_C"))

    # logical column -> logical names
    cur_sce_logical <- pancreasSCE
    cur_sce_logical$IsActivated <- as.logical(seq_len(ncol(pancreasSCE)) %% 2)
    mock_input_logical <- list(node_color_by           = "IsActivated",
                                node_color_by_selection = c("0", "1"),
                                node_color_advanced1    = "#FF0000",
                                node_color_advanced2    = "#00FF00")
    result_logical <- .select_node_color(mock_input_logical, cur_sce_logical)
    expect_type(result_logical, "character")
    expect_equal(names(result_logical), c("FALSE", "TRUE"))
})

test_that("select_node_shape helper works", {

    library(cytomapper)
    data("pancreasSCE")

    # early return: node_shape_by_selection is NULL
    mock_input <- list(node_shape_by_selection = NULL)
    expect_null(.select_node_shape(mock_input, pancreasSCE))

    # node_shape_advanced{i} NULL -> NA_integer_ -> returns NULL
    mock_input <- list(node_shape_by           = "CellType",
                       node_shape_by_selection = c("celltype_A", "celltype_B"),
                       node_shape_advanced1    = "19")
    expect_null(.select_node_shape(mock_input, pancreasSCE))

    # all shapes present, character column -> named integer vector
    mock_input <- list(node_shape_by           = "CellType",
                       node_shape_by_selection = c("celltype_A", "celltype_B",
                                                   "celltype_C"),
                       node_shape_advanced1    = "19",
                       node_shape_advanced2    = "15",
                       node_shape_advanced3    = "17")
    result <- .select_node_shape(mock_input, pancreasSCE)
    expect_type(result, "integer")
    expect_equal(unname(result), c(19L, 15L, 17L))
    expect_equal(names(result),
                 c("celltype_A", "celltype_B", "celltype_C"))

    # logical column -> logical names
    cur_sce_logical <- pancreasSCE
    cur_sce_logical$IsActivated <- as.logical(seq_len(ncol(pancreasSCE)) %% 2)
    mock_input_logical <- list(node_shape_by           = "IsActivated",
                                node_shape_by_selection = c("0", "1"),
                                node_shape_advanced1    = "19",
                                node_shape_advanced2    = "15")
    result_logical <- .select_node_shape(mock_input_logical, cur_sce_logical)
    expect_type(result_logical, "integer")
    expect_equal(names(result_logical), c("FALSE", "TRUE"))
    expect_equal(unname(result_logical), c(19L, 15L))
})
