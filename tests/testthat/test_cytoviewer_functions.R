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
  
  expect_error(cytoviewer(image = pancreasImages, object = cur_object, img_id = "ImageNb", cell_id = "CellNb"), 
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fixed = TRUE)
  
  colData(cur_object)[["ImageNb"]] <- colData(pancreasSCE)[["ImageNb"]]
  colData(cur_object)[["CellNb"]] <- NULL
  
  expect_error(cytoviewer(image = pancreasImages, object = cur_object, img_id = "ImageNb", cell_id = "CellNb"), 
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fixed = TRUE)
  
  cur_image <- pancreasImages
  channelNames(cur_image) <- NULL 
  expect_error(cytoviewer(image = cur_image), 
               regexp = "Please specify the 'channelNames' of the 'image' object.",
               fixed = TRUE)
  
  cur_object_2 <- pancreasSCE
  rownames(cur_object_2)[1] <- "HistoneH3"
  
  expect_error(cytoviewer(image = pancreasImages, object = cur_object_2, cell_id = "CellNb"), 
               regexp = "The 'channelNames' of the images need to match the rownames of the 
           'object'.",
               fixed = TRUE)

  expect_error(cytoviewer(image = pancreasImages, object = cur_object_2, cell_id = "CellNb"), 
               regexp = "The 'channelNames' of the images need to match the rownames of the 
           'object'.",
               fixed = TRUE)
  
  cur_object_3 <- pancreasSCE
  colData(cur_object_3)[["ImageNb"]][1] <- 4
  
  expect_error(cytoviewer(image = pancreasImages, object = cur_object_3, img_id = "ImageNb", cell_id = "CellNb"), 
               regexp = "Please provide a unique 'image' for every sample stored in 'object'.",
               fixed = TRUE)
  
  expect_error(cytoviewer(mask = pancreasMasks, object = cur_object_3, img_id = "ImageNb", cell_id = "CellNb"), 
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
  
  
  expect_error(cytoviewer(image = pancreasImages, mask = pancreasImages, img_id = "ImageNb"), 
               regexp = "Segmentation masks must only contain one channel.",
               fixed = TRUE)
  
  cur_mask <- pancreasMasks
  imageData(cur_mask[[1]])[1,1] <- 8.24
  
  expect_error(cytoviewer(image = pancreasImages, mask = cur_mask, img_id = "ImageNb"), 
               regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)
  
  cur_mask_2 <- pancreasMasks
  mcols(cur_mask_2)[["ImageNb"]] <- c(1,1,2)
  
  expect_error(cytoviewer(image = pancreasImages, mask = cur_mask_2, img_id = "ImageNb"), 
               regexp = "Entries in the 'mcols(mask)[,img_id]' slot are not unique.",
               fixed = TRUE)
  
  mcols(cur_mask_2)[["ImageNb"]] <- NULL
  
  expect_error(cytoviewer(image = pancreasImages, mask = cur_mask_2, img_id = "ImageNb"), 
               regexp = "'img_id' not in 'mcols(mask)'.",
               fixed = TRUE)
  
  cur_object_3 <- pancreasSCE
  colData(cur_object_3)[["CellNb"]] <- as.character(colData(cur_object_3)[["CellNb"]])
  
  expect_error(cytoviewer(mask = pancreasMasks, object = cur_object_3, img_id = "ImageNb", cell_id = "CellNb"), 
               regexp = "Cell ids should only contain numeric integer values.",
               fixed = TRUE)
})


test_that("cytoviewer: auxiliary functions works", {
  
  #General help
  cur_out <-.general_help()
  expect_length(cur_out, 12)
  expect_equal(unlist(cur_out[[1]]$children), "Using the Shiny application")
  expect_true(is.character(unlist(cur_out[[2]]$children)))
  expect_equal(unlist(cur_out[[3]]$children), "Interface")
  expect_true(is.character(unlist(cur_out[[4]]$children)))
  expect_equal(unlist(cur_out[[5]]$children), "Image-level visualization")
  expect_true(is.character(unlist(cur_out[[6]]$children)))
  expect_equal(unlist(cur_out[[7]]$children), "Cell-level visualization")
  expect_true(is.character(unlist(cur_out[[8]]$children)))
  expect_equal(unlist(cur_out[[9]]$children), "General controls")
  expect_true(is.character(unlist(cur_out[[10]]$children)))
  expect_equal(unlist(cur_out[[11]]$children), "Image download")
  expect_true(is.character(unlist(cur_out[[12]]$children)))
  
})
