test_that("cytoviewer: app testing works", {
  
  # Load datasets 
  library(cytomapper)
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  # App testing for different input settings
  expect_silent(app <- cytoviewer(image = pancreasImages, 
                                  mask = pancreasMasks, 
                                  object = pancreasSCE, 
                                  img_id = "ImageNb", 
                                  cell_id = "CellNb"))
  expect_equal(class(app), "shiny.appobj")
  expect_equal(class(app$serverFuncSource()), "function")
  
  expect_silent(app_1 <- cytoviewer(image = pancreasImages))
  expect_equal(class(app_1), "shiny.appobj")
  expect_equal(class(app_1$serverFuncSource()), "function")
  
  expect_silent(app_2 <- cytoviewer(image = pancreasImages, 
                                    mask = pancreasMasks, 
                                    img_id = "ImageNb"))
  expect_equal(class(app_2), "shiny.appobj")
  expect_equal(class(app_2$serverFuncSource()), "function")
  
  expect_silent(app_3 <- cytoviewer(mask = pancreasMasks, 
                                    object = pancreasSCE, 
                                    img_id = "ImageNb", 
                                    cell_id = "CellNb"))
  expect_equal(class(app_3), "shiny.appobj")
  expect_equal(class(app_3$serverFuncSource()), "function")
  
  })
  