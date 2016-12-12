context("GetCodeList")

test_that("GetCodeList returns correct empty object for no codes", {
  expect_error(gcl <<- GetCodeList("agriculture", "aproduction", "measuredItemCPC", character()), NA)
  
  expect_is(gcl, "data.table")
  expect_identical(names(gcl), c("code", "description", "selectionOnly", "type", "startDate", 
                                 "endDate"))
  expect_equal(ncol(gcl), 6)
  expect_equal(nrow(gcl), 0)
})

test_that("GetCodeList processing works", {
  testinput <- structure(list(success = TRUE, results = list(structure(list(
    code = "21499.90", description = "Other prepared and preserved fruit and nuts, n.e.c.", 
    id = 4898, subtreeSelectionOnly = FALSE, type = NULL, startDate = NULL, 
    endDate = NULL), .Names = c("code", "description", "id", 
    "subtreeSelectionOnly", "type", "startDate", "endDate")), structure(list(
    code = "H_INDUSE_BIOFUEL", description = "Biofuel items", 
    id = 4886, subtreeSelectionOnly = FALSE, type = NULL, startDate = NULL, 
    endDate = NULL), .Names = c("code", "description", "id", 
    "subtreeSelectionOnly", "type", "startDate", "endDate")), structure(list(
    code = "39120.18", description = "Marc of grape", id = 4924, 
    subtreeSelectionOnly = FALSE, type = NULL, startDate = NULL, 
    endDate = NULL), .Names = c("code", "description", "id", 
    "subtreeSelectionOnly", "type", "startDate", "endDate"))), total = 3), .Names = c("success", 
    "results", "total"))
  
  testoutput <- structure(list(code = c("21499.90", "H_INDUSE_BIOFUEL", "39120.18"
    ), description = c("Other prepared and preserved fruit and nuts, n.e.c.", 
    "Biofuel items", "Marc of grape"), selectionOnly = c(FALSE, FALSE, 
    FALSE), type = c(NA_character_, NA_character_, NA_character_), 
    startDate = c(NA_character_, NA_character_, NA_character_
    ), endDate = c(NA_character_, NA_character_, NA_character_
    )), .Names = c("code", "description", "selectionOnly", "type", 
    "startDate", "endDate"), row.names = c(NA, -3L), class = c("data.table", 
    "data.frame"))
  
  
  expect_equal(GetCodeList.processResult(testinput), testoutput)
  expect_equal(GetCodeList.buildJSON("codes"), list(codes = I("codes")))
})
