context("GetCodeTree")

test_that("buildJSON function works", {
  expect_equal(GetCodeTree.buildJSON("codes"), list(codes = I("codes")))
})

test_that("processResult works", {
  input <- list(structure(list(id = "/CPC/0/01/011/0111", uid = 4, code = "0111", 
                altCode = NULL, description = "Wheat", leaf = FALSE, subtreeSelectionOnly = FALSE, 
                codeForSearch = "0111", label = "[0111] Wheat", children = list(
                structure(list(id = "/CPC/0/01/011/0111/01111", uid = 3000, 
                code = "01111", altCode = NULL, description = "Wheat, seed", 
                leaf = TRUE, subtreeSelectionOnly = FALSE, codeForSearch = "01111", 
                label = "[01111] Wheat, seed", checked = FALSE), .Names = c("id", 
                "uid", "code", "altCode", "description", "leaf", "subtreeSelectionOnly", 
                "codeForSearch", "label", "checked")), structure(list(
                id = "/CPC/0/01/011/0111/01112", uid = 3001, code = "01112", 
                altCode = NULL, description = "Wheat, other", leaf = TRUE, 
                subtreeSelectionOnly = FALSE, codeForSearch = "01112", 
                label = "[01112] Wheat, other", checked = FALSE), .Names = c("id", 
                "uid", "code", "altCode", "description", "leaf", "subtreeSelectionOnly", 
                "codeForSearch", "label", "checked"))), expanded = FALSE, 
                checked = FALSE), .Names = c("id", "uid", "code", "altCode", 
                "description", "leaf", "subtreeSelectionOnly", "codeForSearch", 
                "label", "children", "expanded", "checked")))
  output <- structure(list(parent = "0111", children = "01111, 01112"), .Names = c("parent", 
                      "children"), row.names = c(NA, -1L), class = c("data.table", "data.frame")
                      )
  expect_equal(GetCodeTree.recurseTree(input), output)
})
