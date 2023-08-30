test_that("EikonGetNewsHeadlines works", {


  Eikon <- check_Eikonapi(testMode = "write")
  CheckHeadlines <- try(EikonGetNewsHeadlines(EikonObject = Eikon
                                              , query = "R:MSFT.O", count = 2))

  CorrectOutput <- list(displayDirection = "character", documentType = "character",
       firstCreated = "character", isAlert = "logical", language = "character",
       reportCode = "character", sourceCode = "character", sourceName = "character",
       storyId = "character", text = "character", versionCreated = "character")



  expect_equal(lapply(CheckHeadlines, class), CorrectOutput)
})
