test_that("CorrectCustomInstrument works", {

  expect_equal(CorrectCustomInstrument(symbol = "test" , UUID = "ABCDE-123456")
              , "S)test.ABCDE-123456")
  expect_equal(CorrectCustomInstrument(symbol = "S)test.ABCDE-123456" , UUID = "ABCDE-123456")
               , "S)test.ABCDE-123456")
  expect_equal(CorrectCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456")
               , "S)test.ABCDE-123456")
  expect_equal(CorrectCustomInstrument(symbol = "S)test" , UUID = "ABCDE-123456")
               , "S)test.ABCDE-123456")



})

test_that("CorrectCustomInstrument fails when it should", {
  expect_error(CorrectCustomInstrument(symbol = "testABCDE-123456" , UUID = "ABCDE-123456")
               , "Custom Symbol 'testABCDE-123456' can not be corrected by CorrectCustomInstrument Check manually", fixed = T)


})

test_that("CheckifCustomInstrument works", {
  expect_false(CheckifCustomInstrument(symbol = "test" , UUID = "ABCDE-123456"))
  expect_false(CheckifCustomInstrument(symbol = "test.ABCDE-123456" , UUID = "ABCDE-123456"))
  expect_true(CheckifCustomInstrument(symbol = "S)test.ABCDE-123456" , UUID = "ABCDE-123456"))


})



test_that("CustomInstrumentHolidayBuilder works", {

  expect_equal(CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31")
               , reasons = c("Special Bank Holiday 1", "Special Bank Holiday 2"))
  ,structure(list(list(date = "2023-12-01", reason = "Special Bank Holiday 1"),
                  list(date = "2023-12-31", reason = "Special Bank Holiday 2"))
             , class = "Refinitiv_holidays"))

  expect_equal(CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "2023-12-31"))

               ,structure(list(list(date = "2023-12-01"), list(date = "2023-12-31")), class = "Refinitiv_holidays")
               )



})

test_that("CustomInstrumentHolidayBuilder fails when it should", {

  expect_error( CustomInstrumentHolidayBuilder(dates = NULL)
              , "dates can not be NULL, but should be string in formt YYYY-MM-DD"
              , fixed = TRUE)

  expect_error(CustomInstrumentHolidayBuilder(dates = c("2023-12-01", "12-31-2023"))
               , "dates should be string and should be all in correct format YYYY-MM-DD"
               , fixed = TRUE)


})
