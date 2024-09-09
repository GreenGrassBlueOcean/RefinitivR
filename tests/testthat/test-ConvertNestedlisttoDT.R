# Define test cases


test_that("ConvertNestedlisttoDT handles basic functionality", {
  nested_list_1 <- list(
    list(a = 1, b = 2.5, c = "hello"),
    list(a = 3, b = 4.5, c = "world"),
    list(a = 5, b = 6.5, c = "foo", d = TRUE)
  )
  result_1 <- ConvertNestedlisttoDT(nested_list_1)
  expected_1 <- data.table::data.table(
    a = c(1, 3, 5),
    b = c(2.5, 4.5, 6.5),
    c = c("hello", "world", "foo"),
    d = c(NA, NA, TRUE)
  )
  expect_equal(result_1, expected_1)
})

test_that("ConvertNestedlisttoDT handles missing columns", {
  nested_list_2 <- list(
    list(a = 1, b = 2.5),
    list(a = 3, c = "world"),
    list(b = 4.5, c = "foo")
  )
  result_2 <- ConvertNestedlisttoDT(nested_list_2)
  expected_2 <- data.table::data.table(
    a = c(1, 3, NA),
    b = c(2.5, NA, 4.5),
    c = c(NA, "world", "foo")
  )
  expect_equal(result_2, expected_2)
})

test_that("ConvertNestedlisttoDT handles all elements missing", {
  nested_list_3 <- list(
    list(a = NA, b = NA),
    list(a = NA, b = NA),
    list(a = NA, b = NA)
  )
  result_3 <- ConvertNestedlisttoDT(nested_list_3)
  expected_3 <- data.table::data.table(
    a = rep(NA, 3),
    b = rep(NA, 3)
  )
  expect_equal(result_3, expected_3)
})

test_that("ConvertNestedlisttoDT handles single element lists", {
  nested_list_4 <- list(
    list(a = 1),
    list(a = 2),
    list(a = 3)
  )
  result_4 <- ConvertNestedlisttoDT(nested_list_4)
  expected_4 <- data.table::data.table(
    a = c(1, 2, 3)
  )
  expect_equal(result_4, expected_4)
})

test_that("ConvertNestedlisttoDT handles empty list", {
  nested_list_5 <- list()
  result_5 <- ConvertNestedlisttoDT(nested_list_5)
  expected_5 <- data.table::data.table()
  expect_equal(result_5, expected_5)
})

test_that("ConvertNestedlisttoDT handles real test case", {
nested_list_6 <- list(list(CentralBankName = "Bank Indonesia", DocumentTitle = "Indonesia, Policy Rates, 7-Day Reverse Repo, Reuters Polls, Monthly, Bank Indonesia",
                                RIC = "IDCBRR=ECI", ObservationValue = 6.25)
                           , list(CentralBankName = "Bank Indonesia", DocumentTitle = "Indonesia, ID 7-DAY REVERSE REPO RATE - LONG-TERM OUTLOOK - OCBC, Long-Term Outlook, Quarterly, Reuters",
                                  RIC = "pIDREVREQP=4295887834", ObservationValue = 5))
result_6 <- ConvertNestedlisttoDT(nested_list_6)
expected_6 <-structure(list(CentralBankName = c("Bank Indonesia", "Bank Indonesia")
                            , DocumentTitle = c("Indonesia, Policy Rates, 7-Day Reverse Repo, Reuters Polls, Monthly, Bank Indonesia",
                                                "Indonesia, ID 7-DAY REVERSE REPO RATE - LONG-TERM OUTLOOK - OCBC, Long-Term Outlook, Quarterly, Reuters")
                            , RIC = c("IDCBRR=ECI", "pIDREVREQP=4295887834"), ObservationValue = c(6.25, 5)), row.names = c(NA, -2L)
, class = c("data.table", "data.frame"))
 expect_equal(result_6, expected_6)
 })
