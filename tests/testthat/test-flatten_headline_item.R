test_that("flatten_headline_item returns a flattened list with correct fields", {
  # Create a dummy headline JSON structure
  dummy_headline <- list(
    storyId = "urn:newsml:reuters.com:20250312:nABC123:1",
    newsItem = list(
      `_version` = 1L,
      contentMeta = list(
        creator = list(list(`_qcode` = "NS:RTRS", `_role` = "sRole:source")),
        infoSource = list(list(`_qcode` = "NS:RTRS", `_role` = "sRole:source")),
        language = list(list(`_tag` = "en")),
        subject = list(list(`_qcode` = "G:1"), list(`_qcode` = "M:1QD")),
        urgency = list(`$` = 3L)
      ),
      itemMeta = list(
        firstCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
        versionCreated = list(`$` = "2025-03-12T15:55:31.127Z"),
        title = list(list(`$` = "Dummy headline"))
      )
    )
  )

  flat <- flatten_headline_item(dummy_headline)

  expect_type(flat, "list")
  expect_equal(flat$storyId, "urn:newsml:reuters.com:20250312:nABC123:1")
  expect_equal(flat$version, 1)
  expect_equal(flat$urgency, 3)
  expect_equal(flat$firstCreated, "2025-03-12T15:55:31.127Z")
  expect_equal(flat$versionCreated, "2025-03-12T15:55:31.127Z")
  expect_equal(flat$title, "Dummy headline")
  expect_equal(flat$creator, "NS:RTRS")
  expect_equal(flat$infoSource, "NS:RTRS")
  expect_equal(flat$language, "en")
  expect_equal(flat$subject, "G:1,M:1QD")
})

test_that("flatten_headline_item returns NULL for invalid input", {
  expect_null(flatten_headline_item(NULL))
  expect_null(flatten_headline_item(list(a = 1, b = 2)))
})
