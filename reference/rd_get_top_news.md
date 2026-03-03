# Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection, Then Fetch Stories

This function retrieves the top news packages from the Refinitiv RDP
service using the endpoint \`/data/news/v1/top-news\`. The endpoint
returns a nested JSON structure containing top news groups. Each group
contains one or more pages, where each page represents a specific news
category.

## Usage

``` r
rd_get_top_news(
  RDObject = rd_connection(),
  group = NULL,
  page = NULL,
  raw_output = FALSE,
  debug = FALSE,
  cache = NULL
)
```

## Arguments

- RDObject:

  A connection object returned by
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md).
  If not supplied, defaults to
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md).

- group:

  Optional character string (or regular expression) to filter top news
  groups by name.

- page:

  Optional character string (or regular expression) to filter pages by
  name.

- raw_output:

  If `TRUE`, returns the raw JSON response (list) for each page in a
  named list keyed by `topNewsId`.

- debug:

  If `TRUE`, prints debugging messages.

- cache:

  Controls caching. `NULL` (default) defers to
  `getOption("refinitiv_cache", FALSE)`. `TRUE` uses the function
  default TTL (60 s). `FALSE` disables caching. A positive numeric value
  sets the cache TTL in seconds. See
  [`rd_ClearCache`](https://greengrassblueocean.github.io/RefinitivR/reference/rd_ClearCache.md).

## Value

A data frame (by default) with one row per story and the following
columns:

- `group`

- `page_name`

- `po`

- `revisionId`

- `revisionDate`

- `topNewsId`

- `storyId`

- `title` (the headline)

- `snippet` (the short text summary)

If `raw_output = TRUE`, a named list of raw responses, keyed by each
`topNewsId`, is returned.

## Details

Overview of Top News Groups and Pages:

- Main:

  Pages such as "Front Page", "Central Banks & Global Economy",
  "Sustainable Finance", "World News", and "Regulation & Compliance".

- Breakingviews:

  Generally contains the page "Reuters Breakingviews".

- Banking & Finance:

  Pages like "Financial Services", "Investment Banking", "IFR", and
  "Digital Finance & Crypto".

- Markets:

  Pages such as "Global Markets", "Foreign Exchange", "Fixed Income",
  "Emerging Markets", and "IFR".

- Commodities:

  Pages like "Commodities", "Energy", "Metals", "Agricultural
  Commodities", and "Global Gas, Power & LNG".

- Industries:

  Pages such as "Technology, Media & Telecoms", "Heavy Industry &
  Transport", "Consumer & Retail", and "Healthcare & Pharma".

- Companies:

  Sub-groups such as "U.S. Companies", "European Companies", and "Asian
  Companies".

- Regional:

  Pages such as "Australia & New Zealand", "Japan & the Koreas",
  "Greater China", "Southeast Asia", "India & South Asia", "Middle East
  & Africa", "Europe & Russia", "United Kingdom", "Latin America",
  "United States", and "Canada".

- National Languages:

  News in various languages including Japanese, Chinese, German, French,
  Portuguese, and Russian top news.

- Sports & Lifestyle:

  Pages like "Sport" and "Lifestyle & Entertainment".

- AWP Top News:

  Pages such as "AWP German Top News" and "AWP French Top News".

In addition to returning key fields from the top news packages (group,
page name, revision information, and the `topNewsId`), this function
makes an additional GET call for each page by calling
`/data/news/v1/top-news/<topNewsId>`. This call retrieves the actual
story details including the story identifier (in `storyId`), the title
(in `text`), and a summary (in `snippet`) that can subsequently be used
with `rd_get_news_story`.

## Examples

``` r
if (FALSE) { # \dontrun{

# Examples for Filtering:
# Example 1: Retrieve all top news from the "Main" group
main_news <- rd_get_top_news(group = "^Main$")

# Example 2: Retrieve only the "Front Page" of top news by filtering on page name
front_page_news <- rd_get_top_news(page = "^Front Page$")

# Example 3: Retrieve stories from the "Sports & Lifestyle" group where the page is "Sport"
sports_news <- rd_get_top_news(group = "Sports & Lifestyle", page = "Sport")

# Example 4: Filtering yields no results (empty data frame)
no_news <- rd_get_top_news(group = "NonExistent")
} # }
```
