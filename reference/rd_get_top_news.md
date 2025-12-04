# Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection, Then Fetch Stories

This function retrieves the top news packages from the Refinitiv RDP
service using the endpoint \`/data/news/v1/top-news\`. The endpoint
returns a nested JSON structure containing top news groups. Each group
contains one or more pages, where each page represents a specific news
category.

## Usage

``` r
rd_get_top_news(
  RDObject = RefinitivJsonConnect(),
  group = NULL,
  page = NULL,
  raw_output = FALSE,
  debug = FALSE
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

\*\*Overview of Top News Groups and Pages:\*\*

\- \*\*Main\*\*: Typically includes pages such as "Front Page", "Central
Banks & Global Economy", "Sustainable Finance", "World News", and
"Regulation & Compliance".

\- \*\*Breakingviews\*\*: Generally contains the page "Reuters
Breakingviews".

\- \*\*Banking & Finance\*\*: Often includes pages like "Financial
Services", "Investment Banking", "IFR", and "Digital Finance & Crypto".

\- \*\*Markets\*\*: Usually features pages such as "Global Markets",
"Foreign Exchange", "Fixed Income", "Emerging Markets", and "IFR".

\- \*\*Commodities\*\*: Contains pages like "Commodities", "Energy",
"Metals", "Agricultural Commodities", and "Global Gas, Power & LNG".

\- \*\*Industries\*\*: Contains pages such as "Technology, Media &
Telecoms", "Heavy Industry & Transport", "Consumer & Retail", and
"Healthcare & Pharma".

\- \*\*Companies\*\*: Typically includes sub-groups such as "U.S.
Companies", "European Companies", and "Asian Companies".

\- \*\*Regional\*\*: Groups news by region with pages such as "Australia
& New Zealand", "Japan & the Koreas", "Greater China", "Southeast Asia",
"India & South Asia", "Middle East & Africa", "Europe & Russia", "United
Kingdom", "Latin America", "United States", and "Canada".

\- \*\*National Languages\*\*: Offers news in various languages with
pages such as "日本語トップニュース", "路透中文新闻", "Deutschland",
"L’essentiel de l'actualité", "Brasil", and "Россия".

\- \*\*Sports & Lifestyle\*\*: Contains pages like "Sport" and
"Lifestyle & Entertainment".

\- \*\*AWP Top News\*\*: Includes pages such as "AWP German Top News"
and "AWP French Top News".

In addition to returning key fields from the top news packages (group,
page name, revision information, and the \*\*topNewsId\*\*), this
function makes an additional GET call for each page by calling
\`/data/news/v1/top-news/\<topNewsId\>\`. This call retrieves the actual
story details including the story identifier (in \*\*storyId\*\*), the
title (in \*\*text\*\*), and a summary (in \*\*snippet\*\*) that can
subsequently be used with `rd_get_news_story`.

## Examples
