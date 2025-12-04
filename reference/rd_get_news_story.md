# Retrieve Full News Story from a Refinitiv RDP (JSON) Connection

This function retrieves a full news story identified by its story ID via
a Refinitiv JSON connection. In the RDP response the story text may be
found in different places:

## Usage

``` r
rd_get_news_story(
  RDObject = RefinitivJsonConnect(),
  story_id = NULL,
  raw_output = FALSE,
  debug = FALSE,
  renderHTML = FALSE
)
```

## Arguments

- RDObject:

  A connection object returned by
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md).
  Defaults to
  [`RefinitivJsonConnect()`](https://greengrassblueocean.github.io/RefinitivR/reference/RefinitivJsonConnect.md)
  if not supplied.

- story_id:

  Vector of story IDs.

- raw_output:

  If `TRUE`, returns the raw list of responses.

- debug:

  If `TRUE`, prints debugging messages.

- renderHTML:

  If `TRUE`, the function will open the combined HTML in a browser
  viewer, and also return the HTML string.

## Value

If `raw_output = FALSE` (the default), a named list keyed by story_id,
where each element is a sub-list containing:

- `inline`: Plain text content from `inlineData`.

- `html`: HTML content from `inlineXML`.

- `headline`: Extracted headline.

- `versionCreated`: Version created timestamp.

- `urgency`: Urgency level.

If no content is available, the sub-list fields will be empty or NA. For
`renderHTML = TRUE`, returns a single combined HTML string. If
`raw_output = TRUE`, returns a named list of raw responses keyed by
story_id.

## Details

\- If the response comes from the legacy UDF service, the story is
expected in the `story$storyHtml` element.

\- If the response comes from the RDP service, the content will be
located under `newsItem$contentSet`. In that case, the function first
checks for HTML content in `newsItem$contentSet$inlineXML` (if
available) and, if not, in `newsItem$contentSet$inlineData`.

If no content is found, it falls back to the headline (from
`newsItem$itemMeta$title`) formatted as HTML. If `renderHTML` is TRUE,
any plain URLs in the resulting HTML will be converted to clickable
links via a helper function
[`make_links_clickable()`](https://greengrassblueocean.github.io/RefinitivR/reference/make_links_clickable.md)
(which you should define elsewhere in your package), and the combined
HTML is opened in the viewer. Base64-encoded images in the response are
decoded and embedded.
