% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_news.r
\name{rd_get_news_story}
\alias{rd_get_news_story}
\title{Retrieve Full News Story from a Refinitiv RDP (JSON) Connection}
\usage{
rd_get_news_story(
  RDObject = RefinitivJsonConnect(),
  story_id = NULL,
  raw_output = FALSE,
  debug = FALSE,
  renderHTML = FALSE
)
}
\arguments{
\item{RDObject}{A connection object returned by \code{RefinitivJsonConnect()}. Defaults to \code{RefinitivJsonConnect()} if not supplied.}

\item{story_id}{Vector of story IDs.}

\item{raw_output}{If \code{TRUE}, returns the raw list of responses.}

\item{debug}{If \code{TRUE}, prints debugging messages.}

\item{renderHTML}{If \code{TRUE}, the function will open the combined HTML in a browser viewer,
and also return the HTML string.}
}
\value{
If \code{raw_output = FALSE} (the default), a character vector of the story HTML
  (or a single combined HTML string if \code{renderHTML = TRUE}). Otherwise, the raw list
  of responses.
}
\description{
This function retrieves a full news story identified by its story ID via a
Refinitiv JSON connection. In the RDP response the story text may be found in
different places:
}
\details{
- If the response comes from the legacy UDF service, the story is expected in the
  \code{story$storyHtml} element.

- If the response comes from the RDP service, the content will be located under
  \code{newsItem$contentSet}. In that case, the function first checks for HTML content
  in \code{newsItem$contentSet$inlineXML} (if available) and, if not, in
  \code{newsItem$contentSet$inlineData}.

If \code{renderHTML} is TRUE, any plain URLs in the resulting HTML will be converted
to clickable links via a helper function \code{make_links_clickable()} (which you should
define elsewhere in your package), and the combined HTML is opened in the viewer.
}
