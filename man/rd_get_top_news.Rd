% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_news.r
\name{rd_get_top_news}
\alias{rd_get_top_news}
\title{Retrieve Top News Packages from a Refinitiv RDP (JSON) Connection}
\usage{
rd_get_top_news(
  RDObject = RefinitivJsonConnect(),
  group = NULL,
  page = NULL,
  raw_output = FALSE,
  debug = FALSE
)
}
\arguments{
\item{RDObject}{A connection object returned by \code{RefinitivJsonConnect()}. If not supplied,
defaults to \code{RefinitivJsonConnect()}.}

\item{group}{Optional character string (or regular expression) to filter the top news groups by name.}

\item{page}{Optional character string (or regular expression) to filter the pages by name.}

\item{raw_output}{If TRUE, returns the raw JSON response from the top-news endpoint.}

\item{debug}{If TRUE, prints debugging messages.}
}
\value{
A data frame with one row per top news page and the following columns:
  \code{group}, \code{page_name}, \code{po}, \code{revisionId}, \code{revisionDate},
  \code{topNewsId}, \code{storyId}, \code{title} (news headline), and \code{snippet} (news summary).
}
\description{
This function retrieves the top news packages from the Refinitiv RDP service using the
endpoint `/data/news/v1/top-news`. The endpoint returns a nested JSON structure containing
top news groups. Each group contains one or more pages, where each page represents a specific
news category.
}
\details{
**Overview of Top News Groups and Pages:**

- **Main**: Typically includes pages such as "Front Page", "Central Banks & Global Economy",
  "Sustainable Finance", "World News", and "Regulation & Compliance".

- **Breakingviews**: Generally contains the page "Reuters Breakingviews".

- **Banking & Finance**: Often includes pages like "Financial Services", "Investment Banking",
  "IFR", and "Digital Finance & Crypto".

- **Markets**: Usually features pages such as "Global Markets", "Foreign Exchange",
  "Fixed Income", "Emerging Markets", and "IFR".

- **Commodities**: Contains pages like "Commodities", "Energy", "Metals",
  "Agricultural Commodities", and "Global Gas, Power & LNG".

- **Industries**: Contains pages such as "Technology, Media & Telecoms",
  "Heavy Industry & Transport", "Consumer & Retail", and "Healthcare & Pharma".

- **Regional**: Groups news by region with pages such as "Australia & New Zealand",
  "Japan & the Koreas", "Greater China", "Southeast Asia", "India & South Asia",
  "Middle East & Africa", "Europe & Russia", "United Kingdom", "Latin America",
  "United States", and "Canada".

- **National Languages**: Offers news in various languages with pages such as
  "日本語トップニュース", "路透中文新闻", "Deutschland", "L’essentiel de l'actualité",
  "Brasil", and "Россия".

- **Sports & Lifestyle**: Contains pages like "Sport" and "Lifestyle & Entertainment".

- **AWP Top News**: Includes pages such as "AWP German Top News" and "AWP French Top News".

In addition to returning key fields from the top news packages (group, page name, revision information,
and the **topNewsId**), this function now makes an additional GET call for each page by calling
`/data/news/v1/top-news/<topNewsId>`. This call retrieves the actual story details including the story
identifier (in **storyId**), the title (in **text**), and a summary (in **snippet**) that can subsequently
be used with \code{rd_get_news_story}.
}
\examples{
\dontrun{
  # Retrieve all top news packages along with the actual story details:
  top_news <- rd_get_top_news()

  # Retrieve only the "Main" group:
  top_news_main <- rd_get_top_news(group = "Main")

  # Retrieve only pages whose name contains "Front":
  top_news_front <- rd_get_top_news(page = "Front")
}
}
