test_that("EikonGetNewsHeadlines works", {


  Eikon <- check_Eikonapi(ExecutionMode = "JSON")
  CheckHeadlines <- EikonGetNewsHeadlines(EikonObject = Eikon
                                              , query = "R:MSFT.O", count = 2)

  CorrectOutput <- list(query = "character", displayDirection = "character", documentType = "character",
                        firstCreated = "character", isAlert = "logical", language = "character",
                        reportCode = "character", sourceCode = "character", sourceName = "character",
                        storyId = "character", text = "character", versionCreated = "character")

  expect_equal(lapply(CheckHeadlines, class), CorrectOutput)
})

test_that("EikonGetNewsStory works", {

  Eikon <- check_Eikonapi(ExecutionMode = "JSON")

  story_id <- "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
  stories <- EikonGetNewsStory( story_id = story_id
                              , EikonObject = Eikon, debug = FALSE, raw_output = FALSE)

  CorrectOutput <- "<div class=\"storyContent\" lang=\"en\"><style type=\"text/css\">.storyContent * {border-color:inherit !important;outline-color:inherit !important;}</style><div class=\"storyframe\" id=\"storydiv\"><div class=\"TEXT\" id=\"storybody\" name=\"storybody\"><div id=\"storybodycontent0\"><pre><span class=\"storycontent\">Click the following link to watch video: <a href=\"https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts\" data-type=\"url video-url\" translate=\"no\">https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts</a></span></pre><span class=\"storycontent\">Source: Reuters<br/><br/>\nDescription: Wall Street ended sharply higher on Tuesday, lifted by Tesla, Nvidia and other megacap growth stocks after a drop in monthly job openings cemented expectations of a pause in interest rate hikes by the U.S. Federal Reserve. Lisa Bernhard has more.<br/>\nShort Link: <a href=\"https://refini.tv/44u5KGv\" data-type=\"url\" class=\"tr-link\" translate=\"no\">https://refini.tv/44u5KGv</a><br/><br/>\nVideo Transcript:<br/><br/>\nUS stocks ended sharply higher on Tuesday after a drop in monthly job openings fueled expectations that the US Federal Reserve would pause its interest rate hikes. The Dow gained just under 1%, while the S&amp;P 500 jumped nearly 1.5% and the NASDAQ surged on 1.75%. The strong gains came after the Labor Department's Job Openings and Labor Turnover Survey or JOLTS report showed job openings fell for a third straight month in July, signalling a loosening labor market. But Stephanie Lang, Chief Investment Officer at Homrich Berg Wealth Management, says fewer job openings are just one part of the equation when it comes to achieving the Fedâ\u0080\u0099s goal of 2% inflation.<br/><br/>\nThe JOLTS data that came out today, it was the lowest number of job openings since March of 2021, so starting to see a little bit of loosening there, but itâ\u0080\u0099s still incredibly strong. So, while I think the market took a sigh of relief to say, okay, this is really starting to play out, but itâ\u0080\u0099s really just lowered job openings, and not an uptick in the unemployment rate â\u0080\u0093 because thatâ\u0080\u0099s what really needs to happen to orchestrate a soft landing.<br/><br/>\nAmong individual movers, mega-cap tech stocks led the way, with Tesla rallying more than 7% and chipmaker Nvidia climbing more than 4% with more than $33 billion worth of shares traded in each. Shares of Alphabet gained nearly 3% after the company unveiled fresh artificial intelligence tools and partnerships, including with Nvidia, at its Google Next conference in San Francisco. And Verizon and AT&amp;T each rose more than 3% after Citi upgraded the telecom companies to â\u0080\u009cbuyâ\u0080\u009d from â\u0080\u009cneutral.â\u0080\u009d Focus now turns to the personal consumption expenditures index, the Fed's preferred inflation gauge, which is due on Thursday followed by Fridayâ\u0080\u0099s non-farm payrolls report, offering more clarity on the state of the labor market.</span></div></div></div><p class=\"line-break\"><br/></p><p class=\"tr-copyright\">(c) Copyright Thomson Reuters 2023. Click For Restrictions - http://about.reuters.com/fulllegal.asp</p></div>"

  #Remove line breaks that are somehow added when saving in the archivist database.
  stories <- lapply(stories, function(x)(gsub(pattern = ">\n<", replacement = "><", x = x)))
  CorrectOutput <- lapply(CorrectOutput, function(x)(gsub(pattern = ">\n<", replacement = "><", x = x)))

  expect_equal(stories, CorrectOutput)
})


test_that("EikonGetNewsStory works", {

 Eikon <- check_Eikonapi(ExecutionMode = "JSON")
 story_id2 <- "urn:newsml:newswire.refinitiv.com:20231025:nNRAqewpdh:1"
 stories2 <- EikonGetNewsStory( story_id = story_id2
                                 , EikonObject = Eikon, debug = TRUE, raw_output = TRUE)

 CorrectOutput2 <- list(list(story = list(headlineHtml = "<h1 class=\"storyHeadline\">\n<span class=\"headline\">AAPL Globe says Goldman Sachs recommends Apple, others</span> -\n<span class=\"source\" title=\"CNSWCH\">Canada Stockwatch</span>\n</h1>",
                                          storyHtml = "<div class=\"storyContent\" lang=\"en\"><style type=\"text/css\">.storyContent * {border-color:inherit !important;outline-color:inherit !important;}</style><div class=\"tr-npp-lead\"><p>Apple CDR Shares Issued 7,950,000 Last Close 10/24/2023 $25.83 Wednesday October 25 2023- In the News. The Globe's Scott Barlow writes that Mr. Kostin says in a note: \"The 10- year US Treasury yield reached 4.98 per cent this week, the highest level since July, 2007. Stocks that are most likely to interest Canadian investors include T-Mobile U.S., MGM Resorts... Apple CDR (CAD Hedged) (NEO:AAPL) Shares Issued 7,950,000 Last Close 10/24/2023 $25.83 Wednesday October 25 2023 - In the News</p><p>See Goldman Sachs CDR (CAD Hedged) (NEO:GS) In the News</p></div><div class=\"tr-npp-body\"><p>The Globe and Mail reports in its Wednesday, Oct. 25, edition that Goldman Sachs chief U.S. equity strategist David Kostin is recommending defensive investment positioning through strong balance sheet, large cap stocks likely to buy back shares. The Globe's Scott Barlow writes that Mr. Kostin says in a note: \"The 10-year US Treasury yield reached 4.98 per cent this week, the highest level since July, 2007. Amid higher rates, we expect investors will continue to focus on balance sheet strength and avoid companies that are most vulnerable to increased borrow costs. Performance since the start of September has reflected this dynamic as strong balance sheet stocks outperformed weak balance sheet stocks by 4 pp [percentage points]. For companies with strong balance sheets, we expect investors will reward those firms returning cash to shareholders and will remain skeptical of companies making large capex investments at this stage of the cycle.\" Mr. Kostin has created a basket of 50 companies where buybacks are expected. Stocks that are most likely to interest Canadian investors include T-Mobile U.S., MGM Resorts International, Quest Diagnostics, Salesforce, Apple, F5, Applied Materials and Meta Platforms.</p></div><p class=\"line-break\"><br/></p><p class=\"tr-copyright\">Copyright © 2023 Canjex Publishing Ltd.</p></div>",
                                          storyInfoHtml = "<h5 style=\"direction:ltr\"><span data-version-created-date=\"2023-10-25T12:14:45.562Z\" class=\"releasedDate\">25-Oct-2023 12:14:45</span></h5>")))

 expect_equal(stories2, CorrectOutput2)




})

test_that("EikonGetNewsStory fails when it should", {

  testthat::expect_error( EikonGetNewsStory( story_id = NULL)
                        , "Parameter story_id has to be supplied and cannot be empty"
                        , fixed = TRUE)
})


test_that("EikonGetNewsStory can handle multiple story ids", {

    Eikon <- check_Eikonapi(ExecutionMode = "JSON")
    stories3 <- EikonGetNewsStory( story_id = c( "urn:newsml:newswire.refinitiv.com:20231025:nNRAqewpdh:1"
                                               , "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
                                               )
                                 , EikonObject = Eikon, debug = T, raw_output = T
                                 )

    CorrectOutput3 <- list(list(story = list(headlineHtml = "<h1 class=\"storyHeadline\">\n<span class=\"headline\">AAPL Globe says Goldman Sachs recommends Apple, others</span> -\n<span class=\"source\" title=\"CNSWCH\">Canada Stockwatch</span>\n</h1>",
                                             storyHtml = "<div class=\"storyContent\" lang=\"en\"><style type=\"text/css\">.storyContent * {border-color:inherit !important;outline-color:inherit !important;}</style><div class=\"tr-npp-lead\"><p>Apple CDR Shares Issued 7,950,000 Last Close 10/24/2023 $25.83 Wednesday October 25 2023- In the News. The Globe's Scott Barlow writes that Mr. Kostin says in a note: \"The 10- year US Treasury yield reached 4.98 per cent this week, the highest level since July, 2007. Stocks that are most likely to interest Canadian investors include T-Mobile U.S., MGM Resorts... Apple CDR (CAD Hedged) (NEO:AAPL) Shares Issued 7,950,000 Last Close 10/24/2023 $25.83 Wednesday October 25 2023 - In the News</p><p>See Goldman Sachs CDR (CAD Hedged) (NEO:GS) In the News</p></div><div class=\"tr-npp-body\"><p>The Globe and Mail reports in its Wednesday, Oct. 25, edition that Goldman Sachs chief U.S. equity strategist David Kostin is recommending defensive investment positioning through strong balance sheet, large cap stocks likely to buy back shares. The Globe's Scott Barlow writes that Mr. Kostin says in a note: \"The 10-year US Treasury yield reached 4.98 per cent this week, the highest level since July, 2007. Amid higher rates, we expect investors will continue to focus on balance sheet strength and avoid companies that are most vulnerable to increased borrow costs. Performance since the start of September has reflected this dynamic as strong balance sheet stocks outperformed weak balance sheet stocks by 4 pp [percentage points]. For companies with strong balance sheets, we expect investors will reward those firms returning cash to shareholders and will remain skeptical of companies making large capex investments at this stage of the cycle.\" Mr. Kostin has created a basket of 50 companies where buybacks are expected. Stocks that are most likely to interest Canadian investors include T-Mobile U.S., MGM Resorts International, Quest Diagnostics, Salesforce, Apple, F5, Applied Materials and Meta Platforms.</p></div><p class=\"line-break\"><br/></p><p class=\"tr-copyright\">Copyright © 2023 Canjex Publishing Ltd.</p></div>",
                                             storyInfoHtml = "<h5 style=\"direction:ltr\"><span data-version-created-date=\"2023-10-25T12:14:45.562Z\" class=\"releasedDate\">25-Oct-2023 12:14:45</span></h5>")),
                           list(story = list(headlineHtml = "<h1 class=\"storyHeadline\">\n<span class=\"headline\">Refinitiv Newscasts - U.S. stocks end sharply higher on rate optimism</span> -\n<span class=\"source\" title=\"RTRS\">Reuters News</span>\n</h1>",
                                             storyHtml = "<div class=\"storyContent\" lang=\"en\"><style type=\"text/css\">.storyContent * {border-color:inherit !important;outline-color:inherit !important;}</style><div class=\"storyframe\" id=\"storydiv\"><div class=\"TEXT\" id=\"storybody\" name=\"storybody\"><div id=\"storybodycontent0\"><pre><span class=\"storycontent\">Click the following link to watch video: <a href=\"https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts\" data-type=\"url video-url\" translate=\"no\">https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts</a></span></pre><span class=\"storycontent\">Source: Reuters<br/><br/>\nDescription: Wall Street ended sharply higher on Tuesday, lifted by Tesla, Nvidia and other megacap growth stocks after a drop in monthly job openings cemented expectations of a pause in interest rate hikes by the U.S. Federal Reserve. Lisa Bernhard has more.<br/>\nShort Link: <a href=\"https://refini.tv/44u5KGv\" data-type=\"url\" class=\"tr-link\" translate=\"no\">https://refini.tv/44u5KGv</a><br/><br/>\nVideo Transcript:<br/><br/>\nUS stocks ended sharply higher on Tuesday after a drop in monthly job openings fueled expectations that the US Federal Reserve would pause its interest rate hikes. The Dow gained just under 1%, while the S&amp;P 500 jumped nearly 1.5% and the NASDAQ surged on 1.75%. The strong gains came after the Labor Department's Job Openings and Labor Turnover Survey or JOLTS report showed job openings fell for a third straight month in July, signalling a loosening labor market. But Stephanie Lang, Chief Investment Officer at Homrich Berg Wealth Management, says fewer job openings are just one part of the equation when it comes to achieving the Fedâ\u0080\u0099s goal of 2% inflation.<br/><br/>\nThe JOLTS data that came out today, it was the lowest number of job openings since March of 2021, so starting to see a little bit of loosening there, but itâ\u0080\u0099s still incredibly strong. So, while I think the market took a sigh of relief to say, okay, this is really starting to play out, but itâ\u0080\u0099s really just lowered job openings, and not an uptick in the unemployment rate â\u0080\u0093 because thatâ\u0080\u0099s what really needs to happen to orchestrate a soft landing.<br/><br/>\nAmong individual movers, mega-cap tech stocks led the way, with Tesla rallying more than 7% and chipmaker Nvidia climbing more than 4% with more than $33 billion worth of shares traded in each. Shares of Alphabet gained nearly 3% after the company unveiled fresh artificial intelligence tools and partnerships, including with Nvidia, at its Google Next conference in San Francisco. And Verizon and AT&amp;T each rose more than 3% after Citi upgraded the telecom companies to â\u0080\u009cbuyâ\u0080\u009d from â\u0080\u009cneutral.â\u0080\u009d Focus now turns to the personal consumption expenditures index, the Fed's preferred inflation gauge, which is due on Thursday followed by Fridayâ\u0080\u0099s non-farm payrolls report, offering more clarity on the state of the labor market.</span></div></div></div><p class=\"line-break\"><br/></p><p class=\"tr-copyright\">(c) Copyright Thomson Reuters 2023. Click For Restrictions - http://about.reuters.com/fulllegal.asp</p></div>",
                                             storyInfoHtml = "<h5 style=\"direction:ltr\"><span data-version-created-date=\"2023-08-30T16:43:33.586Z\" class=\"releasedDate\">30-Aug-2023 16:43:33</span></h5>")))



    expect_equal(stories3, CorrectOutput3)
})



test_that("EikonGetNewsStory can handle web urls as only content of story", {

 Eikon <- check_Eikonapi(ExecutionMode = "JSON")
 headlines <- structure(list(query = c("R:MSFT.O", "R:MSFT.O")
               , displayDirection = c("LeftToRight", "LeftToRight")
               , documentType = c("WebUrl", "WebUrl")
               , firstCreated = c("2024-09-02T14:20:10.388Z", "2024-09-02T13:52:38.644Z")
               , isAlert = c(FALSE, FALSE), language = c("L:lv", "L:de")
               , reportCode = c("", ""), sourceCode = c("NS:NEWLVL", "NS:GRUGES")
               , sourceName = c("News.lv (Latvia) (Latvian)", "Business Insider-Gruenderszene (German)")
               , storyId = c("urn:newsml:webnews.refinitiv.com:20240902:nNRAtpvk5p:0", "urn:newsml:webnews.refinitiv.com:20240902:nNRAtpv6mv:0")
               , text = c("Microsoft vienotā uzņēmumu atbalsta pakalpojums --", "4 Wege, wie ihr eure Karriere wieder in Schwung bringen könnt --")
               , versionCreated = c("2024-09-02T14:20:10.672Z", "2024-09-02T13:52:38.919Z"))
          , row.names = c(NA, -2L), class = c("data.table", "data.frame"))

 stories4 <- EikonGetNewsStory(EikonObject = Eikon, story_id = headlines$storyId)

 CorrectOutcome <- c("https://news.lv/IUB/2024/09/02/microsoft-vienota-uznemumu-atbalsta-pakalpojums",
                     "https://www.businessinsider.de/karriere/international-career/4-wege-wie-ihr-eure-karriere-wieder-in-schwung-bringen-koennt/")


 expect_equal(stories4, CorrectOutcome)

})
