test_that("EikonGetNewsHeadlines works", {


  Eikon <- check_Eikonapi(ExecutionMode = "Eikon")
  CheckHeadlines <- EikonGetNewsHeadlines(EikonObject = Eikon
                                              , query = "R:MSFT.O", count = 2)

  CorrectOutput <- list(displayDirection = "character", documentType = "character",
       firstCreated = "character", isAlert = "logical", language = "character",
       reportCode = "character", sourceCode = "character", sourceName = "character",
       storyId = "character", text = "character", versionCreated = "character")



  expect_equal(lapply(CheckHeadlines, class), CorrectOutput)
})

test_that("EikonGetNewsStory works", {


  Eikon <- check_Eikonapi(ExecutionMode = "Eikon")

  story_id <- "urn:newsml:newswire.refinitiv.com:20230829:nRTVm1b2r:5"
  stories <- EikonGetNewsStory( story_id = story_id
                              , EikonObject = Eikon, debug = F, raw_output = F)


  CorrectOutput <- list("<div class=\"storyContent\" lang=\"en\"><style type=\"text/css\">.storyContent * {border-color:inherit !important;outline-color:inherit !important;}</style>\n<div class=\"storyframe\" id=\"storydiv\">\n<div class=\"TEXT\" id=\"storybody\" name=\"storybody\">\n<div id=\"storybodycontent0\">\n<pre><span class=\"storycontent\">Click the following link to watch video: <a href=\"https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts\" data-type=\"url video-url\" translate=\"no\">https://share.newscasts.refinitiv.com/link?entryId=1_hdaqqs6a&amp;referenceId=tag:reuters.com,2023:newsml_RW783129082023RP1_930&amp;pageId=RefinitivNewscasts</a></span></pre>\n<span class=\"storycontent\">Source: Reuters<br/>\n<br/>\nDescription: Wall Street ended sharply higher on Tuesday, lifted by Tesla, Nvidia and other megacap growth stocks after a drop in monthly job openings cemented expectations of a pause in interest rate hikes by the U.S. Federal Reserve. Lisa Bernhard has more.<br/>\nShort Link: <a href=\"https://refini.tv/44u5KGv\" data-type=\"url\" class=\"tr-link\" translate=\"no\">https://refini.tv/44u5KGv</a><br/>\n<br/>\nVideo Transcript:<br/>\n<br/>\nUS stocks ended sharply higher on Tuesday after a drop in monthly job openings fueled expectations that the US Federal Reserve would pause its interest rate hikes. The Dow gained just under 1%, while the S&amp;P 500 jumped nearly 1.5% and the NASDAQ surged on 1.75%. The strong gains came after the Labor Department's Job Openings and Labor Turnover Survey or JOLTS report showed job openings fell for a third straight month in July, signalling a loosening labor market. But Stephanie Lang, Chief Investment Officer at Homrich Berg Wealth Management, says fewer job openings are just one part of the equation when it comes to achieving the Fedâ\u0080\u0099s goal of 2% inflation.<br/>\n<br/>\nThe JOLTS data that came out today, it was the lowest number of job openings since March of 2021, so starting to see a little bit of loosening there, but itâ\u0080\u0099s still incredibly strong. So, while I think the market took a sigh of relief to say, okay, this is really starting to play out, but itâ\u0080\u0099s really just lowered job openings, and not an uptick in the unemployment rate â\u0080\u0093 because thatâ\u0080\u0099s what really needs to happen to orchestrate a soft landing.<br/>\n<br/>\nAmong individual movers, mega-cap tech stocks led the way, with Tesla rallying more than 7% and chipmaker Nvidia climbing more than 4% with more than $33 billion worth of shares traded in each. Shares of Alphabet gained nearly 3% after the company unveiled fresh artificial intelligence tools and partnerships, including with Nvidia, at its Google Next conference in San Francisco. And Verizon and AT&amp;T each rose more than 3% after Citi upgraded the telecom companies to â\u0080\u009cbuyâ\u0080\u009d from â\u0080\u009cneutral.â\u0080\u009d Focus now turns to the personal consumption expenditures index, the Fed's preferred inflation gauge, which is due on Thursday followed by Fridayâ\u0080\u0099s non-farm payrolls report, offering more clarity on the state of the labor market.</span></div>\n</div>\n</div>\n<p class=\"line-break\"><br/></p><p class=\"tr-copyright\">(c) Copyright Thomson Reuters 2023. Click For Restrictions - http://about.reuters.com/fulllegal.asp</p></div>")
  expect_equal(stories, CorrectOutput)
})

test_that("EikonGetNewsStory fails when it should", {

  testthat::expect_error(EikonGetNewsStory( story_id = NULL)
                        , "Parameter story_id has to be supplied and cannot be empty"
                        , fixed = TRUE)
})
