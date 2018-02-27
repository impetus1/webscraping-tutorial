#   A short primer on web-scraping in R   
#    Tampa R Users Group                  
#     Mark Dayton                         
#     (mark-rrnr)                         
# 
# INTRO:
#  Suppose one wanted to collect attributes of one or more
# "R User Groups" (e.g. # of members, 'birth-date' of the group,
#  distribution of length of group membership, etc. etc.)
#  by scraping data from the web.....how would that work?
# 
#  As an example into the topic of 'web scraping with R',
#  the first part of this primer will do just that using 
#  only the assumption that the group uses "meetup.com" as its
#  organizational website (as referenced in a list of R User Groups
#  at https://jumpingrivers.github.io/meetingsR/r-user-groups.html.
#
#  For the second part of the primer, we'll dive into similar
#  mechanisms where the underlying website data is in XML....
#  and close out with an introduction to RSelenium (which can
#  be used to programmatically navigate a website (that may be
#  password-protected), and/or may contain data that is generated
#  dynamically by javascript or ajax, and which can't be "scraped"
#  without tools that can generate the DOM of the website) like
#  RSelenium (and offshoots like rdom)
#
# ---- setup ----
library(rvest)       # a user-friendly package for scraping "simple" html constructs
library(XML2R)       # a extension/wrapper around the XML package
library(selectr)

url1 <- "https://jumpingrivers.github.io/meetingsR/r-user-groups.html#united-states-of-america"

us_rugs <- read_html(url1)                                 # read url1 to create object for US R User Grps

css1a <- "div#united-states-of-america.section.level3"     # from "right-click-inspect"" method

xp1a  <- css_to_xpath(selector=css1a)                      # get xpath equivalent for css (to show all.equal==TRUE)

rugs_urls1a <- html_nodes(us_rugs,css=css1a) %>%
              html_nodes(.,"a") %>% 
               html_attr(.,"href")

rugs_urlsxp1a <- html_nodes(us_rugs,xpath=xp1a) %>% # use xpath this time..."a" -->  denotes nodes w/a URL (with "href" attribute)
               html_nodes(.,"a") %>%     #<<
               html_attr(.,"href")       #<<

all.equal(rugs_urls1a,rugs_urlsxp1a)

# filter & clean URLs before passing to website #2
# Not **all** of the 83 URLs use meetup.com (but most do), and we'll clean them up **before the next step**:

mu_urls <- rugs_urls1a[grep("meetup.com",rugs_urls1a)] %>%
          gsub("\\d+{4}|events/|members|files/","",.)

length(mu_urls) 
mu_urls[1:5]     # 64 out of 83 R User Groups in U.S. use meetup.com

# open up one of the R user group URLs from RStudio
 browseURL(paste0(mu_urls[1],'/members/'))`

# use selectorgadget or right-click-insped method to get css (or xpath)
# scrape meetup.com/meetings for all R User Groups
all_grpinfo <- matrix(nrow=length(mu_urls), ncol=6)
colnames(all_grpinfo) <- c('mu_city', 'mu_st', 'mu_dob', 'mu_nbrmems', 'mu_pstmtgs', 'mu_urls')

mu_infoxp <- '//*[@id="C_metabox"]/div[1]'  # xpath for selecting group attributes from the meetup.com/members page for a given R user group URL
dob_css <- 'div.small.margin-bottom'        # CSS locator for founded date for group
mu_datacss <- 'ul.paddedList.small.margin-bottom' # CSS for (text of) data elements


# CAUTION:  better code may be needed if website doesn't like too many http requests from same IP
# in a short amount of time!
# see https://stackoverflow.com/questions/39056103/iterating-rvest-scrape-function-gives-error-in-open-connectionx-rb-time/39057166/>
# as a learning exercise, limiting the groups to 5 will be fine

n <- 5 # scraping 1st 5 urls
for (i in 1:n) {
mu_htm <- read_html(paste0(mu_urls[i],'/members/'))
mu_info <- html_nodes(mu_htm,xpath=mu_infoxp)
mu_city <- html_text(html_nodes(mu_info,css=".locality"))
mu_st <- html_text(html_nodes(mu_info,css=".region")) %>%
         gsub("\\n","",.)

mu_dob <- html_text(html_nodes(mu_info,css=dob_css),trim=T) %>%
          gsub("Founded\n","",.)

mu_data <- html_nodes(mu_info,css=mu_datacss) %>%
           html_nodes(.,css="a") %>%
           html_text(.)
mu_nbrmems <- strsplit(mu_data[1],"\n")[[1]][2]                           
mu_pstmtgs <- strsplit(mu_data[grep("Past Meet", mu_data)],"\n")[[1]][3]  

all_grpinfo[i,] <- cbind(mu_city, mu_st, mu_dob, mu_nbrmems, mu_pstmtgs, mu_urls[i])   
 }

options(width = 150)
all_grpinfo[1:n,]


##########################
# scraping XML
##########################

library(XML2R)
xml1 <- "http://www.wagertalk.com/spt-opt/schedule.php?host=WAGERTALK&sport=ncaabb&period=0"
xmlobs1 <- XML2Obs(xml1, quiet=TRUE)
table(names(xmlobs1))

# NOTE:  sometimes this website times out....use download.file(xml1,dest='your-xml-file.xml')
# in development mode or to persist files 

xmlobs1 <- add_key(xmlobs1, parent="ODDS//LEAGUE//GAME", recycle="date", key.name="gamedate") # adding key - new col of "gamedate"
xmlobs1 <- add_key(xmlobs1, parent="ODDS//LEAGUE//GAME", recycle="time", key.name="gametime") # adding key - new col of "gametime"
xmlobs1 <- add_key(xmlobs1, parent="ODDS//LEAGUE//GAME//TEAM", recycle="number", key.name="gamewgrnbr") # new col of "gamewgrnbr"
xmlobs1 <- add_key(xmlobs1 , parent="ODDS//LEAGUE//GAME//TEAM", recycle="name", key.name="teamname")
oddsline <- collapse_obs(xmlobs1[grep("^ODDS//LEAGUE//GAME//TEAM//LINE$",names(xmlobs1))])  # this returns a matrix
oddsline <- oddsline[,-grep("url",colnames(oddsline))]            # get rid of the long "url" colname
head(oddsline)

#Post-processing:  task list
##
## - parse the lines for half-points (gsub the "ampersand frac12")
## - perform necessary character-to-numeric conversions
## - ...and then for dates/datetimes, convert to appropriate POSIX.. classes
## - get the "datetime-line-refresh" from the global //TIME attribute and...
## - use that with the "seconds" value to calculate the datetime for a given "LINE"
## - calculate an "hours-to-gametime" column (optional - but easier to digest!)
## - load to a database table 


###################
#Using RSelenium
###################

# installation:  see videos, other google searches, etc
# once installed, use it!
library(RSelenium)
library(stringr)
library(getPass)

rD <- rsDriver(port=4445L, browser="chrome")
remDr <- rD[["client"]]                     

# this utility may be useful
kppw <- getPass()

remDr$navigate("https://kenpom.com")

#NOTE:  this is subscription site - but some pages may be accessed w/out paying the fee
# send login events
kpliip <- remDr$findElement(using = 'name', value = "email")
kpliip$sendKeysToElement(list("ridgeregressionnr@gmail.com"))

kppwip <- remDr$findElement(using = 'name', value = "password")
kppwip$sendKeysToElement(list(kppw, key = "enter"))

# go to the 'fanmatch' page for a given date
remDr$navigate("https://kenpom.com/fanmatch.php?d=2018-02-25")

# after navigating to the table, it is easy to identify the 3rd column of data
# as being of .class='wrong' (shown in RED font) or .class='correct' (GREEN font)
# concact xpaths to search for wnrs and losers WITHIN the table

game_preds <- remDr$findElements(value = "//td[@class = 'wrong'] | //td[@class = 'correct']")

# Now, we can use RSelenium's "$method" technique to get the text 
# which we will parse

game_predsTxt <- sapply(game_preds,function(x) x$getElementText())


#parse the 2 numeric pieces of that column in the fanmatch-table we want (pred scores)
# and the predicted winning team for that game
pred_wnrscr <- as.numeric(sapply(str_extract_all(game_predsTxt, "\\d+"),function (x) x[1]))
pred_loserscr <- as.numeric(sapply(str_extract_all(game_predsTxt, "\\d+"),function (x) x[2]))
pred_pwin <- as.numeric(sapply(str_extract_all(game_predsTxt, "\\d+"),function (x) x[3]))
pred_wnr <- substr(game_predsTxt,1,str_locate(game_predsTxt, "\\d")-2)
head(pred_wnr)


# to get the within-game probabilities, we need to find the gid (game-id)
# which is used in the URL (hover over the "winprob" link in the fanmatch-table
# find ALL elements, then sapply that result to get the element attribute="href" (a link!)

fmUrls  <- remDr$findElements(value = "//span/a")  
fmHrefs <- sapply(fmUrls,function(x) x$getElementAttribute('href')) 

#each game also has a link to the "MVP" for the game, so filter those out
# keeping only "winprob" links

fmWinProbs <- fmHrefs[grep("winprob",fmHrefs)]

# cycle over all games for the given day, and for each game
# use "$executeScript" twice to get the within-game play-by-play data,
for (i in 1:length(fmWinProbs)){
    remDr$navigate(fmWinProbs[[i]])
    winprob <- remDr$executeScript("return dataset;")
    winprobDf <- do.call(rbind.data.frame,winprob)
    gamedata <- remDr$executeScript("return data;")
    gamedataDf <- do.call(cbind.data.frame,gamedata[!names(gamedata) %in% 'input'])
    wpdf <- merge(winprobDf, gamedataDf[names(gamedataDf) %in% c('gid','ymd','gameTime','team1','team2')])
    names(wpdf) <- gsub("WP","visitorWP",names(wpdf))
    names(wpdf) <- gsub("team1","team_away",names(wpdf))
    names(wpdf) <- gsub("team2","team_home",names(wpdf))
    wpdf$homeWP <- with(wpdf,1-visitorWP)
    if (i==1) {all_games <- wpdf
               all_meta <- gamedataDf } 
    else {all_games <- rbind(all_games,wpdf)
          all_meta <- rbind(all_meta,gamedataDf)}
 }
## merge all_meta with predicted scores and predicted winner
results <- cbind.data.frame(all_meta, pred_wnr, pred_pwin, pred_wnrscr, pred_loserscr,stringsAsFactors=FALSE)
results$pred_ttl <- with(results, pred_wnrscr+pred_loserscr)
results$act_ttl <- with(results, score1+score2)
results$pred_diff <- pred_wnrscr - pred_loserscr
results$act_diff  <- ifelse(results$pred_wnr==results$team2,results$score2-results$score1,results$score1-results$score2)

head(results)

### might as well plot something....
library(ggplot2)
ggplot(results) +
  aes(y = act_ttl,
      x = pred_ttl) +
  geom_point() +
  geom_smooth(method = "lm") 

# getting all of kenpom's predictions for the year would be pretty useful
# in determining how accurate he is (relative to the Vegas Lines)
# should one want to wager during a visit to the Madness in March
# Much more processing has to be done, but clearly doable thanks
# to RSelenium!

