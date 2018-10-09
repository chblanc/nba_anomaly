#' ---
#' title: Data Scraper
#' author: Carlos Blancarte
#' ---

#' # Set Up
#' 
#' we will absolutely need to use Selenium to get this data. The package
#' is not available on CRAN anymore so you'll need to install it via
#' `devtools::install_github("ropensci/RSelenium")`

#+ message = FALSE, warning = FALSE
# libraries
library(RSelenium)
library(rvest)
library(tidyverse)

#+ message = FALSE, warning = FALSE
# Global: save results to `R/data`? 
WRITE <- FALSE

#' ## the `data`
#' 
#' courtesy of stats.nba.com; stats for 540 players in the league for the
#' 2017-2018 season. 

# url w/2017-2018 stats
#nba_url <- "https://stats.nba.com/players/traditional"
nba_url <- "https://stats.nba.com/players/traditional/?sort=PTS&dir=-1&Season=2017-18&SeasonType=Regular%20Season"

#' ## starting `Selenium`
#' 
#' we're going to have to do this w/`Selenium` because it's the only way to
#' get the data (which happens to be created dynamically)

#+ message = FALSE, warning = FALSE
remote_driver <- RSelenium::rsDriver(browser = "chrome")
chrome <- remote_driver[["client"]]

# navigate to url
chrome$navigate(nba_url)

# scroll down 5 times, waiting for the page to load at each time
for(i in 1:5){      
  chrome$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(3)    
}

#' rather than clicking through every single page we're going to direct the
#' browser to click on the paginator-tab and set it to _ALL_. 
x <- chrome$findElement(using="css selector", "body > main > div.stats-container__inner > div > div.row > div > div > nba-stat-table > div:nth-child(3) > div > div > select")
x$sendKeysToElement(list("All", key="enter"))

#' ## parse the page source
#' `Selenium` is running a separate web browser which has the ability to
#' scroll and as a result, loads all of the data. now we can make use of
#' `rvest` to pull out the things we care about. 

# get the page html
page_source <- x$getPageSource()

# pull out the tbl w/player stats
tbl <- xml2::read_html(page_source[[1]]) %>%
  html_nodes(xpath="/html/body/main/div[2]/div/div[2]/div/div/nba-stat-table/div[2]/div[1]/table/tbody")

# define the field names:
field_names <- c(
  "rank", "player", "team", "age", "games_played", "wins", "losses",
  "minutes", "points", "fg_made", "fg_attempts", "fg_pct", "three_p_made",
  "three_p_attempts", "three_p_pct", "ft_made", "ft_attempts", "ft_pct",
  "reb_offensive", "reb_defensive", "reb", "assists", "turnovers", "steals",
  "blocks","personal_fouls",  "fantasy_points", "double_doubles",
  "triple_doubles", "plus_minus"
)

# parse every element in `tbl`
df <- map(html_children(tbl), function(h) {
  h %>%
    html_nodes("td") %>%
    html_text(trim=TRUE) %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_remove_all("\n") %>%
    set_names(field_names) %>%
    enframe("col", "value") %>%
    spread(col, value) %>%
    select(field_names)
})

# create a data.frame, fixing variable types along the way
df_out <- df %>% 
  bind_rows() %>% 
  mutate_at(vars(-player, -team), funs(as.numeric)) %>%
  select(-rank)

# save the data as RDS
if (WRITE) saveRDS(df_out, "data/nba_2017_stats.Rds")

# stop the selenium server ----------------------------------------------------
remote_driver[["server"]]$stop() 
