library(stringr)
library(XML)
library(maps)
heritage_parsed <- htmlParse("http://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger",
                             encoding = "UTF-8")
tables <- readHTMLTable(heritage_parsed,stringAsFactors=FALSE)
