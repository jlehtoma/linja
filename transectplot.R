devtools::install_github("rstudio/leaflet")

library("dplyr")
library("magrittr")
library("rvest")
library(maps)
library(rgdal)

#Get the url for the table
urls <- c("http://koivu.luomus.fi/seurannat/linjalaskenta/vakiolinjat.php")

#Scrape the table
taulukko <- html(urls)
data<-taulukko %>%
      html_node("table")%>%
      html_table()

#Change the crappy names for variables
data$D_2015<-as.factor(data$`2015`)
data$Kunta<-data$`Kunta ja reitin nimi`

##Extract HTML code from links
site <- html(urls)
linjat_table <- site %>%
      html_node("table") %>%
      html_nodes("tr")

extract_urls <- function(x) {
      # Create an empty list to hold the results
      transect_data <- list()
      # Get all table data nodes from a single row
      td_nodes <- html_nodes(x, "td")
      # Extract the transect ID
      transect_data[["id"]] <- html_text(td_nodes[[1]])
      # Extract kansalaisen karttapaikka link
      transect_data[["map_url"]] <- html_attr(html_node(td_nodes[[13]], "a"),
                                              "href")
      # Extract kartta pdf link
      transect_data[["map_pdf_url"]] <- html_attr(html_node(td_nodes[[15]], "a"),
                                                  "href")
      # Extract form pdf link
      transect_data[["form_pdf_url"]] <- html_attr(html_node(td_nodes[[16]], "a"),
                                                   "href")
      # Coerce the list to data frame
      return(dplyr::as_data_frame(transect_data))
}

# Skip the first row (it's the header)
transect_data <- lapply(linjat_table[2:length(linjat_table)],
                        function(x) {return(extract_urls(x))})
# Create a data frame
transect_data <- dplyr::bind_rows(transect_data) 

#combine two datasets
d1<-cbind(data,transect_data) 

#Split coordinate file

Koordinates <- data.frame(do.call('rbind', strsplit(as.character(d1$YKJ),':',fixed=TRUE)))

Koordinates<-
      Koordinates%>%
      rename(Lat = X1)%>%
      rename(Long=X2)
Koordinates$Lat<-as.integer(as.character(Koordinates$Lat))
Koordinates$Long<-as.integer(as.character(Koordinates$Long))

#Combine coordinates with rest of the data
d2<-cbind(d1,Koordinates) 
#Remove last empty row
d2<-d2[-567,]

#Create new level for status
levels(d2$D_2015) <- c(levels(d2$D_2015), "Vapaa")

#Replace empty with level "vapaa"
d2$D_2015[d2$D_2015 == '-'] <- 'Vapaa'
droplevels(d2$D_2015)

#Convert to spatial data 
sp_data <- SpatialPointsDataFrame(coords=dplyr::select(d2, Long, Lat),
                                  data=d2, proj4string=CRS("+init=epsg:2393"))
#Fix coordinates
sp_data_wgs84 <- spTransform(sp_data, CRS("+init=epsg:4326"))

#create color palette for factor data
pal<-colorFactor("Set1",domain=NULL,na.color = "#808080")

# make a leaflet plot for data, plot wheter line transect is available or not, 
# add popåups, line transect map and form 
paikka<-sp_data_wgs84$Kunta
urli<-sp_data_wgs84$map_pdf_url
formi<-sp_data_wgs84$form_pdf_url

leaflet(sp_data_wgs84) %>% 
      addTiles()%>%
      addCircleMarkers(radius= 1,color = ~pal(D_2015),
                       popup=(paste0("<a href=", urli , ">", paikka,"</a>","<br />","<a href=", formi , ">","Maastolomake" ,"</a>")))


##################################################
#####end of code rest is blubber.
##################################################





leaflet(sp_data_wgs84) %>% 
      addTiles()%>%
      addCircleMarkers(radius= 1,color = ~pal(2015),popup='<a href="">Link</a>')





data<-read.csv("c:/HY-Data/VALIMAKI/Vilppu/Museo/linjalaskenta.csv",header=TRUE,sep=";")
sp_data <- SpatialPointsDataFrame(coords=dplyr::select(data, Long, Lat),
                                  data=data, proj4string=CRS("+init=epsg:2393"))

sp_data_wgs84 <- spTransform(sp_data, CRS("+init=epsg:4326"))






library("dplyr")
library("magrittr")
library("rvest")

urls <- c("http://koivu.luomus.fi/seurannat/linjalaskenta/vakiolinjat.php")

site <- html(urls)
linjat_table <- site %>%
      html_node("table") %>%
      html_nodes("tr")

extract_urls <- function(x) {
      # Create an empty list to hold the results
      transect_data <- list()
      # Get all table data nodes from a single row
      td_nodes <- html_nodes(x, "td")
      # Extract the transect ID
      transect_data[["id"]] <- html_text(td_nodes[[1]])
      # Extract kansalaisen karttapaikka link
      transect_data[["map_url"]] <- html_attr(html_node(td_nodes[[13]], "a"),
                                              "href")
      # Extract kartta pdf link
      transect_data[["map_pdf_url"]] <- html_attr(html_node(td_nodes[[15]], "a"),
                                                  "href")
      # Extract form pdf link
      transect_data[["form_pdf_url"]] <- html_attr(html_node(td_nodes[[16]], "a"),
                                                   "href")
      # Coerce the list to data frame
      return(dplyr::as_data_frame(transect_data))
}

# Skip the first row (it's the header)
transect_data <- lapply(linjat_table[2:length(linjat_table)],
                        function(x) {return(extract_urls(x))})
# Create a data frame
transect_data <- dplyr::bind_rows(transect_data) 





















scrape_data <- function(url) {
      message("Scraping page ", url)
      # Get the overall page structure
      site <- html(url)
      team_name <- site %>%
            html_nodes(xpath="//th)") %>%
            html_text()
      # Collate everything into a data frame
      dat <- list(team_name)
      return(dat)
}


library(rvest)
lego_movie <- html("http://www.imdb.com/title/tt1490017/")

lego_movie %>% 
      html_node("a span") %>%
      html_text() %>%
      as.numeric()

lego_movie %>%
      html_nodes("#titleCast .itemprop span") %>%
      html_text()



Reitin<br>no.<th>Reitin<br>no.</th> #vr > tbody:nth-child(1) > tr:nth-child(1) > th:nth-child(1)
      
      # Remove unnecessary cruft
      team_name <- gsub(" Fundraising Page", "", team_name)
target <- site %>%
      html_nodes(xpath="//div//p//em") %>%
      extract2(1) %>%
      html_text()
# Not very elegant, but get the sum based on hard coded location
target <- unlist(strsplit(target, "\\s+"))[4]
sum_raised <- site %>%
      html_nodes(xpath="//div//p//span[@class='raised-so-far']") %>%
      html_text()
perc_raised <- site %>%
      html_nodes(xpath="//div//strong//em") %>%
      extract2(1) %>%
      html_text()
n_donations <- site %>%
      html_nodes(xpath="//div//p//span[@class='number-of-donations']") %>%
      html_text() %>%
      as.numeric()
# Collate everything into a data frame
dat <- data.frame(team_name, target, sum_raised, perc_raised, n_donations,
                  stringsAsFactors=FALSE)
return(dat)
}

# Get a list of parsed data
donations_data <- lapply(urls, scrape_data)
# Bind data frames in the list into a single data frame
donations_data <- bind_rows(donations_data)


