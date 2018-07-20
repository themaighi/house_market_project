### Web scrapping function ###
## Here there is the function used by the shiny app to webscrap the values from funda
## It has 2 inputs : the city, which city you want to consider
## days, how many days do you want to webscrap


funda_webscrap <- function(city, day){
  city <- "rotterdam"; day <- 14
  # rd <- rsDriver()
  # #rd <- remoteDriver(browserName = "chrome")
  # remDr <- rd[["client"]]
  # remDr$open()
  
  user_agent <- fread("data/user_agents.csv")
  user_use <- user_agent[,sample(`User agent`, size = 1)]
  referers <- fread("data/referer.csv", sep = ",")
  ref_use <- referers[,sample(referer, size = 1)]
  ip_address <- fread("data/ip_address.csv", sep = ",")
  ip <- ip_address[Https == "yes",sample(`IP Address`, size = 1)]
  port <- ip_address[`IP Address` == ip,Port] 





root <- "https://www.funda.nl"
search <- "/en/koop/"
sort <- "/sorteer-datum-af/"
url <- paste0(root, search,tolower(city), sort)

# html <- readLines("https://www.funda.nl/en/koop/rotterdam")

webpage <- html_session(url,
                        user_agent(user_use),
                        config(referer = ref_use),
                        use_proxy(ip, port))
Sys.sleep(runif(1,min = 1, max = 10))

a <- webpage %>% html_nodes(".pagination-pages a")
Sys.sleep(runif(1,min = 1, max = 10))
links <- html_attr(a, "href")
links <- links[links %like% "koop"]
rank_data_mod <- list()
time_interval <- c() # definition of time interval
max_days <- day


withProgress(message = 'Calculation in progress',
             detail = 'This may take a while...', value = 0,{
               for(i in links[grep(pattern = "koop", x = links)]){#i <- links[1]
                 Sys.sleep(runif(1,min = 1, max = 10))
                 # link_url <- paste0(root, i)
                 # webpage <-  read_html(link_url)
                 # rank_data_html <- html_nodes(webpage,'.search-result-title')
                 # rank_data <- html_text(rank_data_html)
                 # rank_data_mod[[i]] <- as.data.table(gsub("\r\n","",rank_data))
                 
                 print(toupper(i))
                 link_url <- paste0(root, i) #Get link of specific page with house list
                 
                 webpage <-  webpage %>% jump_to(link_url)
                 Sys.sleep(runif(1,min = 1, max = 10))
                 rank_data_html <- html_nodes(webpage,'.search-result-title') #get the title
                 Sys.sleep(runif(1,min = 1, max = 10))
                 day <- html_nodes(webpage,'.result-separator') %>% html_text() %>%
                   trim() ## Day of the week to not webscrap everything
                 Sys.sleep(runif(1,min = 1, max = 10))
                 day <- as.Date(day, format = "%A, %B %d, %Y")
                 if(length(day[is.na(day)]) > 0){day[is.na(day)] <- Sys.Date()}
                 
                 time_interval <- c(time_interval,gsub("\\r\\n", "",day))
                 Sys.sleep(runif(1,min = 1, max = 10))
                 link_web <- html_nodes(webpage,'.search-result-header a') %>% html_attr("href") %>% as.character() %>% .[!is.na(.)] #link houses
                 Sys.sleep(runif(1,min = 1, max = 10))
                 links_done <- read.csv("data/links_webscrapped.csv") %>% as.data.table() %>% .[,as.character(x)]
                 Sys.sleep(runif(1,min = 1, max = 10))
                 links_new <- link_web[grep(pattern = "koop", x = link_web)] %>% .[!. %in% links_done]
                 write.csv(c(links_new, links_done), "data/links_webscrapped.csv", row.names = F)
                 houses_attr <- list()
                 
                 if(length(links_new)>0){
                   for(j in links_new){#j <- link_web[12]
                     Sys.sleep(runif(1,min = 1, max = 10))
                     print(j)
                     house_url <- paste0(root, j)
                     webpage <-  webpage %>% jump_to(house_url)
                     Sys.sleep(runif(1,min = 1, max = 10))
                     houses_attr[[j]] <- data.table(attributes = html_nodes(webpage,"dt") %>% html_text() %>% trim(),
                                                    values = html_nodes(webpage,"dd:not([class *= object])") %>% html_text() %>% trim(),
                                                    id_name = j)
                     Sys.sleep(runif(1,min = 1, max = 10))
                   }
                   Sys.sleep(runif(1,min = 1, max = 3))
                   if(length(time_interval[time_interval %in% (Sys.Date() - max_days)]) > 0){break} # break in case the date is the last possible
                   
                   rank_data <- html_text(rank_data_html)
                   names(houses_attr) <- gsub("\r\n","",rank_data) %>% trim()
                   houses_attr <- lapply(names(houses_attr), function(x){houses_attr[[x]][,trimed_address := x]})
                   rank_data_mod[[i]] <- rbindlist(houses_attr)
                   
                 }
                 incProgress(1/length(links))
               }})
if(length(rank_data_mod) == 0){
  houses <- fread(paste0(nl,"/data/houses - lat and long.csv"))}else{
    
    
    #Get laitude and longitude of different address
    places <- rbindlist(rank_data_mod) 
    places <- places[!values == "Cadastral map",] ## Not sure how to rcode this
    places <- places[!values %in% "",] ## Some are just empty spaces
    places <- places[!attributes %in% c("Ownership situation", "Fees", "Area"),]## Not sure how to rcode this
    places <- places[!values %in% "",]
    places[attributes == "Facilities",values := paste0(values, collapse = ", "), by = id_name]
    places[attributes == "Insulation",values := paste0(values, collapse = ", "), by = id_name]
    places <- dcast.data.table(unique(places), formula = id_name + trimed_address ~ attributes, value.var = "values")
    ## Using the previously used houses ##
    ##nl <- getwd()
    houses <- fread(paste0(nl,"/data/houses - lat and long.csv"))
    places <- places[!trimed_address %in% houses[!(is.na(lon)|is.na(lat)),trimed_address],]
    max_loop <- 5 ## because in case he cannot get it
    if(nrow(places)>0){ # because it can be that is actually already made
      ## This for loop take all the goecode, is done in a while loop, becuase
      ## sometimes gives mistakes due to some adress written
      places[, c("lon", "lat"):= geocode(trimed_address)]
      loop_times <- 1
      while(nrow(places[is.na(lon)|is.na(lat),])>0 & loop_times <= max_loop){
        
        prev_hous <- places[is.na(lon)|is.na(lat), trimed_address]
        places[is.na(lon)|is.na(lat), c("lon", "lat"):= geocode(trimed_address)] 
        if(all(prev_hous %in% places[is.na(lon)|is.na(lat),trimed_address])){
          
          a <- strsplit(prev_hous, split = " ")
          new_names <- llply(a, function(x){#x <- a[[1]]
            l <- length(x)
            n_x <- x[-(l-3)]
            return(paste0(n_x, collapse = " "))
          }) %>% as.data.table()
          
          places[is.na(lon)|is.na(lat),trimed_address := new_names]
          loop_times <- loop_times + 1  
        }
        
      }
    }
    houses <- rbind(houses, places, fill = T)
    houses <- houses[!(lat == 1 & lon == 1),]
    write.csv(houses,paste0(nl,"/data/houses - lat and long.csv"), row.names = F)}## elimina i geocode sbagliati

#houses[,residual := rnorm(nrow(houses))]
return(houses)
}