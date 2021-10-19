install.packages("dm")
install.packages("vroom")

library(dm)
library(janitor)
library(readr)
library(stringr)
library(vroom)
library(tidyverse)


# 1.READ IN CSV files----


##Read files named *_dataset.csv within 00_Data folder.
filenames <- list.files(path="00_Data/",
                        pattern="*_dataset.csv")

##Create list of data frame names without the "olist" & "dataset.csv" part 
names <-str_sub(filenames,7,-13L)

##Load all files (Create a df)
for(i in names){
  filepath <- file.path("00_Data/",paste("olist_",i,"_dataset.csv",sep=""))
  assign(i, vroom(filepath))
}


# 2.DEDUPLICATE the Geolocation----

## There are duplicates in geolocation df - we will
##                                                - de-duplicate state and city
##                                                - take the average of the coordinate (better approach possible?)


## Deduplication
state_zip <- geolocation %>% 
  group_by(geolocation_zip_code_prefix,geolocation_state) %>% 
  summarise(n=n()) %>% 
  slice(which.max(n)) %>% 
  ungroup() %>% 
  select(-n)

city_zip <- geolocation %>% 
  group_by(geolocation_zip_code_prefix,geolocation_city ) %>% 
  summarise(n=n()) %>% 
  slice(which.max(n)) %>% 
  ungroup()%>% 
  select(-n)

## Average Coordinates and join
geolocation <- geolocation %>% 
  group_by(geolocation_zip_code_prefix ) %>% 
  summarise(geolocation_lat=mean(geolocation_lat),
            geolocation_lng=mean(geolocation_lng)) %>% 
  ungroup() %>% 
  left_join(state_zip, by = "geolocation_zip_code_prefix") %>% 
  left_join(city_zip, by = "geolocation_zip_code_prefix")


# Need to run a test to identify if any of my primary key is duplicated

#3.Create a DM object----

## Create a dm object

ecommerce_dm_no_keys <- dm(customers,geolocation,order_items,
                            order_payments,order_reviews,orders,
                            products,sellers)

#4.Define the primary key

## This handy function could help me identify the likely primary keys
# dm_enum_pk_candidates(e_commerce_dm_no_keys,customers)

ecommerce_dm_pks <- 
  ecommerce_dm_no_keys %>% 
  dm_add_pk(table=customers,columns = customer_id) %>% 
  dm_add_pk(table=orders,columns = order_id) %>% 
  dm_add_pk(table=geolocation,columns = geolocation_zip_code_prefix) %>% 
  dm_add_pk(table=sellers,columns = seller_id) %>% 
  dm_add_pk(table=products,columns = product_id) 

  geo_2[duplicated(geo_2$geolocation_zip_code_prefix ), ]