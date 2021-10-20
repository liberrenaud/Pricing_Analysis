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


# Need to run a test to identify if any of my primary key is duplicated (below script could be used?)
#geo_2[duplicated(geo_2$geolocation_zip_code_prefix ), ]

#3.Create a DM object----

## Create a dm object

ecommerce_dm_no_keys <- dm(customers,geolocation,order_items,
                            order_payments,order_reviews,orders,
                            products,sellers)

#4.Define the primary and Foreign Key (Data Model)

        ## This handy function could help me identify the likely primary keys
        # dm_enum_pk_candidates(e_commerce_dm_no_keys,customers)


ecommerce_dm <- 
  ecommerce_dm_no_keys %>% 
    #customer_id Key
  dm_add_pk(table=customers,
            columns = customer_id) %>%
  dm_add_fk(table=orders,
            columns = customer_id,
            ref_table = customers) %>% 
  #order_id Key
  dm_add_pk(table=orders,
            columns = order_id) %>% 
  dm_add_fk(table=order_payments,
            columns = order_id,
            ref_table = orders) %>% 
  dm_add_fk(table=order_reviews,
            columns = order_id,
            ref_table = orders) %>% 
  dm_add_fk(table=order_items,
            columns = order_id,
            ref_table = orders) %>% 
  #geolocation Key
  dm_add_pk(table=geolocation,
            columns = geolocation_zip_code_prefix) %>% 
  dm_add_fk(table=sellers,
            columns = seller_zip_code_prefix ,
            ref_table = geolocation) %>% 
  dm_add_fk(table=customers,
            columns = customer_zip_code_prefix,
            ref_table = geolocation) %>% 
  #seller_id Key
  dm_add_pk(table=sellers,
            columns = seller_id) %>% 
  dm_add_fk(table=order_items,
            columns = seller_id,
            ref_table = sellers) %>%
  #product_id Key
  dm_add_pk(table=products,
            columns = product_id) %>% 
  dm_add_fk(table=order_items,
            columns = product_id,
            ref_table = products) 

  