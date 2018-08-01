library(tidyverse)
library(geosphere)
library(ggmap)

setwd("/Users/elan/Documents/SLS Policy Lab/Louisiana DuPont/Analysis/Final data/")
LA_data_Joe <- read_csv(file = "la_geocoded_new.csv")

# compare data to make sure same versions
LA_data <- read_csv(file = "LouisianaSurvey_DATA_2018-07-05_1102.csv")

#AARON EDITS TO ADD MISSING IDS
to_master <- c(1074, 1076, 1078, 1351, 1815, 1823, 1829, 1460,
                1504, 1505, 1506, 1507, 1508, 1509, 1510, 1511,
                3156, 3162, 3164, 1382, 1385, 1388, 1173, 1174,
                1175, 1176, 1177, 1178, 1179, 1181, 1367, 1154,
                1156, 1158, 1424, 1491, 1492, 1934, 2095, 1725,
                1730, 1732, 1469, 1470, 1471, 1468, 2618, 3065,
                3067, 226, 227, 2625, 2628, 2896, 686, 687, 688,
                713, 909, 910, 911, 1157, 1263, 1265, 1266, 1148,
                1271, 1273, 1275, 1277, 1279, 1282, 1284, 345, 485,
                486, 136, 137, 559, 560, 561, 562, 563, 565, 706,
                3221, 3224, 3228, 2350, 2359, 877, 878, 879, 2194,
                2196, 2175, 2176, 2177, 1706, 1710, 1727, 1648,
                1649, 1650, 1651, 1652, 1136, 1138, 1294, 1295)

LA_data <- LA_data %>%
  mutate(inputter_name = ifelse(record_id %in% to_master, 'MASTER', inputter_name))
  

############################## DATA INPUT check #####################################
######################################################################################
# households not reporting respondents:
LA_merge %>% 
  select(hh_id, respondent) %>% 
  group_by(hh_id) %>% 
  mutate(resp_check = sum(respondent)) %>% 
  ungroup() %>% 
  filter(resp_check != 1) %>% 
  select(hh_id, resp_check)
# hh_id: two respondents: 317, no respondents: 138, 176, 247, 432

# manual pull -- two respondents for 317 intentional (respondent switched half-way)
# for the others, due to lack of MASTER assignment

# make sure correct number of "MASTER" copies made it into the data
# i.e. check that hh_num = nrow() per household id
LA_data$hh_id <- as.integer(LA_data$hh_id)
master_check <- LA_data %>% 
  select(hh_id, hh_num, respondent, person, inputter_name) %>% 
  filter(inputter_name == "MASTER") %>% 
  group_by(hh_id) %>% 
  mutate(N = n()) %>% 
  slice(1) %>% 
  mutate(discrep = ifelse(hh_num == N, NA,
                          ifelse(hh_num > N, "less",
                                 ifelse(hh_num < N, "more"))))

# find any discrepancies in hh_num and n() for each hh_id:
hh_ids_to_fix <- master_check[which(master_check$N != master_check$hh_num), "hh_id"]
# 26 hh_ids with missing MASTER entries

master_check %>% 
  select(hh_id, hh_num, N, respondent, person, inputter_name) %>% 
  filter(hh_id %in% hh_ids_to_fix$hh_id) %>% 
  arrange(hh_id) %>% 
  filter(inputter_name == "MASTER") %>% 
  mutate(num_missing = hh_num - N) %>% 
  select(hh_id, num_missing) %>% 
  print(n = 40)

###################### records to coerce to MASTER ##################
to_master <- c(1074, 1076, 1078, 1351, 1815, 1823, 1829, 1460, 
               1504, 1505, 1506, 1507, 1508, 1509, 1510, 1511, 
               3156, 3162, 3164)

LA_data <- LA_data %>% 
  mutate(inputter_name = replace(inputter_name, 
                                 record_id %in% to_master, 
                                 "MASTER"))

##################### filter only to MASTER copies
LA_data <- LA_data %>% 
  filter(inputter_name == "MASTER")

LA_data_geocodes <- LA_data_Joe %>% 
  select(record_id, hh_id, person, hh_add, hh_add_norm, hh_add_lat, hh_add_lng)

# # check if key c("hh_id", "person") is unique:
# LA_data %>% 
#   count(hh_id, person) %>% 
#   filter(n>1)
#   
#   # careful: c("hh_id", "person") not a unique key, surprisingly...
# 
# LA_data %>% 
#   count(record_id, hh_id, person) %>% 
#   filter(n>1)
# 
# LA_data_geocodes %>% 
#   count(record_id, hh_id, person) %>% 
#   filter(n>1)
# 
# # try record_id -- make LA_data_check subset to compare with LA_data_geocodes
# LA_data_check <- LA_data %>%
#   select(record_id, hh_id, person, hh_add)
# 
# LA_data_geocodes_check <- LA_data_geocodes %>% 
#   select(record_id, hh_id, person, hh_add)
#   
# # setdiff yields values in first list absent from another (eg, (1,2,3), (1,4,3), setdiff = 2)
# setdiff(LA_data_check, LA_data_geocodes_check)


# two record ID discrepancies from Joe's set
# (turns out they have new hh_id numbers):
# A tibble: 2 x 3
#         record_id hh_id person    
# <int>     <int>   <chr>     
#   1       2198    414   Niece     
#   2       2472    408   Brother 59

#######################################################################
######################## ADD GEOCODES #################################
#######################################################################

# create data frame that maps each hh_add to a normed hh_add and lat/lng
LA_data_geocodes <- LA_data_geocodes %>% 
  select(hh_add, hh_add_norm, hh_add_lat, hh_add_lng) %>% 
  group_by(hh_add) %>% 
  slice(1)

# add hh_add_norm, hh_add_lat, hh_add_lng for known addresses
LA_merge <- left_join(LA_data, LA_data_geocodes, by = "hh_add")

# locate outliers:
LA_coordinates_outliers <- LA_merge %>%
  filter(hh_add_lng < -90.5) %>%
  filter(hh_add_lat >30.1)

# fix outliers (where LA mistakenly translated to Los Angeles, not Louisiana)
# LA_coordinates_outliers$hh_add[LA_coordinates_outliers$hh_add == "223 E. 14th St. Reserve, LA"] <- 
# "223 E. 14th St. Reserve, Louisiana"

LA_merge <- LA_merge %>%
  mutate(hh_add_lat = replace(hh_add_lat,
                              hh_add == "223 E. 14th St. Reserve, LA",
                              30.055752),
         hh_add_lng = replace(hh_add_lng,
                              hh_add == "223 E. 14th St. Reserve, LA",
                              -90.544279),
         hh_add_norm = replace(hh_add_norm,
                               hh_add == "223 E. 14th St. Reserve, LA",
                               "223 E 14th St, Reserve, LA, USA"),
         hh_add = replace(hh_add,
                          hh_add == "223 E. 14th St. Reserve, LA",
                          "223 E. 14th St. Reserve, Louisiana"))

# # find addresses missing lat/lng
# sum(is.na(LA_merge$hh_add_lat))
# sum(is.na(LA_merge$hh_add_lng))
# sum(is.na(LA_merge$hh_add_norm))
# only missing 15 addresses' lat/lng
# also need to correct the address in Los Angeles


# replace addresses missing lat-lngs with google geocode:

a <- LA_merge %>% 
  filter(is.na(hh_add_lat)) %>% 
  select(hh_add) %>% 
  unique()
a <- a$hh_add

for (i in 1:length(a)){
  hh <- a[i]
  print(hh)
  b <- geocode(hh)
  
  # deal with outliers
  if (b[[1]] > -89){
    hh_new <- paste0(hh, ", Louisiana")
    b <- geocode(hh_new)
    LA_merge[LA_merge$hh_add == hh, "hh_add"] <- hh_new
    hh <- hh_new
  }
  
  LA_merge[LA_merge$hh_add == hh, "hh_add_lng"] <- b[[1]]
  LA_merge[LA_merge$hh_add == hh, "hh_add_lat"] <- b[[2]]
}



#########################################################################################
######################### CLEAN DATA ####################################################
#########################################################################################
# clean and group (nest) variables:

# Race
# race__1 = 1 if Asian
# race__2 = 1 if Black/African American
# race__3 = 1 if Hispanic
# race__4 = 1 if Native American
# race__5 = 1 if Native Hawaiian or Pacific Islander
# race__6 = 1 if White
# race__99 = 1 if Other

LA_merge <- LA_merge %>% 
  mutate(race = ifelse(race___6 == 1, 0, # white
                       # black 
                       ifelse(race___2 == 1, 1,
                              # hispanic
                              ifelse(race___3 == 1, 2, 
                                     # other
                                     ifelse((race___1 == 1 | race___4 == 1 | race___5 == 1 | race___99 == 1), 3, 4)))))


#################### non-respondent answers ############################################
# constrain non-respondent answers to NAs (some are notated as 0)
LA_merge <- LA_merge %>% 
  mutate(race = replace(race,respondent == 0, NA))

#########################################################################################
################################# CANCER ################################################
#########################################################################################


# # check that cancer variable matches the reports of individual types of cancer:
# summary(LA_merge[,118:138])
# 
# test <- LA_merge %>%
#   mutate(cancer_any = ifelse(rowSums(LA_merge[,118:138]) > 0, 1,
#                              ifelse(is.na(cancer), NA, 0)),
#          respiratory = ifelse(rowSums(LA_merge[, c("asthma___1",
#                                                    "bronchitis___1",
#                                                    "sinus___1")]) > 0, 1, 0 ))
# identical(test$cancer_any, LA_merge$cancer)
#   # FALSE (ok, find mismatches)
# 
# # find mismatches: ONE mismatch: row 1276
# which(test$cancer_any != LA_merge$cancer)
#   # row 1276
# 
# options(dplyr.width = Inf)  # display more columns 
# test %>%
#   slice(which(cancer_any != cancer)) %>%
#   select(hh_id, respondent, person, cancer, cancer_any, 118:138)


########## Years
# year cancer began


# convert character vars to numerical
LA_merge <- LA_merge %>% 
  mutate_at(vars(ends_with("_c_yr")), as.numeric)


# generate var for # cancers reported per individual
LA_merge <- LA_merge %>%
  mutate(cancer = ifelse(rowSums(LA_merge[,118:138]) > 0, rowSums(LA_merge[,118:138]),
                         ifelse(is.na(cancer), NA, 0)),
         
         # dummy variable for whether individual reported any cancer
         cancer_d = ifelse(cancer > 0, 1,
                           ifelse(is.na(cancer), NA, 0)),
         
         #respiratory diseases
         respiratory = ifelse(rowSums(LA_merge[, c("asthma___1",
                                                   "bronchitis___1",
                                                   "sinus___1")]) > 0, 1, 0))

LA_merge <- LA_merge %>% 
  
  # var for # cancers reported per household
  group_by(hh_id) %>% 
  
  # dummy variable for whether household currently has cancer resident
  mutate(hh_cancer_d = max(cancer_d, na.rm = TRUE),
         
         # var for number of residents with cancer in each household
         hh_cancer_num = sum(cancer_d, na.rm = TRUE),
         
         # var for number of respondents deceased in household
         hh_cancer_deaths = ifelse(respondent == 1 & hh_death_c == 1, 
                                   # all deaths reported in one field, separated by ";"
                                   str_count(hh_death_c_id, ";") + 1,
                                   ifelse(respondent == 1 & hh_death_c == 0, 0, NA)),
         
         # code out "don't know"s ( = 2)
         hh_cancer_death_d = ifelse(hh_death_c == 1, 1,
                                    ifelse(hh_death_c == 2 | hh_death_c == 0, 0, NA)),
         
         # dummy var for cancer death in hh
         hh_cancer_death_d = max(hh_cancer_death_d, na.rm = TRUE),
         hh_cancer_death_d = replace(hh_cancer_death_d, is.infinite(hh_cancer_death_d), NA),
         
         
         # dummy var for hh affected by cancer (current or deceased)
         hh_cancer_affected = ifelse(hh_cancer_death_d == 1 | hh_cancer_d == 1, 1, 0)) %>%
  
  ungroup()



# summary for cancer deceased by household (only asked of respondent)
LA_merge %>% 
  filter(respondent == 1) %>% 
  select(hh_cancer_death_d) %>% 
  summary()


## todo
# add var for # of deaths in family (not resident, but near plant) (count by number of ;)
LA_merge <- LA_merge %>% 
  mutate(family_c_death_num = ifelse(respondent == 1,
                                     str_count(family_c_death_id, ";") + 1, NA))
# note: str_count counts a NA as NA


##################### RESIDENCY restriction (St. John)####################
# var for household affected by cancer after moving to St. John's 

# subtract out years of residence from hh_years
# account for hh_old_years where town was Reserve or Laplace

# full list of reported former residences below.  Ones to keep are:
# [1] "2 streets over (Reserve)" 
# [7] "Bellpoint"
# [26] "Edgard"
# [27] "Edgard, St Johns"
# [46] "La Place"
# [48] "Laplace"
# [49] "LaPlace"
# [73] "on 26th St., Reserve"
# [77] "Reserve"                             
# [78] "Reserve (next door)"
# [79] "Reserve and Laplace"
# [86] "St John Parish"
# [91] "St. John's Parish"                   
# [92] "St. Johns the Baptist Parish"
# [109] "West Bank"

# Full list
# [1] "2 streets over (Reserve)"            "Atlanta"                            
# [3] "Avondale"                            "Baton Rouge"                        
# [5] "Baton Rough"                         "Beaumont"                           
# [7] "Bellpoint"                           "Belmont"                            
# [9] "Belrose"                             "Boutte (Saint Charles Parish)"      
# [11] "Cambridge"                           "Chalmette"                          
# [13] "Charlotte"                           "Chattanooga"                        
# [15] "Chicago"                             "Clarksville"                        
# [17] "Convent"                             "Dallas"                             
# [19] "Deotredam"                           "Deptford"                           
# [21] "Destrehan"                           "Destrehan, St. Charles Parish"      
# [23] "Dogtown"                             "Donadsonville"                      
# [25] "Dorden Prairie"                      "Edgard"                             
# [27] "Edgard, St Johns"                    "Forthood"                           
# [29] "Garryville"                          "Garyville"                          
# [31] "Gilbert"                             "Gonzales"                           
# [33] "Gramercy"                            "Greensburg"                         
# [35] "Gretna"                              "Hahnville"                          
# [37] "Hahnville, St. Charles Parish"       "Homa"                               
# [39] "Homewood"                            "Houston"                            
# [41] "Jackson"                             "Jefferson"                          
# [43] "Kenner"                              "Kenter"                             
# [45] "Killone"                             "La Place"                           
# [47] "Lancaster"                           "Laplace"                            
# [49] "LaPlace"                             "Lions"                              
# [51] "Los Angeles"                         "Lucher"                             
# [53] "Lutcher"                             "Lutcher, St. John's Parish"         
# [55] "Lyons"                               "Marrero"                            
# [57] "Marrero (West Bank New Orleans)"     "Metairie"                           
# [59] "Metarie"                             "Monte"                              
# [61] "Monterary"                           "Montz"                              
# [63] "Mount Airy"                          "Mount Evie"                         
# [65] "Mt. Airy"                            "n/a"                                
# [67] "N/A"                                 "New Orleans"                        
# [69] "New York City"                       "Norco"                              
# [71] "Oakland"                             "Oklahoma City"                      
# [73] "on 26th St., Reserve"                "Pomona"                             
# [75] "Ponchatoula"                         "Raceland"                           
# [77] "Reserve"                             "Reserve (next door)"                
# [79] "Reserve and Laplace"                 "River Forest"                       
# [81] "River Ridge"                         "Rossville"                          
# [83] "Sacramento"                          "San Antonio"                        
# [85] "St Charles Parish"                   "St John Parish"                     
# [87] "St. Augustine"                       "St. Charles Parish"                 
# [89] "St. Charles Parish (city not given)" "St. James Parish"                   
# [91] "St. John's Parish"                   "St. Johns the Baptist Parish"       
# [93] "St. Louis"                           "St. Rose"                           
# [95] "St. Rose, St. Charles Parish"        "Thibodaux"                          
# [97] "Tigerville"                          "Vacherie"                           
# [99] "Vacherie St. James Parish"           "Vacherie, St. James Parish"         
# [101] "Vagary"                              "Vaser"                              
# [103] "Vashry"                              "Vasser, St. James Parish"           
# [105] "Wake Forest"                         "Wallace"                            
# [107] "Watson"                              "Wenatchee"                          
# [109] "West Bank"                          

#################### number deceased by household ########################
hh_cancer_deaths <- LA_data %>% 
  select(hh_id, cancer, hh_death_c, hh_death_c_age, family_c_death, family_c_death_st_john) %>% 
  filter(hh_death_c == 1)

family_cancer_deaths <- LA_data %>% 
  select(hh_id, cancer, family_c_death, family_c_death_st_john) %>% 
  filter(family_c_death == 1)

# code whether each member was in St. John the Baptist Parish 


#########################################################################################
# other ailments (columns 34 - 50, adhd___1 through to sinus___1)

LA_merge <- LA_merge %>% 
  mutate(other_illness = ifelse(rowSums(LA_merge[, 34:50]) > 0, 1, 0),
         respiratory = ifelse(rowSums(LA_merge[, c("asthma___1",
                                                   "bronchitis___1",
                                                   "sinus___1")]) > 0, 1, 0 ))


# only include if ailment began after residence in St. John's began (see Residency above)


##################### chemical smells
# restrict only to respondents
# # restrict to respondents:
# LA_respondents <- LA_merge %>% 
#   filter(respondent == 1)

# adjust smell_inside to either several times a week or daily, or not at all.
LA_merge <- LA_merge %>% 
  mutate(smell_inside_weekly = ifelse(smell_inside %in% 3:4, 1,
                                      ifelse(is.na(smell_inside), NA, 0)))





##################### DISTANCE from plant ################################
?mapdist

# Denka location: c(lon = -90.524850, lat = 30.057581)

LA_merge$denka_lat <- rep(30.057581,length(LA_merge$hh_id))
LA_merge$denka_lng <- rep(-90.524850, length(LA_merge$hh_id))

# define function for distance between two lat-lngs
denka_dist <- vector("double", nrow(LA_merge))

for (i in 1:nrow(LA_merge)) {
  hh_lng <- LA_merge$hh_add_lng[i]
  hh_lat <- LA_merge$hh_add_lat[i]
  denka_dist[i] <- distm(c(-90.524850, 30.057581),
                         c(hh_lng, hh_lat),
                         distHaversine)
}

# fix this later (use apply, rather than for-loop)

LA_merge$denka_dist <- denka_dist


######################### proportions by distance #########################

# add zone var for distance to plant:
# zone 1: within 1 km
# zone 2: within 2 km
# zone 3: within 3.5 km


# generate vars for inner/outer zones (innermost is zone 1, within 1 km, outermost zone 3)
LA_merge$zone <- ifelse(LA_merge$denka_dist <= 1000, 1,
                        ifelse(LA_merge$denka_dist > 1000 & LA_merge$denka_dist <= 2000, 2,
                               ifelse(LA_merge$denka_dist > 2000, 3, NA)))


# proportion of households with cancer (current or deceased) by zone
hh_prop_by_zone <- LA_merge %>% 
  group_by(hh_id) %>% 
  select(hh_id, hh_cancer_affected, zone) %>% 
  slice(1) %>% 
  group_by(zone) %>% 
  summarize(hh_prop_cancer = mean(hh_cancer_affected, na.rm = TRUE))

hh_prop_by_zone$hh_prop_cancer
# [1] 0.4476190 0.2656827 0.2236025

# plot:
ggplot(data = hh_prop_by_zone) +
  geom_col(mapping = aes(x = zone, y = hh_prop_cancer),
           position = position_stack(reverse=TRUE))

# plot in colour by yes/no:
ggplot(data = LA_merge %>% group_by(hh_id) %>% slice(1)) +
  geom_bar(mapping = aes(x = zone, fill = as.factor(hh_cancer_affected)),
           position = 'fill')

# TODO:
# proportion of people with cancer (current or deceased), by zone




########################## children ######################################
# hh_id index of households with children
hh_ids_child <- LA_merge %>% 
  filter(age <= 17) %>% 
  select(hh_id) %>% 
  unique()

#########
# fix variables that only apply to children:
# nosebleeds
# headaches

LA_merge <- LA_merge %>% 
  mutate(hh_nosebleeds = replace(hh_nosebleeds,
                                 !(hh_id %in% hh_ids_child$hh_id), NA),
         hh_headaches = replace(hh_headaches,
                                !(hh_id %in% hh_ids_child$hh_id), NA))

# identical(test$hh_nosebleeds, LA_merge$hh_nosebleeds)
# identical(test$hh_headaches, LA_merge$hh_headaches)
# 
# test %>% 
#   filter(!(hh_id %in% hh_ids_child$hh_id)) %>% 
#   select(hh_nosebleeds, hh_headaches) %>% 
#   summary()
# # all NAs now, as desired, leaving the rest unchanged
# 
# test %>% 
#   filter(hh_id %in% hh_ids_child$hh_id) %>% 
#   select(hh_nosebleeds, hh_headaches) %>% 
#   summary()





################################# subsets #################################
# restrict to respondents:
LA_respondents <- LA_merge %>% 
  filter(respondent == 1)


# restrict to households with children:
LA_children <- LA_merge %>% 
  filter(hh_id %in% hh_ids_child$hh_id)

