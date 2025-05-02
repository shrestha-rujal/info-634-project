activitiesCheck = activities[!duplicated(activities$activity), ]
activitiesCheck = activitiesCheck[order(activitiesCheck$activity), ]
print(activitiesCheck)

activitiesSubtypeCheck = activities[!duplicated(activities$subtype), ]
activitiesSubtypeCheck = activitiesSubtypeCheck[order(activitiesSubtypeCheck$activity), ]
print(activitiesSubtypeCheck)

# Extracting numeric rating scores
satisfaction_ratings = visitor_satisfaction[]
satisfaction_ratings$recommendation <- as.numeric(gsub("[^0-9.]", "", satisfaction_ratings$recommend_rating))
satisfaction_ratings$satisfaction <- as.numeric(gsub("[^0-9.]", "", satisfaction_ratings$satisfaction_rating))
satisfaction_ratings$rating <- (2*satisfaction_ratings$satisfaction + satisfaction_ratings$recommendation)/2

activities_categories = merge(activities[], visitor_satisfaction_reasons[], by="response_id", all.x = TRUE)
activities_categories = merge(activities_categories[], satisfaction_ratings[,c("response_id","rating")], by="response_id", all.x = TRUE)
head(activities_categories)

library(dplyr)

lowLevelCat = [
  national park, art gallery, museum, cycling, hot pools/spa, walk/hike/etc, tour/film location, geothermal park, glacier, 
  music/theatre performance, marine park/reserve, te ao maori landmark/experiences, wildlife sanctuary/reserve/park, air activities, 
  vineyard/brewery/trail, bungee jumping/canyoning, extreme ride, farm/orchard, fishing, food/drink festival, garden/flower show, 
  glow worms, hunting, aquatic activities, horse-riding, mountain/cave activities, important building/site, natural attractions, 
  golf, quad biking, snow sports, theme parks, restaurants, marine animal experience, watching/playing sports, 
  expos and indoor activities
]

#highLevelCat = [
#  "Adrenaline Activities" = air activities, bungee jumping/canyoning, extreme ride,                     |
#  "Adrenaline Sports" = mountain/cave activities, snow sports,                                          |
#  "Art Galleries, Museums, and National Landmarks" = art gallery, museum, important building/site,      | 
#  "Aquatic Sports and Rides" = aquatic activities,                                                      |
#  "Cultural Experiences and Locations" = te ao maori landmark/experiences,                              |
#  "Cycling and Off-Road Biking" = cycling, quad biking                                                  |
#  "Farm, Orchard, Garden, and Flower Shows" = farm/orchard, garden/flower show                          |
#  "Food and/or Drink Activities" = restaurants, food/drink festival, vineyard/brewery/trail             |
#  "Hot Pools and Spas" = hot pools/spas                                                                 |
#  "Hunting and Fishing" = hunting, fishing                                                              |
#  "Marine Wildlife Activities" = marine animal experience, marine park/reserve,                         |
#  "Music/Theatre Performances and Events" = music/theatre performance                                   |
#  "Natural Attractions and Experiences" = national park, natural attractions, geothermal park, glaciers |
#  "Recreational Sport Activities and Events" = golf, watching/playing sports                            |
#  "Walks, Hikes, and Trails" = walk/hike/etc,                                                           |
#  "Wildlife Activities" = wildlife sanctuary/reserve/park, glow worms, horse-riding,                    |
#  ?"Theme Parks, Film Locations, and Tours" = tour/film location, theme parks,                           |
#  ?"Indoor Recreational Activities and Expos" = expos and indoor activities                             |
#]

activitiesGrouped <- activities_categories %>%
  mutate(activity = recode(activity,
                           "A film location, tour or experience" = "Theme Parks, Film Locations, and Expos",
                           "Other exhibition / Expo" = "Theme Parks, Film Locations, and Expos",
                           "Other themed experience (e.g. All Blacks Experience, Antarctic Centre)" = "Theme Parks, Film Locations, and Expos",
                           "Theme park or leisure park" = "Theme Parks, Film Locations, and Expos",
                           "Mountain climbing, rock climbing, abseiling, caving" = "Adrenaline Sports",
                           "Skiing, snowboarding, sledging or other snow sport" = "Adrenaline Sports",
                           "A public museum or art gallery" = "Art Galleries, Museums, and National Landmarks",
                           "Other important building or site" = "Art Galleries, Museums, and National Landmarks",
                           "Other water activity such as water-skiing, sailing" = "Aquatic Sports",
                           "Rafting, canoeing, kayaking" = "Aquatic Sports",
                           "Scuba diving or snorkelling" = "Aquatic Sports",
                           "Swimming / surfing" = "Aquatic Sports",                    
                           "A place that is significant to Māori, such as a landmark, remains of a Māori pa (fortified hill), etc" = "Cultural Experiences and Locations",
                           "Cycling" = "Cycling, Mountain Biking, and Cycle Trails",
                           "Air activities such as paragliding, hang gliding, ballooning, skydiving" = "Extreme Experiences",
                           "Bungy jumping" = "Extreme Experiences",
                           "Canyon Swing / canyoning" = "Extreme Experiences",
                           "Extreme ride e.g. Luge, fly-by-wire, zorbing, flying fox/zip-lining" = "Extreme Experiences",
                           "Farm or orchard" = "Farm, Orchard, Garden, and Flower Shows",
                           "Garden visit or flower show" = "Farm, Orchard, Garden, and Flower Shows",
                           "Brewery, Gin, Vineyard / wine trail" = "Food and/or Drink Activities",
                           "Food and/or wine/alcohol event or festival" = "Food and/or Drink Activities",	
                           "Top-end restaurants" = "Food and/or Drink Activities",
                           "Health spa or day spa" = "Hot Pools and Spas",
                           "Hot pools" = "Hot Pools and Spas",
                           "Fishing" = "Hunting and Fishing",
                           "Hunting" = "Hunting and Fishing",
                           "A marine park or marine reserve" = "Marine Wildlife and Nature Activities",
                           "Swimming with seals or dolphins" = "Marine Wildlife and Nature Activities",
                           "Trip to watch whales, penguins or dolphins" = "Marine Wildlife and Nature Activities",
                           "A live performance of dance" = "Music/Theatre Performances and Events",
                           "A live performance of music" = "Music/Theatre Performances and Events",
                           "A play / drama" = "Music/Theatre Performances and Events",
                           "Other live performance" = "Music/Theatre Performances and Events",
                           "A beach" = "Natural Attractions and Experiences",
                           "A geothermal park (hot mud and possibly geysers)" = "Natural Attractions and Experiences",
                           "A glacier e.g. Fox, Franz Josef, Hooker Valley" = "Natural Attractions and Experiences",
                           "A national park" = "Natural Attractions and Experiences",
                           "Other natural attraction (e.g. mountain, lake, river, forest, etc.)" = "Natural Attractions and Experiences",
                           "Stargazing" = "Natural Attractions and Experiences",
                           "Jet-boating" = "Outdoors Vehicle Rides",
                           "Other boating" = "Outdoors Vehicle Rides",
                           "Quad biking, 4WD vehicle tour" = "Outdoors Vehicle Rides",
                           "Playing golf" = "Recreational Sport Activities and Events",
                           "Watching or playing other sport" = "Recreational Sport Activities and Events",
                           "Watching or playing some sport" = "Recreational Sport Activities and Events",
                           "A casino" = "Recreational Sport Activities and Events",
                           "Went for a walk, hike, trek or tramp" = "Walks, Hikes, and Trails",
                           "A wildlife sanctuary (a sanctuary for New Zealand's endangered species)" = "Wildlife Activities and Sanctuaries",
                           "Glow worm tour" = "Wildlife Activities and Sanctuaries",
                           "Horse riding / horse trekking" = "Wildlife Activities and Sanctuaries",
                           "Zoo or wildlife park" = "Wildlife Activities and Sanctuaries"
  ))
table(activitiesGrouped$activity)

# Removing "None of these" and "Not sure" as they are not useful for providing recommendations
activitiesFiltered <- activitiesGrouped[activitiesGrouped$activity != "None of these", ]
activitiesFiltered <- activitiesFiltered[activitiesFiltered$activity != "Not sure", ]

# Removing unnecessary columns
activitiesFiltered <- activitiesFiltered %>% select(-category)
activitiesFiltered <- activitiesFiltered %>% select(-subtype)
activitiesFiltered <- activitiesFiltered %>% select(-satisfaction_reason)

write.csv(activitiesFiltered,"C:\\Users\\OEM\\Documents\\R\\activities_categorized.csv", row.names = FALSE) 
