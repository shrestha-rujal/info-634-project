library(tidyverse)
getwd()

# set path to directory of the csv files
# setwd(paste(getwd(), "/", sep = ""))

getwd()

# read all csv files
accommodation <- read.csv("./datasets/accommodation.csv", header = TRUE)
activities <- read.csv("./datasets/activities.csv", header = TRUE)
branding <- read.csv("./datasets/branding.csv", header = TRUE)
decision_making_process <- read.csv("./datasets/decision_making_process.csv", header = TRUE)
ease_of_organisation_reasons <- read.csv("./datasets/ease_of_organisation_reasons.csv", header = TRUE)
ease_of_organisation <- read.csv("./datasets/ease_of_organisation.csv", header = TRUE)
environment <- read.csv("./datasets/environment.csv", header = TRUE)
expenditure_by_industry <- read.csv("./datasets/expenditure_by_industry.csv", header = TRUE)
expenditure_by_payment_type <- read.csv("./datasets/expenditure_by_payment_type.csv", header = TRUE)
impact_of_covid19 <- read.csv("./datasets/impact_of_covid19.csv", header = TRUE)
itinerary <- read.csv("./datasets/itinerary.csv", header = TRUE)
maori_cultural_experience <- read.csv("./datasets/maori_cultural_experience.csv", header = TRUE)
maori_cultural_sentiment <- read.csv("./datasets/maori_cultural_sentiment.csv", header = TRUE)
mobility <- read.csv("./datasets/mobility.csv", header = TRUE)
other_countries_visited <- read.csv("./datasets/other_countries_visited.csv", header = TRUE)
poor_experiences <- read.csv("./datasets/poor_experiences.csv", header = TRUE)
secondary_purpose_of_visit <- read.csv("./datasets/secondary_purpose_of_visit.csv", header = TRUE)
self_transport <- read.csv("./datasets/self_transport.csv", header = TRUE)
survey_main_header <- read.csv("./datasets/survey_main_header.csv", header = TRUE)
transport_methods <- read.csv("./datasets/transport_methods.csv", header = TRUE)
travel_party <- read.csv("./datasets/travel_party.csv", header = TRUE)
visitor_satisfaction_reasons <- read.csv("./datasets/visitor_satisfaction_reasons.csv", header = TRUE)
visitor_satisfaction <- read.csv("./datasets/visitor_satisfaction.csv", header = TRUE)

View(accommodation)
View(activities)
View(branding)
View(decision_making_process)
View(ease_of_organisation_reasons)
View(ease_of_organisation)
View(environment)
View(expenditure_by_industry)
View(expenditure_by_payment_type)
View(impact_of_covid19)
View(itinerary)
View(maori_cultural_experience)
View(maori_cultural_sentiment)
View(mobility)
View(other_countries_visited)
View(poor_experiences)
View(secondary_purpose_of_visit)
View(self_transport)
View(survey_main_header)
View(transport_methods)
View(travel_party)
View(visitor_satisfaction_reasons)
View(visitor_satisfaction)

directory_path <- "./datasets"

file_list <- list.files(
  path = directory_path, pattern = "\\.csv$",
  full.names = TRUE
)

file_list

data_list <- lapply(file_list, read.csv)

names(data_list) <- tools::file_path_sans_ext(basename(file_list))

names(data_list)

View(data_list$accommodation)
View(data_list$activities)
View(data_list$branding)
View(data_list$decision_making_process)
View(data_list$ease_of_organisation_reasons)
View(data_list$ease_of_organisation)
View(data_list$environment)
View(data_list$expenditure_by_industry)
View(data_list$expenditure_by_payment_type)
View(data_list$impact_of_covid19)
View(data_list$itinerary)
View(data_list$maori_cultural_experience)
View(data_list$maori_cultural_sentiment)
View(data_list$mobility)
View(data_list$other_countries_visited)
View(data_list$poor_experiences)
View(data_list$secondary_purpose_of_visit)
View(data_list$self_transport)
View(data_list$survey_main_header)
View(data_list$transport_methods)
View(data_list$travel_party)
View(data_list$visitor_satisfaction_reasons)
View(data_list$visitor_satisfaction)

lapply(data_list, View)
