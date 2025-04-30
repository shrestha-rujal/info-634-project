# READ ME!!

# combined_dataset contains relevant columns from: cleaned_demographics, activities,
# itinerary, secondary_purpose_of_visit, visitor_satisfaction, and visitor_satisfaction_reasons.

# combined_unique_ids contains the same data except the columns from activities and itinerary
# as those tables contain duplicate response_id values and are better kept in those tables.
# so using this dataset with the activites and itinerary tables as well, is best.

# Creating a combined dataset for all relevant data (includes duplicate response ids)
combined_dataset = merge(cleaned_demographics[,c("response_id","country_of_residence","country_of_residence_group","gender","first_nz_trip","purpose_of_visit_main","purpose_subtype","single_or_others","no_people_over_15","no_people_under_15","age_range")], activities[], by="response_id")
combined_dataset = merge(combined_dataset[], itinerary[], by="response_id")
combined_dataset = merge(combined_dataset[], secondary_purpose_of_visit[], by="response_id")
combined_dataset = merge(combined_dataset[], visitor_satisfaction[], by="response_id")
combined_dataset = merge(combined_dataset[], visitor_satisfaction_reasons[], by="response_id")
head(combined_dataset)

# Creating a combined dataset only including unique response_id values
# "activities" and "itinerary" have been ommitted as they are better stored in seperate tables
combined_unique_ids = merge(cleaned_demographics[,c("response_id","country_of_residence","country_of_residence_group","gender","first_nz_trip","purpose_of_visit_main","purpose_subtype","single_or_others","no_people_over_15","no_people_under_15","age_range")], secondary_purpose_of_visit[], by="response_id")
combined_unique_ids = merge(combined_unique_ids[], visitor_satisfaction[], by="response_id")
combined_unique_ids = merge(combined_unique_ids[], visitor_satisfaction_reasons[], by="response_id")
head(combined_unique_ids)

# Checking that there are no duplicate response_ids
dup_rows <- duplicated(combined_unique_ids[c("response_id")]) | duplicated(combined_unique_ids[c("response_id")], fromLast = TRUE)
combined_unique_ids[dup_rows, ]

# Exporting both tables as .csv
write.csv(combined_dataset,"C:\\Users\\OEM\\Documents\\R\\combined_dataset.csv", row.names = FALSE) 
write.csv(combined_unique_ids,"C:\\Users\\OEM\\Documents\\R\\combined_unique_ids.csv", row.names = FALSE) 