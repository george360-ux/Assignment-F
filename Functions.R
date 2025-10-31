## Functions for the Strawberry Data Cleaning


drop_one_value_col <- function(df) {
  # Find column names to be dropped
  single_value_cols <- names(df)[sapply(df, n_distinct) == 1]
  
  # Remove the columns with only one unique value
  cleaned_df <- df %>%
    select(where(~ n_distinct(.) > 1))
  
  # Check if any columns were dropped and report
  if (length(single_value_cols) == 0) {
    print("No columns with only one unique value were found.")
  } else {
    print(paste("Columns dropped:", paste(single_value_cols, collapse = ", ")))
  }
  
  # Return the cleaned data frame
  return(cleaned_df)
}



drop_duplicate_rows <- function(df) {
  # Step 1: Identify "conflicting" rows where State, Year, Data Item are the same
  # but Period or Value differ.
  conflicts <- df %>%
    group_by(State, Year, `Data Item`) %>%
    # Filter for groups that have more than one distinct Value or more than one distinct Period
    filter(n_distinct(Value) > 1 | n_distinct(Period) > 1) %>%
    ungroup()
  
  #  if (nrow(conflicts) > 0) {
  #    message("Warning: Inconsistent rows detected. These rows have the same State, Year, and Data Item but conflicting Value or Period information.")
  #    print(conflicts)
  #  }
  
  # Step 2: Clean the data
  cleaned_df <- df %>%
    mutate(Period = str_replace(Period, "MARKETING YEAR", "YEAR")) %>%
    group_by(across(-c(Period, Value))) %>% # group by everything except Period and Value
    mutate(is_duplicate = n() > 1) %>% 
    ungroup() %>%
    mutate(Period = if_else(is_duplicate, "YEAR", Period)) %>%
    # Now group by your final key columns and remove duplicates
    group_by(State, Year, `Data Item`, Period) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(-is_duplicate)
  
  return(cleaned_df)
}
