#' @param url Location of data in .csv.gz format
reading_data <- function(url){
  first_read <- data.table::fread(url)
  
  processed <- first_read %>%
    tidyr::separate_rows(amenities, sep = ',') %>%
    dplyr::mutate(amenities = trimws(gsub('[\"]', '', amenities)),
           amenities = gsub('\\]', '', amenities),
           amenities = gsub('\\[', '', amenities)) 
  
  return(processed)
}

top_amenities <- function(data, number){
  data %>%
    dplyr::count(amenities) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::top_n(number) %>%
    dplyr::pull(amenities)
}

create_analysis_set <- function(data, 
                                amenities_vector, 
                                test_var,
                                inactive_date = Sys.Date() - 365,
                                room_types = c('Entire home/apt', 'Private room'),
                                standard_baths = 1:3,
                                standard_beds = 0:5,
                                review_lower_bound = 0,
                                min_nights_upper_bound = 7){
  
  data %>%
    dplyr::filter(amenities %in% amenities_vector) %>%
    tidyr::pivot_wider(names_from = amenities, 
                values_from = amenities,
                values_fn = ~ !is.na(.x),
                values_fill = FALSE) %>%
    dplyr::mutate(bathroom = as.numeric(stringr::str_extract(bathrooms_text, '[0-9]')),
           price = as.numeric(gsub(",", "", gsub('\\$', '', price)))) %>%
    # Gets rid of hotels and shared rooms
    dplyr::filter(room_type %in% room_types &
             # Gets rid of long term rentals (like apartments)
             minimum_nights < min_nights_upper_bound &
             # Gets rid of brand new listings 
             number_of_reviews > review_lower_bound &
             # Gets rid of inactive listings
             last_review >= inactive_date,
           (bathroom %in% standard_baths) & beds %in% standard_beds) %>%
    dplyr::select(price, beds, bathroom, all_of(test_var), room_type)
}
  