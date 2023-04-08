#' @param url Location of Airbnb data in .csv.gz format
reading_data <- function(url){
  data.table::fread(url) %>%
    tidyr::separate_rows(amenities, sep = ',') %>%
    dplyr::mutate(amenities = trimws(gsub('[\"]', '', amenities)),
                  amenities = gsub('\\]', '', amenities),
                  amenities = gsub('\\[', '', amenities)) 
  
}

#' @param data data.frame; data including Airbnb amenities
#' @param number integer; top number of amenties to include
top_amenities <- function(data, number){
  data %>%
    dplyr::count(amenities) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::top_n(number) %>%
    dplyr::pull(amenities)
}

#' @param data data.frame; Airbnb data
#' @param amenities_vector character vector; vector of amenities to review
#' @param test_var character; name of stratifying variable
#' @param inactive_date character date; last active date where listing should be removed
#' @param room_types character vector; types of rooms to include
#' @param standard_baths numeric vector; number of baths to include
#' @param standard_beds numeric vector; number of beds to include
#' @param review_lower_bound integer; minimum number of reviews to include
#' @param price_upper_bound numeric; maximum price per night to include
#' @param min_nights_upper_bound integer; minimum nights upper bound
create_analysis_set <- function(data, 
                                amenities_vector, 
                                test_var,
                                inactive_date = Sys.Date() - 365,
                                room_types = c('Entire home/apt', 'Private room'),
                                standard_baths = 1:3,
                                standard_beds = 0:5,
                                review_lower_bound = 0,
                                price_upper_bound = 5000,
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
                    reviews_per_month > review_lower_bound &
                    # Gets rid of inactive listings
                    last_review >= inactive_date &
                    # Prevent really high prices
                    price <= price_upper_bound & 
                    (bathroom %in% standard_baths) & beds %in% standard_beds) %>%
    dplyr::mutate(dist_from_mpls = sqrt((latitude - 44.978375)^2 + (longitude + 93.271713)^2)) %>%
    dplyr::select(reviews_per_month, price, beds, bathroom, all_of(test_var), room_type, dist_from_mpls)
}
