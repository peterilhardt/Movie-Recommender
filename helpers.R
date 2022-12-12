library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(Matrix)

read_movies <- function() {
  # read in and format the movies metadata
  movies = read_delim(file.path('data', 'movies.dat'), delim = '::',
             col_names = c('MovieID', 'Title', 'Genre'),
             col_types = 'icc') %>% 
    mutate(Year = as.numeric(str_sub(Title, start = -5L, end = -2L)),
           Title = iconv(Title, 'latin1', 'UTF-8'),  # convert accented characters
           Genre = str_split(Genre, coll('|')),  # creates a list column
           Title = str_sub(Title, start = 1L, end = -8L)) %>%   
    # let's separate the genres into individual one-hot encoded columns
    unnest(Genre) %>% 
    mutate(temp = 1) %>% 
    pivot_wider(names_from = Genre, values_from = temp, values_fill = 0)
  return(movies)
} 

get_user_ratings <- function(value_list, all_movie_ids, prefix = "items") {
  # extract user's provided ratings from app and convert to 1-row sparse matrix
  dat <- data.frame(MovieID = sapply(strsplit(names(value_list), "_"), 
                                     function(x) ifelse(x[[1]] == prefix, 
                                                        x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat <- dat[!is.na(dat$MovieID),]
  dat$MovieID = paste0('m', dat$MovieID)
  dat$Rating = as.numeric(dat$Rating)
  dat <- dat[!is.na(dat$Rating),]
  
  out = matrix(0, nrow = 1, ncol = length(all_movie_ids), 
               dimnames = list('NewUser', all_movie_ids))
  out[, dat$MovieID] = dat$Rating
  return(as(out, 'sparseMatrix'))
}

filter_weights <- function(weights, neighbors = 30, weighted = TRUE) {
  # filter original weights matrix (representing 100 nearest item neighbors)
  # down to number of neighbors requested by user (needed for IBCF)
  p = ncol(weights)
  start = p - neighbors + 1
  weights = apply(weights, 1, function(r) {
    idx = order(r, decreasing = FALSE)[start:p]
    r[-idx] = 0  # set weights to 0 for non-neighbors
    
    if (!weighted) {
      r[idx] = 1  # treat all neighbors equally -> weights of 1
    }
    return(r)
  })
  
  weights = as(t(weights), 'sparseMatrix')
  return(weights)
}

filter_weights2 <- function(weights, neighbors = 30, weighted = TRUE) {
  # filter original weights matrix (representing 100 nearest item neighbors)
  # down to number of neighbors requested by user (needed for IBCF)
  # this version is much slower but more memory-efficient, maintains sparsity
  p = ncol(weights)
  start = p - neighbors + 1
  for (i in 1:nrow(weights)) {
    idx = order(weights[i,], decreasing = FALSE)[start:p]
    weights[i, -idx] = 0  # set weights to 0 for non-neighbors

    if (!weighted) {
      weights[i, idx] = 1  # treat all neighbors equally -> weights of 1
    }
  }
  return(weights)
}

topN <- function(pred_ratings, movies, n = 10) {
  # get the top N recommended movies based on predicted ratings from UBCF/IBCF
  movies_f = movies %>% 
    select(MovieID, Title, Year, image_url) %>% 
    mutate(MovieID_mod = paste0('m', MovieID))
  
  pred_ratings = pred_ratings[1,]
  ids = sort(pred_ratings, decreasing = TRUE, na.last = NA)
  ids = names(ids[1:min(n, length(ids))])
  out = data.frame(MovieID_mod = ids) %>% 
    left_join(movies_f, by = 'MovieID_mod') %>% 
    select(MovieID, Title, Year, image_url)
  
  return(out)
}

reconcile_number_of_ratings <- function(new_recs_df, popular_films, movies, 
                                        new_user_ratings = NULL, n = 10){
  # if fewer than N films are returned by the recommender, append films from 
  # the "most popular films" list that weren't already recommended
  # of course, we don't want to recommend films the user already rated/watched
  if (nrow(new_recs_df) >= n) {
    return(new_recs_df)
  }
  
  num_to_add = n - nrow(new_recs_df)
  candidate_ids = setdiff(popular_films$MovieID, new_recs_df$MovieID)
  if (!is.null(new_user_ratings)) {
    rated_ids = colnames(new_user_ratings)[new_user_ratings[1,] != 0]
    rated_ids = as.numeric(str_replace_all(rated_ids, 'm', ''))
    candidate_ids = setdiff(candidate_ids, rated_ids)
  }
  
  add_ids = candidate_ids[1:num_to_add]
  add_df = data.frame(MovieID = add_ids) %>% 
    left_join(movies, by = 'MovieID') %>% 
    select(MovieID, Title, Year, image_url)
  out = new_recs_df %>% 
    bind_rows(add_df)
  return(out)
}
