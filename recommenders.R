library(dplyr)
library(tidyr)
library(stringr)
library(proxy)

center_normalize <- function(ratings_matrix) {
  # remove the row mean from each row ignoring NAs (or zeros for sparseMatrix)
  if (is(ratings_matrix, 'sparseMatrix')) {
    binary = ratings_matrix != 0
    user_means = rowSums(ratings_matrix) / rowSums(binary)
    user_means[is.na(user_means)] = 0
    normalized = ratings_matrix
    normalized@x = ratings_matrix@x - user_means[ratings_matrix@i + 1]
  } else {  # regular matrix
    binary = !is.na(ratings_matrix)
    user_means = rowSums(ratings_matrix, na.rm = TRUE) / rowSums(binary)
    user_means[is.na(user_means)] = 0
    normalized = ratings_matrix - user_means
  }
  return(list(normalized, user_means))
}

cosine_similarity <- function(X, Y) {
  # fast C++ version of pairwise cosine similarity that ignores NAs
  sim = proxy::simil(X, Y, method = 'cosine')
  return(as(sim, 'matrix'))
}

recommend_most_popular <- function(ratings, movies, genre = 'All', n = 10) {
  # recommend the N movies with the most ratings, regardless of what the ratings are
  if (genre == 'All') {
    movies_f = movies %>% 
      select(MovieID, Title, Year, image_url) %>% 
      mutate(MovieID_mod = paste0('m', MovieID))
  } else {
    movies_f = movies %>% 
      filter(!! sym(genre) == 1) %>% 
      select(MovieID, Title, Year, image_url) %>% 
      mutate(MovieID_mod = paste0('m', MovieID))
  }
  
  mat_f = ratings[,colnames(ratings) %in% movies_f$MovieID_mod]
  if (is(mat_f, 'sparseMatrix')) {
    counts = colSums(mat_f != 0)
  } else {
    counts = colSums(!is.na(mat_f))
  }
  
  scores = colSums(mat_f, na.rm = TRUE) / counts
  df = tibble(MovieID_mod = names(counts), counts, scores) %>% 
    arrange(desc(counts), desc(scores)) %>% 
    head(n) 
  
  out = df %>% left_join(movies_f, by = 'MovieID_mod') %>% 
    select(MovieID, Title, Year, image_url)
  return(out)
}

recommend_highest_rated <- function(ratings, movies, genre = 'All', n = 10, 
                                    normalize = TRUE, agg = 'sum') {
  # recommend the N most highly-rated movies (by average or summed ratings)
  if (genre == 'All') {
    movies_f = movies %>% 
      select(MovieID, Title, Year, image_url) %>% 
      mutate(MovieID_mod = paste0('m', MovieID))
  } else {
    movies_f = movies %>% 
      filter(!! sym(genre) == 1) %>% 
      select(MovieID, Title, Year, image_url) %>% 
      mutate(MovieID_mod = paste0('m', MovieID))
  }
  
  if (normalize) {
    ratings_norm = center_normalize(ratings)[[1]]
  } else {
    ratings_norm = ratings
  }
  
  mat_f = ratings_norm[,colnames(ratings_norm) %in% movies_f$MovieID_mod]
  scores = colSums(mat_f, na.rm = TRUE)
  if (agg == 'mean') {
    if (is(mat_f, 'sparseMatrix')) {
      tmp = ratings[,colnames(ratings) %in% movies_f$MovieID_mod]
      scores = scores / colSums(tmp != 0)
    } else {
      scores = scores / colSums(!is.na(mat_f))
    }
  }
  df = tibble(MovieID_mod = names(scores), scores) %>% 
    arrange(desc(scores)) %>% 
    head(n) 
  
  out = df %>% left_join(movies_f, by = 'MovieID_mod') %>% 
    select(MovieID, Title, Year, image_url)
  return(out)
}

recommend_UBCF <- function(ratings_matrix, new_user_ratings,
                         normalize = TRUE, weighted = TRUE, neighbors = 20) {
  # predict new ratings using User-Based Collaborative Filtering with specified
  # number of users (neighbors) to compare the current user to
  # returns predicted ratings in matrix (combine with topN to get recommendations)
  # can normalize users' ratings first and/or weight by similarity to new user
  data = as.matrix(ratings_matrix)
  data[data == 0] = NA
  new = as.matrix(new_user_ratings)
  new[new == 0] = NA
  
  # center row-wise
  if (normalize) {
    data = center_normalize(data)[[1]]
    norm = center_normalize(new)
    new = norm[[1]]
    new_means = norm[[2]]
  }
  
  # calculate cosine sim. between each new user and each data user 
  # produces matrix of dim. (no. new users, no. data users)
  weights = cosine_similarity(new, data)
  weights = (weights + 1) / 2  # force into range 0-1
  
  for (i in 1:nrow(weights)) {
    # identify K nearest neighbors by highest similarity scores
    idx = order(weights[i,], decreasing = TRUE, na.last = NA)[1:neighbors]
    weights[i, -idx] = 0  # set weights to 0 for non-neighbors
    
    if (!weighted) {
      weights[i, idx] = 1  # treat all neighbors equally -> weights of 1
    } 
  }
  
  # calculate new ratings as weighted averages of data users' ratings
  # note that for each item we only normalize by the weights (summed) 
  # associated with users in data who had a rating for the item
  data_c = data
  data_c[is.na(data)] = 0
  new_ratings = (weights %*% data_c) / (weights %*% (!is.na(data)))
  new_ratings[!is.finite(new_ratings)] = NA  # remove the NaNs from zero-division
  
  # if user already rated film, don't provide a new rating
  new_ratings[!is.na(new)] = NA
  
  if (normalize) {
    # need to add back the new user means to the new ratings
    # new_ratings@x = new_ratings@x + new_means[new_ratings@i + 1]
    new_ratings = new_ratings + new_means
  }
  
  return(new_ratings)
}

recommend_IBCF <- function(weights, new_user_ratings, normalize = TRUE) {
  # predict new ratings using Item-Based Collaborative Filtering
  # app uses a pre-defined item-item similarity matrix as weights
  # returns predicted ratings in matrix (combine with topN to get recommendations)
  # normalizing shouldn't do anything here, but keeping for consistency
  new = as.matrix(new_user_ratings)
  new[new == 0] = NA
  
  # center row-wise
  if (normalize) {
    norm = center_normalize(new)
    new = norm[[1]]
    new_means = norm[[2]]
  }
  
  # calculate new ratings as weighted averages of new users' ratings 
  # for similar items; note that for each item we only normalize by 
  # the weights (summed) associated with items the new user rated
  new_c = new
  new_c[is.na(new)] = 0
  new_ratings = t((weights %*% t(new_c)) / (weights %*% t(!is.na(new))))
  new_ratings[!is.finite(new_ratings)] = NA  # remove the NaNs from zero-division
  
  # if user already rated film, don't provide a new rating
  new_ratings[!is.na(new)] = NA
  
  if (normalize) {
    # need to add back the new user means to the new ratings
    # new_ratings@x = new_ratings@x + new_means[new_ratings@i + 1]
    new_ratings = new_ratings + new_means
  }
  
  return(new_ratings)
}
