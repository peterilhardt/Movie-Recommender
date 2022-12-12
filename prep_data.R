### Meant to prepare weights and ratings data files for use in application
### Not used in the application itself

library(readr)
library(Matrix)
library(proxy)

prep_ratings <- function(data_dir) {
  # reads ratings from "ratings.dat" file and converts them to sparse matrix
  ratings = read_delim(file.path(data_dir, 'ratings.dat'), delim = '::', 
                       col_names = c('UserID', 'MovieID', 'Rating', 'Timestamp'), 
                       col_types = "iiii")
  
  i = paste0('u', ratings$UserID)
  j = paste0('m', ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = TRUE)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  return(Rmat)
}

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
  # fast C++ version of row-wise cosine similarity from the "proxyC" library
  # by default ignores elements that are NA in either row
  sim = proxy::simil(X, Y, method = 'cosine')
  return(as(sim, 'matrix'))
}

build_ibcf_weights <- function(ratings_matrix, normalize = TRUE, 
                               neighbors = 100, weighted = TRUE) {
  # returns sparse matrix of IBCF weights of dim. (no. items, no. items)
  data = as(ratings_matrix, 'matrix')
  data[data == 0] = NA
  
  # center row-wise
  if (normalize) {
    data = center_normalize(data)[[1]]
  }
  
  # calculate cosine sim. between each pair of items in data 
  # produces huge matrix of dim. (no. items, no. items)
  weights = cosine_similarity(t(data), t(data))
  weights = (weights + 1) / 2  # force into range 0-1
  weights[!is.finite(weights)] = 0
  weights[is.na(weights)] = 0
  weights[weights < 0] = 0  # some numerical precision issues
  diag(weights) = 0
  
  # pre-compute the k nearest items to each item and only save these
  for (i in 1:nrow(weights)) {
    start = ncol(weights) - neighbors + 1
    idx = order(weights[i,], decreasing = FALSE)[start:ncol(weights)]
    weights[i, -idx] = 0  # set weights to 0 for non-neighbors
    
    if (!weighted) {
      weights[i, idx] = 1  # treat all neighbors equally -> weights of 1
    }
  }
  
  weights = as(weights, 'sparseMatrix')
  return(weights)
}

###############################################################################

ratings = prep_ratings('./data')
weights = build_ibcf_weights(ratings, normalize = TRUE, neighbors = 100,
                             weighted = TRUE)
saveRDS(ratings, file = './data/ratings_sparseMat.RDS')
saveRDS(weights, file = './data/IBCF_weights_100_neighbors.RDS')
