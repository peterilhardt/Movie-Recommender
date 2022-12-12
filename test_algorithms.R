### Meant to test application's algorithm selections against those of recommenderlab
### Not used in the application itself

library(recommenderlab)
source('recommenders.R')
source('helpers.R')

############################################################################
# read in ratings data, movies data, and IBCF weights
ratings = readRDS('data/ratings_sparseMat.RDS')
ratings_small = readRDS('data/ratings_sparseMat_small.RDS')
ibcf_weights_small = readRDS('data/IBCF_weights_30_neighbors.RDS')
movies = read_movies()
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0('test_', x, '.jpg?raw=true'))
most_popular = recommend_most_popular(ratings, movies, genre = 'All', n = 200)

# simulate new users' ratings for recommenderlab (first user basically no ratings)
movieIDs = colnames(ratings)
new_ratings = rep(NA, ncol(ratings))
new_ratings[which(movieIDs == "m2001")] = 5  # Lethal Weapon 2
new_user1 = matrix(new_ratings, nrow = 1, ncol = ncol(ratings),
                   dimnames = list(user = paste('TestUser1'), 
                                   item = movieIDs))
new_user1 = as(new_user1, 'realRatingMatrix')
new_ratings = rep(NA, ncol(ratings))
new_ratings[which(movieIDs == "m2858")] = 5  # American Beauty
new_ratings[which(movieIDs == "m260")] = 3  # Star Wars Episode IV
new_ratings[which(movieIDs == "m1196")] = 1  # Star Wars Episode V
new_ratings[which(movieIDs == "m1210")] = 2  # Star Wars Episode VI
new_ratings[which(movieIDs == "m480")] = 2  # Jurassic Park
new_ratings[which(movieIDs == "m2028")] = 4  # Saving Private Ryan
new_user2 = matrix(new_ratings, nrow = 1, ncol = ncol(ratings),
                   dimnames = list(user = paste('TestUser2'), 
                                   item = movieIDs))
new_user2 = as(new_user2, 'realRatingMatrix')
new_ratings = rep(NA, ncol(ratings))
new_ratings[which(movieIDs == "m260")] = 5  # Star Wars Episode IV
new_ratings[which(movieIDs == "m1196")] = 5  # Star Wars Episode V
new_ratings[which(movieIDs == "m1210")] = 5  # Star Wars Episode VI
new_user3 = matrix(new_ratings, nrow = 1, ncol = ncol(ratings),
                   dimnames = list(user = paste('TestUser3'), 
                                   item = movieIDs))
new_user3 = as(new_user3, 'realRatingMatrix')

############################################################################
# test 'Most Popular' algorithm selections against recommenderlab 'POPULAR'
recommender_POPULAR <- Recommender(as(ratings, 'realRatingMatrix'), method = "POPULAR", 
                                   parameter = list(normalize = NULL, 
                                                    aggregationPopularity = colCounts))
pred_reclab = predict(recommender_POPULAR, new_user1, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_most_popular(ratings, movies, genre = 'All', n = 10)
pred_app = reconcile_number_of_ratings(pred_app, most_popular, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))

############################################################################
# test 'Most Highly Rated' algorithm selections against recommenderlab 'POPULAR'
recommender_POPULAR <- Recommender(as(ratings, 'realRatingMatrix'), method = "POPULAR", 
                                   parameter = list(normalize = 'center', 
                                                    aggregationPopularity = colMeans))
pred_reclab = predict(recommender_POPULAR, new_user1, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_highest_rated(ratings, movies, genre = 'All', n = 10,
                                   normalize = TRUE, agg = 'mean')
pred_app = reconcile_number_of_ratings(pred_app, most_popular, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))

############################################################################
# test 'Similar Items' algorithm selections against recommenderlab 'IBCF'
recommender_IBCF <- Recommender(as(ratings, 'realRatingMatrix'), method = "IBCF",
                                parameter = list(normalize = 'center', 
                                                 method = 'Cosine', 
                                                 k = 30))
pred_reclab = predict(recommender_IBCF, new_user2, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_IBCF(ibcf_weights_small, new_user2@data, normalize = TRUE)
pred_app = topN(pred_app, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))

recommender_IBCF <- Recommender(as(ratings, 'realRatingMatrix'), method = "IBCF",
                                parameter = list(normalize = NULL, 
                                                 method = 'Cosine', 
                                                 k = 30))
pred_reclab = predict(recommender_IBCF, new_user3, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_IBCF(ibcf_weights_small, new_user3@data, normalize = FALSE)
pred_app = topN(pred_app, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))

############################################################################
# test 'Similar Users' algorithm selections against recommenderlab 'UBCF'
recommender_UBCF <- Recommender(as(ratings_small, 'realRatingMatrix'), 
                                method = "UBCF",
                                parameter = list(normalize = 'center', 
                                                 method = 'Cosine', 
                                                 nn = 20,
                                                 weighted = TRUE))
pred_reclab = predict(recommender_UBCF, new_user2, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_UBCF(ratings_small, new_user2@data, normalize = TRUE,
                          weighted = TRUE, neighbors = 20)
pred_app = topN(pred_app, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))

recommender_UBCF <- Recommender(as(ratings_small, 'realRatingMatrix'), 
                                method = "UBCF",
                                parameter = list(normalize = NULL, 
                                                 method = 'Cosine', 
                                                 nn = 20,
                                                 weighted = TRUE))
pred_reclab = predict(recommender_UBCF, new_user3, n = 10, type = 'topNList')
pred_reclab = pred_reclab@itemLabels[pred_reclab@items[[1]]]
pred_app = recommend_UBCF(ratings_small, new_user3@data, normalize = FALSE,
                          weighted = TRUE, neighbors = 20)
pred_app = topN(pred_app, movies, n = 10)
print(data.frame(recommenderlab = pred_reclab, app = paste0('m', pred_app$MovieID)))
