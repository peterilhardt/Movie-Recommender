# load functions
source('helpers.R')
source('recommenders.R')

# read in data
movies <- read_movies()
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ibcf_weights = readRDS('data/IBCF_weights_100_neighbors.RDS')
ibcf_weights_small = readRDS('data/IBCF_weights_30_neighbors.RDS')
ratings = readRDS('data/ratings_sparseMat.RDS')
ratings_small = readRDS('data/ratings_sparseMat_small.RDS')

# get a list of the most popular movies to show the user
most_popular = recommend_most_popular(ratings, movies, genre = 'All', n = 200)

show_ratings <- function(num_rows = 20, num_cols = 6, prefix = 'items') {
  # shows table with movies to be rated by user
  renderUI({
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_cols, function(j) {
        list(box(width = floor(12 / num_cols),
                 div(class = 'film', 
                     img(src = most_popular$image_url[(i - 1) * num_cols + j], 
                         style = "max-height: 150")),
                 div(class = 'film', 
                     strong(most_popular$Title[(i - 1) * num_cols + j])),
                 div(class = 'ratinginput', 
                     ratingInput(paste0(prefix, "_", most_popular$MovieID[(i - 1) * num_cols + j]), 
                                 label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
}

display_results <- function(df, num_cols = 6) {
  # displays table of results with movie recommendations
  renderUI({
    recom_result <- df()
    num_rows = ceiling(nrow(recom_result) / num_cols)
    final_row_n = nrow(recom_result) %% num_cols
    final_row_n = ifelse(final_row_n == 0, num_cols, final_row_n)
    
    lapply(1:num_rows, function(i) {
      cols = ifelse(i == num_rows, final_row_n, num_cols)
      list(fluidRow(lapply(1:cols, function(j) {
        box(width = floor(12 / num_cols), status = "success", solidHeader = TRUE, 
            title = paste0("Pick ", (i - 1) * num_cols + j),
            
            div(class = 'film', 
                a(img(src = recom_result$image_url[(i - 1) * num_cols + j], 
                      style = "max-height: 100%"))
            ),
            div(class = 'resulttitle', 
                strong(recom_result$Title[(i - 1) * num_cols + j])
            )
        )        
      }))) # columns
    }) # rows
  }) 
}


shinyServer(function(input, output, session) {
  
  # show the movies to be rated on items and users tabs
  output$ratings_items <- show_ratings(num_rows = 20, num_cols = 6, prefix = "items")
  output$ratings_users <- show_ratings(num_rows = 20, num_cols = 6, prefix = "users")
  movie_df <- reactiveValues(rec_genre = NULL, rec_items = NULL, rec_users = NULL)
  
  # Calculate recommendations when the submit button is clicked
  observeEvent(input$btn_genre, {
    withBusyIndicatorServer("btn_genre", { # showing the busy indicator
      # get list of most popular movies for the genre depending on type selected
      genre = input$genre
      mode = input$rating_type
      normalize = input$normalize_users1
      
      if (mode == 'Most Popular') {
        df = recommend_most_popular(ratings, movies, genre, n = 100)
      } else {  # Highest Rated
        df = recommend_highest_rated(ratings, movies, genre, n = 100, 
                                     normalize = normalize, agg = 'mean')
      }
      
      if (nrow(df) < 100) {
        # if we end up with fewer than N films, append films from the "most 
        # popular" list that weren't already recommended or reviewed by user
        df = reconcile_number_of_ratings(df, most_popular, movies, n = 100)
      }
      movie_df$rec_genre = df
    })
  })
  
  df_genre <- reactive({
    req(!is.null(movie_df[['rec_genre']]))
    N = input$num_show_genre
    movie_df[['rec_genre']] %>% 
      head(N)
  })
  
  observeEvent(input$btn_items, {
    withBusyIndicatorServer("btn_items", { # showing the busy indicator
      # hide the rating container
      # useShinyjs()
      # jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      # runjs(jsCode)
      js$collapse('items_rating_box')
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list, colnames(ratings), 
                                       prefix = 'items')
      
      if (sum(user_ratings) == 0) {
        # if no reviews provided, just recommend the 10 most popular items
        df = most_popular[1:100,]
      } else {
        neighbors = input$num_items_neighbors
        weighted = input$weight_items
        normalize = TRUE
        
        # filter down the original weights to the K nearest neighbors requested
        weights = ibcf_weights
        if (neighbors == 30) {  # use the pre-loaded small version
          weights = ibcf_weights_small
        }
        else if (neighbors < 100) {
          # using the slow, memory-efficient version here due to shinyapps.io
          # free tier memory constraints; "filter_weights" is faster
          weights = filter_weights2(weights, neighbors = neighbors, 
                                   weighted = weighted)
        }
        
        unique_ratings = unique(user_ratings[1,][user_ratings[1,] != 0])
        if (length(unique_ratings) == 1) {
          # if user gives all films the same rating and normalizes, end up with
          # all zeros for IBCF weights -> let's just skip normalization
          normalize = FALSE
        }
        
        # obtain predicted ratings and get top N movie recommendations
        pred_ratings = recommend_IBCF(weights, user_ratings, 
                                      normalize = normalize)
        df = topN(pred_ratings, movies, n = 100)
        if (nrow(df) < 100) {
          # if we end up with fewer than N films, append films from the "most 
          # popular" list that weren't already recommended or reviewed by user
          df = reconcile_number_of_ratings(df, most_popular, movies, 
                                           user_ratings, n = 100)
        }
      }
      movie_df$rec_items = df
    })
  })
  
  df_items <- reactive({
    req(!is.null(movie_df[['rec_items']]))
    N = input$num_show_items
    movie_df[['rec_items']] %>% 
      head(N)
  })
  
  observeEvent(input$btn_users, {
    withBusyIndicatorServer("btn_users", { # showing the busy indicator
      # hide the rating container
      # useShinyjs()
      # jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      # runjs(jsCode)
      js$collapse('users_rating_box')
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list, colnames(ratings), 
                                       prefix = 'users')
      
      if (sum(user_ratings) == 0) {
        # if no reviews provided, just recommend the 10 most popular items
        df = most_popular[1:100,]
      } else {
        neighbors = input$num_users_neighbors
        weighted = input$weight_users
        normalize = input$normalize_users2
        
        unique_ratings = unique(user_ratings[1,][user_ratings[1,] != 0])
        if (length(unique_ratings) == 1 && normalize) {
          # if user gives all films the same rating and normalizes, end up with
          # all zeros/NaNs for UBCF weights -> let's just skip normalization
          normalize = FALSE
        }
        
        # obtain predicted ratings and get top N movie recommendations
        # using only first 500 users here due to memory constraints with shinyapps.io
        pred_ratings = recommend_UBCF(ratings_small, user_ratings, 
                                      normalize = normalize, 
                                      weighted = weighted,
                                      neighbors = neighbors)
        df = topN(pred_ratings, movies, n = 100)
        if (nrow(df) < 100) {
          # if we end up with fewer than N films, append films from the "most 
          # popular" list that weren't already recommended or reviewed by user
          df = reconcile_number_of_ratings(df, most_popular, movies, 
                                           user_ratings, n = 100)
        }
      }
      movie_df$rec_users = df
    })
  })
  
  df_users <- reactive({
    req(!is.null(movie_df[['rec_users']]))
    N = input$num_show_users
    movie_df[['rec_users']] %>% 
      head(N)
  })
  
  # display the recommendations
  output$results_genre <- display_results(df_genre, num_cols = 5)
  output$results_items <- display_results(df_items, num_cols = 5)
  output$results_users <- display_results(df_users, num_cols = 5)
  
})
