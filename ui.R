library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ShinyRatingInput)
library(shinyjs)

source('busy_indicator.R')

collapse_code = "shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}"

shinyUI(
  navbarPage(
    id = 'ui',
    title = 'Movie Recommender',
    windowTitle = 'Movie Recommender',
    inverse = TRUE,
    
    tabPanel('Genres',
      dashboardPage(
        dashboardHeader(disable = TRUE),
        shinydashboard::dashboardSidebar(disable = TRUE),
        dashboardBody(
          sidebarLayout(
            sidebarPanel(
              width = 2,
              includeCSS("css/movies.css"),
              selectizeInput('genre',
                             'Choose a Genre',
                             choices = list('All',
                                            'Action',
                                            'Adventure',
                                            'Animation',
                                            "Children's",
                                            'Comedy',
                                            'Crime',
                                            'Documentary',
                                            'Drama',
                                            'Fantasy',
                                            'Film-Noir',
                                            'Horror',
                                            'Musical',
                                            'Mystery',
                                            'Romance',
                                            'Sci-Fi',
                                            'Thriller',
                                            'War',
                                            'Western'),
                             selected = 'All'
              ),
              br(),
              radioButtons('rating_type',
                'Rating Method',
                choices = list('Most Popular',
                               'Highest Rated'),
                selected = 'Most Popular'
              ),
              checkboxInput('normalize_users1',
                            'Normalize Ratings?',
                            value = TRUE),
              br(),
              strong('Show Me Movies!'),
              withBusyIndicatorUI(
                actionButton("btn_genre", "Submit", 
                             class = "btn-warning")
              )
            ),
            mainPanel(
              width = 10,
              fluidRow(
                div(
                  "This page recommends films that are most popular (i.e. 
                  most often-rated) or most highly-rated based on ratings of ", 
                  a("MovieLens", href = "https://movielens.org"), 
                  " users, and can be filtered by genre.", 
                  class = "introtext"
                )
              ),
              fluidRow(
                useShinyjs(),
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Top Picks",
                    br(),
                    div(class = "resultitems",
                      uiOutput("results_genre")
                    ),
                    sidebar = boxSidebar(
                      id = 'genre_sidebar',
                      width = 33,
                      sliderInput('num_show_genre', ticks = FALSE,
                                  label = 'Number of Recommendations',
                                  min = 1, max = 100, value = 10, step = 1,
                                  round = TRUE)
                    )
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel('Similar Movies',
      dashboardPage(
        dashboardHeader(disable = TRUE),
        shinydashboard::dashboardSidebar(disable = TRUE),
        dashboardBody(
          sidebarLayout(
            sidebarPanel(
              width = 2,
              includeCSS("css/movies.css"),
              numericInput('num_items_neighbors', 
                           'How Many Movies to Compare?',
                           value = 30, min = 1, max = 100, 
                           step = 1),
              br(),
              checkboxInput('weight_items',
                            'Weight Movies by Similarity?',
                            value = TRUE),
              br(),
              strong('Show Me Movies!'),
              withBusyIndicatorUI(
                actionButton("btn_items", "Submit", 
                             class = "btn-warning")
              )
            ),
            mainPanel(
              width = 10,
              includeCSS("css/movies.css"),
              fluidRow(
                div(
                  "This page recommends films that are similar to ones you 
                  like based on your ratings and those of ", 
                  a("MovieLens", href = "https://movielens.org"), 
                  " users. The more ratings you provide, the better the 
                  results will be.", 
                  class = "introtext"
                )
              ),
              fluidRow(
                useShinyjs(),
                extendShinyjs(text = collapse_code, functions = c('collapse')),
                box(id = 'items_rating_box', width = 12, 
                    title = "Rate as many movies as possible", 
                    status = "info", solidHeader = TRUE, collapsible = TRUE,
                    div(class = "rateitems",
                        uiOutput('ratings_items')
                    )
                )
              ),
              fluidRow(
                useShinyjs(),
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Top Picks",
                    br(),
                    div(class = "resultitems",
                        uiOutput("results_items")
                    ),
                    sidebar = boxSidebar(
                      id = 'items_sidebar',
                      width = 33,
                      sliderInput('num_show_items', ticks = FALSE,
                                  label = 'Number of Recommendations',
                                  min = 1, max = 100, value = 10, step = 1,
                                  round = TRUE)
                    )
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel('Similar Users',
      dashboardPage(
        dashboardHeader(disable = TRUE),
        shinydashboard::dashboardSidebar(disable = TRUE),
        dashboardBody(
          sidebarLayout(
            sidebarPanel(
              width = 2,
              includeCSS("css/movies.css"),
              numericInput('num_users_neighbors', 
                           'How Many Users to Compare You to?',
                           value = 20, min = 1, max = 100, 
                           step = 1),
              br(),
              checkboxInput('weight_users',
                            'Weight Users by Similarity?',
                            value = TRUE),
              checkboxInput('normalize_users2',
                            'Normalize Ratings?',
                            value = TRUE),
              br(),
              strong('Show Me Movies!'),
              withBusyIndicatorUI(
                actionButton("btn_users", "Submit",
                            class = "btn-warning")
              )
            ),
            mainPanel(
              width = 10,
              includeCSS("css/movies.css"),
              fluidRow(
                div(
                  "This page recommends films that ", 
                  a("MovieLens", href = "https://movielens.org"), 
                  " users with similar interests to you liked based on their 
                  ratings. The more ratings you provide, the better the 
                  results will be.", class = "introtext"
                )
              ),
              fluidRow(
                useShinyjs(),
                extendShinyjs(text = collapse_code, functions = c('collapse')),
                box(id = 'users_rating_box', width = 12, 
                    title = "Rate as many movies as possible", 
                    status = "info", solidHeader = TRUE, collapsible = TRUE,
                    div(class = "rateitems",
                        uiOutput('ratings_users')
                    )
                )
              ),
              fluidRow(
                useShinyjs(),
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Top Picks",
                    br(),
                    div(class = "resultitems",
                        uiOutput("results_users")
                    ),
                    sidebar = boxSidebar(
                      id = 'users_sidebar',
                      width = 33,
                      sliderInput('num_show_users', ticks = FALSE,
                                  label = 'Number of Recommendations',
                                  min = 1, max = 100, value = 10, step = 1,
                                  round = TRUE)
                    )
                )
              )
            )
          )
        )
      )
    )
  )
)
