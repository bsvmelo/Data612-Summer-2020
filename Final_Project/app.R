library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Anime Recommendations"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      radioButtons("radio", h4("Desired Recommendation:"),
                   choices = c("Content-Based Genre" = "cbgenre", 
                               "Content-Based Anime" = "cbanime",
                               "Collaborative Filtering - Anime" = "cfanime")), 
      
      uiOutput("radiobuttonChoiceOutput"), 
      
      sliderInput("nbr_recs", h4("# of Anime to Recommend"),
                  min = 5, max = 30, value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: HTML table with requested number of observations ----
      tableOutput("reg_rec_list"), 
      tableOutput("sur_rec_list")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$radiobuttonChoiceOutput <- renderUI({
    switch(input$radio,
           cbgenre = selectInput("genre",
                                 label = "Choose Genre",
                                 choices = sort(uniq_genre)),  
           
           cbanime = selectInput("anime1",
                                 label = "Choose Anime",
                                 choices = sort(anime_name$name), 
                                 selected = "Pokemon"), 
           
           cfanime = selectInput("users",
                                 label = "Choose User",
                                 choices = sort(unique(ubcf_mtx$user)), 
                                 selected = 10072
           )
           
    )})
  
  # Show the first "n" observations ----
  output$reg_rec_list <- renderTable({
    if (input$radio == "cbgenre") {
      n_recommended <- input$nbr_recs
      s_genre <- input$genre
      rec_genre <- sort(genre_t[as.character(s_genre), ], decreasing = TRUE)[1:n_recommended]
      rec_id_genre <- as.numeric(names(rec_genre))
      rec_names <- anime_name[anime_name$anime_id %in% rec_id_genre, ]$name
      header <- sprintf("Most viewed Animes in %s", s_genre)
      tbl_reg <- data.frame(rec_names)
      names(tbl_reg)[names(tbl_reg) == "rec_names"] <- header
    }
    else if (input$radio == "cbanime") {
      n_recommended <- input$nbr_recs
      a_name = input$anime1
      a_id <- anime_name[anime_name$name == a_name, ]$anime_id
      recs <- sort(genre_v[as.character(a_id),], decreasing = TRUE)[1:n_recommended]
      recs_id <- as.numeric(names(recs))
      recs_names <- anime_name[anime_name$anime_id %in% recs_id, ]$name
      header <- sprintf("Animes Similar to %s", a_name)
      tbl_reg <- data.frame(recs_names)
      names(tbl_reg)[names(tbl_reg) == "recs_names"] <- header 
    }
    else {
      n_recommended <- input$nbr_recs
      u_id = input$users
      rec_ubcf <- recc_matrix[ ,as.character(u_id)]
      ubcf_animes <- anime_name[anime_name$anime_id %in% rec_ubcf, ]$name
      # create table
      header <- paste0(sprintf("Top %s", n_recommended),sprintf(" recommendations for User: %s", u_id))
      # display the list of similar animes
      tbl_reg <- data.frame(ubcf_animes[1:n_recommended])
      names(tbl_reg)[names(tbl_reg) == "ubcf_animes.1.n_recommended."] <- header 
    }
    tbl_reg
  })
  
  output$sur_rec_list <- renderTable({
    if (input$radio == "cbanime") {
      n_recommended <- 3
      a_name = input$anime1
      a_id <- anime_name[anime_name$name == a_name, ]$anime_id
      
      tmp_anime <- genre_b[as.character(a_id), ]
      genre_w <- rbind(genre_p[!rownames(genre_p) %in% "tmp_anime", ], tmp_anime)
      pop_bm <- as(genre_w, "binaryRatingMatrix")
      pop_sim <- similarity(pop_bm, method = "Jaccard", which = "users")
      genre_pop <- as(pop_sim, "matrix")
      colnames(genre_pop) <- c(pop$anime_id, a_id)
      rownames(genre_pop) <- c(pop$anime_id, a_id)
      
      n_recommended <- 3
      rec_pop <- sort(genre_pop[as.character(a_id), ], decreasing = TRUE)[1:n_recommended]
      rec_id_p <- as.numeric(names(rec_pop))
      rec_names_p <- anime_name[anime_name$anime_id %in% rec_id_p,]$name
      header <- sprintf("Surprise Picks (Lesser-Known Animes Similar to): %s", a_name)
      tbl_sur <- data.frame(rec_names_p)
      names(tbl_sur)[names(tbl_sur) == "rec_names_p"] <- header 
      genre_w <- NA
    }
    else {
      tbl_sur <- NULL
    }
    tbl_sur
  })  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)