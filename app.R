library(shiny)
library(tidyverse)
library(fmsb)

# Import Data
s = read.csv("data/spotify_songs.csv")
options(warn=-1)

# Musical Composition Page
composition = 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("artist", "Artist:", choices = NULL, multiple = FALSE, selected = NULL),
      selectizeInput("album", "Album(s):", choices = NULL, multiple = TRUE, selected = NULL),
      selectizeInput("song", "Song(s):", choices = NULL, multiple = TRUE, selected = NULL),
      div(actionButton("reset", "Reset"), style = "display: inline-block;"),
      div(checkboxInput("group", "Group"), style = "display: inline-block; position: absolute; right: 40px;")
    ),
    mainPanel(plotOutput(outputId = "radar", height = 800, width = 690))
  )

# Table Page
table = dataTableOutput("t")

# About Page
about = includeMarkdown("about.Rmd")

# UI Code
ui = fluidPage(
  # Navbar
  navbarPage(
    title = "Spotify Songs",
    tabPanel("Musical Composition", composition),
    tabPanel("Table", table),
    tabPanel("About", about)
  )
)

# Server Code
server = function(input, output, session) {
  ## Musical Composition Page
  update_choices = function(input_id, choices) {
    updateSelectizeInput(session, input_id, choices = choices, selected = NULL, server = TRUE)
  }
  
  # Initialize choices
  updateSelectizeInput(session, "artist", choices = unique(s$track_artist), selected = "Tom Petty and the Heartbreakers", server = TRUE)
  update_choices("album", unique(s$track_album_name))
  update_choices("song", unique(s$track_name))
  
  # Conditionals
  this_artist = reactive({s |> filter(track_artist %in% input$artist)})  
  this_album = reactive({s |> filter(track_album_name %in% input$album)})  
  this_song = reactive({s |> filter(track_name %in% input$song)}) 
  
  # Use reactiveValues for flags and create a color map
  flags = reactiveValues(artist = FALSE, album = FALSE, song = FALSE)
  color_map = reactiveValues()
  
  # Radar DataFrame
  radar_data = reactiveVal(s |> distinct(track_id, .keep_all = TRUE))

  observeEvent(
    eventExpr = input$artist,
    handlerExpr = {
      if (!flags$album) {update_choices("album", unique(this_artist()$track_album_name))}
      if (!flags$song) {update_choices("song", unique(this_artist()$track_name))}
      flags$artist = TRUE
    }
  )
  
  observeEvent(
    eventExpr = input$album,
    handlerExpr = {
      if (!flags$artist) {update_choices("artist", unique(this_album()$track_artist))}
      if (!flags$song) {update_choices("song", unique(this_album()$track_name))}
      flags$album = TRUE
    }
  )
  
  observeEvent(
    eventExpr = input$song,
    handlerExpr = {
      if (!flags$artist) {update_choices("artist", unique(this_song()$track_artist))}
      if (!flags$album) {update_choices("album", unique(this_song()$track_album_name))}
      flags$song = TRUE
      
      # Update Color Map
      map = isolate(color_map$m)
      song = tail(strsplit(input$song, '"'), 1)
      
      k = gsub(" ", "_", song)
      v = getColor() # from R folder
      if (is.null(names(map)) || !(k %in% names(map))) {
         map[k] = v
         color_map$m = map
      }
    }
  )
  
  # Reset Button
  observeEvent(
    eventExpr = input$reset,
    handlerExpr = {
      updateSelectizeInput(session, "artist", choices = unique(s$track_artist), selected = "Tom Petty and the Heartbreakers", server = TRUE)
      update_choices("album", unique(s$track_album_name))
      update_choices("song", unique(s$track_name))
      flags$artist = FALSE
      flags$album = FALSE
      flags$song = FALSE

      radar_data(s |> distinct(track_id, .keep_all = TRUE))
    }
  )
  
  # Radar Chart
  output$radar = renderPlot({
    req(input$song)

    data = s |> 
      filter(track_name %in% input$song) |>
      group_by(track_name) |>
      summarize(
        artist = track_artist,
        album = track_album_name,
        album_release_date = track_album_release_date,
        popularity = track_popularity,
        genre = playlist_genre,
        sub_genre = playlist_subgenre,
        duration_ms = duration_ms,
        tempo = tempo,
        danceability = mean(danceability, na.rm=TRUE),
        energy = mean(energy, na.rm=TRUE),
        speechiness = mean(speechiness, na.rm=TRUE),
        acousticness = mean(acousticness, na.rm=TRUE),
        instrumentalness = mean(instrumentalness, na.rm=TRUE),
        liveness = mean(liveness, na.rm=TRUE),
        valence = mean(valence, na.rm=TRUE)
      ) |> ungroup() |> distinct(track_name, .keep_all = TRUE)
    radar_data(
      data  |>
        rename("Track Name" = track_name, Artist = artist, Album = album,
               "Album Release Date" = album_release_date, "Track Popularity" = popularity,
               Genre = genre, "Sub-genre" = sub_genre, "Duration (ms)" = duration_ms,
               "Tempo (bpm)" = tempo, Danceability = danceability, Energy = energy,
               Speechiness = speechiness, Acousticness = acousticness, 
               Instrumentalness = instrumentalness, Liveness = liveness, Valence = valence)
    )
    t = enframe(isolate(color_map$m), name = "track_name", value = "rgb")
    d = data |>
      select(track_name, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) |>
      mutate(track_name = gsub(" ", "_", track_name)) |>
      left_join(t, join_by("track_name"))
    
    clrs = unlist(d |> select(rgb))
    data = d |> select(-c(track_name, rgb))
    
    if (input$group) {
      data = rbind(1,0, as.data.frame(t(colMeans(data, na.rm=TRUE))))
      clrs = "black"
    } else {
      data = rbind(1,0, as.data.frame(data, na.rm=TRUE))
    }

    radarchart(data, 
               seg = 5,
               title="Song(s) Composition Chart", 
               plty = 1,
               pty = 16,
               plwd = 4,
               cglty = 1,
               cglcol = "grey",
               axistype =  1,
               axislabcol = "grey",
               caxislabels = seq(0, 1, .2),
               calcex = 1.25,
               vlcex = 1.25,
               pcol = clrs, 
               pfcol = alpha(clrs, 0.2)
               )

    legend_labels = d |> mutate(track_name = gsub("_", " ", track_name)) |>
      select(track_name) |> unlist()
    legend("bottom", legend = legend_labels, col = clrs, lty = 1, lwd = 2, cex = 0.75)
  })
  
  # Output Table
  output$t = renderDataTable({radar_data()})
}

shinyApp(ui = ui, server = server)
