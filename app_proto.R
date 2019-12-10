
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(gapminder)
library(ggplot2)
library(ggridges)
library("viridis")

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")


df <- read_csv("movies.csv")

genre <- 'Comedy'
year_info <- c(1900, 1980)
options(repr.plot.width = 10, repr.plot.height = 10)

count_per_genre <- function(genre = 'Comedy', year_info = c(1950,2010)) {
  
  # Function creates a plot of Number of movies produced vs. genre 
  # in between years
  #
  # --------
  # @param : 
  #        genre : provide a genre to highlight
  #        year_info : provide a list/vector with 2 values 
  #                    which give the start and end year to sort by
  #
  # --------
  # @Return :
  #        a plot of 'Genre' vs. 'Number of movies produced'
  #
  # --------
  # @Example : 
  #        count_per_genre('Comedy', list(1990,2010))
  #        count_per_genre('Drama', c(1990,2010))
  # --------
  
  #Filtered between the dates
  A <- df %>% filter(between(df$year, 
                             as.integer(year_info[1]), 
                             as.integer(year_info[2])))
  
  #Count number of movies per genre
  A <- A %>%
    drop_na(Major_Genre) %>%
    group_by(Major_Genre) %>%
    summarise(count_per_genre = n())
  
  #Used for highlighting specific genre
  A <- A %>%
    mutate(to_highlight = ifelse(Major_Genre == genre,"Yes", "No"))
  
  #Return the ggplot
  A <- ggplot(A, aes(Major_Genre, count_per_genre, fill = to_highlight)) +
    geom_bar(stat= 'identity', position = 'dodge') +
    scale_fill_manual(values = c("Yes" = "orange", "No" = "Grey"), guide = FALSE) +
    labs(y = "Number of movies produced", x = "", title = 'Popularity of Genres') +
    coord_flip() + 
    theme_bw(20)

    ggplotly(A, width = 1000, height = 700)
}




average_box_office <- function(genre = 'Comedy', year_info = list(1980,2010)) {
  
  # Function creates a plot of Number of movies produced vs. genre 
  # in between years
  #
  # --------
  # @param : 
  #        genre : provide a genre to highlight
  #        year_info : provide a list/vector with 2 values 
  #                    which give the start and end year to sort by
  #
  # --------
  # @Return :
  #        a plot of 'Genre' vs. 'Number of movies produced'
  #
  # --------
  # @Example : 
  #        average_box_office('Comedy', list(1990,2010))
  #        average_box_office('Drama', c(1990,2010))
  # --------
  
  #Filtered between the dates
  B <- df %>% filter(between(df$year, 
                             as.integer(year_info[1]), 
                             as.integer(year_info[2])))
  
  #Evaluate international gross, gather for tidy format
  B <- B %>%
    drop_na(Major_Genre) %>%
    mutate(International_Gross = Worldwide_Gross - US_Gross) %>%
    select(Major_Genre, year, US_Gross, International_Gross) %>%
    gather(key = 'Gross', value = 'amount', -Major_Genre, -year) %>%
    group_by(year, Gross) %>%
    summarise(amount = mean(amount))
  
  #Return the ggplot
  B_plot <- ggplot(B, aes(year, amount, fill = Gross)) +
    geom_area() +
    labs(x = "Year", y = 'Dollars', title = 'Average box office')  +
    theme_bw(20)
  
  ggplotly(B_plot, width = 1000, height = 700)
  
}


ridgelineplot <- function(genre = 'Comedy', year_info = list(1950,2010)) {
  
  # Function creates a ridgeline plot of profit ratio vs. IMDB rating
  # in between years
  #
  # --------
  # @param : 
  #        genre : provide a genre to highlight
  #        year_info : provide a list/vector with 2 values 
  #                    which give the start and end year to sort by
  #
  # --------
  # @Return :
  #        a plot of 'Genre' vs. 'Number of movies produced'
  #
  # --------
  # @Example : 
  #        heatmap('Comedy', list(1990,2010))
  #        heatmap('Drama', c(1990,2010))
  # --------
  
  #Filtered between the dates
  C <- df %>% filter(between(df$year, 
                             as.integer(year_info[1]), 
                             as.integer(year_info[2])))
  
  #Evaluate international gross, gather for tidy format
  C <- C %>%
    drop_na(Major_Genre) %>%
    subset(profit_ratio <= 20) 
  
  
  
  #Return the ggplot
  C_plot <- ggplot(C, aes(y = Major_Genre, 
                          x = IMDB_Rating,
                          color = profit_ratio,
                          fill = ..x..
                          #fill 
  )) +
    geom_density_ridges_gradient(alpha = 0.2) +
    scale_fill_viridis(option = 'plasma', name = 'Profit ratio') +
    labs(x = 'IMDB rating', 
         y = 'Genre', 
         title = 'Critical reception vs. Genre',
         legend = 'Profit ratio') + 
    theme_bw(20)
  
  ggplotly(C_plot, width = 1000, height = 700)
}

ridgelineplot()

histogram <- count_per_genre(genre, year_info)
graph_hist <- dccGraph(id='histogram',
                       figure=histogram)

area <- average_box_office()
graph_area <- dccGraph(id='area',
                       figure=area)

ridgeline <- ridgelineplot()
graph_ridgeline <- dccGraph(id='ridgeline',
                            figure=ridgeline)

count_per_genre(genre, year_info)
app$layout(
  htmlDiv(
    list(
      htmlH1("Test title"),
      htmlH2('Test H2'),
      graph_hist,
      graph_area,
      graph_ridgeline,
      htmlDiv(),
      dccMarkdown("[Data Source](https://github.com/vega/vega-datasets/blob/master/data/movies.json)")
    )
  )
)


app$run_server()
