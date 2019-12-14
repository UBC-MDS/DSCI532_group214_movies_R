
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(dplyr)
library(plotly)
library(gapminder)
library(ggplot2)
library(ggridges)
library("viridis")

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

df <- read_csv("movies.csv")
df <- df %>% drop_na(Major_Genre)

genre <- 'Action'
year_info <- c(1950, 2000)
options(repr.plot.width = 10, repr.plot.height = 10)

count_per_genre <- function(genre = 'Action', year_info = c(1950,2010)) {
  
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
  #install
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
    group_by(Major_Genre) %>%
    summarise(count_per_genre = n())
  
  #Used for highlighting specific genre
  A <- A %>%
    mutate(to_highlight = ifelse(Major_Genre == genre,"Yes", "No"))
  
  #Return the ggplot
  A <- ggplot(A, aes(Major_Genre, count_per_genre, fill = to_highlight)) +
    geom_bar(stat= 'identity', position = 'dodge') +
    scale_fill_manual(values = c("Yes" = "orange", "No" = "Grey")) +
    labs(y = "Number of movies produced", x = "", title = 'Popularity of Genres') +
    coord_flip() 
  
  A <- A + theme(legend.position = 'none')
  
  ggplotly(A)
}


average_box_office <- function(genre = 'Action', year_info = list(1950,2000)) {
  
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
  
  #Filtered the selected genre
  B <- B %>% filter(Major_Genre == genre)
  
  #Evaluate international gross, gather for tidy format
  B <- B %>%
    mutate(International_Gross = Worldwide_Gross - US_Gross) %>%
    select(Major_Genre, year, US_Gross, International_Gross) %>%
    gather(key = 'Gross', value = 'amount', -Major_Genre, -year) %>%
    group_by(year, Gross) %>%
    summarise(amount = mean(amount))
  
  #Return the ggplot
  B_plot <- ggplot(B, aes(year, amount, fill = Gross)) +
    geom_area() +
    labs(x = "Year", y = 'Dollars', title = 'Average box office') 
  
  B_plot <- B_plot + theme(legend.position = 'bottom')
  
  B_plot
  ggplotly(B_plot)  %>%
    layout(legend = list(
      orientation = "h",
      y = 1
    )
    )
  
}


violinplot <- function(genre = 'Action', year_info = list(1950,2000)) {
  
  # Function creates a violin plot of profit ratio vs. IMDB rating
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
    subset(profit_ratio <= 10) %>%
    mutate(to_highlight = ifelse(Major_Genre == genre,"Yes", "No"))
  
  
  #Return the ggplot
  C_plot <- ggplot(C, aes(x = Major_Genre, y = profit_ratio)) +
    geom_violin(aes(fill=factor(to_highlight)), position = 'dodge') +
    scale_fill_manual(values = c("Yes" = "orange", "No" = "Grey"), guide = FALSE) +
    coord_flip() +
    labs(y = 'Profit Ratio',
         title = 'Profit vs. Genre',
         legend = 'Genre') + 
    theme_bw(20)
  
  ggplotly(C_plot, width = 1000, height = 700) %>% layout(showlegend = FALSE)
}

histogram <- count_per_genre(genre, year_info)
graph_hist <- dccGraph(id='histogram',
                       figure=histogram)

area <- average_box_office()
graph_area <- dccGraph(id='area',
                       figure=area)

violin <- violinplot()
graph_violin <- dccGraph(id='violin',
                         figure=violin)

yearMarks <- map(seq(1930, 2010, by = 20), as.character)
names(yearMarks) <- seq(1930, 2010, by = 20)

yearSlider <- dccRangeSlider(
  id = "year",
  marks = yearMarks,
  min = 1930,
  max = 2010,
  step = 1,
  value = list(1930, 2010)
)

genreDropdown <- dccDropdown(
  id = "genre",
  options = map(
    levels(as.factor(df$Major_Genre)), function(x){
      list(label=x, value=x)
    }),
  value = 'Action'
)

app$layout(
  htmlDiv(
    list(
      htmlDiv(
        list(
          htmlImg(src='https://i.imgur.com/N2UOAry.jpg'),
          htmlH1("Interactive Movie DashBoard"),
          htmlH4("Smaller Text")
        ), style = list('columnCount'=3, 'background-color'= 'black', 'color'='white')
      ),
      
      htmlDiv(
        list(
          htmlDiv(
            list(
              htmlDiv(
                list(
                  htmlP("Select a genre from the dropdown:"),
                  genreDropdown,
                  htmlH4(" "),
                  htmlP("Select a range of years"),
                  yearSlider
                ), style = list('background-color'='lightgrey', 'columnCount'=1, 'width'='20%')
              ),
              htmlDiv(
                list(
                  htmlDiv(
                    list(
                      htmlDiv(
                        list(
                          htmlP("Histogram"),
                          graph_hist
                        ), style=list('width'='100%')
                      ),
                      htmlDiv(
                        list(
                          htmlP("Area chart"),
                          graph_area
                        ), style=list('width'='100%')
                      )
                    ), style = list('display'='flex')
                  ),
                  htmlDiv(
                    list(
                      
                      htmlDiv(
                        list(
                          htmlP('Violin graph'),
                          graph_violin
                        ), style = list('width' = "100%")
                      ),
                      
                      htmlDiv(
                        list(
                          htmlP('Summary Text')
                          
                        ), style = list('columnCount' = 1)
                      )
                    ), style = list('display'='flex')
                  )
                  # htmlDiv(
                  # dccMarkdown("[Data Source](https://github.com/vega/vega-datasets/blob/master/data/movies.json)")
                  
                )#, style = list('width'="30%", 'background-color'='lightgrey', 'display'='flex')
              )
            ), style=list('display'='flex')
          )
        ), style = list('display'='flex')
      )
    )
  )
)

app$callback(
  output=list(id = 'histogram', property = 'figure'),
  #based on values of year range and genre
  params=list(input(id = 'genre', property = 'value'), input(id = 'year', property='value')),
  function(genre, year_value) {
    count_per_genre(genre, year_value)
  }
)

app$callback(
  output=list(id = 'area', property = 'figure'),
  #based on values of year range and genre
  params=list(input(id = 'genre', property = 'value'), input(id = 'year', property='value')),
  function(genre, year_value) {
    average_box_office(genre, year_value)
  }
)

app$callback(
  output=list(id = 'violin', property = 'figure'),
  #based on values of year range and genre
  params=list(input(id = 'genre', property = 'value'), input(id = 'year', property='value')),
  function(genre, year_value) {
    violinplot(genre, year_value)
  }
)


app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))

