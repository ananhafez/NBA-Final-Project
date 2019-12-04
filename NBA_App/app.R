#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(ggplot2)
library(purrr)
library(tidyverse)
library(readr)

nba_season_stats <- read.csv("Data/Seasons_stats_complete.csv") %>% filter(Year != "0")

plot_1 <- nba_season_stats %>% 
    group_by(Year) %>% 
    summarize(total_shots = sum(FGA), total_3p = sum(X3PA)) %>% 
    mutate(prop_3p = round((total_3p/total_shots), digits = 2)) %>% 
    ggplot(aes(x = Year, y = prop_3p)) + geom_line() + 
    labs(title = "How the 3-Point Shot grew in the NBA", 
         x = "NBA Season", y = "Proportion of All Shots that were 3-Pointers") +
    xlim(1950, 2020)

points_over_time <- nba_season_stats %>% 
    group_by(Year) %>% 
    summarize(total_2points = sum(X2P * 2), 
              total_3points = sum(X3P * 3), 
              total_ft = sum(FT), 
              total_points = (total_2points + total_3points + total_ft), 
              prop_2points = total_2points/total_points, 
              prop_3points = total_3points/total_points, 
              prop_ft = total_ft/total_points) 

plot_2 <- ggplot(points_over_time, aes(Year)) + 
    geom_line(aes(y = prop_2points, color = "var0")) + 
    geom_line(aes(y = prop_3points, colour = "var1")) + 
    geom_line(aes(y = prop_ft, colour = "var2")) + 
    ylim(0,1) + 
    scale_colour_manual(labels = c("2-Pointers", "3-Pointers", "Free Throws"), values = c("red", "green", "blue")) + 
    xlim(1950, 2020) + 
    labs(title = "Sources of NBA Points Over Time", 
         y = "Percentage of Total Points", x = "NBA Season") + 
    theme(legend.title = element_blank())

year_options <- nba_season_stats %>% group_by(Year) %>% select(Year) %>% count() %>% select(Year)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(
        "Analysis of 3-pointers in the NBA",
        tabPanel(
            title = "Introduction",
            h5("By Anan Hafez"),
            h3("About:"),
            p(
                "I've loved the game of basketball since I was a young kid. Growing up in Los Angeles, I loved going to Lakers games with my dad, watching them win two championships in a row, and buying as many Kobe Bryant jerseys as my parents would let me. I even played in a YMCA basketball league and my team won the championship! (Would you believe that?) Over the years though, I've seen a massive shift the way basketball is played. Every kid now wants to be like Steph Curry, shooting from super far away and repeatedly making it. In recent years, teams are allegedly taking many more 3-point shots than when I was a kid and point totals are rising becuase of it. Games that used end with with 80 or 90 points now regularly end with over 100, 110, and sometimes 120 points. In fact, there have already been a few games in the most recent season that have ended with 150+ points! I wanted to test this theory with R. Are players really taking more 3-point shots than ever before, and how does that affect other parts of professional basketball? The three-point shot, as many fans know, was not always in the game. When Wilt and Russell used to play, each field goal counted as two points and free throw one. In the 1979-1980 season, the NBA adopted the three-point line, further from which, a shot would count as 3 points. This change, undeniably, has changed the game to head to toe. Hopefully, with many visualizations and (finally) clean code, I can see how and why professional basketball has changed in the past few decades. "
            )
            # br(),
            # h3("Organization of the App:"),
            # p(
            #     "Why Texas:? On this page I show why Texas is one of, if not the, most important states when discussing the death penalty."
            # ),
            # p(
            #     "Most Common Words: On this page, you can see specific visualizations of the breakdown of sentiments in all Last Statements by selecting which sentiment to include in a pie chart. There is also a word cloud that shows the most commonly used words based on the sentiments selected."
            # ),
            # p(
            #     "Time Plot: This graph plots the percentage of positive or negative words. Users can look at all subjects or subset based on race and/or age."
            # ),
            # p(
            #     "Specific Inmates: This tab allows users see a random set of subjects and their last statements."
            # ),
            # br(),
            # h3("Some Findings:"),
            # p(
            #     "As shown on the 'Why Texas?' tab, I found that Texas conducted nearly half of the death row executions nationwide in 2018. Though executions have decreased since 1999, Texas has maintained a large margin of the total executions and now, as stated, conducted the majority of them. Additionally, though 31 states have not outlawed the death penalty, less than 10 actually conducted any executions thsi year. This reaffirms some of the statements made in the Medium articles that discuss death penalty being more of a de factor life sentence in most states.   Sentiment Analysis begins on the Most Common Words tab which shows that a majority of the words in Last Statements are connote positivity and/or trust. The word cloud below shows that one of the most common words in Last Statements by the subjects was the word good with 55 entries.  The time plot is inconclusive. However, it does show that the percentage of positive words used has increased at a higher rate than the percentage of negative words used. "
            # ),
            # p(
            #     "Poke around with the data and see if you can find more interesting insights."
            # ),
            # br(),
            # h2("Relevant Reading Materials and Sources:"),
            # p(
            #     "https://medium.com/bigger-picture/kill-the-death-penalty-ea38c8929e30, https://medium.com/s/story/love-is-the-most-common-word-in-death-row-last-statements-f15ab0e8ad16, https://deathpenaltyinfo.org/views-executions, http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
            # )
        ),
        
        tabPanel(
            title = "Three Pointer?",
            h3("What's a Three Point Shot?"),
            p(
                "A three-point field goal ('3-pointer') is a field goal in a basketball game made from beyond the three-point line, a designated arc surrounding the basket. A successful attempt is worth three points, in contrast to the two points awarded for field goals made within the three-point line and the one point for each made free throw."
            ), 
            br(),
            imageOutput("court")
        ),
        
        tabPanel(
            title = "Growth of 3's",
            h3("More Shots from Distance"),
            p(
                "Ever since its addition in 1980, teams and players have been turning to 3-point shots for a large share of their points. Teams like it because they can get an extra point by stepping a few extra feet back; this means they can convert them as regulary as 2-point shots. Players like it because the farther away they can be from the basket, the more seperation they can get between themselves and a defender."
            ),
            br(),
            h4("Point Source Shares Each Season:"),
            br(),
            
            sidebarPanel(
                p("Select a season to see the breakdown of point sources for each NBA season."),
                sliderInput(
                    inputId = "year",
                    label = "Season:",
                    min = 1950,
                    max = 2019,
                    value = 1950
                ),
                br(),
                
            ),
            br(),
            mainPanel(plotOutput("piePlot")),
            
            # The breaks may not be needed but I wanted more space between the piePlot and the next graph.
            
            br(),
            br(),
            br(),
            br(),
            h4("The Past 70 Years of NBA Scoring:"),
            p(
                "Prior to 1980, three pointers represented 0% of the scoring in the league as it was not yet introduced. After 1980, the three-point line took on an increasingly important role with a road bump in the mid-90s. In 2008, three pointers officially surpassed free throws as the second most important scoring method in the league; and it does not look like its prevalence is declining any time soon. In fact, if the trend persists, by 2030, three points will be the most common increment in a basketball game."
            ),
            plotOutput("overallPlot")
        ),
        
        tabPanel(
            title = "Why so many 3's?",
            h3("Accuracy Comes First"),
            p(
                "The three-point shot first became popularized by the American Basketball Association (ABA), introduced in its inaugural 1967–68 season. ABA commissioner George Mikan stated the three-pointer 'would give the smaller player a chance to score and open up the defense to make the game more enjoyable for the fans'. During the 1970s, the ABA used the three-point shot, along with the slam dunk, as a marketing tool to compete with the NBA."
            ),
            br(),
            br(),
            br(),
            sidebarPanel( p(
               "3-pointers were not such a prevelant force in the game of basketball until players actually got good at it. One of the most prolific players in the NBA today, Stephen Curry, shoots 3-pointers with remarkable accuracy. Let's take a look back at his 2015-2016 season where he set the record for the most threes made in a season, 402. While the league averaged 36% from the three that season, Curry was getting 54% right behind the three point line at 22 feet. By being accurate from distance, almost as accurate from close range, Curry showcased a new, efficient form of game strategy." 
            ) ),
            mainPanel( 
                plotOutput("plot3") 
                ),
            br(),
            br(),
            br(),
            h3("Accurate Players are Extremely Efficient from Range"),
            sidebarPanel( p(
                "To show efficiency, we multiply a player's field goal percentage by the type of shot it is using distance, 2-pointer or 3-pointer. When you have a player as accurate as Steph Curry, shooting behind the three point line becomes efficient, much more then 2-pointers. Curry's top 6 points per shot distance come form behind 3-point line. In fact, Curry scores more per shot on average 30 feet from the basket than 10 feet. To someone unfamiliar with basketball, this might not make sense. Steph Curry makes shots 10 feet away 67% of the time and 30 feet away only 45% of the time. The difference is that one of these shots is worth 2 points and one is worth 3. The extra point makes up for the loss in accuracy. After all, points are the ultimate currency in the NBA." 
            ) ),
            mainPanel( 
                plotOutput("plot4") 
            )
    )
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$piePlot <- renderPlot({
        piechart <- nba_season_stats %>% 
            group_by(Year) %>% 
            summarise("2-Pointers" = sum(X2P * 2), "3-Pointers" = sum(X3P * 3), "Free Throws" = sum(FT)) %>% 
            pivot_longer(-Year, names_to = "source", values_to = "count") %>% 
            filter(Year == input$year) %>% 
            ggplot(aes(x = "", y=count, fill = factor(source))) + geom_bar(stat="identity", width = 1) + 
            theme(axis.line = element_blank(), 
                  plot.title = element_text(hjust=0.5)) +
            labs(fill = "Shot Type",
                 x = NULL,
                 y = NULL,
                 caption = "Source: NBA") 
        
        
        piechart + coord_polar(theta = "y", start = 0)
        
        
    })
    
    # I use renderPlotly since I want my plot to be ggplotly with all the included functionality
    
    output$overallPlot <- renderPlot({
        plot_2
    })
    
    output$court <- renderImage({
        list(src = "court.jpg",
             contentType = 'image/jpg',
             width = 800
             # alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$plot3 <- renderPlot({
        plot_3
    })
    
    output$plot4 <- renderPlot({
        plot_4
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
