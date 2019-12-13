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
library(ggrepel)
library(ggplot2)
library(purrr)
library(tidyverse)
library(readr)
library(gganimate)
library(gifski)
library(png)

nba_season_stats <- read.csv("Data/Seasons_stats_complete.csv") %>% 
    filter(Year != "0") %>% 
    filter(Tm != "TOT")

dirty_curry_stats <- read.csv("Data/curry_shooting.csv") %>% 
    select(shot_made_flag, shot_type, shot_distance)

player_career_stats <- read.csv("Data/players.csv")

dirty_player_salaries <- read.csv("Data/salaries_1985to2018.csv")

player_salaries <- player_career_stats %>% 
    left_join(dirty_player_salaries, by = c("X_id" = "player_id")) %>% 
    select(name, season_start, salary, team) %>% 
    filter(season_start != "NA")

curry_stats <- dirty_curry_stats %>% 
    group_by(shot_distance) %>% 
    count(shot_made_flag) %>% 
    mutate(shot_made_flag = as.logical(shot_made_flag)) %>%
    # mutate(shot_made_flag = if_else(TRUE, "made", "missed")) %>% 
    pivot_wider(names_from = shot_made_flag, values_from = n) %>% 
    mutate_all(~replace(., is.na(.), 0)) 

colnames(curry_stats)<- c("shot_distance","made","missed")

curry_stats_2<- curry_stats %>% 
    mutate(total = made + missed) %>% 
    mutate(fgp = made/total) %>% 
    mutate(efficiency = if_else(shot_distance < 22, fgp * 2, fgp * 3)) %>% 
    filter(shot_distance %in% c(5:30))

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
    geom_line(aes(y = prop_2points, color = "var0"), size = 1.2) + 
    geom_line(aes(y = prop_3points, colour = "var1"), size = 1.2) + 
    geom_line(aes(y = prop_ft, colour = "var2"), size = 1.2) + 
    ylim(0,1) + 
    scale_colour_manual(labels = c("2-Pointers", "3-Pointers", "Free Throws"), values = c("red", "green", "blue")) + 
    xlim(1950, 2020) + 
    labs(title = "Sources of NBA Points Over Time", 
         y = "Percentage of Total Points", x = "NBA Season") + 
    theme(legend.title = element_blank())

plot_3 <- ggplot(curry_stats_2, aes(shot_distance, fgp)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    ylim(0,1) + geom_vline(xintercept = 22, colour="#BB0000", alpha = 0.7) +
    labs(title = "Steph Curry's Shot Accuracy by Distance",
         x = "Shot Distance",
         y = "Field Goal %",
         caption = "Data from 2015-2016 Season courtesy of NBA.com",
         subtitle = "Only Minor Drop-off after 3-Point Line (Red Line)")

plot_4 <- ggplot(curry_stats_2, aes(shot_distance, efficiency)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) + 
    geom_vline(xintercept = 22, colour="#BB0000", alpha = 0.7) +
    geom_hline(yintercept = 1.35, alpha = 0.7) +
    ylim(0,2) +
    labs(title = "Steph Curry's Shot Efficiency by Distance",
         x = "Shot Distance",
         y = "Average Points per Shot",
         caption = "Data from 2015-2016 Season courtesy of NBA.com",
         subtitle = "Curry's top 6 Efficiencies are behind 3-Point Line (Red Line)")

year_options <- nba_season_stats %>% filter(Year >= 1980) %>% group_by(Year) %>% select(Year) %>% count() %>% select(Year)

year_options2 <- nba_season_stats %>% filter(Year >= 1990, Year <= 2017) %>% group_by(Year) %>% select(Year) %>% count() %>% select(Year)

p <- nba_season_stats %>% 
    filter(Year >= 1980) %>%
    group_by(Tm, Year) %>% 
    summarise(percent_3p = sum(X3P)/sum(X3PA), total_3p = sum(X3PA)) %>% 
    ggplot(aes(x = total_3p, y = percent_3p, color = Tm)) + 
    geom_point() +
    geom_label_repel(aes(label = Tm),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') +
    transition_time(Year) +
    ylim(0, 0.5) +
    xlim(0, 3750) +
    labs(y = "Three Point %",
         x = "Three Pointers Attempted over Season",
         title = "Skills vs. Usage",
         subtitle = "Season: {frame_time}") +
    theme(legend.position='none') 

p2 <- animate(p, nframes = 40, fps = 2)

anim_save(filename = "plot6.gif", p2)

stats_and_salaries <- nba_season_stats %>% 
    full_join(player_salaries, by = c("Player" = "name", "Year" = "season_start")) 

salary_reg <- stats_and_salaries %>% 
    filter(salary != "NA") %>% 
    filter(Year >= 1990) %>% 
    filter(Pos != "C") 

plot_8 <- salary_reg %>% 
    ggplot(aes(x = X3P, y = salary)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Comparing Players' 3PM and Salary per Season",
         x = "Three Pointers Made",
         y = "Salary",
         subtitle = "From 1990 to 2017")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(
        "Analysis of 3-pointers in the NBA",
        tabPanel(
            title = "Introduction",
            h4("By Anan Hafez"),
            h3("Why Basketball?"),
            p(
                "I've loved the game of basketball since I was a young kid. Growing up in Los Angeles, I loved going to Lakers games with my dad, watching them win two championships in a row, and buying as many Kobe Bryant jerseys as my parents would let me. I even played in a YMCA basketball league and my team won the championship! (Would you believe that?) Over the years though, I've seen a massive shift the way basketball is played. Every kid now wants to be like Steph Curry, shooting from super far away and repeatedly making it. In recent years, teams are allegedly taking many more 3-point shots than when I was a kid and point totals are rising becuase of it. Games that used end with with 80 or 90 points now regularly end with over 100, 110, and sometimes 120 points. In fact, there have already been a few games in the most recent season that have ended with 150+ points! I wanted to test this theory with R. Are players really taking more 3-point shots than ever before, and how does that affect other parts of professional basketball? The three-point shot, as many fans know, was not always in the game. When Wilt and Russell used to play, each field goal counted as two points and free throw one. In the 1979-1980 season, the NBA adopted the three-point line, further from which, a shot would count as 3 points. This change, undeniably, has changed the game to head to toe. Hopefully, with many visualizations and (finally) clean code, I can see how and why professional basketball has changed in the past few decades. Check out the video below to see some the excitement I'm talking about!"
            ),
            br(),
            HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/zPis_kF7Lgo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        ),
        
        tabPanel(
            title = "Three Pointer?",
            h3("What's a Three Point Shot?"),
            sidebarPanel(
            p(
                "A three-point field goal ('3-pointer') is a field goal in a basketball game made from beyond the three-point line, a designated arc surrounding the basket. A successful attempt is worth three points, in contrast to the two points awarded for field goals made within the three-point line and the one point for each made free throw."
            )), 
            br(),
            mainPanel(imageOutput("court"))
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
                br()
                
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
            h4("Some Backstory..."),
            p(
                "The three-point shot first became popularized by the American Basketball Association (ABA), introduced in its inaugural 1967–68 season. ABA commissioner George Mikan stated the three-pointer 'would give the smaller player a chance to score and open up the defense to make the game more enjoyable for the fans'. During the 1970s, the ABA used the three-point shot, along with the slam dunk, as a marketing tool to compete with the NBA."
            ),
            h3("Accuracy Comes First"),
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
            ),
        tabPanel(
            title = "Team Usage",
            h3("Are more accurate teams taking more 3-pointers?"),
            sidebarPanel(
                p("Select a season"),
                selectInput(
                    inputId = "year2",
                    label = "Season:",
                    choices = year_options,
                    selected = "1980"
                ),
                p("Note: NBA adopted Three Point Shot in 1980 Season")
            ),
            br(),
            mainPanel(plotOutput("plot5")),
            br(),
            br(),
            br(),
            br(),
            br(),
            p(
                "This section of my project examines the skill against the usage of 3-pointers by a team in a given season. The x-axis represents the number of threes attempted while the y-axis represents the proportion of those made in a season. A team, realistically, hopes to find itself in the top-right corner of the graph, which represents a high skill of shooting threes and fully utilizing the skill. The top-left corner represents a high accuracy (aka high skills) but the team has under-utilized the skill. The bottom-left corner represents the team's awareness of its low three-pointer accuracy and its decision not to shoot threes. The bottom-right corner is the worst case scenario, representing a team that is very bad at shooting threes, but somehow decides to heavily rely on it. Also, notice how the x-axis limits shift over the years. This helps illustrate how large threes have become even if some teams don't believe in them."
            ),
            br(),
            p(
                "The graphic begins with 1980, the first year NBA teams could shoot threes in a game. That year was led by the San Diego Clippers, with 543 attempts over the course of the season. Compare that to the most recent season in 2019 where the Houston Rockets, led by the analytics of Daryl Morey of MIT, shot a record 3721 threes! As shown before, Morey understands that if players can shoot with enough accuracy they can always outscore an opposing team. The Rockets have built their entire team and playstyle around this strategy. Despite being one the worst defensive teams in the league, the Rockets have consistenly maintained winning records and playoff runs because this strategy actually works. If the Rockets as a team were slightly more accurate, perhaps closer to 38% or 39% from three, they would likely end up NBA champions. All because of threes!"
            ),
            br(),
            fluidRow( align = "center",
                imageOutput("plot6")
                      )
        ),
        tabPanel(
            title = "Effect on Salaries",
            h3("Are better 3-point shooters making more money?"),
            sidebarPanel(
                p("Select a season"),
                selectInput(
                    inputId = "year3",
                    label = "Season:",
                    choices = year_options2,
                    selected = "1990"
                ),
                checkboxInput(
                    inputId = "table",
                    label = "Show Coefficient Table",
                    value = FALSE
                )),
            br(),
            mainPanel(plotOutput("plot7"),
                      br(),
                      tableOutput("gttable")),
            br(),
            p(
             "Looking at our graphic, it seems that there is an effect on how many 3-pointers a player makes and their salary for that season. However, as we examine more recent years this effect gets more pronounced. Clicking our coefficient table, we see two terms: Intercept and X3P. The Intercept coefficient is the average starting salary for a player who hasn't made any 3-pointers. The X3P coefficient is the average jump in salary for every 3-pointer a player makes over the course of the season. Notice how in the 1990 season, everytime a player made a 3-pointer, they got an extra $4,400 on average. By the 2017 season, every 3-pointer you made got you an extra $77,600. In the NBA today, it pays to be a great shooter."   
            ),
            h3("In Aggregate:"),
            fluidRow( align = "center",
                      plotOutput("plot8")
            ),
            tableOutput("gttable2")
        ),
        tabPanel(
            title = "Wrap-Up & Contact",
            h3("Video Summary:"),
            br(),
            HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/1C2-iIYLBmY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
            br(),
            h3("Contact:"),
            p("Anan Hafez"),
            p("Harvard College Class of 2022"),
            p("Project for Gov 1005 by David Kane"),
            p("Email: ananhafez@college.harvard.edu"),
            p("GitHub: ananhafez")
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
    
    output$plot5 <- renderPlot({
        nba_season_stats %>% 
            filter(Year == input$year2) %>% 
            group_by(Tm, Year) %>% 
            summarise(percent_3p = sum(X3P)/sum(X3PA), total_3p = sum(X3PA)) %>% 
            ggplot(aes(x = total_3p, y = percent_3p, color = Tm)) + 
            geom_point() +
            geom_label_repel(aes(label = Tm),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50') +
            ylim(0, 0.5) +
            labs(y = "Three Point %",
                 x = "Three Pointers Attempted over Season",
                 title = "Skills vs. Usage") +
            theme(legend.position='none')
    })
    
    output$plot6 <- renderImage({
        list(src = "plot6.gif",
             contentType = 'image/gif',
             width = 600
             # alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$plot7 <- renderPlot({
       
        salary_reg %>% filter(Year == input$year3) %>% 
            ggplot(aes(x = X3P, y = salary)) + 
            geom_point() + 
            geom_smooth(method = "lm", se = FALSE) +
            labs(title = "Comparing Players' 3PM and Salary per Season",
                 x = "Three Pointers Made",
                 y = "Salary")
    })
    
    output$gttable <- renderTable({
        if (input$table == TRUE) {
            salary_fit <- salary_reg %>% filter(Year == input$year3) %>% lm(formula = salary ~ X3P)
            
            salary_fit %>%
                tidy(conf.int = TRUE, conf.level = 0.90) %>%
                mutate_if(is.numeric, round, digits = 2) %>%
                clean_names() %>% 
                select(-std_error, -statistic, -p_value) %>% 
                rename("5th Percentile" = conf_low,
                       "95th Percentile" = conf_high,
                       "Coefficient" = estimate,
                       "Term" = term) %>% 
                gt()
            }
        })
   
     output$plot8 <- renderPlot({
        plot_8
    })
    
     output$gttable2 <- renderTable({
         salary_fit %>%
             tidy(conf.int = TRUE, conf.level = 0.90) %>%
             mutate_if(is.numeric, round, digits = 2) %>%
             clean_names() %>% 
             select(-std_error, -statistic, -p_value) %>% 
             rename("5th Percentile" = conf_low,
                    "95th Percentile" = conf_high,
                    "Coefficient" = estimate,
                    "Term" = term) %>% 
             gt()
     })
}
# Run the application 
shinyApp(ui = ui, server = server)
