library(shinythemes)
library(jsonlite)
library(dplyr)
library(plotly)

tab_landing_page <- tabPanel(
  "File Upload",
  titlePanel(strong("Welcome to Hingeify.")),
  mainPanel(
    p("DISCLAIMER: Images and graphs look best on a desktop internet browser."),
    p("This simple webapp will take in your raw Hinge match data and return
  meaningful results. If you haven't downloaded your data yet, navigate",
  tagList(a("here", href="https://hingeapp.zendesk.com/hc/en-us/articles/360011235813-How-do-I-request-a-copy-of-my-personal-data-")),
  "to request a copy of your personal data from Hinge. Due to privacy reasons,
  Hinge removes all personal information about the others whom you've
  interacted with on the app. Therefore, the age range, interests, etc. of
  your interactions cannot be determined and analyzed.", style = "font-size: 18px"),
  
  p("When you have received your data, look for a file named \"matches.json\"
    and upload it here. This website was made just for fun and will not
    collect any personal data from you. For more information, please read the
    About section on this site.", style = "font-size: 18px"),
  br(),
    fileInput(inputId = "matchesJSON",
              label = "Input data file (matches.json):",
              multiple = FALSE,
              accept = ".json"),
    br(),
    p("Click through the tabs above after uploading your file to view results!",
      style = "font-size: 18px"),
    br(), br(), br(),
    p(strong(em("Sample screenshots:")), style = "font-size: 20px"),
  
    fluidRow(
      column(width = 7, img(src = "sample_overview.jpg", width = "1250px")),
      column(width = 7, img(src = "sample_data1.jpg", width = "1250px"))
    ),
    fluidRow(
      column(width = 7, img(src = "sample_data2.jpg", width = "1250px")),
      column(width = 7, img(src = "sample_data3.jpg", width = "1250px"))
    )
  )
)

tab_text_overview <- tabPanel(
  "Overview",
  titlePanel("Results: Overview"),
  mainPanel(
    p("Here you will find some basic statistics and percentages about
      your activity and interactions with others on Hinge. Your total
      number of interactions with others come down to you liking their
      profile or them liking yours. Therefore, your total Interactions
      = (likes sent) + (likes received). Matches mean that both parties
      have sent a like to each other.",
      style = "font-size: 18px"),
    p(""),
    htmlOutput(outputId = "placeholder_1"),
    htmlOutput(outputId = "text_analysis")
  )
)

tab_data_analysis <- tabPanel(
  "Analysis",
  titlePanel("Results: Data Analysis"),
    
  mainPanel(
    p("On this page, you will see graphs produced from your personal Hinge
      data. They range from simple bar graphs to more complex ones that
      provide insight about your app usage and habits. To change your view
      for each graph, simply drag-click the area on the graph that you want to 
      expand. You can reset the view by double-clicking anywhere on the graph.
      Some of the graphs allow you to select a date range to display your data.
      To reset the dates and see the entire graph again, you can input the same
      start and end dates or double-click the graph.",
      style = "font-size: 18px"),
    p(strong("By the Numbers:"),
      "a bar graph of the frequencies of likes you sent
      and received, and total matches that occurred throughout your Hinge
      experience.", style = "font-size: 18px"),
    p(strong("Total Matches Per Day:"),
      "the frequency that you matched with people
      per day, from the day you created your Hinge account. Hover over a
      bar to see the exact date and number of matches you received! Use the
      date selector to see a particular range of values or drag-click over a
      certain part of the graph. Double-click on the graph to reset.",
      style = "font-size: 18px"),
    p(strong("Total Matches Over Time:"),
    "a line graph showing your matches with others
      over time. Hover over the line to see the exact number of matches on
      a particular date. Use the
      date selector to see a particular range of values or drag-click over a
      certain part of the graph. Double-click on the graph to reset.",
    style = "font-size: 18px"),
    p(strong("Likes & Matches Throughout the Day:"),
      "displays the total number of likes
      you sent out and matches you received during each hour of day.",
      em("**Note: some of the times may be inaccurate depending on your time zone.
      The data displayed is shown based on the data that is given, which is
      in line with the timezone of the Hinge server.**"),
      style = "font-size: 18px"),
    br(),
    
    htmlOutput(outputId = "placeholder_2"),
    plotlyOutput(outputId = "fig3"),
    br(), br(), br(),
    dateRangeInput(inputId = "date_range4", "Date Range: (to reset: select
                   same start & end date or refresh the page)"),
    plotlyOutput(outputId = "fig4"),
    br(), br(), br(),
    dateRangeInput(inputId = "date_range1", "Date Range:"),
    plotlyOutput(outputId = "fig1"),
    br(), br(), br(),
    plotlyOutput(outputId = "fig2")
  )
)

tab_about <- tabPanel(
  "About",
  titlePanel("About the Creator"),
  mainPanel(
    p("I am a Junior at the University of Washington studying Informatics and
    Mathematics. The inspiration for making this web-app was when a friend
    showed me a video of someone doing something similar with their downloaded
    Hinge data. I thought it was a really cool concept and wanted to expand
    more on that idea to do some deeper analysis on it. My friend kindly
    lent me their data to help me build and design this project.",
      style = "font-size: 18px"),
    p("This is supposed to be a fun project that will not collect any
    personal information about you. I hope you found using this web-app
    interesting, and learned something new about yourself or your Hinge
      habits!", style = "font-size: 18px"),
    uiOutput(outputId = "github"),
    uiOutput(outputId = "linkedin")
  ),
)

ui <- navbarPage(
  theme = shinytheme("darkly"),
  "Hingeify",
  
  tab_landing_page,
  tab_text_overview,
  tab_data_analysis,
  tab_about
)
