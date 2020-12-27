library(shinythemes)

tab_landing_page <- tabPanel(
  "File Upload",
  titlePanel("Hingeify or something"),
  mainPanel(
    
    p("This simple webapp will take in your raw Hinge match data and return
  meaningful results. If you haven't downloaded your data yet, navigate",
  tagList(a("here", href="https://hingeapp.zendesk.com/hc/en-us/articles/360011235813-How-do-I-request-a-copy-of-my-personal-data-")),
  "to request a copy of your personal data from Hinge. Due to privacy reasons,
  Hinge removes all personal information about the others whom you've
  interacted with on the app. Therefore, the age range, interests, etc. of
  your interactions cannot be determined and analyzed."),
  
  p("When you have received your data, look for a file named \"matches.json\"
    and upload it here. This website was made just for fun and will not
    collect any personal data from you. For more information, please read the
    About section on this site."),
  br(),
    fileInput(inputId = "matchesJSON",
              label = "Input data file (matches.json):",
              multiple = FALSE,
              accept = ".json"),
    br(),
    p("Click through the tabs above after uploading your file to view results!")
  )
)

tab_text_overview <- tabPanel(
  "Overview",
  titlePanel("Results: Overview"),
  mainPanel(
    htmlOutput(outputId = "placeholder_1"),
    br(),
    p("Here you will find some basic statistics and percentages about
      your activity and interactions with others on Hinge."),
    htmlOutput(outputId = "text_analysis")
  )
)

tab_data_analysis <- tabPanel(
  "Analysis",
  titlePanel("Results: Data Analysis"),
  mainPanel(
    htmlOutput(outputId = "placeholder_2"),
    br(),
    splitLayout(
      verticalLayout(
        p("On this page, you will see graphs produced from your personal Hinge
          data. They range from simple bar graphs to more complex ones that
          provide insight about your app usage and habits."),
        p("By the Numbers: a bar graph of the frequencies of likes you sent
          and received, and total matches that occurred throughout your Hinge
          experience."),
        p("Total Matches Over Time: "),
        p("Likes & Matches Throughout the Day: ")),
      plotlyOutput(outputId = "fig3"),
      cellArgs = list(style = "white-space: normal;")
    ),
    br(), br(), br(),
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
    lent me their data to help me build and design this project."),
    p("This is supposed to be a fun project that will not collect any
    personal information about you. I hope you found using this web-app
    interesting, and learned something new about yourself or your Hinge
      habits!"),
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
  tab_about,
  
  mainPanel(
      
      # textOutput("test"),
      # uiOutput(outputId = "file_check"),
      

  )
)
