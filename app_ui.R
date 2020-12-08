library(shiny)
library(plotly)

data_states <- read.csv("data/states_all.csv")
states_extended_data <- read.csv("data/states_all_extended.csv")
enrollment <- read.csv("data/Enrollment.csv")
gt <- read.csv("data/Gifted\ and\ Talented.csv")
ap <- read.csv("data/Advanced\ Placement.csv")
ib <- read.csv("data/International\ Baccalaureate.csv")

intro_page <- tabPanel(
  "Introduction Page",
  p("Education and academia are what drive the innovations of our world. 
  In a country where success tends to equate to wealth, education is necessary 
  to achieve this success and is highly valued. However, equity in student 
  education can be compromised due to many surrounding variables. Our project 
  explores the subject of education in the United States in the 2010s. We 
  focused our efforts on finding discrepancies in students’ performances based 
  variables such as demographics and government funding."),
  
  p("Some questions we were trying to answer included investigating the effect 
  of state education spendings on the scores for student standadized exams 
  (NAEP), studying the correlation between student race and their achievements,
  as well as examining the demographics of students in advanced classes."),
  
  p("The dataset we used to answer these questions was from the US Census Bureau
  and the National Center for Education Statistics, which detailed information 
  on education in different states including number of students enrolled in 
  certain grades, the revenue and expenditure of states, average testing scores
  and demographic data. We found this dataset to be particularly helpful in 
  answering our questions on how race and state spendings on education impact
  student achievement as they contain demographic and performance statistics."),
  
  p("Another dataset we utilized was from the U.S. Department of Education,
  which was collected through the Civil Rights Data Collection by the Department
  of Education Office for Civil Rights. The data here was collected through
  self-reports by public schools and public school districts, relying on the 
  schools' and districts' own data gathering methods. Information here 
  encompasses a comprehensive report of school and school district 
  characteristics, early childhood education, pathways to college and career,
  etc.")
)

summary_page <- tabPanel(
  "Major Takeaways",
  p("Through our analysis of datasets detailing statistics of state education 
  spendings, average student scores on national standardized exams, and their 
  demographic information, we discovered patterns of correlation, mostly between
  race and academic performance."),
  p("In our bar chart for average NAEP scores, Asians and Whites were found to 
  score highest regardless of test subject and grade level. Blacks/African
  Americans scored the lowest. While we acknowledge that race is not the only 
  factor that can affect this performance, the disparities between races are
  clear."),
  p("Our heatmap detailing the relationship between state spendings and NAEP 
  scores did not find much correlation between these two variables. States with
  a higher average amount spent per student did not match with higher scores 
  almost anywhere in America."), 
  p("Through our stacked bar chart displaying the distribution of race across 
  educational programs, over 50% of the AP (Advanced Placement) and Gifted and
  Talented programs are consisted of white students, which is disproportionately
  high compared to total enrollment numbers. All other race groups except 
  Asians and two or more races display the opposite, with enrollment in all
  advanced education programs lower than total enrollment. To us, this suggests
  boundaries to advanced education are not equal."),
  p("Our major takeaways include believing that spendings are not the biggest
  factors in creating a successful academic environment. Something else we
  found especially highlighted was the uneven distribution of races across
  educational programs, likely a major contributor to the outcome of average
  standardized test scores. National average scores also detail obvious 
  differences. Overall, we feel that while money is not a key factor in student
  success, the barriers faced by students of different backgrounds is something
  America has yet to equalize.")
)

  bar_chart_page <- tabPanel(
  "Bar Chart",
  titlePanel("Average NAEP Scores by Race"),
  sidebarLayout(
    sidebarPanel(
      score_type <- radioButtons(
        "score_type",
        label = "Choose the Type of NAEP Test Scores You Would like to View",
        choices = list("Reading", "Mathematics")
      ),
      grade_level <- radioButtons(
        "grade_level",
        label = "Choose the Grade Level You Would like to View",
        choices = list("4th Grade" = "G04", 
                       "8th Grade" = "G08")
      ),
      race_input <- checkboxGroupInput("race_input",
                                       "Select the Races You Would like to View",
                                       c("White",
                                         "Black/African American",
                                         "Latinx",
                                         "Asian", 
                                         "American Indian/Alaska Native", 
                                         "Hawaiian Native/Pacific Islander", 
                                         "Two or More Races"),
                                       selected = c("White",
                                            "Black/African American",
                                            "Latinx",
                                            "Asian", 
                                            "American Indian/Alaska Native", 
                                            "Hawaiian Native/Pacific Islander", 
                                            "Two or More Races"))
    ),
    mainPanel(
      plotlyOutput("barchart")
    )
  )
) 

heatmap_page <- tabPanel(
  "Heatmap",
  titlePanel("Average Spendings and NAEP Scores By State"),
  sidebarLayout(
    sidebarPanel(
      h4("Manipulate the values below and view different data
               on the maps to the right."),
      fill <- selectInput(
        "fill",
        label = "Choose the Grade You Want to Evaluate",
        choices = list(
          "4th Grade" = "grade_4",
          "8th Grade" = "grade_8"
        ),
        selected = "grade_4"
      ),
      color_high <- selectInput(
        "color_high",
        label = "Choose the High Color You Would Like to View the Maps In",
        choices = list(
          "Red" = "red",
          "Orange" = "orange",
          "Yellow" = "yellow",
          "Blue" = "blue",
          "Green" = "green",
          "Purple" = "purple",
          "White" = "white",
          "Black" = "black"
        ),
        selected = "red"
      ),
      color_low <- selectInput(
        "color_low",
        label = "Choose the Low Color You Would Like to View the Maps In",
        choices = list(
          "Red" = "red",
          "Orange" = "orange",
          "Yellow" = "yellow",
          "Blue" = "blue",
          "Green" = "green",
          "Purple" = "purple",
          "White" = "white",
          "Black" = "black"
        ),
        selected = "yellow"
      )
    ),
    mainPanel(
      plotlyOutput("moneymap"),
      plotlyOutput("scores")
    )
  )
)

stacked_bar_chart_page <- tabPanel(
  "Stacked Bar Chart",
  titlePanel("Distribution of Race Across Educational Programs"),
  sidebarLayout(
    sidebarPanel(
      h4("Choose an educational program to view the distribution of race."),
      fill <- selectInput(
        inputId = "program",
        label = "Choose the program You Want to Evaluate",
        choices = list(
          "All Students" = "All Students",
          "Advanced Placement" = "Advanced Placement",
          "Gifted and Talented" = "Gifted and Talented",
          "International Baccalaureate" = "International Baccalaureate",
          "All Programs" = "All Programs"
        ),
        selected = "all_students"
      ),
    ),
    mainPanel(
      plotlyOutput("distribution")
    )
  )
  
)

ui <- navbarPage(
  "Factors that Affect Education ",
  intro_page,
  bar_chart_page,
  heatmap_page,
  stacked_bar_chart_page,
  summary_page
)