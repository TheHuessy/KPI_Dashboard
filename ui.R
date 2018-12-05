library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(shinyWidgets)

#RUN THESE BEFORE ACTIVATING THE APP TO HOST IT DIRECTLY FROM THIS PC AND ALLOW OTHERS TO USE IT
#################################################
# options(shiny.port = 7775)
# 
# options(shiny.host = "10.241.172.209")
#################################################



#################         OFFICIAL LOGOS ARE HERE:         #################
# https://drive.google.com/drive/folders/1Qg17LqCOS08WKJx3U-id-8rR3ugMVIM5 #
############################################################################





# Define UI for application that draws a histogram
shinyUI(
  dashboardPagePlus(

    
    dashboardHeaderPlus(title = "KPI Dashboard Demo"),
    dashboardSidebar(
##### SIDEBAR TABS #####
      sidebarMenu(
        menuItem("Welcome",
                 tabName = "Land",
                 icon = icon("info-sign", lib = "glyphicon")
                 ),
        menuItem("Administration and Finance",
                 tabName = "AnF",
                 icon = icon("usd", lib = "glyphicon")
                 ),
        menuItem("Arts and Culture",
                 tabName = "AC",
                 icon = icon("paint-brush", lib = "font-awesome")
                 ),
        menuItem("Boston Fire Department",
                 tabName = "BFD",
                 icon = icon("fire", lib = "glyphicon")
                 ),
        menuItem("Boston Police Department",
                 tabName = "BPD",
                 icon = icon("user", lib = "glyphicon")
                 ),
        menuItem("Boston Public Schools",
                 tabName = "BPS",
                 icon = icon("apple", lib = "glyphicon")
                 ),
        menuItem("Civic Engagement",
                 tabName = "CE",
                 icon = icon("inbox", lib = "glyphicon")
                 ),
        menuItem("Economic Development",
                 tabName = "ED",
                 icon = icon("cog", lib = "glyphicon")
                 ),
        menuItem("Environment",
                 tabName = "EEOS",
                 icon = icon("leaf", lib = "glyphicon")
                 ),
        menuItem("Health and Human Services",
                 tabName = "HHS",
                 icon = icon("plus-sign", lib = "glyphicon")
                 ),
        menuItem("Housing",
                 tabName = "Hou",
                 icon = icon("home", lib = "glyphicon")
                 ),
        menuItem("Innovation and Technology",
                 tabName = "DoIT",
                 icon = icon("off", lib = "glyphicon")
                 ),
        menuItem("Mayor's Office",
                 tabName = "MO",
                 icon = icon("king", lib = "glyphicon")
                 ),
        menuItem("Operations",
                 tabName = "Ops",
                 icon = icon("flag", lib = "glyphicon")
                 ),
        menuItem("Streets",
                 tabName = "St",
                 icon = icon("road", lib = "glyphicon")
                 )
        )
      ),
##### DASBOARD BODY #####
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Styler.css"),
        tags$link(rel = "stylesheet", 
                  href = "https://use.fontawesome.com/releases/v5.4.2/css/all.css",
                  integrity="sha384-/rXc/GQVaYpyDdyxK+ecHPVYJSN9bmVFBvjA/9eOB+pb3F2w2N6fc5qB9Ew5yIns",
                  crossorigin="anonymous"
                  ),
        tags$link(rel = "stylesheet", 
                  href = "https://use.fontawesome.com/releases/v5.4.2/css/v4-shims.css"
        )
      ),
      
      tabItems(
##### Landing Page ##### 
tabItem(tabName = "Land",
        fluidPage(
          h1("Welcome to the New KPI Dashboard"),
          #(br()),
          (br()),
          h4("DISCLAIMER"),
          br(),
          h5("This dashboard is not complete. It is currently 
             under construction and there will be elements missing. 
             Furthermore, this dashboard is not live, meaning that 
             the data reported in this dashboard do not automatically
             update. This functionality is coming soon. This is still 
             a work in progress and we as ask that you be patient."),
          (br()),
          h4("HOW TO USE"),
          br(),
          h5("Each tab to the left contains data for the respective cabinet.
             The data are presented in two ways: as an interactive table 
             (for tables longer than one row) and a corresponding chart. If 
             you would like to see a chart for a certain metric, simply click 
             on the metric row in the table and the chart will update."),
          br(),
          h5("The charts are also interactive. You can use your mouse to hover over 
             the different pieces of the chart and generate a small popup of the data being presented. 
             If you would like to zoom in on a particular piece of the graph, simply click and drag 
             around the area you'd like to zoom in on and the chart will zoom to that box. To reset 
             the zoom window, double click somewhere on the empty part of the chart."),
          br(),
          h4("FEATURES COMING SOON"),
          br(),
          h5("As this platform is still under development, there are still 
             pieces that haven't been added yet. We are currently working on several additions to 
             this version, including: "),
          h5(strong("- Getting missing data for certain departments")),
          h5(strong("- Developing department reports that can be easily downloaded directly from this interface")),
          h5(strong("- Various aesthetic changes")),
          h5(strong("- General proofreading")),
          br(),
          h4("FEEDBACK"),
          br(),
          h5(strong("Any and all feedback is appreciated."), "If there is 
             something you see on the dashboard that looks funny or odd, please don't hesitate 
             to mention it. If there is a function or feature you think should be added, removed, or changed please 
             also don't hesitate to reach out."),
          h5(strong("Send any feedback to James Huessy at", a(href = "mailto:james.huessy@boston.gov?subject=KPI Dashboard Feedback","james.huessy@boston.gov")))
        )
),
##### A&F ##### 
        tabItem(tabName = "AnF",
                fluidPage(
                  # widgetUserBox(title = "Assessing",
                  #               src = "https://www.bostonpublicschools.org/cms/lib/MA01906464/Centricity/Domain/187/BPS%20Black%20Logo.png",
                  #               type = 1,
                 
                 h3("The Administration and Finance Cabinet ensures that city services are delivered with high quality, with high ethical standards, are financially prudent, are responsive to the needs of the citizens of Boston, and consistent with the laws and ordinances governing municipal government."),
                  br(),
                  boxPlus(title = "Assessing",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,

                      # fluidRow(
                      #   valueBoxOutput("AnF.1.M"),
                      #   valueBoxOutput("AnF.1.MY"),
                      #   valueBoxOutput("AnF.1.Y")
                      #   
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AnF.AssTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AnF.AssPlt")
                        )
                      ),
                      width = 12
                      ),
                  boxPlus(title = "Auditing",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("AnF.2.M"),
                      #   valueBoxOutput("AnF.2.MY"),
                      #   valueBoxOutput("AnF.2.Y")
                      #   
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AnF.AudTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AnF.AudPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Budget",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("AnF.3.M"),
                      #   valueBoxOutput("AnF.3.MY"),
                      #   valueBoxOutput("AnF.3.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AnF.BudTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AnF.BudPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Human Resources",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("AnF.4.M"),
                      #   valueBoxOutput("AnF.4.MY"),
                      #   valueBoxOutput("AnF.4.Y")
                      #   ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AnF.HrTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AnF.HrPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Purchasing",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("AnF.5.M"),
                      #   valueBoxOutput("AnF.5.MY"),
                      #   valueBoxOutput("AnF.5.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AnF.PurTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AnF.PurPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Treasury & Collecting",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          fluidRow(
                            infoBoxOutput("AnF.TC.IB.1"),
                            infoBoxOutput("AnF.TC.IB.2"),
                            infoBoxOutput("AnF.TC.IB.3")

                          ),
                          fluidRow(
                            box(title = "Table",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 6,
                                dataTableOutput("AnF.TCTbl")
                            ),
                            box(title = "Chart",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 6,
                                plotlyOutput(outputId = "AnF.TCPlt")
                            )
                          ),
                          width = 12
                  )
                  )
                ),

##### ARTS AND CULTURE #####
        tabItem(tabName = "AC",
                fluidPage(
                  h3("The mission of the Arts and Culture Cabinet is to foster the growth and well-being of the cultural community and promote participation in the arts. Recognizing the importance of creativity across all policy areas, the cabinet seeks to promote access to arts and culture to all the City's residents, and to make Boston a municipal arts leader."),
                  br(),
                  boxPlus(title = "Arts and Culture",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("AC.1.M"),
                      #   valueBoxOutput("AC.1.MY"),
                      #   valueBoxOutput("AC.1.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("AC.ArtTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "AC.ArtPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Boston Public Library",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("AC.2.M1",
                                      width = 12),
                        infoBoxOutput("AC.2.MY"),
                        infoBoxOutput("AC.2.Y")
                      ),
                    #  uiOutput("BPS.Stats"),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("LUS")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "AC.LusPlt"
                                         )
                            )
                        ),
                      width = 12
                      )
                  )
                ),
##### BFD #####
tabItem(tabName = "BFD",
        fluidPage(
          h3("We are an organization of dedicated professionals who are committed to serving the community by protecting life, property, and the environment through prevention, education, emergency medical, civil defense and fire service. We protect all Boston residents and the hundreds of thousands of people who work, shop and visit the City. Learn more about the Boston Fire Department, and how our divisions work together."),
          br(),
          boxPlus(title = "Boston Fire Department",
                  closable = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
              fluidRow(
                infoBoxOutput("BFD.IB.1"),
                infoBoxOutput("BFD.IB.2"),
                infoBoxOutput("BFD.IB.3")
                ),
              fluidRow(
                box(title = "Table",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput("BFDTbl")
                ),
                box(title = "Chart",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput(outputId = "BFDPlt")
                )
              ),
              width = 12
          )
        )
),
##### BPD #####
        tabItem(tabName = "BPD",
                fluidPage(
                  h3("The Boston Police Department is dedicated to working in partnership with the community to fight crime, reduce fear and improve the quality of life in our neighborhoods. Our Mission is Community Policing."),
                  br(),
                  boxPlus(title = "Boston Police Department",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("BPD.IB.1"),
                        infoBoxOutput("BPD.IB.2"),
                        infoBoxOutput("BPD.IB.3")
                      ),
                      fluidRow(
                        infoBoxOutput("BPD.IB.4"),
                        infoBoxOutput("BPD.IB.5"),
                        infoBoxOutput("BPD.IB.6")
                      ),
                      fluidRow(
                        infoBoxOutput("BPD.IB.7"),
                        infoBoxOutput("BPD.IB.8"),
                        infoBoxOutput("BPD.IB.9")
                      ),
                      # fluidRow(
                      #   valueBoxOutput("BPD.1.M"),
                      #   valueBoxOutput("BPD.1.MY"),
                      #   valueBoxOutput("BPD.1.Y")
                      # ),
                      fluidRow(
                      box(title = "Table",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          dataTableOutput("BPDTbl")
                          ),
                      box(title = "Chart",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          plotlyOutput(outputId = "BPDPlt")
                          )
                      ),
                      width = 12
                      ),
                  boxPlus(title = "Over Time by Unit",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      # fluidRow(
                      #   valueBoxOutput("BPD.1.M"),
                      #   valueBoxOutput("BPD.1.MY"),
                      #   valueBoxOutput("BPD.1.Y")
                      # ),
                      fluidRow(
                        box(title = "Hours by Unit",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("BPD_OTHTbl")
                        ),
                        box(title = "Hours by Unit",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "BPD_OTHPlt")
                        )
                      ),
                      fluidRow(
                        box(title = "$ Paid by Unit",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("BPD_OTPTbl")
                        ),
                        box(title = "$ Paid by Unit",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "BPD_OTPPlt")
                        )
                      ),
                      width = 12
                  )
                  )
                ),
##### BPS #####
        tabItem(tabName = "BPS",
                fluidPage(
                  h3("Boston Public Schools partners with the community, families, and students to develop in every student the knowledge, skill, and character to excel in college, career, and life. Our responsibility is to ensure every child has great teachers and great school leaders. In our system, we tailor instruction to meet the individual needs of every student."),
                  br(),
                  boxPlus(title = "Boston Public Schools",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      width = 12,
                      fluidRow(
                        infoBoxOutput("BPS.IB.1", width = 6),
                        infoBoxOutput("BPS.IB.2", width = 6)
                      ),
                      fluidRow(
                        infoBoxOutput("BPS.IB.3", width = 6),
                        infoBoxOutput("BPS.IB.4", width = 6)
                      ),
                      # fluidRow(
                      #   valueBoxOutput("BPS.1.M"),
                      #   valueBoxOutput("BPS.1.MY"),
                      #   valueBoxOutput("BPS.1.Y")
                      # ),
                      fluidRow(
                      box(title = "Student Attendance (daily)",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          dataTableOutput(outputId = "SchoolGTbl",
                                          width = "100%")
                          ),
                      box(title = "Student Attendance Chart",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          plotlyOutput(outputId = "BPSDaily")
                      )
                      ),
                      fluidRow(
                      box(title = "Student Attendance by School",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          dataTableOutput(outputId = "SchoolTbl",
                                   width = "100%")
                          ),
                      box(title = "School Attendance Chart",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          plotlyOutput(outputId = "BPSALL",
                                       height = "600px")
                      )
                      )
                      )
                  )
                ),
##### CIVIC ENGAGEMENT #####
        tabItem(tabName = "CE",
                fluidPage(
                  h3("The Civic Engagement Cabinet seeks to improve the delivery of City Services as well as create opportunities for all Boston Residents to participate in local government."),
                  br(),
                  boxPlus(title = "Civic Engagement",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                       fluidRow(
                         infoBoxOutput("CE.IB.1", width = 6),
                         infoBoxOutput("CE.IB.2", width = 6)
                       ),
                      fluidRow(
                        infoBoxOutput("CE.IB.3", width = 6),
                        infoBoxOutput("CE.IB.4", width = 6)
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("CETbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "CEPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### ECONOMIC DEVELOPMENT #####
        tabItem(tabName = "ED",
                fluidPage(
                  h3("The mission of the Economic Development Cabinet is to lead a broad effort to streamline and support areas of focus that contribute to Boston's economy including tourism, jobs and employment, business development, and real estate development."),
                  br(),
                  boxPlus(title = "Consumer Affairs",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("ED.1.M"),
                      #   valueBoxOutput("ED.1.MY"),
                      #   valueBoxOutput("ED.1.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("ED.CalTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "ED.CalPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Economic Development",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("ED.2.M"),
                      #   valueBoxOutput("ED.2.MY"),
                      #   valueBoxOutput("ED.2.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("ED.EcdTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "ED.EcdPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### ENVIRONMENT #####
        tabItem(tabName = "EEOS",
                fluidPage(
                  h3("The mission of the Environment, Energy and Open Space Cabinet is to coordinate several City departments and programs to enhance sustainability, preserve historic and open space resources, protect the health and safety of the built environment, prepare for climate change, and provide public spaces to gather and recreate in Boston."),
                  br(),
                  
                  boxPlus(title = "Parks Department",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("EV.Prk.IB.1", width = 6),
                        infoBoxOutput("EV.Prk.IB.2", width = 6)
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("EV.PrkTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "EV.PrkPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### HEALTH AND HUMAN SERVICES #####
        tabItem(tabName = "HHS",
                fluidPage(
                  h3("The Health and Human Services Cabinet is committed to promoting the health and well-being of the City's residents particularly the most vulnerable. The provision of social, recreational, health and support services to city residents, particularly the homeless, persons with disabilities, women, the elderly, youth, immigrants and veterans, will be coordinated and made available in a customer-friendly and culturally sensitive manner."),
                  br(),
                  boxPlus(title = "Boston Center for Youth and Families",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("HHS.ByfTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "HHS.ByfPlt")
                        )
                      ),
                      width = 12),
                  boxPlus(title = "Boston Public Health Commission",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("HHS.Bph.IB.1"),
                        infoBoxOutput("HHS.Bph.IB.2"),
                        infoBoxOutput("HHS.Bph.IB.3")
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("HHS.BphTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "HHS.BphPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### HOUSING #####
        tabItem(tabName = "Hou",
                fluidPage(
                  h3("The Cabinet is committed to making Boston the most livable city in the nation by working with its many communities to build strong neighborhoods through the strategic investment of public resources."),
                  br(),
                  boxPlus(title = "Housing",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                       fluidRow(
                         infoBoxOutput("HOU.IB.1", width = 12)#,
                      #   valueBoxOutput("HOU.1.MY"),
                      #   valueBoxOutput("HOU.1.Y")
                       ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("HOUTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "HOUPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### DOIT #####
        tabItem(tabName = "DoIT",
                fluidPage(
                  h3("The mission of the Information and Technology Cabinet is to provide systems and technologies that develop and support department personnel with information relative to their operations, support strategic planning, promote effective resource management, enhance customer service and promote internal and external electronic and voice communications."),
                  br(),
                  boxPlus(title = "Department of Innovation and Technology",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("IT.IB.1", width = 12)
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("DoitTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "DoitPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### MAYOR'S OFFICE #####
        tabItem(tabName = "MO",
                fluidPage(
                  h3("The agencies reporting to the Mayor's Office represent the Mayor and the City in legal matters, public relations, and elections. The Mayor's vision for the future of the City is reflected in the policies and directions carried forward by the staff of these offices."),
                  br(),
                  boxPlus(title = "Law",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                      # fluidRow(
                      #   valueBoxOutput("MO.1.M"),
                      #   valueBoxOutput("MO.1.MY"),
                      #   valueBoxOutput("MO.1.Y")
                      # ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            dataTableOutput("MOTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            h2("Coming Soon..."),
                            plotlyOutput(outputId = "MOPlt")
                        )
                      ),
                      width = 12)
                  )
                ),
##### OPERATIONS #####
        tabItem(tabName = "Ops",
                fluidPage(
                  h3("The Operations Cabinet oversees all operational activities that intersect with the management of central facilities. The cabinet includes departments that set policies for intergovernmental relations and central municipal properties."),
                  br(),
                  boxPlus(title = "Inspectional Services",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          fluidRow(
                            infoBoxOutput("OP.Ins.IB.1", width = 4),
                            infoBoxOutput("OP.Ins.IB.2", width = 4),
                            infoBoxOutput("OP.Ins.IB.3", width = 4)
                          ),
                          
                          fluidRow(
                            box(title = "Table",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 6,
                                dataTableOutput("OP.InsTbl")
                            ),
                            box(title = "Chart",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 6,
                                plotlyOutput(outputId = "OP.InsPlt")
                            )
                          ),
                          width = 12
                  ),
                  boxPlus(title = "Property Management",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("Ops.IB.1"),
                        infoBoxOutput("Ops.IB.2"),
                        infoBoxOutput("Ops.IB.3")
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("OpsTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "OpsPlt")
                        )
                      ),
                      width = 12
                      )
                  )
                ),
##### STREETS #####
        tabItem(tabName = "St",
                fluidPage(
                  h3("The mission of the Streets Cabinet is to innovate, develop, implement, support and manage all programs, projects and policies that enhance clean, well-lit, attractive and efficient infrastructure that moves vehicular and pedestrian traffic safely."),
                  br(),
                  boxPlus(title = "Transportaton",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("ST.Tra.IB.1", width = 6),
                        infoBoxOutput("ST.Tra.IB.2", width = 6)
                      ),
                      fluidRow(
                        infoBoxOutput("ST.Tra.IB.3", width = 6),
                        infoBoxOutput("ST.Tra.IB.4", width = 6)
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("ST.TraTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "ST.TraPlt")
                        )
                      ),
                      width = 12
                      ),
                  boxPlus(title = "Department of Public Works",
                          closable = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                      fluidRow(
                        infoBoxOutput("ST.Dpw.IB.1", width = 6),
                        infoBoxOutput("ST.Dpw.IB.2", width = 6)
                      ),
                      fluidRow(
                        infoBoxOutput("ST.Dpw.IB.3", width = 6),
                        infoBoxOutput("ST.Dpw.IB.4", width = 6)
                      ),
                      fluidRow(
                        box(title = "Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dataTableOutput("ST.DpwTbl")
                        ),
                        box(title = "Chart",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput(outputId = "ST.DpwPlt")
                        )
                      ),
                      width = 12
                      )
                  )
                )
        )
      )
    )
  )