############################ umowaUI.R ###################################
# Author: Melissa L Muradian
# Written May 2023
# Last updated April 2024

##########################################################################
#############################  UI  #######################################
#############################  UI  #######################################
    
  ui <- navbarPage(title = div("Data Dashboard",
                                # Create Right Side Logo/Image with Link
                                tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://umowa.org\"><img src=\"umowa_logo.png\" alt=\"alt\" style=\"float:right;width:225px;padding-top:0px;\"> </a></div>');
                                                 console.log(header)")
                                )),
                                          
                      # tags$a(img(src="umowa_logo.png", height=52), href= "https://umowa.org/",
                      #        # style = "position: relative; top: -10px;"),
                      #        style = "position: relative; top: -15px; margin: 0px; border: 0px;
                      #        padding: 0px; background-color: white;"),
                      # 
               fluid=TRUE,
               windowTitle = "UMOWA Dashboard", #title for browser tab
               theme = shinytheme("flatly"), #Theme of the app. tried "cerulean"
               position = "static-top", # "fixed-top",
               # position = "fixed-top", # NEEDS LINE BELOW TO PAD THE TOP
               # tags$style(type="text/css", "body {padding-top: 70px;}"),
               # Using column(12,...) here instead of fluidRow is from a Dean Attali post where he explains:
               # When using `navbarPage`, the header and footer are inserted inside an HTML "row". As a result 
               # of that, because bootstrap applies a -15px margin-left to rows, the header/footer content is 
               # misaligned with the content in tabs, Bootstrap also says that "only columns may be immediate 
               # children of rows" (http://getbootstrap.com/css/#grid) # PHEW!! Thank you DA!
               footer = column(12, style = "background-color: #313a4a;",# #373737;",
                                  tags$head(tags$style(".butt{background-color: #313a4a;} .butt{color: #ffffff;}")),
                                  # tags$head(tags$style(".butt{background-color: #373737;} .butt{color: #add8e6;}")),
                                  # width = NULL, 
                                  # column( width = 3, height = 240, offset = 0, class="butt",# background = "olive", # try: aqua, light-blue, navy, lime, or black
                                  #         tags$a(img(src="umowa_logo.png", height = 60), href= "https://umowa.org/")),
                                  # column( width = 8, offset = 0, class="butt",
                                  # p("UMOWA’s mission is to understand, conserve, and enhance the unique ecological and 
                                  #   recreational resources of the Upper Missouri river. Please visit our main website 
                                  #   for even more information and ways to get involved. ")),
                                  #fluidRoww
                                  # column( width = 4, offset = 0, 
                                  #         h5(""),
                                  #         p(""), p(""), p("")),
                                  column( width = 5, align="center", height = 240, offset = 1, class="butt",
                                          h4("CONTACT US"),
                                          p("Upper Missouri Watershed \n Alliance"), 
                                          p("PO Box 377                    "), 
                                          p("Helena, MT 59624"),
                                          br(),
                                          p("Sherry Meador, Board Chair"), 
                                          p("(406) 431-7638"),
                                          p("smeador@umowa.org"),
                                          br()
                                  ),
                                  column( width = 5, align="center", height = 240, offset = 0, class="butt",
                                          h4("CONTRIBUTORS"),
                                          p("Upper Missouri Watershed Alliance"),
                                          p("The MACD Water Quality 
                                            Education and Outreach Mini-Grant Program"),
                                          p("Montana DEQ"),
                                          p("Melissa Muradian, Ecosa Consulting"),
                                          p("Dave Stagliano, Montana Biological Survey"),
                                          p("Jason Mullen, Montana Fish, Wildlife, & Parks")),
                                  column(width=1)
                                  # column( width = 3, img(src="IMG_3480 copy 2.jpg", height=350))
               ),
                                          # p("United States Geological Survey"))),
               # tabPanel("Summary Metrics",
               #          # fluidRow(column( width=6, img(src="BugsBanner.jpg", height=175)),
               #          #          column(width=5, offset=1, h3("Summary or explanatory text here"))),
               #          tabsetPanel(id="dashTabs",
               #                      # Here, they're all in the same style--bargraphs by year, selector for sites      
               #                      tabPanel("Intro"),
               #                      tabPanel("HBI Score"),
               #                      tabPanel("MMI")),
               #          br()),
               #                      
      
           tabPanel("Aquatic Insects",
                    fluidRow( #column(width=6, offset=0, 
                             h2(HTML("Benthic Macroinvertebrate Data"),style="text-align:center"),
                             h4(HTML("Collected by UMOWA, 2015-2023"),style="text-align:center")),
                             # column( width=6, align="center")),
                             # column( width=6, align="center", img(src="BugsBanner.jpg", height=150))),
                    tabsetPanel(id="bugsTabs",
                          # Here, they're all in the same style--bargraphs by year, selector for sites      
                          tabPanel("HBI Profile",
                                   br(),
                                   sidebarLayout( position = "left",
                                                  sidebarPanel( width = 3,
                                                                radioButtons("hbiProfYr", "Select Year",
                                                                             c("2015"=2015,"2016"=2016,"2017"=2017,"2018"=2018,
                                                                               "2019"=2019,"2021"=2021,"2022"=2022,"2023"=2023)) # hand update each yr
                                                  ),
                                                  mainPanel( offset = 1,plotOutput("plotHBIProf", height = "575px", width = "575px"), width = 8)
                                   ),
                                   h4("HBI River Profile"),
                                   p("These figures show HBI water quality rankings for each site, as the river flows, for a given year. 
                                     Use the options in the grey panel at top-left to click through the years. Holter Reservoir
                                     is shown at bottom center and the river in this stretch flows to the northeast, which is the 
                                     top-right corner. For further details on how HBI is calculated, see below."),
                                   p("For HBI water quality rankings for all years at a given site, click the", strong("HBI Time Series"),
                                     "tab in the mint-green colored submenu on this—the Aqautic Insects page."),
                                   br(),br(),
                                   h4("Hilsenhoff Biotic Index"),
                                   p("The Hilsenhoff Biotic Index (HBI) is a metric that classifies habitat quality
                                    in stream ecosystems based on the predetermined pollution tolerances of the observed 
                                    macroinvertebrate taxa."), 
                                   p("The biotic index is ranked for water quality and degree of organic pollution, as follows:"),
                                    tableOutput("HBI1Table"),
                                   p("For more information on HBI, including how the index is calculated, see the wiki page 
                                      https://en.wikipedia.org/wiki/Hilsenhoff_Biotic_Index"),
                                   br(),br()
                                   ),
                          tabPanel("MDEQ Thresholds",
                                   br(),
                                   sidebarLayout( position = "left",
                                                  sidebarPanel( width = 3, h4("Montana DEQ Impairement Thresholds for Mountain Multimetric Macroinvertebrate Protocol"),
                                                                radioButtons("mmiIndex1", "Select Index",
                                                                             c("% EPT"=3, "% Midges"=4)), # "% BDO"=7)),
                                                                radioButtons("mmiSite1", "Select Site",
                                                                             c("LPPC Upstream"=1 , "LPPC Downstream"=2, "Craig"=3, "Dearborn Upstream"=4,
                                                                               "Dearborn Downstream"=5, "Hardy"=6, "Cascade"=7))),
                                                  mainPanel( plotOutput("plotMMI", click = "plotClick", height = "500px"), width = 9)
                                   ),
                                   h4("Montana DEQ Impairement Thresholds for Mountain Multimetric Macroinvertebrate Protocol"),
                                   p("At top left in the grey panel, find options to select MMI protocol based on % EPT or % Midges for each site. The figures
                                     show the MMI standards set for Montana's mountain streams applied to UMOWA's data collected each year by our Macroinvertebrate 
                                     Study. These standards use similar biological standing as HBI, citing the tolerance of midges to sediment and organic pollution
                                     and the intolerance of the three taxa in the % EPT metric: 'E' for ", em("ephemeroptera")," or mayflies; 'P' for", em("plecoptera"),
                                     " or stoneflies; and 'T' for ", em("trichoptera")," or caddisflies. Note: the metric % EPT is a sum of realtive abundance of the 3 
                                     taxa within the total macroinvertebrate community. For the breakdown of their individual percentages, click the", strong("Percentages"),
                                     "tab in the mint-green colored submenu on this—the Aqautic Insects page. For a glimpse at the total community sampled at each site 
                                     in 2022, click the", strong("2022 Total Community"), "tab in the mint-green colored submenu on this—the Aqautic Insects page."),
                                   br(),br()),
                          tabPanel("Percentages",
                                   br(),
                                   sidebarLayout( position = "left",
                                                  sidebarPanel( width = 3, h4("Macroinvertebrate Percentages by Taxa"),
                                                                radioButtons("miIndex2", "Select Taxa",
                                                                             c("% Mayfly"=5, "% Caddis"=6, "% Tricos"=7)),
                                                                radioButtons("miSite2", "Select Site",
                                                                             c("LPPC Upstream"=1 , "LPPC Downstream"=2, "Craig"=3, "Dearborn Upstream"=4,
                                                                               "Dearborn Downstream"=5, "Hardy"=6, "Cascade"=7))),
                                                  mainPanel( plotOutput("plotMI2", height = "500px"), width = 9)
                                   ),
                                   h4("Macroinvertebrate Percentages by Taxa"),
                                   p("These figures show the relative abundance of the selected taxa through time for a given site. Use the options 
                                     in the grey panel at top left to select taxa and site. For what the relative percentage of these taxa can tell
                                     us about habitat and water quality, click the", strong("MDEQ Thresholds"),"tab in the mint-green colored submenu 
                                     on this—the Aqautic Insects page. "),
                                   br(),br() ),
                          tabPanel("HBI Time Series",
                                   br(),
                                   sidebarLayout( position = "left",
                                                  sidebarPanel( width = 3,
                                                                radioButtons("hbiSite", "Select Site",
                                                                             c("LPPC US"=1 , "LPPC DS"=2, "Craig"=3, "Dearborn US"=4,
                                                                               "Dearborn DS"=5, "Hardy"=6, "Cascade"=7))
                                                  ),
                                                  # Can I create a bit of html output here to show the coordinates
                                                  # of the plot click using something like:
                                                  # "You selected (x,y) coordinates: (input$plot_click[[1]][1],input$plot_click[[1]][2])"
                                                  # might be worht looking up points near click example.
                                                  mainPanel( width = 9,
                                                             fluidRow( column(10,plotOutput("plotHBI1", click = "plotClick", height = "500px")),
                                                                       column(2)))
                                   ),
                                   h4("HBI Time Series"),
                                   p("These figures show HBI water quality rankings for all years at a given site. Use the options in the grey panel 
                                      at top-left to click through the sitess. For further details on how how HBI is calculated, see below."),
                                   p("For HBI water quality rankings as the river flows for a given year, click the", strong("HBI River Profile"),
                                     "tab in the mint-green colored submenu on this—the Aqautic Insects page."),
                                   br(),br(),
                                   h4("Hilsenhoff Biotic Index"),
                                   p("The Hilsenhoff Biotic Index (HBI) is a metric that classifies habitat quality
                                     in stream ecosystems based on the predetermined pollution tolerances of the observed 
                                     macroinvertebrate taxa."), 
                                   p("The biotic index is ranked for water quality and degree of organic pollution, as follows:"),
                                   tableOutput("HBI2Table"),
                                   p("For more information on HBI, including how the index is calculated, see the wiki page 
                                     https://en.wikipedia.org/wiki/Hilsenhoff_Biotic_Index"),  br(),br()
                                   ),
                          tabPanel("Total community",
                                   br(),
                                   sidebarLayout( position = "left",
                                                  sidebarPanel( width = 3, h4("Taxa Composition by Site"),
                                                                radioButtons("tcIndex1", "Select Year",
                                                                             c("2022"=1, "2023"=2)),
                                                                plotOutput("TotalLegend", height="250px")),
                                                  mainPanel( plotOutput("plotTotalMI", height = "510px"), width = 9)
                                   ),
                                   h4("Total community, sampled in the fall"),
                                   p("This figure shows the relative abundance for each taxa within the total community, sampled at each site 
                                     in the fall of the chosen year. The key at the top left shows which color corresnpondes which taxa and doubles as a list of all
                                     the different taxa found on the river bottom. If you follow the height of one color through the sites (columns), you can see
                                     how the relative abundance of that taxa changes by site. For instance: maylfies dominated at Hardy; the Holter site
                                     had the highest relative abundance of scuds and isopods; and very few stoneflies were present at any of the sites, with 
                                     the highest percentage observed at the Dearborn confluence."), 
                                   br(),br()
                                   ),
                   
                    div(style="display: inline-block; width: 100%; height: 50%;",
                    htmlOutput('BugsDownloadButton', inline=TRUE)),
                    br(),br())),
  
           tabPanel("Water Quality",
                    fluidRow( #column(width=6, offset=0, 
                             h2(HTML("Water Quality Data"),style="text-align:center"),
                             h4(HTML("Collected by UMOWA, 2016-2023"),style="text-align:center")),
                             # column( width=6, align="center")),
                             # column( width=6, align="center", img(src="WQBanner.jpg", height=150))),
                    tabsetPanel(id="WQTabs",
                                tabPanel("Through time",
                                  br(),
                                  sidebarLayout( position = "left",
                                                 sidebarPanel( width = 3, h4("Water Quality Through Time"),
                                                               radioButtons("WQIndex1", "Select Attribute",
                                                                            c("Total Nitrogen"=6, "Inorganic Nitrogen"=5, "Total Phosphorus"=7, 
                                                                              "Total dissolved solids"=3, "Total suspended solids"=4)),
                                                               radioButtons("WQSite1", "Select Site",
                                                                            c("LPPC Upstream"=1 , "LPPC Downstream"=2, "Craig"=3, "Dearborn Upstream"=4,
                                                                              "Dearborn Downstream"=5, "Hardy"=6, "Cascade"=7))),
                                                 mainPanel( plotOutput("plotWQ1", click = "plotClick", height = "550px"), width = 9)
                                  )
                                ),
                                
                                tabPanel("Scatterplots",
                                         br(),
                                  sidebarLayout( position = "right",
                                                 mainPanel( plotOutput("plotWQ3", click = "plotClick", height = "550px"), width = 9),
                                                 sidebarPanel( width = 3, h4("Water Quality Scatterplots"),
                                                               # fluidRow(
                                                               #   column( width=4, 
                                                               h4(""),
                                                               radioButtons("WQIndex3", "Select Attribute",
                                                                            c("Total Nitrogen"=6, "Inorganic Nitrogen"=5, "Total Phosphorus"=7, 
                                                                              "Total dissolved solids"=3, "Total suspended solids"=4)),
                                                               radioButtons("ExpVar1", "Select the explanatory variable",
                                                                            c("Discharge"=1,"Time"=2, "Temp (C)"=3))) 
                                                              # column(width=8,
                                                              #        h4("A place for explanatory text able to change with radio selection")))
                                  )
                                )
                                # tabPanel("Site trends",
                                #####
                                #          br(),
                                #   sidebarLayout( position = "left",
                                #                  sidebarPanel( width = 3, h4("Water Quality Site Trends by Season"),
                                #                                radioButtons("WQIndex2", "Select Attribute",
                                #                                             c("Total Nitrogen"=6, "Inorganic Nitrogen"=5, "Total Phosphorus"=7, 
                                #                                               "Total dissolved solids"=3, "Total suspended solids"=4))),
                                #                                #fluidRow(
                                #                                  #column( width=4, 
                                #                                          
                                #                                  # column( width=8,
                                #                                  #         h4("This is example text and is able to change with index selection"),
                                #                                  #         p("TDS stands for Total Dissolved Solids and is a common measure of water quality that is related to
                                #                                  #           conductivity and salinity level. TDS concentration describes the presence of inorganic salts (sodium,
                                #                                  #           calcium, and magnesium, for example) and small amounts of organic matter (decaying plant
                                #                                  #           and animal material) in water. The material in TDS can have natural sources, such as geological
                                #                                  #           condition and seawater, and/or can have sources from human activities, such as domestic/industrial
                                #                                  #           waste or agriculture.")
                                #                                  #         )
                                #                                  # )),
                                #                  
                                #                  mainPanel( plotOutput("plotWQ2", click = "plotClick", height = "500px"), width = 9)
                                #                                  )
                                #     )
                                #####
                                ),
                    h4("It is possible that high nutrient levels are the biggest threat that the Upper
                       Missouri River is facing at present"),
                    p("Higher nutrient levels are causally related to the excessive aquatic plant and
                      algae growth. Nutrient levels in the mainstem Missouri River have been trending
                      upward over the past 4 study years, most notably in the Spring and Fall samples,
                      while below tributaries, concentrations are somewhat mediated by the inflows."),
                    p("Years of collecting and analyzing waterquality data have made it clear
                      that the majority of nutrient issues on the Missouri River below Holter Dam are
                      not from local sources but are originating from one or more of the upstream
                      reservoirs."),
                    p("Over and over we observe nutrient levels (TN, NN, TP) with concentrations exceeding
                      the recommended levels set by MDEQ. It is critical that UMOWA continue to monitor
                      water quality to see if these nutrient concentrations stabilize below impairment
                      levels or continue an upward trajectory."),
                    br(), br(),
                    div(style="display: inline-block; width: 100%; height: 50%;",
                        htmlOutput('WQDownloadButton', inline=TRUE)),
                    br(), br()
                    ),
           tabPanel("Temperature and Discharge",
                    fluidRow(# column(width=6, offset=0, 
                      h2(HTML("Water Temperature and Discharge Data"),style="text-align:center"),
                      h4(HTML("Collected by USGS, 2015-2023"),style="text-align:center")),
                    # column( width=6, align="center")),
                    # column( width=6, align="center", img(src="TroutBanner.jpg", height=150))),
                    br(),
                    sidebarLayout( position = "right",
                                   mainPanel( plotOutput("plotWQ4", height = "550px"), width = 9),
                                   sidebarPanel(width=3, h4("Temperature and Discharge below Holter Dam"),
                                      p("Temperature and discharge are displayed in the same panel, occupying 
                                      different ranges of the same vertical axis."),
                                      p("Discharge during spring runoff in 2018 was far higher than any other flows 
                                        observed in the last 8 years (max of approx. 15,000 cfs) while the annual cycle 
                                        of water temperature has remained stable below the dam."),
                                      p("Discharge in this segment of the Missouri River is controlled by 
                                      USBoR to provide water to irrigators"))
                    ),  br(),br()),
                    
           tabPanel("Trout",
                   fluidRow(# column(width=6, offset=0, 
                     h2(HTML("Trout Abundance Data"),style="text-align:center"),
                     h4(HTML("Provided by MT Dept. of Fish Wildlife, collected 1980-2023"),style="text-align:center")),
                   # column( width=6, align="center")),
                   # column( width=6, align="center", img(src="TroutBanner.jpg", height=150))),
                   br(),
                   sidebarLayout(position = "left",
                                 sidebarPanel(width=4, offset=0,
                                              # style = "background-color: #94b75a; color: #36454F",
                                              h4("Population numbers of Rainbow and Brown trout are doing well!"),
                                              p("The figure shows number of catchable size trout (length of 10 inches 
                                               and greater) per mile over the last 30+ years. Points are observed numbers 
                                               and lines describe the linear trend  (linear regression) for each site-species 
                                               combination."),
                                              p("Longterm trends are that Rainbows at Craig are 
                                               swiftly increasing while Rainbows at Cascade and Browns at both locations
                                               are holding steady with a slow rate of increase."), 
                                              p("Montana Department of Fish Wildlife and Parks has collected population
                                               data on Rainbow and Brown trout since 2000 at two sites: near Craig, MT and
                                               near Cascade, MT. The Department does not collect population data on 
                                               Mountain Whitefish, eventhough they are a recreationally and ecologically 
                                               important native species.")),
                                             
                                 mainPanel(width=7, offset=0, plotOutput("plotFishAIO", height = "550px")),
                  ), br(), br() ),
                   
          tabPanel("About", 
                   fluidRow(# column(width=6, offset=0, 
                     # h2(HTML("Data-Collection Sites"),style="text-align:center"),
                     # h4(HTML("for"),style="text-align:center"),
                     h2(HTML("Overview of UMOWA's Macroinvertebrate and Water Quality Studies"),
                        style="text-align:center"),
                     br(),br()),
                   # column( width=6, align="center")),
                   # fluidRow(
                   #   h2(HTML(),style="position:relative; padding-left:15px;")),
                   # position:relative; top:10px; padding-left:12px; float:left;"
                   sidebarLayout( position = "right", 
                                  sidebarPanel( width = 6, offset=0,
                                                htmlOutput("sitePhotoLabel"),
                                                imageOutput("sitePhotos")), 
                                  # box( width = NULL,
                                  # h4("Little Prickly Pear, upstream (LPPC U or LU)"),
                                  # img(src="IMG_3480 copy 2.jpg", height=350)),
                                  mainPanel( width = 6, offset=0,
                                             # h2("Data-Collection Sites"), 
                                             # h5("Click a pin on the map for more info"),
                                             wellPanel(
                                               h4(HTML("Interactive Map of Sampling Sites"),style="text-align:center"),
                                               leafletOutput('map'),
                                               h4(HTML("Click a marker on the map to select a different site"),style="text-align:center")
                                               # box( width = NULL,
                                               #      htmlOutput("locationSelection"))
                                             )
                                  )),
                   
                   sidebarLayout( position = "right",
                                  sidebarPanel( width = 5, offset=0,
                                                h4("Occurance of Data Collection"),
                                                h5("UMOWA collected data on macroinvertebrates (MI) and water quality (WQ) during the periods 
                                                   below"),
                                                tableOutput("IntroTable"),
                                                # p("“MI” indicates collection of macroinvertebrate data and “WQ” indicates
                                                #   collection of water quality data. For a given data type,
                                                p("*In spring of 2017 MI data was not collected from sites downstream of Craig.")),
                                  # p("**MI data collected in 2021 and 2022 are being processed and not yet included.")),
                                  mainPanel( width = 6, offset=0,
                                             htmlOutput("siteIntro"))),  br(),br()
                   )
          ########
        
    )
    


  # medium grey: #a9a9a9
  # charcoal grey: #36454F
  # also valid colors: "olive", # try: aqua, light-blue, navy, lime, or black
  








