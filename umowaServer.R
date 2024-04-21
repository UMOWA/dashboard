########################## umowaServer.R #################################
# Author: Melissa L Muradian
# Written May 2023
# Last updated April 2024

##########################################################################
###########################  Server  #####################################
###########################  Server  #####################################
server <- function(input, output, session) {
  
  ######### Reactive Values ##########
  # create a reactive value that will store the click position
  rv <- reactiveValues(
    location = "LPPC US" # sample site
  )
  
  ######### Observers ##############
  # If reactive value input$m_marker_click changes, update locations: Used in Water Quality graphing functions
  observeEvent(input$map_marker_click, {
    rv$location = input$map_marker_click$id
  })
  
  
  ######### Adding to the Output List ##############
  # IntroBlurbTop
  
  output$map <- renderLeaflet({
    leaflet( options = leafletOptions(minZoom=7, maxZoom=18) ) %>%
      setView(lng = -111.91174, lat = 47.135, zoom = 10)  %>%
      addTiles() %>%  # xAdd default OpenStreetMap map tiles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      # addProviderTiles(providers$CartDB.Voyager) %>% # or try: $USGS.USImageryTopo
      addMarkers(lng=long, lat = lat, popup=popLocs, layerId = locAbbrev, label = locAbbrev,
                 labelOptions = labelOptions(noHide = T)) %>%
      addMarkers(lng=long2, lat = lat2, popup=popLocs2, layerId = locAbbrev2, label = locAbbrev2,
                 labelOptions = labelOptions(noHide = T), clusterOptions = markerClusterOptions()) %>%
      addMarkers(lng=long3, lat = lat3, popup=popLocs3, layerId = locAbbrev3, label = locAbbrev3,
                 labelOptions = labelOptions(noHide = T), clusterOptions = markerClusterOptions())
  })
  
  output$IntroTable <- renderTable({ 
    # Cols 2:4 require updating by hand each yr
    data.frame( Year = YEARS,
                Spring = c("MI,--", "MI,WQ", "MI*,--", "--,WQ", "--,--", "--,WQ", 
                           "MI,WQ", "--,WQ", "--,WQ"),
                Summer = c("MI,--", "MI,WQ",  "MI,WQ", "MI,WQ", "--,--", "--,WQ", 
                           "--,WQ", "--,WQ", "--,WQ"),
                Fall   = c("MI,--", "MI,WQ",  "MI,WQ", "MI,WQ", "MI,WQ", "--,WQ", 
                           "--,WQ", "MI,WQ", "MI,WQ"))
    # rownames = T
    })
  
  output$sitePhotoLabel <- renderUI({
    h4(HTML(paste("Image of", rv$location)),style="text-align:center")
    
  })
  
  output$sitePhotos <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_sitePhotos_width
    height <- session$clientData$output_sitePhotos_height
    # For high-res displays, this will be greater than 1
    # pixelratio <- session$clientData$pixelratio
    # return list
    list(src = paste0("www/",rv$location,".jpg"), 
         height = height,
         width = width,
         alt = "Site photo")
  },deleteFile = FALSE)
  
  # was used with htmlOutput("sitePhotos") in the UI
  # output$sitePhotos <- renderUI({
  #   box( width = NULL,
  #        h4(rv$location),
  #        img(src=paste0(rv$location,".jpg"), height=350)
  #   )
  # })
  
  output$siteIntro <- renderUI({
    box( width = NULL,
         h4(HTML(rv$location),style="text-align:center"),
     if(rv$location == "LPPC US"){
      p("UMOWA collects macroinvertebrate population data (beginning in 2015) and water quality data (beginning in 2016) from 
        Little Prickly Pear Creek, Upstream (LPPC US). We can pair UMOWA's macroinvertebrate and water quality data
        from this site with temperature and flow data from USGS's Holter Dam site to build an understanding of how water
        quality and water quantity interact along this reach of the Missouri River. From the map above you can see that 
        UMOWA's data collection occurs immediately upstream of the confluence of Little Prickly Pear Creek with the 
        Missouri River, so when data from this site is paired with data from the site Downstream of Little Prickly Pear 
        Creek we can quantify the impact of Little Prickly Pear Creek on downstream moacroinvertebrates and water quality 
        in the mainstem.")
    } else if(rv$location == "LPPC DS"){
      p("Data collected from the site downstream of Little Prickly Pear Creek (LPPC DS) is paired with data 
        from the upstream site (LPPC US) to quantify the tributary's impact on downstream and water quality
        and macroinvertebrate populations. You can use the navigation tabs (at top) to explore trends and 
        patterns in these data for the site 'Little Prickly Pear Creek, Downstream' by selecting LPPC DS.")
    } else if(rv$location == "Craig"){
       p("Data collection at Craig provides...")
    } else if(rv$location == "Dearborn US"){
       p("Dearborn US collection occurs imm. upstream of the confluence of the Dearborn River with the Missouri...")
    } else if(rv$location == "Dearborn DS"){
       p("Dearborn US collection occurs downstream of the confluence of the Dearborn River with the Missouri...")
    } else if(rv$location == "Hardy"){
       p("Collection at the Hardy Bridge...")
    } else {
      p("The Cascade site is sampled by UMOWA for sediment and nutrient concentrations (see table above), sampled by MT FWP for 
        trout abundance, and sampled by USGS for river flow (discharge). You can use the navigation tabs at the top of this 
        website to explore trends and patterns in and amongst these data for the Cascade site.")
    }
    )
  })
  #####
  
  #####################################
  ######## Aquatic Insects ############
  ##### plotTotalMI
  output$plotTotalMI <- renderPlot({
    
    i <- as.numeric(input$tcIndex1)
    
    par(mar=c(5,6,1,1), oma=c(1,1,2,1))
    p <- barplot(height=t(as.matrix(TotalComm[[i]])), names=rep("",8),
                 # names=c("Holter","LPPC \nUpstream","LPPC \nDownstream","Craig","Dearborn \nUpstream",
                 #         "Dearborn \nDownstream", "Hardy", "Cascade"), 
                 horiz=F, ylim=c(0,100),las=1, cex.lab=2, cex.axis=1.5, xlab="2022",
                 cex.main=1.5, ylab="Percent of Total Community", mgp=c(4, 1, 0),
                 col=c("dodgerblue","red","gold","olivedrab4","dodgerblue4","grey","darkolivegreen1",
                       "black","lightpink"))
    mtext(side=1, line=2, at=p,
         text=c("Holter","LPPC \nUp.","LPPC \nDwn.","Craig","Dearborn \nUp.",
                  "Dearborn \nDwn.", "Hardy", "Cascade"),
         cex=1.4)
    
     
  })
  
  output$TotalLegend <- renderPlot({
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(1:10, type='n', main="", xlab="", ylab="", xaxt="n", yaxt="n")
    legend("topleft", bty="n", cex=1.5,
           fill=c("dodgerblue","red","gold","olivedrab4","dodgerblue4","grey","darkolivegreen1",
                            "black","lightpink"),
           legend=c( "Mayflies","Stonefiles","Caddisflies","Riffle Beetles","Midges","Flatworms",
                     "Aquatic Snails/Clams","Scuds/Isopods","Aquatic Moths"))
  })
  
  ##### plotMI2
  output$plotMI2 <- renderPlot({
    i <- as.numeric(input$miSite2)
    j <- as.numeric(input$miIndex2)
    
    par(mar=c(3,6,4,1), oma=c(1,1,2,1))
    if(j==7){ # Tricos are in the single digits
      p <- barplot(height=locationTS[[i]][,j], names=YEARS, horiz=F, col="dodgerblue",
                   ylim=c(0,30),las=1, cex.lab=2, cex.axis=1.5, cex.names=1.5,cex.main=1.5,
                   ylab="Percent of Total Community", main="% Tricos (a Mayfly subtaxa)", mgp=c(4, 1, 0))
      text(x=p,y=locationTS[[i]][,j]+2, labels=paste(round(locationTS[[i]][,j],1),"%"), cex=1.3)
      if(i==2 & j==7){
        text(x=p[9],y=locationTS[[i]][9,j]-15, labels=paste(round(locationTS[[i]][9,j],1),"%"), cex=1.3)
      }
      if(i==4 & j==7){
        text(x=p[7],y=locationTS[[i]][7,j]-5, labels=paste(round(locationTS[[i]][7,j],1),"%"), cex=1.3)
      }
      
    } else{
      p <- barplot(height=locationTS[[i]][,j], names=YEARS, horiz=F,
                   ylim=c(0,80),las=1, cex.lab=2, cex.axis=1.5, cex.names=1.5,cex.main=1.5,
                   ylab="Percent of Total Community", mgp=c(4, 1, 0),
                   main=ifelse(j==5, "% Mayfly","% Caddis"),
                   col=ifelse(j==5, "dodgerblue","gold"))
      text(x=p,y=locationTS[[i]][,j]+3, labels=paste(round(locationTS[[i]][,j],1),"%"), cex=1.3)
    }
  })

  ##### plotHBIProfile
  
  output$plotHBIProf <- renderPlot({
    i <- as.numeric(input$hbiProfYr)
    dat <- subset(midat, Year==i, HBI)
    
    par(mar=c(0,0,0,0), oma=c(2,0,2,0))
    # Set up a plot area with no plot
    plot(1:100, type='n', main="", xlab="", ylab="", las=1,
         xaxs="i",yaxs="i", xaxt="n", yaxt="n")
    # Get the plot information so the image will fill the plot box
    lim <- par()
    rasterImage(my_image, 
                xleft=3, xright=99.5, 
                ybottom=2, ytop=87)
    # Draw white circles first
    points(c(45,47.5,53,57.5,61.5,69,91.5), c(18,22,29,42,43.5,51,79), 
           pch=21, cex=5, bg="white", col=1)
    # Site labels
    text(x=c(40,54.5,59.5,50,71,62,83), y=c(20,20,29,43,39,53.5,79), 
         labels=c("LPPC \nUp.", "LPPC \nDwn.", "Craig", "Dearborn \nUp.", 
                  "Dearborn \nDownstream", "Hardy \nBridge", "Cascade"),
         lwd=1.1,cex=1.5)
    # Fill in colors according to HBI rank 
    points(c(45,47.5,53,57.5,61.5,69,91.5), c(18,22,29,42,43.5,51,79),
           pch=21, cex=5, col=1, lwd=2,
           bg=colorsHBI[.bincode(dat$HBI,cutoffHBI,right=T,include.lowest=T)] )
    legend(19,107, 
"HBI correlates macroinvertebrate health to
water quality using the fact that a subset 
of macroinvertebrates are indicator  
species for nutrient and 
sediment pollution", 
           cex=1.5,bty="n")
    # load in clipped HBI scale
    rasterImage(hbi_scale, 
                xleft=3, xright=24, 
                ybottom=33, ytop=98)
    legend(70,31,
paste("Circle colors show 
the HBI rank for 
each site from the 
fall sampling 
in", i), 
           cex=1.2,bty="n")
    
  })
  

  ##### plotHBI1
  output$plotHBI1 <- renderPlot({
    i <- as.numeric(input$hbiSite)
     
    par(mar=c(3,6,1,3), oma=c(1,2,2,5))
    plot( 1:length(YEARS), rep(NA,length(YEARS)), xlab = "", xaxt = "n",
          ylab = "HBI", mgp=c(4,1,0), axes = F, cex.lab=2,
          pch=19, ylim = c(0,10))
    for(x in 1:7){ # 7 HBI levels
      polygon(x=c(0.5,10,10,0.5), y=c( rep((10-levelsHBI[2*x-1]),2), rep((10-levelsHBI[2*x]),2)),
              col=colorsHBI[x], border=NA, xpd = NA)
    }
    points(1:length(YEARS), 10-locationTS[[i]][,8], pch=19, cex=2) # HBI is column 8
    # mtext(2015:2022, 1, line = 1.7, at = c(1:8), cex=1.5 )
    axis(1, at=1:length(YEARS), YEARS, pos = 0, cex.axis=1.5)
    axis(2, at=seq(0,10,2), seq(10,0,-2), pos = 0.5, las = 1,cex.axis=1.5)
    mtext( miSite[i], 3, cex = 2)
    mtext(c("Excellent", "Very Good", "Good", "Fair", "Fairly Poor", "Poor", "Very Poor"),
          4, line = 1.5, at=10-c(2,4,5,6,7,8,9.25), las = 1, cex = 1.5)

  })
  #####

  ##### plotMMI
  output$plotMMI <- renderPlot({
    i <- as.numeric(input$mmiSite1)
    j <- as.numeric(input$mmiIndex1)
    
    par(mar=c(3,7,7,3), oma=c(1,1,1,5))
    plot( 1:length(YEARS), rep(NA,length(YEARS)), xlab = "", xaxt = "n", cex.main=1.3,
          ylab = "Percent of Total Community", mgp=c(5,1,0), axes = F, cex.lab=2,
          pch=19, ylim = c(0,100),
main=ifelse(j==3,
"Samples with combined % Mayfly, Caddis, Stonefly (% EPT) less
less than 60 % of the total community are impaired for mountain aquatic life",
"Samples with Midges exceeding 40% of the total community
are impaired for mountain aquatic life"))
    for(x in 1:4){
      if(j==3){
        polygon(x=c(0.5,100,100,0.5), 
                y = c(rep(levelsMMI.EPT[2*x-1],2), rep(levelsMMI.EPT[2*x],2)),
                col=colorsMMI[x], border=NA, xpd = NA)
        mtext(c("Good", "Fair", "Poor", "Very Poor"),
              4, line = 1.5, at=c(70,50,30,10), las = 1, cex = 1.5)
      } else {
        polygon(x=c(0.5,100,100,0.5), 
                y = c(rep(levelsMMI.Midges[2*x-1],2), rep(levelsMMI.Midges[2*x],2)),
                col=colorsMMI[x], border=NA, xpd = NA)
        mtext(rev(c("Good", "Fair", "Poor", "Very Poor")),
              4, line = 1.5, at=c(70,40,25,10), las = 1, cex = 1.5)
      }
    }
    points(1:length(YEARS), locationTS[[i]][,j], pch=19, cex=2)
    # abline(h=60, col=1, lwd=2)
    axis(1, at=1:length(YEARS), YEARS, pos = 0, cex.axis=1.5)
    axis(2, at=seq(0,100,20), seq(0,100,20), pos = 0.5, las = 1,cex.axis=1.5)
    mtext( miSite[i], 3, cex = 1.5)
    
  })
    
    
#### Old style with bar plot and redline, above/below dichotomy  
#   output$plotMMI <- renderPlot({
#     i <- as.numeric(input$mmiSite1)
#     j <- as.numeric(input$mmiIndex1)
#     par(mar=c(3,6,4,1), oma=c(1,1,2,1))
#     # if(j==8){ # Tricos are in the single digits
#     #   barplot(height=locationTS[[i]][,j], names=2015:2022, col="blue", horiz=F,
#     #           ylim=c(0,20),las=1)
#     # } else{
#      p <- barplot(height=locationTS[[i]][,j], names=2015:2022, col="blue", horiz=F,
#               ylim=c(0,100),ylab="Percent of Total Community", las=1, cex.main=1.5,
#               main=ifelse(j==3,
# "Samples with combined % Mayfly, Caddis, Stonefly (% EPT) less
# less than 60 % of the total community are impaired for mountain aquatic life",
# "Samples with Midges exceeding 40% of the total community 
# are impaired for mountain aquatic life"),
#               cex.lab=2, cex.axis=1.5, cex.names=1.5, mgp=c(4, 1, 0))
#       if(j==3){
#         abline(h=60, col="red", lwd=2)
#         cols <- ifelse(locationTS[[i]][,j]<60,2,1)
#         text(x=p,y=locationTS[[i]][,j]+3, labels=paste(round(locationTS[[i]][,j],1),"%"), 
#              cex=1.3, col=cols )
#         
#       } else {
#         abline(h=40, col="red", lwd=2)
#         cols <- ifelse(locationTS[[i]][,j]>40,2,1)
#         text(x=p,y=locationTS[[i]][,j]+3, labels=paste(round(locationTS[[i]][,j],1),"%"), 
#              cex=1.3, col=cols )
#       }
#      
#   })
#   #####
  

  ########### Create data download button ################
  output$BugsDownloadButton <- renderUI ({
    downloadButton('BugsDownload', paste0("Download macroinvertebrate data"), class = "button",
                   tags$head(tags$style(".button{background-color: #2a52be;} .button{color: white;}")),
                   width="50%")

  })
  output$BugsDownload <- downloadHandler(
    filename = function(){
      paste0("UMOWA MI data", Sys.Date(),".csv")
    },
    content = function(con = con){
      write.csv(midat, file = con, row.names = F)
      # data.frame(dat1 = 1:10, dat2 = 2:11)
    }
  )
  ######## End Aquatic Insects ############
  #########################################
  
  ########################################################################
  ## Fish tab
  ### Try all in one plot
  # Still need to add regression lines to fascilitate text
  # Note I was using red (2) and "deepskyblue", but changed to black (1)
  # moderate green rgb(.416,.639,.278)
  output$plotFishAIO <- renderPlot({
    # par(mar=c(5,7,0,2),oma=c(0,0,2,0))
    par(mar=c(4,6,2,1), oma=c(1,1,1,1), mgp=c(4,1,0),cex.lab=1.5,cex.axis=1.25,
        col.axis="grey25", col.lab="grey25")
    # abline(lm(height ~ bodymass))
    plot(Abundance[,1], 1.609*Abundance[,2], pch=15,
         cex=1.75, las=1, xlab="", ylab="Number of catchable size trout per mile", tcl=-.75,
         main="", col=1, ylim=c(0,8000), #lwd.ticks=1.5,
         xaxp=c(1980,2020,4), bty="n")
    abline(lm(1.609*Abundance[,2] ~ Abundance[,1]),col=1,lwd=2)
    abline(lm(1.609*Abundance[,5] ~ Abundance[,1]),
           col=rgb(.416,.639,.278),lwd=2)
     points(Abundance[,1], 1.609*Abundance[,5],
            pch = 17, cex=1.75, lwd = 2, col = rgb(.416,.639,.278))
     abline(lm(1.609*Abundance[,8] ~ Abundance[,1]),
            col=1,lty=2,lwd=2)
     points(Abundance[,1], 1.609*Abundance[,8], mgp=c(4,1,0),
          pch=22, cex=1.75, lwd=2, col=1)
     abline(lm(1.609*Abundance[,11] ~ Abundance[,1]),
            col="dodgerblue",lty=3,lwd=3)
     points(Abundance[,1], 1.609*Abundance[,11],
            pch = 24, cex=1.75, lwd = 2, col = "dodgerblue")
     # axis(1,tcl=-.75, labels=NA, lwd=1.5)
     axis(1,at=1980:nYR, labels=NA)
          # labels=c(1980,rep(NA,9),1990,rep(NA,9),2000,rep(NA,9),
                   # 2010,rep(NA,9),2020,NA,NA),
     mtext("Year",1,3,cex=1.5,col="grey25")
     # mtext("Abundance of Rainbow and Brown Trout at Craig and Cascade",3,.5,
     #       cex=1.6,col="grey25")
     legend("topleft", cex=1.2, text.col="grey25", bty="n",
            legend = c("Rainbows at Craig", "Rainbows at Cascade",
                       "Browns at Craig", "Browns at Cascade"),
            col=c(1,1,rgb(.416,.639,.278),"dodgerblue"), pch=c(15,22,17,24),
            # col=c("deepskyblue","deepskyblue",2,2), pch=c(15,22,17,24),
            bg="transparent")
  })
  #######


 
  ################################################################
  output$HBI1Table <- output$HBI2Table <- renderTable({
    data.frame( "Biotic Index" = c("0.00-3.5", "3.51-4.50", "4.51-5.50", "5.51-6.50", "6.51-7.50", "7.51-8.50", "8.51-10.00"),
                "Water Quality" = c("Excellent", "Very Good", "Good", "Fair", "Fairly Poor", "Poor", "Very Poor"),
                "Degree of Organic Pollution"  = c("No apparent organic pollution","Possible slight organic pollution",
                                                   "Some organic pollution","Fairly significant organic pollution",
                                                   "Significant organic pollution","Very significant organic pollution",
                                                   "Severe organic pollution")
                  )

  })


  ########################################################################

  ##### plotWQ1 
  output$plotWQ1 <- renderPlot({
    i <- as.numeric(input$WQSite1)
    j <- as.numeric(input$WQIndex1)+1

    par(mar=c(4,7,4,1), oma=c(1,1,1,1), mgp=c(5,1,0),cex.lab=1.5,cex.axis=1.5)
    plot(WQ.locationTS[[1]]$Date, WQ.locationTS[[1]]$DisCFS/DisDiv[j-1], xlab = "Time", 
         ylab = WQ.Labels[j], col = "dodgerblue", type="l",lwd=2, las = 1, 
         ylim = WQ.ylim[[j-1]])
    # in next line 1 was i, which made discharge appear/disappear and change with site (so
    # that discharge for cascade showed up for cascade, but the relative change in discharge 
    # is same for Holter or Cascade)
    points(WQ.locationTS[[i]]$Date, WQ.locationTS[[i]][,j+3], pch = 19, cex=2)
    legend("topleft", c(WQ.Labels[j], "Holter Discharge (cfs)"), pch = c(19,NA), lty = c(NA,1), 
           col = c(1,"dodgerblue"), lwd=c(NA,2),cex=1.5, bty="n")

  })


  # output$textWQ1 <- renderUI({
    # if(rv$metric == "TDS"){
    #   p("TDS stands for Total Dissolved Solids and is a common measure of water quality that is related to
    #     conductivity and salinity level. TDS concentration describes the presence of inorganic salts (sodium,
    #     calcium, and magnesium, for example) and small amounts of organic matter (decaying plant
    #     and animal material) in water. The material in TDS can have natural sources, such as geological
    #     condition and seawater, and/or can have sources from human activities, such as domestic/industrial
    #     waste or agriculture.")
    # p("In the figure above you can see TDS concentration measured at most 3 times a year between 2016 and 2021
    #   within the context of
    #   average monthly discharge or flow. The most obvious annual pattern in the discharge data (blue line)
    #   is the pulse of the spring freshet. You can see that TDS (black dots) does not seem to follow an annual trend:
    #   we observed comparatively low TDS values and comparatively high TDS values during the springtime freshet
    #   of each of the 6 years shown. In addition to flow, other factors such as air and water temperature drive natural sources
    #   of TDS as well as agriculture-related TDS.")
    # } else if(rv$location == "LPPC DS"){
    #   p(

    #   )}
    # else {}
  # })


  ### WQPlot3
  # Scatterplots of all data for each attribute
  output$plotWQ3 <- renderPlot({
    i <- as.numeric(input$ExpVar1)
    j <- as.numeric(input$WQIndex3)
    par(mar=c(4,7,4,1), oma=c(1,1,1,1), mgp=c(5,1,0),cex.lab=1.5,cex.axis=1.5)
        # col.axis="grey25", col.lab="grey25")
    if(i==1){
      plot(log(WQ.locationTS[[1]]$DisCFS_lm), WQ.locationTS[[1]][,j+4], xlab = "", ylab = WQ.Labels[j+1],
           las = 1, ylim = WQ.ylim[[j]], pch = 19, col = WQ.colors[1], cex=1.3)
      mtext(paste("How much variability in",WQ.Labels[j+1],"\nis explained by discharge?"),3,.5,
            cex=2)
      abline(lm(dat[,(j+4)]~log(dat$DisCFS_lm)),col=1, lwd=2)
      legend("topleft", legend=paste("R-sq. =",round(summary(lm(dat[,(j+4)]~log(dat$DisCFS_lm)))$r.squared,2)), 
             cex=1.2,bty="n")
      # abline(lm(WQ.locationTS[[1]][,j+4]~WQ.locationTS[[1]]$DisCFS_lm),col = WQ.colors[1])
      for(k in 2:7){
        points(log(WQ.locationTS[[k]]$DisCFS_lm), WQ.locationTS[[k]][,j+4], pch = 19, col = WQ.colors[k],
               cex=1.3)
        # abline(lm(WQ.locationTS[[k]][,j+4]~WQ.locationTS[[k]]$DisCFS_lm),col = WQ.colors[k])
      }
      mtext("ln Discharge (cfs)",1,3,cex=1.5)
      legend("topright", miSite, col = WQ.colors, pch = 19, cex=1.2, bty="n")
    } 
    if(i==2){
      plot(WQ.locationTS[[1]]$Date, WQ.locationTS[[1]][,j+4], xlab = "", ylab = WQ.colNames[j+1],
           las = 1, ylim = WQ.ylim[[j]], pch = 19, col = WQ.colors[1], cex=1.3)
      mtext(paste("How much variability in",WQ.Labels[j+1],"\nis explained by time?"),3,.5,
            cex=2)
      abline(lm(dat[,(j+4)]~dat$Date),col=1, lwd=2)
      legend("topright", legend=paste("R-sq. =",round(summary(lm(dat[,(j+4)]~dat$Date))$r.squared,2)), 
             cex=1.2,bty="n")
      # abline(lm(WQ.locationTS[[1]][,j+4]~WQ.locationTS[[1]]$Date),col = WQ.colors[1])
      for(k in 2:7){
        points(WQ.locationTS[[k]]$Date, WQ.locationTS[[k]][,j+4], pch = 16, col = WQ.colors[k],
               cex=1.3)
        # abline(lm(WQ.locationTS[[k]][,j+4]~WQ.locationTS[[k]]$Date),col = WQ.colors[k])
      }
      mtext("Year",1,3,cex=1.5)
      legend("topleft", miSite, col = WQ.colors, pch = 19, cex=1.2, bty="n")
    }
    if(i==3){
      plot(WQ.locationTS[[1]]$TempC, WQ.locationTS[[1]][,j+4], xlab = "", ylab = WQ.colNames[j+1],
           las = 1, ylim = WQ.ylim[[j]], pch = 19, col = WQ.colors[1], cex=1.3)
      mtext(paste("How much variability in",WQ.Labels[j+1],"\nis explained by water temp?"),3,.5,
            cex=2)
      abline(lm(dat[,(j+4)]~dat$TempC),col=1, lwd=2)
      legend("topright", legend=paste("R-sq. =",round(summary(lm(dat[,(j+4)]~dat$TempC))$r.squared,2)), 
             cex=1.2,bty="n")
      # abline(lm(WQ.locationTS[[1]][,j+4]~WQ.locationTS[[1]]$Date),col = WQ.colors[1])
      for(k in 2:7){
        points(WQ.locationTS[[k]]$TempC, WQ.locationTS[[k]][,j+4], pch = 19, col = WQ.colors[k],
               cex=1.3)
        # abline(lm(WQ.locationTS[[k]][,j+4]~WQ.locationTS[[k]]$Date),col = WQ.colors[k])
      }
      mtext("Water Temperature at Holter (C)",1,3,cex=1.5)
      legend("topleft", miSite, col = WQ.colors, pch = 19, cex=1.2, bty="n")
    }
  })
  
  ########### Create data download button ################
  output$WQDownloadButton <- renderUI ({
    downloadButton('WQDownload', paste0("Download water quality data"), class = "button",
                   tags$head(tags$style(".button{background-color: #2a52be;} .button{color: white;}")),
                   width="50%")
    
  })
  output$WQDownload <- downloadHandler(
    filename = function(){
      paste0("UMOWA WQ data", Sys.Date(),".csv")
    },
    content = function(con = con){
      write.csv(justwqdat, file = con, row.names = F)
      # data.frame(dat1 = 1:10, dat2 = 2:11)
    }
  )

  ### WQPlot4
  # Temp at Holter
  output$plotWQ4 <- renderPlot({
    par(mar=c(4,5,4,1), oma=c(1,1,1,1), mgp=c(3,1,0),cex.lab=1.5,cex.axis=1.5)
    plot(WQ.locationTS[[1]]$Date, WQ.locationTS[[1]][,4]/100,  xlab = "", 
         ylab = expression(Temperature~(C)~and~Discharge~10^-2~(cfs)), type="l", ylim = c(0,160),
         las = 1, lwd=2, col = "white", axes = F)
    abline(h=c(seq(0,140,20),5,10,15), col="grey60")
    lines(WQ.locationTS[[1]]$Date, WQ.locationTS[[1]][,4]/100, lwd = 2.5, col = "dodgerblue")
    lines(WQ.locationTS[[1]]$Date, WQ.locationTS[[1]][,6], lwd = 2, col = 1)
    axis(1, at= as.Date(paste0(YEARS+1,"-01-01"), format = "%Y-%m-%d"),
         labels=paste0("Jan 1\n",YEARS+1), mgp=c(3,3,0))
    axis(2, at=c(seq(0,160,20),5,10,15), labels=c(seq(0,160,20),5,10,15), las = 1)
    legend("topleft", c("Holter Temperature (C)", "Holter Discharge (cfs)"), cex=1.5,lty = 1,
           col = c(1,"dodgerblue"), bty="n", lwd=2)

  })

  
  # ##### plotBugs2 # I did update this to take the full time series through 2022 with all 3 seasons, BTW.
  # output$plotBugs2 <- renderPlot({
  #   j <- as.numeric(input$miIndex2)
  #   k <- as.numeric(input$season)
  # 
  #   par(mar=c(4,5,1,1), oma=c(1,1,2,1))
  #   plotCI( 0:41, seasonLs[[k]][,j], uiw = 1.96*seasonLs[[k]][,j+1],
  #           pch = 16, xaxt = "n", xlab = "", ylab = miIndexName[j], sfrac = 0,
  #           las = 1, axes = F, col = "grey50", mgp=c(4,1,0), cex.lab=1.3,
  #           main=paste(miIndexName[j]," in ",season[k]), cex.main = 1.25)
  #   for(x in 1:7){
  #     segments(x0=(-1+(x-1)*6), y0=apply( matrix(seasonLs[[k]][,j],6,7),
  #                                         2, mean, na.rm = T)[x],
  #              x1=x*6-1, lwd = 2)
  #   }
  #   # points(0:41, seasonLs[[k]][,miIndexCol[j]], pch = 19,
  #   #        col = c( "#8B3800", "#8B8B00", "#008B1C", "#00538B", "#53008B", 1))
  #   abline(v = c(5,11,17,23,29,35),lty = 2)
  #   for(x in 1:7){
  #     axis(1, at = (6*(x-1)):(6*x-2), labels = 2015:2019, mgp = c(3,.5,0),
  #          cex.axis = .75, outer=F)
  #   }
  #   axis(2, pos = -1, las = 1)
  #   # mtext(season[k],3,.25, outer = T)
  #   mtext( milabels, 1, line = 1.75, at = c(2,8,14,20,26,32,38))
  #   mtext(c("LPPC", "LPPC", "Dearborn R.", "Dearborn R."), 1, line = 2.75,
  #         at = c(2,8,20,26))
  # })
  # 

  
} # end Server




