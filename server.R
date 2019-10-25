



# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
  
  observe_helpers(help_dir = "www/help/")
  
  
  rs <- reactiveValues(rs = NULL, 
                      counter = 1, 
                      meteo = meteo)
  
  
  observe({
    
    temp <- plant %>% filter(plant == input$plant)
    
    updateSliderInput(session, "RUE", value = temp$value[temp$param == "RUE"])
    updateSliderInput(session, "RGR", value = temp$value[temp$param == "RGR"])
    updateSliderInput(session, "kl1", value = temp$value[temp$param == "kl1"])
    updateSliderInput(session, "kl2", value = temp$value[temp$param == "kl2"])
    updateSliderInput(session, "kl3", value = temp$value[temp$param == "kl3"])
    
    req(input$soils)
    
    temp <- soils %>% filter(type == input$soils)
    
    print(temp)
    updateSliderInput(session, "initASW1", value = temp$ASW1)
    updateSliderInput(session, "initASW2", value = temp$ASW2)
    updateSliderInput(session, "initASW3", value = temp$ASW3)
    print("Update")
    
  })
  
  
  # RESET THE SIMULATIONS
  observeEvent(input$resetSim, {
    rs$sim <- rs$sim[rs$sim$sim == max(rs$sim$sim),]
  })
  
  observeEvent(input$runSim, {
    
    # PARAMETERS
    RUE <- input$RUE
    TEc <- 9
    RootGrowthRate <- input$RGR #20
    PotentialDLAI <- 0.1
    k <- 0.45
    InitialLAI <- 1.5
    InitialBiomass <- 45
    VPDfrac <- 1
    
    tinit <- 30
    tmax <- 60
    
    
    rs$meteo <- rs$meteo %>% 
      mutate(VPDcalc = 0.75*(svpmax-svpmin)*10*VPDfrac)
    
    soil <- data.frame(param = rep(c("depth", "ll", "ul", "sw", "kl"), 3),
                       layer = rep(c(1:3), each=5), 
                       value = rep(c(300, 50, 100, 100, 0.06), 3)
    )
    
    depth1 <- 300
    depth2 <- 300
    depth3 <- 300
    totDepth <- depth1 + depth2 + depth3
    
    kl1 <- input$kl1
    kl2 <- input$kl2
    kl3 <- input$kl3
    
    sd1 <- 0.5 
    sd2 <- 1.5
    sd3 <- 4
    
    # Initialise dataframe
    sim <- data.frame(das = c(tinit)) %>% 
      mutate(root_depth = ifelse(das * RootGrowthRate < totDepth,
                         das * RootGrowthRate , 
                         totDepth)) %>% 
      mutate(ASW1 = input$initASW1, 
             ASW2 = input$initASW2, 
             ASW3 = input$initASW3) %>% 
      mutate(totASW = ASW1 + ASW2 + ASW3) %>% 
      
      mutate(ps1 = ifelse(root_depth >= depth1,
                          1 * ASW1 * kl1, 
                          (root_depth / depth1) * ASW1 * kl1)) %>% 
      mutate(ps2 = ifelse(root_depth <= depth1,
                          0, 
                          ifelse(root_depth > depth1+depth2,
                              1 * ASW2 * kl2, 
                              ((root_depth-depth1) / depth2) * ASW2 * kl2))) %>% 
      mutate(ps3 = ifelse(root_depth <= depth1+depth2, 
                          0, 
                          ((root_depth - depth1 - depth2)/depth3) * ASW3 * kl3)) %>% 
      mutate(potsupply = ps1 + ps2 + ps3) %>% 
      mutate(lai = InitialLAI) %>% 
      mutate(li = 1 - exp(-k*lai)) %>% 
      mutate(potdemand = rs$meteo$Radn[rs$meteo$DAS == das] * li * RUE / 
               (TEc / (rs$meteo$VPDcalc[rs$meteo$DAS == das]/10))) %>% 
      mutate(sd = potsupply / potdemand) %>% 
      mutate(leafexpeffect = ifelse( sd <= sd1, 
                           0, 
                           ifelse(sd > sd2,
                                  1, 
                                  (sd - sd1)/(sd2-sd1)))) %>% 
      mutate(Dlai = leafexpeffect * PotentialDLAI) %>% 
      mutate(transpiration = min(potdemand, potsupply)) %>% 
      mutate(SWaterUse = transpiration) %>% 
      mutate(BioWater = potsupply * TEc / (rs$meteo$VPDcalc[rs$meteo$DAS == das]/10)) %>% 
      mutate(BioLight = rs$meteo$Radn[rs$meteo$DAS == das] * li * RUE) %>% 
      mutate(DBiomass = ifelse(sd > 1, BioLight, BioWater)) %>% 
      mutate(biomass = DBiomass + InitialBiomass)
      
      
    # Update dataframe 
    
    rewater <- input$rewater + tinit
    for(i in c((tinit+1):(tmax))){

      tempP <- sim[sim$das == i-1,]
      temp <- tempP %>% mutate(das = i)
      
      temp$ASW1 <- tempP$ASW1-(tempP$ps1/tempP$potsupply)*tempP$transpiration
      temp$ASW2 <- tempP$ASW2-(tempP$ps2/tempP$potsupply)*tempP$transpiration
      temp$ASW3 <- tempP$ASW3-(tempP$ps3/tempP$potsupply)*tempP$transpiration
      
      if(i == rewater){
        temp$ASW1 <- soils$ASW1[soils$type == "wet"]
        temp$ASW2 <- soils$ASW2[soils$type == "wet"]
        temp$ASW3 <- soils$ASW3[soils$type == "wet"]
        rewater = rewater + input$rewater
        print(paste0("rewater ",rewater))
      }
      
      temp <- temp %>%
        mutate(root_depth = ifelse(das * RootGrowthRate < totDepth,
                                   das * RootGrowthRate , 
                                   totDepth)) %>% 
        mutate(totASW = ASW1 + ASW2 + ASW3) %>% 
        mutate(ps1 = ifelse(root_depth >= depth1,
                            1 * ASW1 * kl1, 
                            (root_depth / depth1) * ASW1 * kl1)) %>% 
        mutate(ps2 = ifelse(root_depth <= depth1,
                            0, 
                            ifelse(root_depth > depth1+depth2,
                                   1 * ASW2 * kl2, 
                                   ((root_depth-depth1) / depth2) * ASW2 * kl2))) %>% 
        mutate(ps3 = ifelse(root_depth <= depth1+depth2, 
                            0, 
                            ((root_depth - depth1 - depth2)/depth3) * ASW3 * kl3)) %>% 
        mutate(potsupply = ps1 + ps2 + ps3) %>% 
        mutate(lai = tempP$lai + tempP$Dlai) %>% 
        mutate(li = 1 - exp(-k*lai)) %>% 
        mutate(potdemand = rs$meteo$Radn[rs$meteo$DAS == das] * li * RUE / 
                 (TEc / (rs$meteo$VPDcalc[rs$meteo$DAS == das]/10))) %>% 
        mutate(sd = potsupply / potdemand) %>% 
        mutate(leafexpeffect = ifelse( sd <= sd1, 
                                       0, 
                                       ifelse(sd > sd2,
                                              1, 
                                              (sd - sd1)/(sd2-sd1)))) %>% 
        mutate(Dlai = leafexpeffect * PotentialDLAI) %>% 
        mutate(transpiration = min(potdemand, potsupply)) %>% 
        mutate(SWaterUse = transpiration + tempP$SWaterUse) %>% 
        mutate(BioWater = potsupply * TEc / (rs$meteo$VPDcalc[rs$meteo$DAS == das]/10)) %>% 
        mutate(BioLight = rs$meteo$Radn[rs$meteo$DAS == das] * li * RUE) %>% 
        mutate(DBiomass = ifelse(sd > 1, BioLight, BioWater)) %>% 
        mutate(biomass = DBiomass + tempP$biomass)
      
      sim <- rbind(sim, temp)
    }
    
    sim$sim <- rs$counter
    rs$counter <- rs$counter+1
    
    
    rs$sim <- rbind(rs$sim, sim)
    
  })
  
  
  
  
  
  
  
  output$plantPlot <- renderPlot({
    
    req(rs$sim)
    
    if(input$plot_plant == "biomass"){
    
      pl <- ggplot(rs$sim, aes(das, biomass, colour=factor(sim))) + 
        geom_line() + 
        geom_point()  +
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Total biomass [g]") + 
        ggtitle("Total biomass [g]")
    
    } else if(input$plot_plant == "lai"){
      
      pl <- ggplot(rs$sim, aes(das, lai, colour=factor(sim))) + 
        geom_line() + 
        geom_point()  +
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Leaf Area Index [-]") + 
        ggtitle("Leaf Area Index [-]")
      
    } else if(input$plot_plant == "sd"){
      
      pl <- ggplot(rs$sim, aes(das, sd, colour=factor(sim))) + 
        geom_line() + 
        geom_point()  +
        scale_colour_manual(values=cbPalette)+
        geom_hline(yintercept = 1, lty=2) +
        xlab("Time [DAS]") + ylab("Supply/Demand ratio [-]") + 
        ggtitle("Supply/Demand ratio [-]")
      
    }else if(input$plot_plant == "root_depth"){
      
      pl <- ggplot(rs$sim, aes(das, -root_depth, colour=factor(sim))) + 
        geom_line() + 
        geom_point()  +
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Root depth [mm]") + 
        ggtitle("Root depth [mm]")
      
    }else if(input$plot_plant == "transpiration"){
      
      pl <- ggplot(rs$sim, aes(das, transpiration, colour=factor(sim))) + 
        geom_line() + 
        geom_point()  +
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Transpiration [mm]") + 
        ggtitle("Transpiration [mm]")
      
    }
    
    
    pl
    
  })
  
  
  output$enviPlot <- renderPlot({
    
    req(rs$sim)
    
    if(input$plot_envi == "rad"){
      
      pl <- ggplot(rs$meteo, aes(DAS, Radn)) + 
        geom_line() + 
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Radiation [MJ/m2]") + 
        ggtitle("Radiation [MJ/m2]")
      
    }else if(input$plot_envi == "vpd"){
      
      print(rs$meteo)
      
      pl <- ggplot(rs$meteo, aes(DAS, VPDcalc)) + 
        geom_line() + 
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("VPD [hPa]") + 
        ggtitle("Vapour Pressure Deficit [hPa]")
      
    }
    
    pl
    
  })
  
  
  output$corrPlot <- renderPlot({
    
    # temp <- rs$sim %>% 
    #   mutate(x = input$xval,
    #          y = input$yval)
    
    ggplot(rs$sim, aes_string(input$xval, input$yval)) + 
      geom_point(aes(colour = factor(sim))) +
      geom_smooth(aes(colour = factor(sim)),  method="lm", se=F)
    
    
  })
  
  
  output$soilPlot <- renderPlot({
    
    req(rs$sim)
    
    if(input$plot_soil == "swc"){
    
      temp <- rs$sim %>% 
        select(sim, das, ASW1, ASW2, ASW3) %>% 
        gather("layer", "value", -c(das, sim))
      
      pl <- ggplot(temp, aes(das, value, colour=factor(sim), lty=layer)) + 
        geom_line() + 
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Soil water content [%]") + 
        ggtitle("Soil water content [%]")
      
    }else if(input$plot_soil == "tswc"){
      pl <- ggplot(rs$sim, aes(das, totASW, colour=factor(sim))) + 
        geom_line() + 
        geom_point() + 
        scale_colour_manual(values=cbPalette)+
        xlab("Time [DAS]") + ylab("Soil water content [%]") + 
        ggtitle("Soil water content [%]")
      
    }
    
    pl
    
  })
  
  
  
  
  ##------ DOWNLOADS
  
  output$download_current <- downloadHandler(
    filename = function() {
      "current_sim.csv"
    },
    content = function(file) {
      if(is.null(rs$sim)){return ()}
      write.csv(rs$sim[rs$sim$sim == max(rs$sim$sim),], file = file)
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      "all_sim.csv"
    },
    content = function(file) {
      if(is.null(rs$sim)){return ()}
      write.csv(rs$sim, file = file)
    }
  )
  
  
  
}