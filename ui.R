library(shinydashboard)

ui <- dashboardPage(skin = "black", 
    dashboardHeader(title = "SimpleCropBot"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Model", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("th"))
        )
        
    ),
    dashboardBody(
        
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                fluidRow(
                    column(width = 6,
                           
                       box(
                           status = "success",  width = NULL,solidHeader = TRUE, title = "Command center",
                           
                           actionButton(inputId = "runSim", label="Run model", icon("rocket"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           
                           actionButton(inputId = "resetSim", label="Reset simulations", icon("warning"), style="color: #fff; background-color: #f38f18; border-color: #bc6f14"),
                           
                           tags$hr(),
                           downloadButton("download_current", "Current simulation"),
                           downloadButton("download_all", "All simulations")
                           
                       ),
                       
                       
                       tabBox(
                           # Title can include an icon
                           title = tagList(shiny::icon("gear"), "Parameters"), width = NULL,
                           tabPanel("Plant",
                                    
                                helper(selectInput("plant", "Plant species", unique(plant$plant)), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "PLANT", size = "m"),   
                                
                                bsCollapse(multiple = FALSE, open = "col3", id = "collapse3",
                                           
                                    bsCollapsePanel("Shoot",
                                    
                                        helper(sliderInput("RUE", "Radiation Use Efficiency [g/MJ]", 0.1, 4, 1.6, step = 0.05), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "RUE", size = "m")
                                    ), 
                                    bsCollapsePanel("Root",
                                                    
                                        helper(sliderInput("RGR", "Root growth Rate [mm/day]", 1, 50, 20, step = 1), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "RGR", size = "m"),
                                        
                                        helper(sliderInput("kl1", "Extraction rate - layer 1 [mm/day]", 0.01, 0.1, 0.06, step = 0.01), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "KL", size = "m"),
                                        
                                        helper(sliderInput("kl2", "Extraction rate - layer 2 [mm/day]", 0.01, 0.1, 0.05, step = 0.01), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "KL", size = "m"),
                                        
                                        helper(sliderInput("kl3", "Extraction rate - layer 3 [mm/day]", 0.01, 0.1, 0.05, step = 0.01), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "KL", size = "m")
                                    )
                                )
  
                           ), 
                           
                           tabPanel("Environment",
                                    
                                    helper(selectInput("soils", "Soil type", unique(soils$type)), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "SOIL", size = "m"),
                                    
                                    bsCollapse(multiple = FALSE, open = "col3", id = "collapse3",
                                               
                                       bsCollapsePanel("Soil water content",
                                        helper(sliderInput("initASW1", "Initial soil water content - layer 1 [%]", 1, 50, 40, step = 1), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "initASW", size = "m"),
                                    
                                        helper(sliderInput("initASW2", "Initial soil water content - layer 2 [%]", 1, 50, 40, step = 1), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "initASW", size = "m"),
                                
                                        helper(sliderInput("initASW3", "Initial soil water content - layer 3 [%]", 1, 50, 40, step = 1), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "initASW", size = "m")
                                       )
                                    ),
                                    
                                    helper(sliderInput("rewater", "Rewatering frequence [days]", 1, 100, 100), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "REWATER", size = "m")
                       )
                       )
                       
                    ),
                    column(width = 6, 
                       tabBox(
                           # Title can include an icon
                           title = tagList(shiny::icon("leaf"), "Results"),width = NULL,
                           tabPanel("Plant",
                                selectInput("plot_plant", "Variable to plot", multiple = F,  
                                        choices = c("Biomass [g]" = "biomass", 
                                            "Root depth [mm]" = "root_depth",
                                            "LAI [-]" = "lai",
                                            'Transpiration [mm]' = "transpiration",
                                            "Supply/Demand [-]" = "sd"), selected = 1),
                                plotOutput("plantPlot")
                           ), 
                           tabPanel("Soil", 
                                    selectInput("plot_soil", "Variable to plot", multiple = F,  
                                                    choices = c("Soil water content [%]" = "swc",
                                                                "Total soil water content [%]" = "tswc"), selected = "tswc"),
                                    plotOutput("soilPlot")
                           ),
                           
                           tabPanel("Environment",
                                    selectInput("plot_envi", "Variable to plot", multiple = F,  
                                                    choices = c("Radiation [MJ/m2]" = "rad",
                                                                "Vapour Pressure Deficit [hPa]" = "vpd"), selected = 1),
                                    plotOutput("enviPlot")
                           ), 
                           
                           tabPanel("Correlations",
                                    
                                        
                                selectInput("xval", "X variable", 
                                            c("Biomass [g]" = "biomass", 
                                              "LAI [-]" = "lai", 
                                              "Root depth [mm]" = "root_depth",
                                              "Transpiration [mm]" = "transpiration", 
                                              "Water use [mm]" = "SWaterUse"), 
                                            selected='lai'),
                                selectInput("yval", "Y variable", 
                                            c("Biomass [g]" = "biomass", 
                                              "LAI [-]" = "lai", 
                                              "Root depth [mm]" = "root_depth",
                                              "Transpiration [mm]" = "transpiration", 
                                              "Water use [mm]" = "SWaterUse"), 
                                            selected='root_depth'),
                                plotOutput("corrPlot")
                           )
                       )
                    )
                )
            ),
            
            # Second tab content
            tabItem(tabName = "about",
                    fluidRow(
                        box(
                            title = "About the model", solidHeader = TRUE, width = 6, status = "primary",
                            helpText("SimpleCropBot is simple crop model that has been developed to support teaching. The original model dynamics were created by Xavier Draye an Graeme Hammer. The web application was developped by Guillaume Lobet"),
                            tags$hr(),
                            helpText("The code of this web app is open source and is freely available. "),
                            actionButton(inputId='ab1', label="Source code", icon = icon("th"), onclick ="window.open('https://github.com/plantmodelling/simplecropbot', '_blank')")
                        ),
                        
                        
                        box(
                            title = "How to cite SimpleCropBot",  solidHeader = TRUE, width = 6, status = "warning",
                            tags$strong("A sunflower simulation model: I. Model development, Agronomy Journal, 85:725-735 (1993)"),
                            helpText("Chapman, S.C., Hammer, G.L., and Meinke, H."),
                            actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('https://doi.org/10.1016/j.envsoft.2014.07.009', '_blank')")

                        ),
                        
                        box(
                            title = "MIT Licence",
                            helpText("SimpleCropBot is released under a Apache licence."),
                            helpText("
Copyright (c) 2019 Forschungszentrum Juelich
Copyright (c) 2019 UCLouvain

Licensed under the Apache License, Version 2.0 (the 'License'');
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an 'AS IS' BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.")
                        ),
                        
                        box(
                            title =  "Disclaimer",
                            helpText("This software is provided by the copyright holders and contributors 'as is' and any express or implied warranties, including, but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In no event shall the copyright holder or contributors be liable for any direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if advised of the possibility of such damage.")
                        )
                        
                        
                    )
                )
        )
    )
)