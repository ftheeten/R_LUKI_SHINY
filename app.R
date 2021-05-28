#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(gridExtra)



phenoluki<-NULL
main_file="C:\\PROJECTS_R\\ATLAS_LUKI\\big_pheno.txt"
plot_flo<-NULL

op<-NULL

init<-function(path)
{
    print("LOAD")
    phenoluki<<- data.table(read.csv2(path, h=TRUE, sep=";"))
    phenoluki$def<-as.numeric(as.character(phenoluki$def))
    phenoluki$flo<-as.numeric(as.character(phenoluki$flo))
    phenoluki$fru<-as.numeric(as.character(phenoluki$fru))
    phenoluki$diss<-as.numeric(as.character(phenoluki$diss))
    print("Loaded")
   
}


init_chart<-function(species)
{
    plot_flo<<-NULL
    #print(species)
    s<-species
    tmp <- phenoluki[sps == s,]
    #View(tmp)
    cp<-tmp %>% dplyr::group_by(sps, mth) %>% count(flo) %>% tidyr::spread(key=flo, value=n)
    if("1" %in% colnames(cp))
    {
      #View(cp)
      colnames(cp)<-c('sps', 'mth', 'absent', 'present', 'none')
      cp[is.na(cp)]<-0
      cp$mth<-as.factor(cp$mth)
      cp$total=cp$present+ cp$absent+ cp$none
      cp$ratio<-cp$present/cp$total
    
    
      plot_flo<<-ggplot(cp, aes(x=mth, y=ratio))+ geom_bar(stat='identity') + coord_polar()
    
    }
}

get_species<-function()
{
    print("select")
    tmp<-unique(phenoluki$sps)
    
    sort(tmp)
}    

init(main_file)
    
# Define UI for application that draws a histogram
ui <- fluidPage(
   
    # Application title
    titlePanel("Phenology Luki (floraison)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "selectedSpecies",
                        label = "Select a species", 
                        choices =get_species())
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotflo"),
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plotflo <- renderPlot({
      init_chart(input$selectedSpecies)
      plot_flo
      
      #par(op)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
