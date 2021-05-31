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
library(data.table)
library(ggplot2)
library(gridExtra)



phenoluki<-NULL
main_file=paste0(getwd(),"/big_pheno.txt")
plot_flo<-NULL
plot_def<-NULL
plot_fru<-NULL
plot_diss<-NULL
flag_display<-FALSE

op<-NULL

init<-function(path)
{
  #print("LOAD")
  phenoluki<<- data.table(read.csv2(path, h=TRUE, sep=";"))
  phenoluki$def<-as.numeric(as.character(phenoluki$def))
  phenoluki$flo<-as.numeric(as.character(phenoluki$flo))
  phenoluki$fru<-as.numeric(as.character(phenoluki$fru))
  phenoluki$diss<-as.numeric(as.character(phenoluki$diss))
  #print("Loaded")
  
}

generate_plot<-function(cp, title)
{
  returned<-NULL
  flag_display<<-FALSE
  if("1" %in% colnames(cp))
  {
    #View(cp)
    colnames(cp)<-c('sps', 'mth', 'absent', 'present', 'none')
    cp[is.na(cp)]<-0
    cp$mth<-as.factor(cp$mth)
    cp$total=cp$present+ cp$absent+ cp$none
    cp$ratio<-cp$present/cp$total
    nb_obs=sum(cp$total)/nrow(cp)
    
    returned<-ggplot(cp, aes(x=mth, y=ratio, fill=mth))+ geom_bar(stat='identity') + coord_polar() + ggtitle(paste0(title," ", nb_obs, " units" )) + scale_x_discrete(labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
    flag_display<<-TRUE
  }
  returned
}

init_chart<-function(species)
{
  plot_flo<<-NULL
  plot_def<<-NULL
  plot_fru<<-NULL
  plot_diss<<-NULL
  #print(species)
  s<-species
  tmp <- phenoluki[sps == s,]
  tmp_grp<- tmp %>% dplyr::group_by(sps, mth)
  #View(tmp)
  cp_flo <- tmp_grp  %>% count(flo) %>% tidyr::spread(key=flo, value=n)
  cp_def <- tmp_grp %>% count(def) %>% tidyr::spread(key=def, value=n)
  cp_fru <- tmp_grp %>% count(fru) %>% tidyr::spread(key=fru, value=n)
  cp_diss <- tmp_grp %>% count(diss) %>% tidyr::spread(key=diss, value=n)
  plot_flo<<-generate_plot(cp_flo, "Floraison")
  plot_fru<<-generate_plot(cp_fru, "Fructification")
  plot_def<<-generate_plot(cp_def, "Défloraison")
  plot_diss<<-generate_plot(cp_diss, "Dissémination")
}

get_species<-function()
{
  #print("select")
  tmp<-unique(phenoluki$sps)
  
  sort(tmp)
}    

init(main_file)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Phenology Luki (floraison)"),
  
  # Sidebar with a slider input for number of bins 
  verticalLayout(
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
    if(flag_display)
    {
      grid.arrange(grobs=list(plot_flo,plot_fru,plot_def,plot_diss), ncol=2)
    }
    #par(op)
  }, height = 1000, width = 1000 )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

