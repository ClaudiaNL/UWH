library(shinydashboard)
library(tidyr)
library(tidygeocoder)
library("tidyverse")
library("readxl")
library("ggplot2")
library("shiny")

ui <- dashboardPage(
  dashboardHeader(title = "Bewerbungen UWH"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 200)),
                box(radioButtons(inputId = "geschlecht_slider", label = "Geschlecht", choices = c("m", "w", "NA")), height=220),
                  #sliderInput("slider2", "Number of observations:", 1, 100, 50)
                box(plotOutput("plot2", height = 200)),
                box(plotOutput("plot3", height = 200)),
                box(plotOutput("plot4", height = 200)),
                box(plotOutput("plot5", height = 200)),
                box(plotOutput("plot6", height = 200)),
                box(plotOutput("plot7", height = 200)),
                box(plotOutput("plot8", height = 200)),
                box(plotOutput("plot9", height = 200)
                )
              
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)
  server <- function (input, output) {
# Output plot 1 - begin
  output$plot1 <- renderPlot({
    
    BewerbungenGeschlechtJahr <- BewerMerge %>% count(geschlecht, jahr) %>% select("jahr", "geschlecht", "n") 
    View(BewerbungenGeschlechtJahr)
      BewerbungenGeschlechtJahr2 <- filter(BewerbungenGeschlechtJahr, geschlecht==input$geschlecht_slider)  
      View(BewerbungenGeschlechtJahr2)
      
      if(input$geschlecht_slider == 'm'){ color <- "Blue" }
      else if(input$geschlecht_slider == 'w'){ color <- "Pink" }
      else{ color <- "Purple" }
      
      BewerbungenGeschlJahrPlot <- ggplot(data=BewerbungenGeschlechtJahr2, aes(x=jahr, y=n, fill=geschlecht)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() + scale_fill_manual(values=c(color)) + labs(title="Bewerbungen nach Geschlechtern", x="Semester", y = "Anz. Bewerbungen")
      BewerbungenGeschlJahrPlot
    
  })
  # Output plot 1 - end
  
  # Plot 2 - begin
  #Analyse Bewerb pro Jahr
  output$plot2 <- renderPlot({
    
    BewerbungenJahr <- BewerMerge %>% count(jahr)
    View(BewerbungenJahr)
    BewerbungsnJahrPlot <-ggplot(data=BewerbungenJahr, aes(x=jahr, y=n)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+
      labs(title="Bewerbungen pro Jahr gesamt", 
           x="Jahr", y = "Anz. Bewerbungen")
    BewerbungsnJahrPlot
  })
  # Plot 2 - end
  
  # Plot 3 - begin
  output$plot3 <- renderPlot({
    #Analyse PLZ pro Jahr
    BewerbungenPLZJahr <- BewerMerge %>% select(jahr,plz)
    View(BewerbungenPLZJahr)
    
    BewerbungenJahrMABA <- BewerMerge %>% count(jahr, master_bachelor)
    View(BewerbungenJahrMABA)
    BewerbungsJahrMABAPlot <-ggplot(data=BewerbungenJahrMABA, aes(x=jahr, y=n, fill=master_bachelor)) +
      geom_bar(stat="identity", position=position_dodge())+
      labs(title="Bewerbungen pro Jahr nach MA/BA", 
           x="Jahr", y = "Anz. Bewerbungen") +
      scale_fill_manual(values=c('black','steelblue'))+
      geom_text(aes(label=n), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      theme_classic()
    BewerbungsJahrMABAPlot
  })  
  # Plot 3 - end
  
  # Plot 4 - begin
  output$plot4 <- renderPlot({
    #Analyse Bewerb pro Semester
    BewerbungenSemester <- BewerMerge %>% count(jahr, semester) %>% unite("jahrsemester", jahr:semester, remove = FALSE)
    View(BewerbungenSemester)
    BewerbungsnSemesterPlot <-ggplot(data=BewerbungenSemester, aes(x=jahrsemester, semester, y=n)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme_minimal()+
      geom_text(aes(label=n), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Bewerbungen pro Semester gesamt", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungsnSemesterPlot
    
  })  
  # Plot 4 - end
  
  # Plot 5 - begin
  output$plot5 <- renderPlot({
    #Analyse Bewerb pro Semester pro Studiengang
    BewerbungenSemStud <- BewerMerge %>% count(jahr, semester, studiengang) %>% 
      unite("jahrsemester", jahr:semester, remove = FALSE) %>% 
      arrange(.,jahrsemester)
    View(BewerbungenSemStud)
    BewerbungenSemStudPlot <- ggplot(data=BewerbungenSemStud, aes(x=jahrsemester, y=n, fill=studiengang)) +
      geom_bar(stat="identity") +
      # geom_text(aes(y=, label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      labs(title="Bewerbungen gesamt nach Studieng‰ngen", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungenSemStudPlot
    
  })  
  # Plot 5 - end
  
  # Plot 6 - begin
  output$plot6 <- renderPlot({
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtJahr <- BewerMerge %>% count(geschlecht, jahr) %>% select("jahr", "geschlecht", "n")
    View(BewerbungenGeschlechtJahr)
    BewerbungenGeschlJahrPlot <- ggplot(data=BewerbungenGeschlechtJahr, aes(x=jahr, y=n, fill=geschlecht)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungenGeschlJahrPlot
  })  
  # Plot 6 - end
  
  # Plot 7 - begin
  output$plot7 <- renderPlot({
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtJahrAnteil <- BewerbungenGeschlechtJahr %>%
      arrange(jahr, geschlecht) %>%
      group_by(jahr)  %>%
      #summarise (n=n()) %>%
      mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
    # mutate (anteil = n/sum(n))   # %>%
    #select("jahr", "geschlecht", "n", "anteil")
    View(BewerbungenGeschlechtJahrAnteil)
    BewerbungenGeschlJahrAnteilPlot <- ggplot(data=BewerbungenGeschlechtJahrAnteil, aes(x=jahr, y=rel.freq, fill=geschlecht)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungenGeschlJahrAnteilPlot
    
  })  
  # Plot 7 - end
  
  # Plot 8 - begin
  output$plot8 <- renderPlot({
    #Analyse Geschlechterverteilung pro Studiengang
    BewerbungenGeschlechtStudiengang <- BewerMerge %>% count(geschlecht, studiengang)
    View(BewerbungenGeschlechtStudiengang)
    BewerbungenGeschlStudgPlot <- ggplot(data=BewerbungenGeschlechtStudiengang, aes(x=studiengang, y=n, fill=geschlecht)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungenGeschlStudgPlot
    
  })  
  # Plot 8 - end
  
  # Plot 9 - begin
  output$plot9 <- renderPlot({
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtStudiengangAnteil <- BewerbungenGeschlechtStudiengang %>%
      arrange(studiengang, geschlecht) %>%
      group_by(studiengang)  %>%
      #summarise (n=n()) %>%
      mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
    # mutate (anteil = n/sum(n))   # %>%
    #select("jahr", "geschlecht", "n", "anteil")
    View(BewerbungenGeschlechtStudiengangAnteil)
    BewerbungenGeschlStudAnteilPlot <- ggplot(data=BewerbungenGeschlechtStudiengangAnteil, aes(x=studiengang, y=rel.freq, fill=geschlecht)) +
      geom_bar(stat="identity") +
      #apply(data, 2, function(x){x*100/sum(x,na.rm=T)})+
      geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungenGeschlStudAnteilPlot
  })  
  # Plot 9 - end
  

}
shinyApp(ui, server)

