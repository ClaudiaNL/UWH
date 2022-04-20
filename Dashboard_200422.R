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
      menuItem("Barchar", tabName = "Barchar", icon = icon("dashboard")),
      menuItem("Geomline", tabName = "Geomline", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Barchar",
              h3("Dashboard - Barchar"),
              fluidRow(
                #box(selectInput(inputId = "jahr_slider",label="Jahr",choices = c("Alle",2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011))),
                box(radioButtons(inputId = "geschlecht_slider", label = "Geschlecht", choices = c("Alle","m", "w", "NA")), height=220),
                box(plotOutput("plot1", height = 200)),
                #sliderInput("slider2", "Number of observations:", 1, 100, 50)
                box(plotOutput("plot2", height = 200)),
                box(plotOutput("plot3", height = 200)),
                box(plotOutput("plot4", height = 200)),
                box(plotOutput("plot6", height = 200)),
                box(plotOutput("plot7", height = 200)),
                box(selectInput(inputId = "studium_slider",label="Art des Studiums",choices = c("Alle","Bachelor","Master")),
                    plotOutput("plot9", height = 200)),
                box(selectInput(inputId = "studiengang_slider",label="Studiengang",choices = c("Alle","GM","Man","PPE","PPÖ","SO")),
                    plotOutput("plot5", height = 200)
                )
                
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "Geomline",
              h3("Dashboard - GeomLine"),
              box(radioButtons(inputId = "geschlecht_slider2", label = "Geschlecht", choices = c("Alle","m", "w", "NA")),
              plotOutput("geomline_1", height = 400, width = 600),width=600, height = 600),
       
              box(selectInput(inputId = "jahr_slider",label="Jahr",choices = c("Alle","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011")),
              plotOutput("geomline_2", height = 400, width = 600),width=600, height = 600)
      )
    )
  )
)
server <- function (input, output) {
  # Output plot 1 - begin
  output$plot1 <- renderPlot({
    
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtJahr <- BewerMerge %>% count(geschlecht, jahr) %>% select("jahr", "geschlecht", "n")
    BewerbungenGeschlechtJahr2 <- filter(BewerbungenGeschlechtJahr, geschlecht==input$geschlecht_slider)  
    View(BewerbungenGeschlechtJahr2)
    # View(BewerbungenGeschlechtJahr)
    if(input$geschlecht_slider=="Alle"){
      BewerbungenGeschlJahrPlot <- ggplot(data=BewerbungenGeschlechtJahr, aes(x=jahr, y=n, fill=geschlecht))+  geom_bar(stat="identity") +
      geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenGeschlJahrPlot <- ggplot(data=BewerbungenGeschlechtJahr2, aes(x=jahr, y=n, fill=geschlecht)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
      theme_minimal() +
      scale_fill_manual(values=c('steelblue','pink', 'purple')) +
      labs(title="Bewerbungen nach Geschlechtern", 
           x="Semester", y = "Anz. Bewerbungen")
    }
        
    BewerbungenGeschlJahrPlot
    
  })
  # Output plot 1 - end
  

  # Plot nuevo 2 - begin
  output$plot2 <- renderPlot({
    #Analyse Geschlechterverteilung pro Studiengang
    BewerbungenGeschlechtStudiengang <- BewerMerge %>% count(geschlecht, studiengang)
    BewerbungenGeschlechtStudiengang2 <- filter(BewerbungenGeschlechtStudiengang, geschlecht==input$geschlecht_slider)
    #View(BewerbungenGeschlechtStudiengang)
    if(input$geschlecht_slider=="Alle"){
      BewerbungenGeschlStudgPlot <- ggplot(data=BewerbungenGeschlechtStudiengang, aes(x=studiengang, y=n, fill=geschlecht)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenGeschlStudgPlot <- ggplot(data=BewerbungenGeschlechtStudiengang2, aes(x=studiengang, y=n, fill=geschlecht)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    BewerbungenGeschlStudgPlot
    
  })  
  # Plot nuevo 2 - end
  
  # Plot nuevo 3 - begin
  output$plot3 <- renderPlot({
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtStudiengangAnteil <- BewerbungenGeschlechtStudiengang %>%
      arrange(studiengang, geschlecht) %>%
      group_by(studiengang)  %>%
      #summarise (n=n()) %>%
      mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
    # mutate (anteil = n/sum(n))   # %>%
    #select("jahr", "geschlecht", "n", "anteil")
    #View(BewerbungenGeschlechtStudiengangAnteil)
    
    BewerbungenGeschlechtStudiengangAnteil2 <- filter(BewerbungenGeschlechtStudiengangAnteil, geschlecht==input$geschlecht_slider)
    
    if(input$geschlecht_slider=="Alle"){
      BewerbungenGeschlStudAnteilPlot <- ggplot(data=BewerbungenGeschlechtStudiengangAnteil, aes(x=studiengang, y=rel.freq, fill=geschlecht)) +
        geom_bar(stat="identity") +
        #apply(data, 2, function(x){x*100/sum(x,na.rm=T)})+
        geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenGeschlStudAnteilPlot <- ggplot(data=BewerbungenGeschlechtStudiengangAnteil2, aes(x=studiengang, y=rel.freq, fill=geschlecht)) +
        geom_bar(stat="identity") +
        #apply(data, 2, function(x){x*100/sum(x,na.rm=T)})+
        geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    BewerbungenGeschlStudAnteilPlot
  })  
  # Plot nuevo3 - end
  
  # Plot nuevo 4- begin
  output$plot4 <- renderPlot({
    #Analyse Geschlechterverteilung pro Jahr
    BewerbungenGeschlechtJahrAnteil <- BewerbungenGeschlechtJahr %>%
      arrange(jahr, geschlecht) %>%
      group_by(jahr)  %>%
      #summarise (n=n()) %>%
      mutate(rel.freq = paste0(round(100 * n/sum(n), 1), "%"))  #%>%
    # mutate (anteil = n/sum(n))   # %>%
    #select("jahr", "geschlecht", "n", "anteil")
    # View(BewerbungenGeschlechtJahrAnteil)
    
    BewerbungenGeschlechtJahrAnteil2 <- filter(BewerbungenGeschlechtJahrAnteil, geschlecht==input$geschlecht_slider)
    
    if(input$geschlecht_slider=="Alle"){
      BewerbungenGeschlJahrAnteilPlot <- ggplot(data=BewerbungenGeschlechtJahrAnteil, aes(x=jahr, y=rel.freq, fill=geschlecht)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenGeschlJahrAnteilPlot <- ggplot(data=BewerbungenGeschlechtJahrAnteil2, aes(x=jahr, y=rel.freq, fill=geschlecht)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=rel.freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        scale_fill_manual(values=c('steelblue','pink', 'purple')) +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Semester", y = "Anz. Bewerbungen") 
    }
    
    BewerbungenGeschlJahrAnteilPlot
    
  })  
  # Plot nuevo 4 - end
  
  # Plot 6 - begin
  #Analyse Bewerb pro Jahr
  output$plot6 <- renderPlot({
    
    BewerbungenJahr <- BewerMerge %>% count(jahr)
    # View(BewerbungenJahr)
  
      BewerbungsnJahrPlot <-ggplot(data=BewerbungenJahr, aes(x=jahr, y=n)) +
        geom_bar(stat="identity", fill="steelblue") +
        geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
        theme_minimal()+
        labs(title="Bewerbungen pro Jahr gesamt", 
             x="Jahr", y = "Anz. Bewerbungen")
    
    BewerbungsnJahrPlot
  })
  # Plot 6 - end
  
  # Plot 9 - begin
  output$plot9 <- renderPlot({
    #Analyse PLZ pro Jahr
    BewerbungenPLZJahr <- BewerMerge %>% select(jahr,plz)
    # View(BewerbungenPLZJahr)
    
    BewerbungenJahrMABA <- BewerMerge %>% count(jahr, master_bachelor)
    BewerbungenJahrMABA2 <- filter(BewerbungenJahrMABA, master_bachelor==input$studium_slider)
    # View(BewerbungenJahrMABA)
    if(input$studium_slider=="Alle"){
      BewerbungsJahrMABAPlot <-ggplot(data=BewerbungenJahrMABA, aes(x=jahr, y=n, fill=master_bachelor)) +
        geom_bar(stat="identity", position=position_dodge())+
        labs(title="Bewerbungen pro Jahr nach MA/BA", 
             x="Jahr", y = "Anz. Bewerbungen") +
        scale_fill_manual(values=c('black','steelblue'))+
        geom_text(aes(label=n), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        theme_classic()
    }
    else{
      BewerbungsJahrMABAPlot <-ggplot(data=BewerbungenJahrMABA2, aes(x=jahr, y=n, fill=master_bachelor)) +
        geom_bar(stat="identity", position=position_dodge())+
        labs(title="Bewerbungen pro Jahr nach MA/BA", 
             x="Jahr", y = "Anz. Bewerbungen") +
        scale_fill_manual(values=c('black','steelblue'))+
        geom_text(aes(label=n), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        theme_classic()
    }
    BewerbungsJahrMABAPlot
  })  
  # Plot 9 - end
  
  # Plot 7 - begin
  output$plot7 <- renderPlot({
    #Analyse Bewerb pro Semester
    BewerbungenSemester <- BewerMerge %>% count(jahr, semester) %>% unite("jahrsemester", jahr:semester, remove = FALSE)
    # View(BewerbungenSemester)
    BewerbungsnSemesterPlot <-ggplot(data=BewerbungenSemester, aes(x=jahrsemester, semester, y=n)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme_minimal()+
      geom_text(aes(label=n), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Bewerbungen pro Semester gesamt", 
           x="Semester", y = "Anz. Bewerbungen")
    BewerbungsnSemesterPlot
    
  })  
  # Plot 7 - end
  
  # Plot 5 - begin
  output$plot5 <- renderPlot({
    #Analyse Bewerb pro Semester pro Studiengang
    BewerbungenSemStud <- BewerMerge %>% count(jahr, semester, studiengang) %>% 
      unite("jahrsemester", jahr:semester, remove = FALSE) %>% 
      arrange(.,jahrsemester)
    BewerbungenSemStud2 <- filter(BewerbungenSemStud, studiengang==input$studiengang_slider)
    
    # View(BewerbungenSemStud)
    if(input$studiengang_slider=="Alle"){
      BewerbungenSemStudPlot <- ggplot(data=BewerbungenSemStud, aes(x=jahrsemester, y=n, fill=studiengang)) +
        geom_bar(stat="identity") +
        # geom_text(aes(y=, label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        labs(title="Bewerbungen gesamt nach Studieng‰ngen", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenSemStudPlot <- ggplot(data=BewerbungenSemStud2, aes(x=jahrsemester, y=n, fill=studiengang)) +
        geom_bar(stat="identity") +
        # geom_text(aes(y=, label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)+
        theme_minimal() +
        labs(title="Bewerbungen gesamt nach Studieng‰ngen", 
             x="Semester", y = "Anz. Bewerbungen")
    }
    BewerbungenSemStudPlot
    
  })  
  # Plot 5 - end
  
  # Geomline 1 - begin
  BewerMerge22 <- merge(x = BewerMerge, y = Bewerbungen2, by.x = "bewerbungen", by.y = "Bewerbungs-ID2", all.x = TRUE) %>% 
    
    select("bewerber_id", "bewerbungen", "plz", "land_wohnort", "staatsbürgerschaft", "geschlecht",
           "studiengang", "master_bachelor", "bewerbungseingang", "bewerbung_für_semester", "semester", "jahr", "vorauswahl_bestanden", 
           "aws_am", "ergebnis_aws", "zusage_für_semester", "studienplatz_angenommen", "verschoben_bis_semester", 
           "grund_für_absageverschiebung", "bewerbungsgebühr_erlassen", "doppelstudium", "Jahr_B")
  View (BewerMerge22)
  output$geomline_1 <- renderPlot({
    BewerbungenGeschlechtJahr3 <- BewerMerge22 %>% count(geschlecht, Jahr_B) %>% select("Jahr_B", "geschlecht", "n")
    BewerbungenGeschlechtJahr33 <- filter(BewerbungenGeschlechtJahr3, geschlecht==input$geschlecht_slider2)
    #Geom_line
    if(input$geschlecht_slider2=="Alle"){
      BewerbungenGeschlJahrPlot3 <- ggplot(data=BewerbungenGeschlechtJahr3, aes(x=Jahr_B, y=n, colour=geschlecht)) 
      g <- BewerbungenGeschlJahrPlot3
      g+geom_line(aes(x=Jahr_B, y=n, color= geschlecht)) +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3)+
        theme_minimal() +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Jahr", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenGeschlJahrPlot3 <- ggplot(data=BewerbungenGeschlechtJahr33, aes(x=Jahr_B, y=n, colour=geschlecht)) 
      g <- BewerbungenGeschlJahrPlot3
      g+geom_line(aes(x=Jahr_B, y=n, color= geschlecht)) +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3)+
        theme_minimal() +
        labs(title="Bewerbungen nach Geschlechtern", 
             x="Jahr", y = "Anz. Bewerbungen")
    }
    #Ende Geom line
  })  
  # Geomline 1- end
  
  # Geomline 2 - begin
  View (Bewerbung.neu)
  output$geomline_2 <- renderPlot({
    BewerbungenJahrMonat1 <- Bewerbung.neu %>% count(BewUwe,Jahr,Monat) %>% select("BewUwe","Jahr", "Monat","n")
    BewerbungenJahrMonat2 <- filter(BewerbungenJahrMonat1, Jahr==input$jahr_slider)
    #Geom_line
    if(input$jahr_slider=="Alle"){
      BewerbungenJahrMonatPlot1 <- ggplot(data=BewerbungenJahrMonat1, aes(x=Monat, y=n, colour=Jahr)) 
      g <- BewerbungenJahrMonatPlot1
      g+geom_line(aes(x=Monat, y=n, color= Jahr)) +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3)+
        theme_minimal() +
        labs(title="Bewerbungen nach Jahr",
             x="Monat", y = "Anz. Bewerbungen")
    }
    else{
      BewerbungenJahrMonatPlot1 <- ggplot(data=BewerbungenJahrMonat2, aes(x=Monat, y=n, colour=Jahr)) 
      g <- BewerbungenJahrMonatPlot1
      g+geom_line(aes(x=Monat, y=n, color= Jahr)) +
        geom_text(aes(label=n), vjust=1.6, color="black", position = position_dodge(0.9), size=3)+
        theme_minimal() +
        labs(title="Bewerbungen nach Jahr",
             x="Monat", y = "Anz. Bewerbungen")
    }
    #Ende Geom line
  })  
  # Geomline 2- end
}
shinyApp(ui, server)

