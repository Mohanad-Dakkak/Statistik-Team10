library(shiny)
library(shinythemes)

titanicRoh <- read.csv("d:/HTW/B3.5/Projektarbeit/titanic.csv")
titanic <- titanicRoh
summary<-as.data.frame(summary(titanic)) 
summary(titanic$Age)



#Age
#allWithoutMaster <- titanic$Age[grepl("Master", titanic$Name, fixed=TRUE) == FALSE ]
#allWithoutMasterMean <- round(mean(allWithoutMaster, na.rm=TRUE), digits=2)
#allWithoutMasterMean
#meanAge <- round(mean(titanic$Age, na.rm=TRUE), digits=2)
#meanAge - Unterschied 1 Jahr zwischen den Means
masterMales <- titanic$Age[titanic$Sex == 'male' & titanic$Age<17]
masterMean <- round(mean(masterMales , na.rm=TRUE),2)
titanic$Age[is.na(titanic$Age) == TRUE & grepl("Master", titanic$Name, fixed=TRUE) == TRUE] <- masterMean


titanic <- na.omit(titanic)

titanic <-titanic[titanic$Embarked != "",]

#Preis
klasseEins <- titanic[titanic$Pclass == 1,]
medianKlasseEins <- median(klasseEins$Fare)
#medianKlasseEins
titanic$Fare[titanic$Fare == 0.00 & titanic$Pclass ==1] <- medianKlasseEins

klasseZwei <- titanic[titanic$Pclass == 2,]
medianKlasseZwei <- median(klasseZwei$Fare)
#medianKlasseZwei
titanic$Fare[titanic$Fare == 0.00 & titanic$Pclass == 2] <- medianKlasseZwei

klasseDrei <- titanic[titanic$Pclass == 3,]
medianKlasseDrei <- median(klasseDrei$Fare)
#medianKlasseDrei
titanic$Fare[titanic$Fare == 0.00 & titanic$Pclass == 3] <- medianKlasseDrei

#keine duplicate
unique(titanic)


titanicNamen <- titanic$Name
titanicNamen <- str_split_fixed(titanicNamen, " \\((.*?)\\)", 2)
titanic$Name <- titanicNamen[,1]

tickets <- titanic$Ticket

titanic$Ticket <- gsub("[^0-9]", "", tickets)




shinyApp(
  
  
  ui = tagList(
    
    navbarPage(
      
      theme = shinytheme("flatly"),
      
      
      "Team 10",
      tabPanel("Hintergrund",
               
               headerPanel(title = "Beschreibung und Summary "),
               
               
               mainPanel("Hier wird alles beschrieben"
                 
                 
                 
               )
      ),
      tabPanel("Analyse",
               
               tabsetPanel(
                 tabPanel("kategorische Variablen Analyse",
                          sidebarPanel(
                            
                            
                            selectInput("select", label = h3("Select table"), 
                                        choices = list("Gender table" = 1, 
                                                       "Survived table" = 2,
                                                       "Class table" = 3,
                                                       "Embarked table" = 4,
                                                       "Parents table" = 5,
                                                       "Siblings table" = 6), 
                                        selected = 1),
                            
                            
                            
                            
                            
                          ), mainPanel(
                            
                            # plotOutput("value"),
                            
                            plotOutput("sexTable"),
                            
                            plotOutput("survivedTable"),
                            
                            plotOutput("classTable"),
                            
                            plotOutput("embarkedTable"),
                            
                            plotOutput("parentsTable"),
                            
                            plotOutput("siblingsTable"),
                            
                          )
                  
                 ), tabPanel("Alter Analyse",
                 
                             sidebarPanel(
                               
                               
                               selectInput("select", label = h3("Select filter"), 
                                           choices = list("by 5 breaks" = 1, 
                                                          "by 10 breaks" = 2,
                                                          "by 15 breaks" = 3,
                                                          "boxplot" = 4,
                                                          "by 10 years apart" = 5),
                                           selected = 1),
                               
                                           
                               
                               
                             ), mainPanel(
                               
                               plotOutput("ageTable"),
                               plotOutput("ageTable2"),
                               plotOutput("ageTable3"),
                               plotOutput("ageTable4"),
                               plotOutput("ageTable5"),
                             )
                             
                             
                             
                             
                             
                             
                             
                             
                             
                                    
                             
                 ), tabPanel("Multivariate Zusammenhaenge", 
                   
                             sidebarPanel(
                               
                               selectInput("select", label = h3("Select filter"), 
                                           choices = list("cTAgess" = 1, 
                                                          "cTSexS" = 2,
                                                          "policy" = 3,
                                                          "cTPClass" = 4),
                                           selected = 1),
                               
                               sliderInput("slider1", label = h3("Filter by age"), min = 0.5, 
                                           max = 80, value = 30)
                             
                               
                               
                               
                             ), mainPanel(
                             
                             plotOutput("cTAges"),  
                             plotOutput("cTSexS"),
                             plotOutput("policy"),
                             plotOutput("cTPClass"),
                             
                             )
                                       
                 )
                 
                 ),                
            ),
      
      tabPanel("Info", "Hier wird unsere Team, Quellen usw. erwähnt")
    )
  ),
  
  
  server = function(input, output) {
    
    
    output$sexTable <- renderPlot({
      sexTable <- table(titanic$Sex)
      barplot(sexTable/sum(sexTable), main = "Gender Table")
    })
    
    output$survivedTable <- renderPlot({
      survivedTable <- table(titanic$Survived)
      barplot(survivedTable/sum(survivedTable), main = "Survived Table")
    })
    
    output$classTable <- renderPlot({
      classTable<- table(titanic$Pclass)
      barplot(classTable/sum(classTable), main = "Class Table")
    })
    
    output$embarkedTable <- renderPlot({
      embarkedTable <- table(titanic$Embarked)
      barplot(embarkedTable/sum(embarkedTable), main = "Embarked Table")
    })
    
    output$parentsTable <- renderPlot({
      parentsTable<-table(titanic$Parch)
      barplot(parentsTable/sum(parentsTable), main = "Parent Table")
    })  
    
    output$siblingsTable <- renderPlot({
      siblingsTable<-table(titanic$SibSp)
      barplot(siblingsTable/sum(siblingsTable), main = "Siblings Table")
    }) 
    
    
    output$ageTable <- renderPlot({
      hist(titanic$Age, xlab="Alter", ylab="Haeufigkeit", breaks=seq(0,max(titanic$Age), 5))
    })
    
    output$ageTable2 <- renderPlot({
      hist(titanic$Age, xlab="Alter", ylab="Haeufigkeit", breaks=seq(0,max(titanic$Age, na.rm=TRUE), 10))
    })
    
    output$ageTable3 <- renderPlot({
      hist(titanic$Age, xlab="Alter", ylab="Haeufigkeit", breaks=seq(0,max(titanic$Age, na.rm=TRUE)+10, 15))
    })
    
    output$ageTable4 <- renderPlot({
      boxplot(titanic$Age)
    })
    
    output$ageTable5 <- renderPlot({
      barplot(tableAge/sum(tableAge))
    })
    
    output$cTAges <- renderPlot({
      mosaicplot(cTAges)
    })
    
    output$cTSexS <- renderPlot({
      mosaicplot(cTSexS)
    })
    
    output$policy <- renderPlot({
      mosaicplot(policy)
    })
    
    output$cTPClass <- renderPlot({
      mosaicplot(cTPClass)
    })
    
  }
)
