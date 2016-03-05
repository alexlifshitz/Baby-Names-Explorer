library(shiny)
library(Hmisc)
library(dplyr)
library(scales)
library(ggplot2)

library(maps)
library(mapproj)



source('names_dist.R')
baby.names<- readRDS("data/baby_names.Rds")

states_list<-scan("data/states.txt", what="", sep="\n")

shinyServer(function(input, output) {
     
     n=10
     
     
     output$text1 <- renderText({ 
          paste("Top 10", input$gender, "baby names born in ", input$state, " in ", input$years)
     })
     
     df_table<-reactive({
          dff<-filter(baby.names, Year==input$years)
          if (input$state !="All States"){
               dff<-filter(dff, State==input$state)
          }
          dff<-filter(dff, Gender==substr(input$gender,1,1))
     })    
          
          
     df_filtered<-reactive({
          dff<-df_table()
          Total <- sum(dff$Count)
          dff_n<- group_by(dff, Name) %>% dplyr::summarize(Count=sum(Count)) %>% top_n(n,Count)
          dff_n <- mutate(dff_n, Percent= Count/Total) %>% arrange(desc(Percent))
          dff_n <- dff_n[1:n,]
          dff_n <- within(dff_n, Name <- factor(Name, levels=names(sort(table(Name), 
                                                                        decreasing=TRUE))))
          return(dff_n)
     })
     
     output$text5 <- renderText({ 
          dff_n<- df_filtered()
          paste0("These names account for ", format(sum(dff_n$Percent)*100, digits=2), "% of ",input$gender, " babies")
     })
     
     
     df_dist<-reactive({
          dff<-filter(baby.names, Year==input$years)
          dff<-filter(dff, Gender==substr(input$gender,1,1))
          
          if (input$state !="All States"){
               ref<-filter(dff, State==input$state)
          }
          else{
               ref<-dff
          }
               
          
          ref<- group_by(ref, Name) %>% dplyr::summarize(Count=sum(Count)) %>% arrange(desc(Count))
          dff<- group_by(dff, State, Name) %>% dplyr::summarize(Count=sum(Count)) %>% arrange(desc(Count)) %>% select(-Count)
          dsum<-dplyr::summarize(dff, dist = my.dist(ref$Name, Name, n)) %>% mutate(State_long=state.name[match(State,state.abb)])
          
          
          return(dsum)
     })
     
     df_line<-reactive({
          dff_n<-df_filtered()
          #n_y=10
          dff<-filter(baby.names, Gender==substr(input$gender,1,1))
          if (input$state !="All States"){
               dff<-filter(dff, State==input$state)
          }
          y<-group_by(dff, Year) %>% dplyr::summarize(Total = sum(Count)) 
          dff<- merge(dff, y)
          dff<-group_by(dff, Year)%>% mutate(Rank=dense_rank(desc(Count))) 
          dff<-filter(dff,Name %in% dff_n$Name)%>% mutate(Rank=100-ifelse(Rank>100,100, Rank))
          #dff<-filter(dff,Name %in% dff_n$Name) %>% mutate(Percent = Count/Total) %>% select(Year, Name, Percent)
        
          return(dff)
     })
     
     output$barplot <- renderPlot({
          
          dff_n<-df_filtered()
          ggplot(dff_n, aes(x=reorder(Name, Percent), y=Percent, fill = as.factor(Name))) + geom_bar(stat = "identity") +
               geom_text(data=dff_n,aes(x=reorder(Name, Percent),y=Percent/2,label=Name),size=8)+
               theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank())+
               guides(fill=FALSE)+ scale_y_continuous(labels=percent)+coord_flip()
     })
     
     output$text2 <- renderText({ 
          paste("Similarity in baby names popularity with respect to ",  input$state, "(",input$gender,"names,",input$years, ")")
     })
     
     output$table_all <- renderDataTable(df_table(), options = list(pageLength = 10))
     
     output$mapplot <- renderPlot({
          dsum <- df_dist()
          
          nb=2*n
          shades <- colorRampPalette(c("darkgreen","white"))(nb)
          br <- as.integer(cut2(dsum$dist, seq(0,n, length.out = nb)))
          
          dsum$fills <- shades[br]
         
          namevec <- map(database = "state", col = "white",fill=T, namesonly=TRUE)
          namevec<-gsub(":.*", "", namevec)
          ind<- match(namevec,tolower(dsum$State_long))
          map(database = "state")
          map(database = "state",col = dsum$fills[ind],fill=TRUE,add=TRUE)
          text(x=state.center$x, y=state.center$y, state.abb, cex=0.9, font=2)
          ind<- match(input$state, state.abb)
          text(x=state.center$x[ind], y=state.center$y[ind], input$state, cex=0.9, col = "white", font=2)
          
          #text(x=state.center$x, y=state.center$y-0.6, dsum$dist[match(state.abb,dsum$State)], cex=0.6)
          #           ggplot(all_states_dist, aes(x=dist1, y=dist2))+ geom_point(size=6, color="lightpink", alpha=0.6)+
          #                geom_text(data=all_states_dist,aes(x=dist1, y=dist2,label=State)) 
          
     })
     
     output$text3 <- renderText({ 
          paste("Dark green areas correspond to high similarity in selection of baby names 
               with repect to the selected state, light green or white areas correspond to low similarity.")
     })
     
     output$text4 <- renderText({
          paste(input$gender, "names popularity in",input$state, "vs Time")
     })

     # output$lineplot <- renderPlot({
     # 
     #      dff<-df_line()
     #      ggplot(dff, aes(x=Year, y=Rank, group = as.factor(Name),color = Name)) + geom_line(lwd=1)
     # 
     # })
     
#      output$text5 <- renderText({ 
#           paste("Similarity in Names Popularity with respect to ",  input$state, "(",input$gender,"names,",input$years, ")")
#      })
     
})