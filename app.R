library(shiny)
library(shinyjs)

ui<-fluidPage(
  headerPanel("Monty Hall"), 
  useShinyjs(),
  tabsetPanel(
  tabPanel("Overview",
           fluidRow(column(3),
                    column(6,
           div( 
             style='border: 0px solid; text-align:left; font-size:12pt',
                  br(),
                  strong("Let's Make a Deal!"),
                  br(),
                  uiOutput(outputId = "monty"),
                  br(),
                  HTML("<ul><li>Pick a door, win the prize behind it! (Easy, right?) </li>
                       <li> Behind one door is a <b>New Car</b> (good)! </li>
                       <li> Behind the other doors, there are goats (<i>baaah</i>d)*  </li> <br>
                       <li> You will be a contestant! You choose a door... </li>
                       <li> Then, the host opens one of the remaining doors  </li>
                       <li> You can <i>Stay</i> with your first door, or <i>Switch</i> to the other  </li>
                       <li> What will you decide? Click the Game tab to play!  </li>
                       </ul>"),
                  br(),
                  HTML("<i>* If you'd rather have a goat than a new car, please realize that <br> you could sell the car, buy a goat, and have lots of $$$ left...</i>"),br(),
                  HTML("<i>(Money which you could use, if desired, to buy more goats)</i>"),
                  uiOutput(outputId= "xkcd"),
             br(),br(),br(),
             p("Confused? Here is a dramatized demonstration:"),
             HTML("<div style='width: 450px; height: 350px; overflow: hidden'><iframe width='450' height='350' src='https://www.youtube.com/embed/cXqDIFUB7YU' frameborder='0' style='position: relative; top: -55px;'></iframe></div>")
           ))),column(3)),
  tabPanel("Game",
  fluidRow(br(),
      column(4,align="right",div( 
        style='border: 2px solid; width:205px;height:285px; background-color: brown; text-align:center;  font-size:12pt;color:white;',
        p('Door 1'),uiOutput(outputId = "image1"))
         ),
      column(4,align="center",div( 
        style='border: 2px solid; width:205px;height:285px; background-color: brown; text-align:center;  font-size:12pt;color:white;',
        p('Door 2'),uiOutput(outputId = "image2"))
        ),
      column(4,align="left",div( 
        style='border: 2px solid; width:205px;height:285px; background-color: brown; text-align:center;  font-size:12pt;color:white;',
        p('Door 3'),uiOutput(outputId = "image3"))
        )
  ),
  fluidRow(
    column(4,align="right",div(style='width:200px;height:60px;text-align:center',actionButton("door1", "Door 1"))),
    column(4,align="center",div(style='width:200px;height:60px;text-align:center',actionButton("door2", "Door 2"))),
    column(4,align="left",div(style='width:200px;height:60px;text-align:center',actionButton("door3", "Door 3")))
  ),
      
  fluidRow(column(12,div(style='height:60px',htmlOutput(paste("pick"))))),
  fluidRow(column(1),column(11,actionButton("reset",HTML("<b>New Game</b>"))))
  #fluidRow(textOutput(paste("door1")),textOutput(paste("door2")),textOutput(paste("door3"))
),
tabPanel("Cumulative",

htmlOutput("counter"),
plotOutput("cumplot"),
plotOutput("totplot")
),

tabPanel("Simulation",
         br(),
         strong("Simulation of Many Monty Halls"),
         br(),
         
sliderInput("iterate", "Number of Iterations:",
              min = 1, max = 1000,
              value = 1, step = 1,
              animate=FALSE),
plotOutput("simplot"),
br(),
br(),
strong("Code for simulation in R"), br(),
code(" outcome<-vector()"), br(),
code(" iterate<-1000  ### set number of iterations"), br(),
code("for(i in 1:iterate){"), br(),
code("car <- sample(doors,1)  ### randomly put the car behind a door "), br(),
code("pick <- sample(doors,1)  ### randomly pick a door"), br(),
code("open <- sample(doors[doors != car & doors != pick],1)  ### open one"), br(),
code("switch <- doors[doors !=pick & doors != open] ### door if switch"), br(),

code("if(pick==car) outcome[i]<-'stay.win' "), br(),
code("if(switch==car) outcome[i]<-'switch.win'} "), br(),

code("newout<-vector()"), br(),
code("for(i in 1:iterate)"), br(),
code("newout[i]<-(sum(outcome[1:i]=='stay.win'))/i"), br(),

code("montydat<-data.frame(Game=rep(1:iterate,2),winprop=c(newout,1-newout),Choice=c(rep('Stay',iterate),rep('Switch',iterate)))"), br(),
code("ggplot(montydat,aes(Game,winprop,color=Choice))+geom_line()+ylab(`Cumulative Win Proportion``)+scale_y_continuous(breaks=1:6/6,labels=c('1/6','1/3','1/2','2/3','5/6','1'))
")
),

tabPanel("Explanation",br(),br(),p("I tried to embed the explanation as an html file directly into this Shiny app. It worked, but it messed up the functionality of the app itself. Instead, here is a direct link to the file:"),
         HTML('<a href="http://www.nathanielwoodward.com/MontyHallExp">http://www.nathanielwoodward.com/MontyHallExp</a>'))
))


#######################
library(dplyr)
library(ggplot2) 
doors<-c('A','B','C')
#outcomes=vector()

#######################

server <-function(input,output){
  
rv<-reactiveValues(door1=0,door2=0,door3=0,pick=0,open="",switch="",car=sample(doors,1),outcomes=vector())
  
output$monty<- renderUI({tags$img(src ='monty.jpeg')})
output$xkcd<- renderUI({tags$img(src ='monty_hall_xkcd.png',width=462)})

#### Plot cumulative win proportion for stay and switch

output$totplot<-renderPlot({
  if(length(rv$outcomes)>0){
    montytime<-data.frame(Game=rep(1:length(rv$outcomes)),Outcome=ifelse(rv$outcomes=='Stay and Win' | rv$outcomes=="Switch and Win","Win","Lose"),
                          Choice=ifelse(rv$outcomes=="Switch and Win" | rv$outcomes=="Switch and Lose","Switch","Stay"))
    montytime<-montytime%>%group_by(Choice,Outcome)%>%summarize(n=n())%>%mutate(prop=n/sum(n))
    ggplot(montytime,aes(x=Choice,y=n,fill=Outcome))+geom_bar(stat="identity",position="dodge")+ylab("Total Outcomes")+scale_fill_manual(values=c("#FF0000","#32CD32"))+
    ggtitle("Win/Loss Frequency if you Switch/Stay")
    #+scale_y_continuous(breaks=1:6/6,labels=c("1/6","1/3","1/2","2/3","5/6","1"))
     }
})

output$cumplot<-renderPlot({
  if(length(rv$outcomes)>0){
    montytime<-data.frame(Game=rep(1:length(rv$outcomes)),Outcome=ifelse(rv$outcomes=='Stay and Win' | rv$outcomes=="Switch and Win","Win","Lose"),
                          Choice=ifelse(rv$outcomes=="Switch and Win" | rv$outcomes=="Switch and Lose","Switch","Stay"))
    montytime<-montytime%>%group_by(Choice,Outcome)%>%summarize(n=n())%>%mutate(prop=n/sum(n))
ggplot(montytime,aes(x=Choice,y=prop,fill=Outcome,label=paste(round(100*prop),"%",sep="")))+coord_flip()+geom_bar(stat="identity",position="stack")+ylab("Proportion of Outcomes")+
  scale_y_continuous(breaks=1:6/6,labels=c("1/6","1/3","1/2","2/3","5/6","1"))+geom_text(size = 10,color="white", position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#FF0000","#32CD32"))+ggtitle("Win/Loss Percentage if you Stay/Switch")
}
})  

output$counter<-renderText(paste('<br><center><b>Number of games played:</b> ',length(rv$outcomes),'</center><br><br>',sep=""))

#Create a forloop that simulates the monty hall game

simplot<-reactive({

outcome<-vector()

for(i in 1:input$iterate)
{
car <- sample(doors,1)      ### randomly put the car behind a door
pick <- sample(doors,1)                   ### randomly pick a door
open <- sample(doors[doors != car & doors != pick],1)  ## open one
switch <- doors[doors !=pick & doors != open]   ### door if switch

if(pick==car) outcome[i]<-"stay.win" 
if(switch==car) outcome[i]<-"switch.win" 
}

newout<-vector()
for(i in 1:input$iterate)
newout[i]<-(sum(outcome[1:i]=="stay.win"))/i

montytime<-data.frame(Game=rep(1:input$iterate,2),winprop=c(newout,1-newout),Choice=c(rep("Stay",input$iterate),rep("Switch",input$iterate)))
ggplot(montytime,aes(Game,winprop,color=Choice))+geom_line()+ylab("Cumulative Win Proportion")+scale_y_continuous(breaks=1:6/6,labels=c("1/6","1/3","1/2","2/3","5/6","1"))
})

output$simplot<-renderPlot({simplot()})

  
##################################

observeEvent(input$reset,{
rv$door1=0
rv$door2=0
rv$door3=0
rv$pick=0
rv$open=""
rv$switch=""
rv$car=sample(doors,1)
delay(100,hide("image1"))
delay(100,hide("image2"))
delay(100,hide("image3"))
shinyjs::show("door1")
shinyjs::show("door2")
shinyjs::show("door3")
output$pick<-renderText({paste("<center><font size='4'><b>Let's make a deal:</b><br>Pick a door and you get to keep what's behind it!<br></font></center>")})
output$image1<-NULL
output$image2<-NULL
output$image3<-NULL
})
  
  
observeEvent(input$door1, {
    rv$door1<-rv$door1+1
    if(rv$door1==1 & rv$door2==0 & rv$door3==0){
    rv$pick<-"A"
    rv$open <- sample(doors[doors != rv$car & doors != rv$pick],1) 
    rv$switch <- doors[doors !=rv$pick & doors != rv$open]  
    output$pick<-renderText({paste("<font size='4'><center><b>  You chose Door 1! <br> Do you want to keep Door 1 or switch?</b></center></font>")})
  
    if(rv$open=="B" & rv$pick=="A"){output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door2");
    insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"))}
    
    else if(rv$open=="C" & rv$pick=="A"){output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door3");
    insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image3"))}
}
    if(rv$door1==2 & rv$car=="A" & rv$open=="B" & rv$pick=="A"){
      output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door3");
      insertUI(selector="#door1", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image3"));
      output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Stay and Win")
    }
    else if(rv$door1==2 & rv$car=="A" & rv$open=="C" & rv$pick=="A"){
      output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door2");
      insertUI(selector="#door1", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image2"));
      output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Stay and Win")
    }
    else if(rv$door1==2 & rv$car=="C" & rv$open=="B" & rv$pick=="A"){
      output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door3");
      insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image3"));
      output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Stay and Lose")
    }
    else if(rv$door1==2 & rv$car=="B" & rv$open=="C" & rv$pick=="A"){
      output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door2");
      insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image2"));
      output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Stay and Lose")
    }

    if(rv$door1==1 & rv$door2==1 & rv$car=="A" & rv$open=="C"& rv$pick=="B"){
      output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door2");
      insertUI(selector="#door1", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image2"));
      delay(100,show("image1"));
      output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Switch and Win")
    }
    if(rv$door1==1 & rv$door2==1 & rv$car=="B" & rv$open=="C"& rv$pick=="B"){
      output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door2");
      insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image2"));
      delay(100,show("image1"));
      output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Switch and Lose")
    }
    
    if(rv$door1==1& rv$door3==1 & rv$car=="A" & rv$open=="B"& rv$pick=="C"){
      output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door3");
      insertUI(selector="#door1", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image3"));
      output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Switch and Win")
    }
    else if(rv$door1==1& rv$door3==1 & rv$car=="C" & rv$open=="B"& rv$pick=="C"){
      output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
      output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
      #shinyjs::hide("door1");
      shinyjs::hide("door3");
      insertUI(selector="#door1", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
      delay(100,show("image1"));
      delay(100,show("image3"));
      output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
      rv$outcomes<-c(rv$outcomes,"Switch and Lose")
    }

   
    output$door1<-renderText({paste(rv$door1)})
    output$door2<-renderText({paste(rv$door2)})
    output$door3<-renderText({paste(rv$door3)})
    })

 ####################


observeEvent(input$door2, {
  rv$door2<-rv$door2+1
  if(rv$door1==0 & rv$door2==1 & rv$door3==0){
    rv$pick<-"B"
    rv$open <- sample(doors[doors != rv$car & doors != rv$pick],1) 
    switch <- doors[doors !=rv$pick & doors != rv$open]   
    #toggle("image2")
    output$pick<-renderText({paste(" <font size='4'><center><b>You chose Door 2! <br> Do you want to keep Door 2 or switch?</b></center></font>")})
    
    if(rv$open=="A" & rv$pick=="B"){output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1")
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"))}
    
    if(rv$open=="C"& rv$pick=="B"){output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door3")
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image3"))}
  }
  if(rv$door2==2 & rv$car=="B" & rv$open=="A"& rv$pick=="B"){
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    #shinyjs::hide("door2");
    shinyjs::hide("door3");
    insertUI(selector="#door2", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Stay and Win")
  }
  if(rv$door2==2 & rv$car=="B" & rv$open=="C"& rv$pick=="B"){
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1");
    #shinyjs::hide("door2");
    insertUI(selector="#door2", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image1"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Stay and Win")
  }
  if(rv$door2==2 & rv$car=="C" & rv$open=="A"& rv$pick=="B"){
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    #shinyjs::hide("door2");
    shinyjs::hide("door3");
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Stay and Lose")
  }
  if(rv$door2==2 & rv$car=="A" & rv$open=="C"& rv$pick=="B"){
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    shinyjs::hide("door1");
    #shinyjs::hide("door2");
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image1"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Stay and Lose")
  }
  
  if(rv$door1==1 & rv$door2==1 & rv$car=="A" & rv$open=="C" & rv$pick=="A"){
    output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door2");
    #shinyjs::hide("door1");
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"));
    delay(100,show("image2"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Switch and Lose")
  }
  else if(rv$door1==1 & rv$door2==1 & rv$car=="B" & rv$open=="C" & rv$pick=="A"){
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    #shinyjs::hide("door2");
    shinyjs::hide("door1");
    insertUI(selector="#door2", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"));
    delay(100,show("image2"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Switch and Win")
  }
  if(rv$door3==1 & rv$door2==1 & rv$car=="B" & rv$open=="A"& rv$pick=="C"){
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door3");
    #shinyjs::hide("door2");
    insertUI(selector="#door2", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Switch and Win")
  }
  else if(rv$door3==1 & rv$door2==1& rv$car=="C" & rv$open=="A"& rv$pick=="C"){
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door3");
    #shinyjs::hide("door2");
    insertUI(selector="#door2", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Switch and Lose")
  }
  

  output$door1<-renderText({paste(rv$door1)})
  output$door2<-renderText({paste(rv$door2)})
  output$door3<-renderText({paste(rv$door3)})
})

observeEvent(input$door3, {
  rv$door3<-rv$door3+1
  if(rv$door1==0 & rv$door2==0 & rv$door3==1){
    rv$pick<-"C"
    rv$open <- sample(doors[doors != rv$car & doors != rv$pick],1) 
    switch <- doors[doors !=rv$pick & doors != rv$open]   
    #toggle("image2")
    output$pick<-renderText({paste("<font size='4'><center><b>You chose Door 3! <br> Do you want to keep Door 3 or switch?</b></center></font>")})
    
    if(rv$open=="A" & rv$pick=="C"){output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1")
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"))}
    else if(rv$open=="B" & rv$pick=="C"){output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door2")
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"))}
  }
  if(rv$door3==2 & rv$car=="C" & rv$open=="A" & rv$pick=="C"){
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    #shinyjs::hide("door3");
    shinyjs::hide("door2");
    insertUI(selector="#door3", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")})
    rv$outcomes<-c(rv$outcomes,"Stay and Win")
  }
  if(rv$door3==2 & rv$car=="C" & rv$open=="B" & rv$pick=="C"){
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1");
    insertUI(selector="#door3", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    #shinyjs::hide("door3");
    delay(100,show("image1"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Stay and Win")
  }
  if(rv$door3==2 & rv$car=="B" & rv$open=="A" & rv$pick=="C"){
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    #shinyjs::hide("door3");
    shinyjs::hide("door2");
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Stay and Lose")
  }
  if(rv$door3==2 & rv$car=="A" & rv$open=="B" & rv$pick=="C"){
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    shinyjs::hide("door1");
    #shinyjs::hide("door3");
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Stay and Lose")
  }
  
  if(rv$door2==1& rv$door3==1 & rv$car=="B" & rv$open=="A"& rv$pick=="B"){
    output$image2<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    #shinyjs::hide("door3");
    shinyjs::hide("door2");
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Switch and Lose")
  }
  else if(rv$door2==1& rv$door3==1 & rv$car=="C" & rv$open=="A"& rv$pick=="B"){
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image2<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    #shinyjs::hide("door3");
    shinyjs::hide("door2");
    insertUI(selector="#door3", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image2"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Switch and Win")
  }
  
  if(rv$door1==1 & rv$door3==1 & rv$car=="A" & rv$open=="B" & rv$pick=="A"){
    output$image1<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image3<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1");
    #shinyjs::hide("door3");
    insertUI(selector="#door3", ui = tags$audio(src = "baa.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#ff0000'><b>You <i>Lose!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Switch and Lose")
  }
  else if(rv$door1==1 & rv$door3==1 & rv$car=="C" & rv$open=="B" & rv$pick=="A"){
    output$image3<- renderUI({tags$img(src ='car.png',height=250,width=195)});
    output$image1<- renderUI({tags$img(src ='goat.png',height=250,width=195)});
    shinyjs::hide("door1");
    #shinyjs::hide("door3");
    insertUI(selector="#door3", ui = tags$audio(src = "kaching.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none"));
    delay(100,show("image1"));
    delay(100,show("image3"));
    output$pick<-renderText({paste("<center><font size='8', color='#32CD32'><b>You <i>Win!</i></b></font></center>")});
    rv$outcomes<-c(rv$outcomes,"Switch and Win")
  }
  
  
  output$door1<-renderText({paste(rv$door1)})
  output$door2<-renderText({paste(rv$door2)})
  output$door3<-renderText({paste(rv$door3)})
}) 
}

shinyApp(ui,server)
