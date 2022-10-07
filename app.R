require(shiny)
require(ggplot2)
require(matrixStats)

require(condMVNorm)
require(R.CenGMR)
# require(MomTrunc)
# require(truncnorm)


path=getwd()
# source(paste0(substring(path, 
#                         1, 
#                         tail(unlist(gregexpr(pattern ='/',path)),n=1)),
#               "MixCenMVReg_EM.R"))

# source(paste0(substring(path, 
#                         1, 
#                         tail(unlist(gregexpr(pattern ='/',path)),n=1)),
#               "MixCenUVReg_EM.R"))

# source(paste0(substring(path, 
#                         1, 
#                         tail(unlist(gregexpr(pattern ='/',path)),n=1)),
#               "Util_Func.R"))

#source("MixCenMVReg_EM.R")
#source("Util_Func.R")

load("elecsys_data2_rename.RData")
load('mod2.RData')


y=as.matrix(elecsys_data[c('Ab42','tTau','pTau')])
x=as.matrix(elecsys_data[c('Intercpt','Age','Female','Edu','African.A')])
c=as.matrix(elecsys_data[c('C_Ab42','C_tTau','C_pTau')])
select_nomissing=function(input){
    return(any(is.na(input))==F)
}
nomissing=apply(x,1,select_nomissing)

x=x[nomissing,]
y=y[nomissing,]
c=c[nomissing,]




Cluster=mod2$Class

Cluster[Cluster==1]="Non-AD"
Cluster[Cluster==2]="AD-like"
Cluster[Cluster==3]="Control-like"

Cluster=factor(Cluster, levels=c("AD-like","Control-like","Non-AD"))


ui <- fluidPage(
    titlePanel(title="Posterior Calculator"),
    sidebarPanel(
        titlePanel(title=h4("CSF Inputs :", align="left")),
        
        #### Liangkang Edited
        selectInput(inputId = "select",label = "Input method",choices = list("Slider" = 1, "Input box" = 2), selected = 1),
        selectInput(inputId = "censor",label = "Censor or not",choices = list("Censored" = 1, "Not censored" = 2), selected = 2),
        
        
        sliderInput("Abeta", "Aβ42: (200~1700)",min = 200, max = 1700,step=0.1,value=200),

        
        numericInput(inputId = "Abeta_n",label = "Aβ42: (200~1700)",min = 200, max = 1700,value=200),
        
        
        sliderInput("tTau",  "tTau: (80~1300)",min = 80, max = 1300,step=0.1,value=80),
        numericInput(inputId = "tTau_n",label = "tTau: (80~1300)",min = 80, max = 1300,value=80),
        
        
        
        
        sliderInput("pTau",  "pTau: (8~120)",min = 8, max = 120,step=0.1,value=8),
        numericInput("pTau_n",  "pTau: (8~120)",min = 8, max = 120,value=8),
        radioButtons("Abeta_C", label = h5("Aβ42 Censored"), 
                                                 choices = list("Left Censored" = -1, "Not Censored" = 0, "Right Censored" = 1),
                                                 selected = 0),
        radioButtons("tTau_C", label = h5("tTau Censored"), 
                                                        choices = list("Left Censored" = -1, "Not Censored" = 0, "Right Censored" = 1),
                                                        selected = 0),
        
        radioButtons("pTau_C", label = h5("pTau Censored"), 
                                                 choices = list("Left Censored" = -1, "Not Censored" = 0, "Right Censored" = 1),
                                                 selected = 0),
        
        
        
              # br() element to introduce extra vertical spacing ----
        br(),
        titlePanel(title=h4("Demographic Inputs :", align="left")),
        sliderInput("Age",  "Age (yr): (0~120)",min = 20, max = 100,step=1,value=50),
        sliderInput("Edu",  "Education (yr): (0~30)",min = 0, max = 30,step=1,value=12),
        # sliderInput("Moc",  "MoCA score: (0~40)",min = 0, max = 40,step=1,value=25),
        radioButtons("Sex", "Gender :",c("Female" = 1,"Male" = 0), selected=0),
        radioButtons("Race", "Race : (Is African-American?)",c("Yes"= 1,"No" = 0), selected=0),
        # radioButtons("APOE4", "APOE4 genetype :",c("ε4/ε4"= 2,
        #                                            "ε4/ε2 or ε4/ε3" = 1,
        #                                            "ε4 Negative"= 0,
        #                                            "Unknown"=9),selected=0)
    ),
    
    sidebarPanel(
      fileInput(inputId = "file",label = "Choose CSV file",accept = ".csv"),
      downloadButton(outputId = "downloadData","download")

      ),
    


    

  

    
    mainPanel(
        titlePanel(title=h4("Posterior Probabilities :", align="left")),
        uiOutput("Posterior"),
        titlePanel(title=h4("Scatter Plots on Modeled Clusters", align="left")),
        tabsetPanel(type = "tabs",
                      tabPanel("Aβ42 vs. tTau", plotOutput("plot1")),
                      tabPanel("Aβ42 vs. pTau", plotOutput("plot2")),
                      tabPanel("tTau vs. pTau", plotOutput("plot3")))
        
        
    )

)

server <- function(input,output){
  

    
    output$plot1<-renderPlot(
        {
          if(input$select==1){
            ggplot(as.data.frame(y), aes(x=tTau, y=Ab42, color=as.factor(Cluster)))+
            geom_point() + scale_color_discrete(name = "Cluster")+
            geom_point(aes(x=input$tTau, y=input$Abeta), shape=10, fill="Black", color="Black", size=10)+
            theme(legend.position = "top",text = element_text(size = 20))+
            xlab("tTau")+ylab("Aβ42")+
            lims(x= c(80,1300), y = c(200,1700))
            
        
          }else{
            ggplot(as.data.frame(y), aes(x=tTau, y=Ab42, color=as.factor(Cluster)))+
              geom_point() + scale_color_discrete(name = "Cluster")+
              geom_point(aes(x=input$tTau_n, y=input$Abeta_n), shape=10, fill="Black", color="Black", size=10)+
              theme(legend.position = "top",text = element_text(size = 20))+
              xlab("tTau")+ylab("Aβ42")+
              lims(x= c(80,1300), y = c(200,1700))
        }
            
        },height = 600,width = 600)
    
    output$plot2<-renderPlot(
        {
          
          if(input$select==1){
            ggplot(as.data.frame(y), aes(x=pTau, y=Ab42, color=as.factor(Cluster)))+
            geom_point() + scale_color_discrete(name = "Cluster")+
            geom_point(aes(x=input$pTau, y=input$Abeta), shape=10, fill="Black", color="Black", size=10)+
            theme(legend.position = "top",text = element_text(size = 20))+
            xlab("pTau")+ylab("Aβ42")+
            lims(x= c(8,120), y = c(200,1700))
          }else{
            ggplot(as.data.frame(y), aes(x=pTau, y=Ab42, color=as.factor(Cluster)))+
              geom_point() + scale_color_discrete(name = "Cluster")+
              geom_point(aes(x=input$pTau_n, y=input$Abeta_n), shape=10, fill="Black", color="Black", size=10)+
              theme(legend.position = "top",text = element_text(size = 20))+
              xlab("pTau")+ylab("Aβ42")+
              lims(x= c(8,120), y = c(200,1700))
          }
          
            
        },height = 600,width = 600)
    
    
    output$plot3<-renderPlot(
        {
          if(input$select==1){
            ggplot(as.data.frame(y), aes(x=pTau, y=tTau, color=as.factor(Cluster)))+
            geom_point() + scale_color_discrete(name = "Cluster")+
            geom_point(aes(x=input$pTau, y=input$tTau), shape=10, fill="Black", color="Black", size=10)+
            theme(legend.position = "top",text = element_text(size = 20))+
            xlab("pTau")+ylab("tTau")+
            lims(x= c(8,120), y = c(80,1300))
          }else{
            ggplot(as.data.frame(y), aes(x=pTau, y=tTau, color=as.factor(Cluster)))+
              geom_point() + scale_color_discrete(name = "Cluster")+
              geom_point(aes(x=input$pTau_n, y=input$tTau_n), shape=10, fill="Black", color="Black", size=10)+
              theme(legend.position = "top",text = element_text(size = 20))+
              xlab("pTau")+ylab("tTau")+
              lims(x= c(8,120), y = c(80,1300))
          }
          
            
        },height = 600,width = 600)
    
    
    output$Posterior=renderTable({

        if(input$select==1){
          Y=t(as.matrix(c(input$Abeta,input$tTau,input$pTau)))
          ######
          
          
          if(input$censor==2){ C=Y*0 }else{
            C=t(as.matrix(c(input$Abeta_C,input$tTau_C,input$pTau_C)))
          }
        


        # Gene=(input$APOE4==1)*c(1,0,0)+(input$APOE4==2)*c(0,1,0)+(input$APOE4==9)*c(0,0,0)
        # X=t(as.matrix(c(1,(input$Age-66)/10,input$Sex,(input$Edu-16)/10, input$Moc-25,Gene, input$Race)))
          X=t(as.matrix(c(1,(input$Age-66)/10,input$Sex,(input$Edu-16)/10, input$Race)))
        }else{
          Y=t(as.matrix(c(input$Abeta_n,input$tTau_n,input$pTau_n)))
          C=Y*0 #Subject to change here later
          
          
          # Gene=(input$APOE4==1)*c(1,0,0)+(input$APOE4==2)*c(0,1,0)+(input$APOE4==9)*c(0,0,0)
          # X=t(as.matrix(c(1,(input$Age-66)/10,input$Sex,(input$Edu-16)/10, input$Moc-25,Gene, input$Race)))
          X=t(as.matrix(c(1,(input$Age-66)/10,input$Sex,(input$Edu-16)/10, input$Race)))
          }


        G=length(mod2$Pie)

        log.ind_density=matrix(NA,nrow=1,ncol=G)
        mu_hat=list()

        for(g in 1:G){
            mu_hat[[g]]=as.numeric(X)%*%mod2$Beta[[g]] 
            log.ind_density[,g]=log(mod2$Pie[g])+eval_density(Y,C,mu_hat[[g]],mod2$Sigma[[g]]) 
        }


        p=exp(sweep(log.ind_density, 1, apply(log.ind_density,1,logSumExp)))
        colnames(p)=c("Control-like","AD-like","Non-AD")
        p=t(as.matrix(p[, order(colnames(p))]))
        p
    })
    
    
    output$downloadData=downloadHandler(filename = paste("data-", Sys.time(), ".csv", sep=""),
                                        content = function(filename){
                                          
                                          nfile=input$file
                                          ext <- tools::file_ext(nfile$datapath)
                                          req(nfile)
                                          validate(need(ext == "csv", "Please upload a csv file"))
                                          
                                          file=read.csv(nfile$datapath)
                                          
                                          
                                          C=matrix(0,nrow = length(file$Gender),ncol = 3)
                                          Y=matrix(data = c(file$Ab42,file$tTau,file$pTau),ncol = 3,byrow = FALSE)
                                          
                                          
                                          sex=file$Gender
                                          sex[which(file$Gender=="Female")]=1
                                          sex[which(file$Gender=="Male")]=0
                                          sex=as.numeric(sex)
                                          
                                          race=file$Race
                                          race[which(grepl(pattern = "African",x = file$Race)|grepl(pattern = "Black",x = file$Race))]=1
                                          race[which(!(grepl(pattern = "African",x = file$Race)|grepl(pattern = "Black",x = file$Race)))]=0
                                          race=as.numeric(race)
                                          
                                          a=rep(1,length(file$Gender))
                                          
                                          
                                          X=matrix(c(a,(file$Age.Deident-66)/10,sex,(file$Educ-16)/10, race),ncol = 5,byrow = FALSE)
                                          
                                          
                                          G=length(mod2$Pie)
                                          
                                          log.ind_density=matrix(NA,nrow=length(file$Gender),ncol=G)
                                          mu_hat=list()
                                          
                                          for(g in 1:G){
                                            mu_hat[[g]]=X%*%mod2$Beta[[g]]
                                            for (i in 1:length(file$Gender)) {
                                              log.ind_density[i,g]=log(mod2$Pie[g])+eval_density(Y[i,],C[i,],mu_hat[[g]][i,],mod2$Sigma[[g]]) 
                                            }
                                            
                                          }
                                          
                                          
                                          p=exp(sweep(log.ind_density, 1, apply(log.ind_density,1,logSumExp)))
                                          colnames(p)=c("Control-like","AD-like","Non-AD")
                                          
                                          outputfile=cbind(file,p)
                                          write.csv(outputfile,filename,row.names = FALSE)
      
    }
    )
        
    
}


shinyApp(ui, server)