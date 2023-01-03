library('leaflet')


caschool = read.csv('D:/google drive/2021 S1/FIT5147/Data Exploration Project/CASchools_formatted_2.csv')
m = leaflet(caschool) %>% addTiles()
pal = colorFactor(palette = 'Set3', domain =  caschool$county)

m %>% addCircles(lng = ~lng, lat = ~lat,,color = ~pal(county),
               label = ~county,radius = ~(average_score),fillOpacity = 0.6,opacity = 0.8)


%>%
  addLegend(position = "bottomleft",pal = pal, values =  ~county,opacity = 1, title = "county")

library(corrplot)
library("PerformanceAnalytics")

library(corrplot)
corrplot(M, type = "upper",is.corr = TRUE, order = "hclust", 
         tl.col = "black", tl.srt = 45)
library(dplyr)

temp = caschool %>% select(6:11, 14)
M <- cor(temp)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "#FCE903", "#FFFF00",
                           "cyan", "#007FFF", "blue", "#00007F"))
corrplot.mixed(M, lower.col = col1(10),upper = "pie", tl.col = "black",upper.col = col1(10))



#Sankey
library(networkD3)

URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)
Energy

head( Energy$links )
head( Energy$nodes )
p <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 12, nodeWidth = 30)
p
library(dplyr)
median_score = median(caschool$average_score)
max_score = max(caschool$average_score)
min_score = min(caschool$average_score)
median_score = median(caschool$average_score)

mean_calworks = mean(caschool$calworks)
mean_lunch = mean(caschool$lunch)
mean_com = mean(caschool$computer)
mean_exp = mean(caschool$expenditure)
mean_income = mean(caschool$income)
mean_eng = mean(caschool$english)


l = list()
v1=count(caschool  %>% filter(average_score <=median_score) %>% filter(calworks <=mean_calworks))$n
v2=count(caschool  %>% filter(average_score <=median_score) %>% filter(calworks >mean_calworks))$n
v3=count(caschool  %>% filter(average_score >median_score) %>% filter(calworks <=mean_calworks))$n
v4=count(caschool  %>% filter(average_score >median_score) %>% filter(calworks >mean_calworks))$n


v5 = count(caschool  %>% filter(average_score <=median_score) %>% filter(lunch <=mean_lunch))$n
v6=count(caschool  %>% filter(average_score <=median_score) %>% filter(lunch >mean_lunch))$n
v7=count(caschool  %>% filter(average_score >median_score) %>% filter(lunch <=mean_lunch))$n
v8=count(caschool  %>% filter(average_score >median_score) %>% filter(lunch >mean_lunch))$n

v9 = count(caschool  %>% filter(average_score <=median_score) %>% filter(computer <=mean_com))$n
v10=count(caschool  %>% filter(average_score <=median_score) %>% filter(computer >mean_com))$n
v11=count(caschool  %>% filter(average_score >median_score) %>% filter(computer <=mean_com))$n
v12=count(caschool  %>% filter(average_score >median_score) %>% filter(computer >mean_com))$n

c1 = count(caschool  %>% filter(average_score <=median_score) %>% filter(expenditure <=mean_exp))$n
c2=count(caschool  %>% filter(average_score <=median_score) %>% filter(expenditure >mean_exp))$n
c3=count(caschool  %>% filter(average_score >median_score) %>% filter(expenditure <=mean_exp))$n
c4=count(caschool  %>% filter(average_score >median_score) %>% filter(expenditure >mean_exp))$n

i1 = count(caschool  %>% filter(average_score <=median_score) %>% filter(income <=mean_income))$n
i2=count(caschool  %>% filter(average_score <=median_score) %>% filter(income >mean_income))$n
i3=count(caschool  %>% filter(average_score >median_score) %>% filter(income <=mean_income))$n
i4=count(caschool  %>% filter(average_score >median_score) %>% filter(income >mean_income))$n

e1 = count(caschool  %>% filter(average_score <=median_score) %>% filter(english <=mean_eng))$n
e2=count(caschool  %>% filter(average_score <=median_score) %>% filter(english >mean_eng))$n
e3=count(caschool  %>% filter(average_score >median_score) %>% filter(english <=mean_eng))$n
e4=count(caschool  %>% filter(average_score >median_score) %>% filter(english >mean_eng))$n


#caschool$sc_level = 
df2 = data.frame( name = c('Low calworks', 'Hight calworks','Low lunch','High lunch','Low computer','High computer',
                           'Low expenditure','High expenditure','Low income','High income','Low english','High english',
                           'Low average score','Hight average score'))
end = length(df2$name)-1
df = data.frame(source  = c(0,1,0,1,2,3,2,3,4,5,4,5,6,7,6,7,8,9,8,9,10,11,10,11),target = rep(c(end-1,end-1,end,end),times = 6),value =c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,c1,c2,c3,c4,i1,i2,i3,i4,e1,e2,e3,e4))
#df = data.frame(source  = c(0),target = c(2),value = c(v1))
df2$group <- as.factor(c('a','b','a','b','a','b','a','b','a','b','a','b','c','d'))
my_color <- 'd3.scaleOrdinal() .domain(["a", "b", "c","d"]) .range(["#69b3a2", "steelblue", "orange", "#FF8000"])'
#df$group<- as.factor(c('a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'))
  sankeyNetwork(Links = df, Nodes = df2, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                 fontSize = 20, nodeWidth = 14, NodeGroup="group",colourScale=my_color)#,LinkGroup="group"

  
  