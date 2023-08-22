library(tidyverse)
df=readxl::read_excel('API_HTI_DS2_en_excel_v2_5738369.xls')
variables=c(
  "Foreign direct investment, net inflows (BoP, current US$)",
  "External debt stocks, public and publicly guaranteed (PPG) (DOD, current US$)",
  "External debt stocks, private nonguaranteed (PNG) (DOD, current US$)",
  "Personal remittances, received (current US$)",
  "Personal remittances, paid (current US$)",
  #"Domestic credit provided by financial sector (% of GDP)"
  "Domestic credit to private sector (% of GDP)",
  "Foreign direct investment, net inflows (% of GDP)", # New investment inflows less disinvestment
  "Personal remittances, received (% of GDP)",
  "GDP (current US$)",
  
)
variables

# Structured dataset 
df=df[3:dim(df)[1],3:dim(df)[2]]   
df=data.frame(t(df))
colnames(df)= df[1,]
rownames(df)=1:dim(df)[1]
df=df[3:dim(df)[1],]  

# consider data from 2000 to 2020
df=df%>%filter(`Indicator Name` %in% c(2000:2020))

# convert all columns to numeric data
str(df) # data types are characters wherease they should be numeric

y=NULL
for (i in 1:length(colnames(df))) {
  y[[i]]=as.numeric(unlist(df[colnames(df)[i]]))  # convert all columns to numeric data
}

df2=data.frame(y)             # create a new data frame where all data are numeric
colnames(df2)=colnames(df)    # provide the same columns names to df2 as it is for df

# Create new variables
df2["Public external debt stock (% of GDP)"]=(df2["External debt stocks, public and publicly guaranteed (PPG) (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2["Private external debt stock (% of GDP)"]=(df2["External debt stocks, private nonguaranteed (PNG) (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2['Total external debt stock (% of GDP)']=(df2["External debt stocks, total (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2['Tax les subsidies on products (% of GDP)']=(df2["Taxes less subsidies on products (current US$)"]/df2["GDP (current US$)"])*100
colnames(df2)[1]='Years'
df2%>%select(
  Years,
  `Public external debt stock (% of GDP)`,
  `Private external debt stock (% of GDP)`,
  `Foreign direct investment, net inflows (% of GDP)`,
  `Personal remittances, received (% of GDP)`,
  `Domestic credit to private sector (% of GDP)`,
  `Total external debt stock (% of GDP)`,
  `Tax les subsidies on products (% of GDP)`
)%>%
  ggplot(aes(x=Years))+
  geom_line(aes(y=`Personal remittances, received (% of GDP)`,colour='Personal remittances'),size=1)+
  geom_line(aes(y=`Domestic credit to private sector (% of GDP)`,colour="Domestic credit to private sector"),size=1)+
  geom_line(aes(y=`Foreign direct investment, net inflows (% of GDP)`,colour="Foreign direct investment"),size=1)+
  geom_line(aes(y=`Private external debt stock (% of GDP)`,colour='Private external debt'),size=1)+
  geom_line(aes(y=`Public external debt stock (% of GDP)`,colour='Public external debt'),size=1)+
  #geom_line(aes(y=`Total external debt stock (% of GDP)`,colour="External debt"),size=1)+
  geom_line(aes(y=`Tax les subsidies on products (% of GDP)`,colour="Taxes less subsidies"),size=1)+
  
  scale_colour_manual("", 
                      breaks = c("Personal remittances", "Domestic credit to private sector", "Foreign direct investment",'Private external debt','Public external debt','Taxes less subsidies'),
                      values = c("forestgreen", "navy", "orange",'steelblue',"brown",'magenta2')) +
  
  
  theme_classic()+ theme(legend.position = "top")+ylab("% of GDP")+
  guides(col = guide_legend(nrow = 3))+
  labs(caption = 'Source: Realized by Raulin L. Cadet, with data from the \n World Development Indicators (WDI). Taxes less \n subsidies are related to products only.')

########################

library(plotly)

df2$Years <- factor(df2$Years, levels = df2[["Years"]])
fig <- fig %>% add_trace(y = ~`Total external debt stock (% of GDP)`, name = 'Tax', fillcolor = 'gray')
fig <- plot_ly(data=df2, x = ~Years, y = ~`Tax les subsidies on products (% of GDP)`, name = 'FDI, net inflows', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = 'navy')
fig <- fig %>% add_trace(y = ~`Foreign direct investment, net inflows (% of GDP)`, name = 'External debt', fillcolor = 'orange')
fig <- fig %>% add_trace(y = ~`Domestic credit to private sector (% of GDP)`, name = 'Domestic credit', fillcolor = 'brown')
fig <- fig %>% add_trace(y = ~`Personal remittances, received (% of GDP)`, name = 'Remittances, received', fillcolor = 'steelblue')
fig <- fig %>% layout(#title = '',
  xaxis = list(title = "",
               showgrid = FALSE),
  yaxis = list(title = "% of GDP",
               showgrid = FALSE),
  showlegend=TRUE,
  legend = list(orientation = 'h'),
  annotations = 
    list(x = 1, y = -0.2, text = "Source: Realized by Raulin Cadet, with data from the World Development Indicators", 
         showarrow = F, xref='paper', yref='paper', 
         xanchor='right', yanchor='auto', xshift=0, yshift=0,
         font=list(size=11, color="gray"))
  
)
fig


######################


#########################################################
###### Create a new data frame with grouped data ########

Indicators=list(unlist(
  df2%>%select(
    # `Public external debt stock (% of GDP)`,
    #`Private external debt stock (% of GDP)`,
    `Foreign direct investment, net inflows (% of GDP)`,
    `Personal remittances, received (% of GDP)`,
    `Domestic credit to private sector (% of GDP)`,
    `Total external debt stock (% of GDP)`
  )
))

df3=data.frame(Indicators);colnames(df3)="Values"
df3['Indicators']=c(
  #rep('Public external debt',dim(df2)[1]),
  #                 rep('Private external debt',dim(df2)[1]),
  rep('FDI, net inflows',dim(df2)[1]),
  rep('Remittances received',dim(df2)[1]),
  rep('Domestic credit',dim(df2)[1]),
  rep('Total external debt',dim(df2)[1])
)
df3['Years']=rep(2000:2020,4)
rownames(df3)=1:dim(df3)[1]