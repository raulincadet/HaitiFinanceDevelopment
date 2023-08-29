library(tidyverse)

df=readxl::read_excel('API_HTI_DS2_en_excel_v2_5738369.xls')


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

df2<-data.frame(y)             # create a new data frame where all data are numeric
colnames(df2)=colnames(df)    # provide the same columns names to df2 as it is for df

# Create new variables
df2["Public external debt stock (% of GDP)"]=(df2["External debt stocks, public and publicly guaranteed (PPG) (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2["Private external debt stock (% of GDP)"]=(df2["External debt stocks, private nonguaranteed (PNG) (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2['Total external debt stock (% of GDP)']=(df2["External debt stocks, total (DOD, current US$)"]/df2["GDP (current US$)"])*100
df2['Tax les subsidies on products (% of GDP)']=(df2["Taxes less subsidies on products (current US$)"]/df2["GDP (current US$)"])*100
df2['Net official development assistance received (% of GDP)']=(df2["Net official development assistance received (current US$)"]/df2["GDP (current US$)"])*100

colnames(df2)[1]<-'Years'

####################################
###### Other Data to import ########
dcred<-readxl::read_excel('DataBRH.xlsx',sheet='BRH')
dcred<-dcred%>%filter(`Years` %in% c(2000:2020))    # consider data from 2000 to 2020
dcred<-dcred[,2:dim(dcred)[2]]
dcred['Years']<-2000:2020

dinv<-readxl::read_excel('DataBRH.xlsx',sheet='IHSI')
dinv<-data.frame(t(dinv))    # transpose dinv so that each column have a variable
col<-dinv[1,]
dinv<-data.frame(dinv[2:(dim(dinv)[1]),c(5,6)]) # remove the first row that contains columns names
colnames(dinv)<-col[5:6]

#######
str(dinv)  # data are not numeric wherease they should.
# Data will be converted to numeric type
z=NULL
for (i in 1:length(colnames(dinv))) {
  z[[i]]=as.numeric(unlist(dinv[colnames(dinv)[i]]))  # convert all columns to numeric data
}
dinv=data.frame(z)
colnames(dinv)=col[5:6]
dinv['Years']=2000:2020

df3<-merge(df2,dcred,on='Years')
df3<-merge(df3,dinv,on='Years')
rownames(df3)<-2000:2020

#################################



df3=df3%>%select(
  Years,
  `Public external debt stock (% of GDP)`,
  `Private external debt stock (% of GDP)`,
  `Foreign direct investment, net inflows (% of GDP)`,
  `Personal remittances, received (% of GDP)`,
  `Domestic credit to private sector (% of GDP)`,
  `Total external debt stock (% of GDP)`,
  `Tax les subsidies on products (% of GDP)`,
  `Net official development assistance received (% of GDP)`,
  `Net domestic credit to public sector (% of GDP)`,
  `Investissement privé (% du PIB)`,
  `Investissement publique (% du PIB)`,
  )

df3%>%
  ggplot(aes(x=Years))+
  geom_line(aes(y=`Personal remittances, received (% of GDP)`,colour='Personal remittances (b)'),size=1.1)+
  geom_line(aes(y=`Domestic credit to private sector (% of GDP)`,colour="Domestic credit to private sector (b)"),size=1.1)+
  geom_line(aes(y=`Foreign direct investment, net inflows (% of GDP)`,colour="Foreign direct investment (b)"),size=1.1)+
  geom_line(aes(y=`Private external debt stock (% of GDP)`,colour='Private external debt (b)'),size=1.1)+
  geom_line(aes(y=`Public external debt stock (% of GDP)`,colour='Public external debt (b)' ),size=1.1)+
  #geom_line(aes(y=`Total external debt stock (% of GDP)`,colour="External debt (b)"),size=1)+
  geom_line(aes(y=`Tax les subsidies on products (% of GDP)`,colour="Taxes less subsidies (b)"),size=1.1)+
  geom_line(aes(y=`Net official development assistance received (% of GDP)`,colour="Net development assistance (b)"),size=1.1)+
  geom_line(aes(y=`Net domestic credit to public sector (% of GDP)`,colour="Net public domestic debt (a)"),size=1.1)+
  geom_line(aes(y=`Investissement privé (% du PIB)`,colour="Domestic private investment (a)"),size=1.1)+
  geom_line(aes(y=`Investissement publique (% du PIB)`,colour="Domestic public investment (a)"),size=1.1)+
  
  scale_colour_manual("", 
                      breaks = c('Net public domestic debt (a)', 'Domestic private investment (a)','Domestic public investment (a)','Private external debt (b)','Public external debt (b)', "Domestic credit to private sector (b)","Personal remittances (b)", "Foreign direct investment (b)",
                                 'Taxes less subsidies (b)','Net development assistance (b)'),
                                 
                      values = c("gray1", "#CC79A7","greenyellow","navy",'red',"forestgreen", "#0072B2",'brown','yellow','orange')) +
  ggtitle("Financing Trends in Haiti")+

  theme_classic()+ theme(legend.position = "top")+ylab("% of GDP")+
  guides(col = guide_legend(nrow = 4))+
  labs(caption = 'Source: Realized by Raulin L. Cadet, with: (a) data from BRH and IHSI; (b) data from the World Bank.Taxes less subsidies \n are related to products only. Domestic private investment is estimated: Domestic investment minus Domestic public investment.')

#######################################################
######## The same graphics with texts in French #######
df3%>%
  ggplot(aes(x=Years))+
  geom_line(aes(y=`Personal remittances, received (% of GDP)`,colour='Transferts de fonds (b)'),size=1.1)+
  geom_line(aes(y=`Domestic credit to private sector (% of GDP)`,colour="Crédit intérieur au secteur privé (b)"),size=1.1)+
  geom_line(aes(y=`Foreign direct investment, net inflows (% of GDP)`,colour="Investissement direct étranger (b)"),size=1.1)+
  geom_line(aes(y=`Private external debt stock (% of GDP)`,colour='Dette extérieure privée (b)'),size=1.1)+
  geom_line(aes(y=`Public external debt stock (% of GDP)`,colour='Dette extérieure publique (b)' ),size=1.1)+
  #geom_line(aes(y=`Total external debt stock (% of GDP)`,colour="External debt (b)"),size=1)+
  geom_line(aes(y=`Tax les subsidies on products (% of GDP)`,colour="Taxes moins subventions (b)"),size=1.1)+
  geom_line(aes(y=`Net official development assistance received (% of GDP)`,colour="Aide (nette) au développement (b)"),size=1.1)+
  geom_line(aes(y=`Net domestic credit to public sector (% of GDP)`,colour="Dette (nette) publique intérieure (a)"),size=1.1)+
  geom_line(aes(y=`Investissement privé (% du PIB)`,colour="Investissement intérieur privé (a)"),size=1.1)+
  geom_line(aes(y=`Investissement publique (% du PIB)`,colour="Investissement intérieur public (a)"),size=1.1)+
  
  scale_colour_manual("", 
                      breaks = c('Dette (nette) publique intérieure (a)', 'Investissement intérieur privé (a)','Investissement intérieur public (a)','Dette extérieure privée (b)','Dette extérieure publique (b)', 
                                 "Crédit intérieur au secteur privé (b)","Transferts de fonds (b)", "Investissement direct étranger (b)",
                                 'Taxes moins subventions (b)','Aide (nette) au développement (b)'),
                      
                      values = c("gray1", "#CC79A7","greenyellow","navy",'red',"forestgreen", "#0072B2",'brown','yellow','orange')) +
  ggtitle("Tendances du Financement de l'Economie Haïtienne")+
  
  theme_classic()+ theme(legend.position = "top")+ylab("% du PIB")+xlab('Années')+
  guides(col = guide_legend(nrow = 4))+
  labs(caption = "Source: Realisé par Raulin L. Cadet, avec: (a) les données de la BRH et de l'IHSI; (b) les données de la Banque Mondiale.\n Les taxes moins les subventions concernent uniquement les produits. L'investissement intérieur privé est \n estimé comme suit: investissement intérieur moins investissement intérieur public.")




