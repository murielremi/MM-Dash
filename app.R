library(shiny)
library(readxl)
library(dplyr)
library(highcharter)
library(tidyr)

ui <- fluidPage(
	fluidRow(
		column(6,
			dateRangeInput("te_date_range", "Date Range", min="2001-03-01", max="2020-12-01", start="2011-03-01", end="2020-03-01"),
			highchartOutput("transactionEvolution")
		),
		column(6,
			dateRangeInput("ndayaccounts_date_range", "Date Range", min="2001-03-01", max="2020-12-01", start="2011-03-01", end="2020-03-01"),
			highchartOutput("nDayAccounteEvolutionComparison")
		)
	),
	fluidRow(
		column(6,
			dateRangeInput("registeredaccag_date_range", "Date Range", min="2001-03-01", max="2020-12-01", start="2011-03-01", end="2020-03-01"),
			highchartOutput("registeredAccountsAgents")
		)
	),
)

server <- function(input, output) {
	mydata  <- read_excel("data/data.xlsx")
	transactionType = c("All","Airtime","Bill payments","Bulk payments","Cash-in","Cash-out","IR","Merchant payments","P2P")
	regionGlobalVolume <- mydata[mydata$Geo_view=="Region" & mydata$Geo_name == "Global" & mydata$Unit == "Volume" & mydata$Attribute %in% transactionType,] %>% 
	select(-Measure,-Geo_view,-Geo_name,-Unit,-Metric) %>% 
	as.data.frame()

	regionGlobalVolume <- regionGlobalVolume %>% pivot_longer(cols=names(regionGlobalVolume)[-1], names_to="date") %>% as.data.frame()
	regionGlobalVolume <- regionGlobalVolume %>% pivot_wider(names_from=Attribute, values_from=value) %>% as.data.frame() 
	regionGlobalVolume$date <- as.POSIXct(regionGlobalVolume$date, format="%d/%m/%Y", tz="UTC")
	regionGlobalVolume <- regionGlobalVolume[regionGlobalVolume$date>="2011-03-01",]

	hc_transaction_evolution <- highchart() %>% 
	hc_title(text="Evolution du nombre de transaction") %>%
	hc_xAxis(categories= regionGlobalVolume$date)
	for(n in names(regionGlobalVolume)[!names(regionGlobalVolume) %in% c("date", "All")]){
		hc_transaction_evolution <- hc_transaction_evolution %>% hc_add_series(data=regionGlobalVolume[[n]], type="column", name=n)
	}
	hc_transaction_evolution <- hc_transaction_evolution %>% hc_plotOptions(column=list(stacking="normal"))
	hc_transaction_evolution <- hc_transaction_evolution %>% hc_add_series(data=regionGlobalVolume$All, type="line", color="black", name="Total")
	output$transactionEvolution <- renderHighchart({hc_transaction_evolution})

	nDayAccountData <- mydata[mydata$Unit == "Accounts" & mydata$Attribute != "Registered",] %>% 
	select(-Measure, -Geo_view, -Geo_name, -Unit, -Metric) %>% 
	as.data.frame()

	nDayAccountData <-  nDayAccountData %>% 
	pivot_longer(cols=names(nDayAccountData)[-1], names_to="date") %>% 
	as.data.frame() %>% 
	group_by(Attribute, date) %>% 
	summarise(
		value=sum(value)
	)
	nDayAccountData <- nDayAccountData %>% pivot_wider(names_from = Attribute, values_from = value) %>% as.data.frame()
	nDayAccountData$date <- as.POSIXct(nDayAccountData$date, format="%d/%m/%Y", tz="UTC")
	nDayAccountData <- nDayAccountData[order(nDayAccountData$date),]
	nDayAccountData <- nDayAccountData[nDayAccountData$date>="2011-03-01",]

	hc_nday_accounts <- highchart() %>% 
	hc_title(text="Evolution active 30-day accounts vs active 90-day accounts") %>%
	hc_xAxis(categories= nDayAccountData$date)
	for(n in names(nDayAccountData)[-1]){
		hc_nday_accounts <- hc_nday_accounts %>% hc_add_series(data=nDayAccountData[[n]], type="column", name=n)
	}

	# stacking: "normal", "overlap", "percent", "stream"
	hc_nday_accounts <- hc_nday_accounts %>% hc_plotOptions(column=list(stacking=""))
	output$nDayAccounteEvolutionComparison <- renderHighchart({hc_nday_accounts})

	registeredAccountsAgents <- mydata[mydata$Unit %in% c("Accounts", "Agents") & mydata$Attribute=="Registered",] %>% 
	select(-Geo_view, -Geo_name, -Attribute, -Unit, -Metric) %>% 
	as.data.frame()

	registeredAccountsAgents <- registeredAccountsAgents %>% pivot_longer(cols = names(registeredAccountsAgents)[-1], names_to = "date") %>% 
	as.data.frame() %>% 
	group_by(Measure,date) %>% 
	summarise(value=sum(value))

	registeredAccountsAgents <- registeredAccountsAgents %>% pivot_wider(names_from = Measure, values_from = value) %>% as.data.frame()	
	registeredAccountsAgents$date <- as.POSIXct(registeredAccountsAgents$date, format="%d/%m/%Y", tz="UTC")
	registeredAccountsAgents <- registeredAccountsAgents[order(registeredAccountsAgents$date),]
	registeredAccountsAgents <- registeredAccountsAgents[registeredAccountsAgents$date>="2011-03-01",]

	hc_registeredAccountsAgents <- highchart() %>% 
	hc_title(text="Evolution registered accounts vs registered agents") %>%
	hc_xAxis(categories= registeredAccountsAgents$date)
	for(n in names(registeredAccountsAgents)[-1]){
		hc_registeredAccountsAgents <- hc_registeredAccountsAgents %>% hc_add_series(data=registeredAccountsAgents[[n]], type="line", name=n)
	}

	output$registeredAccountsAgents <- renderHighchart({hc_registeredAccountsAgents})
}

# Run the application 
shinyApp(ui = ui, server = server)
