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
	output$transactionEvolution <- renderHighchart({
		transactionType = c("All","Airtime","Bill payments","Bulk payments","Cash-in","Cash-out","IR","Merchant payments","P2P")
		regionGlobalVolume <- mydata[mydata$Geo_view=="Region" & mydata$Geo_name == "Global" & mydata$Unit == "Volume" & mydata$Attribute %in% transactionType,] %>% 
		select(-Measure,-Geo_view,-Geo_name,-Unit,-Metric) %>% 
		as.data.frame()

		regionGlobalVolume <- regionGlobalVolume %>% pivot_longer(cols=names(regionGlobalVolume)[-1], names_to="date") %>% as.data.frame()
		regionGlobalVolume <- regionGlobalVolume %>% pivot_wider(names_from=Attribute, values_from=value) %>% as.data.frame() 
		regionGlobalVolume$date <- as.POSIXct(regionGlobalVolume$date, format="%d/%m/%Y", tz="UTC")
		regionGlobalVolume <- regionGlobalVolume[regionGlobalVolume$date>=input$te_date_range[1],]

		highchart() %>% 
		hc_title(text="Evolution du nombre de transaction") %>%
		hc_xAxis(categories= regionGlobalVolume$date) %>%
		hc_add_series(data=regionGlobalVolume[["Airtime"]], type="column", name="Airtime") %>%
		hc_add_series(data=regionGlobalVolume[["Bill payments"]], type="column", name="Bill payments") %>%
		hc_add_series(data=regionGlobalVolume[["Bulk payments"]], type="column", name="Bulk payments") %>%
		hc_add_series(data=regionGlobalVolume[["Cash-in"]], type="column", name="Cash-in") %>%
		hc_add_series(data=regionGlobalVolume[["Cash-out"]], type="column", name="Cash-out") %>%
		hc_add_series(data=regionGlobalVolume[["IR"]], type="column", name="IR") %>%
		hc_add_series(data=regionGlobalVolume[["Merchant payments"]], type="column", name="Merchant payments") %>%
		hc_add_series(data=regionGlobalVolume[["P2P"]], type="column", name="P2P") %>%
		hc_plotOptions(column=list(stacking="normal")) %>%
		hc_add_series(data=regionGlobalVolume$All, type="line", color="black", name="Total")
	})

	output$nDayAccounteEvolutionComparison <- renderHighchart({
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
		nDayAccountData <- nDayAccountData[nDayAccountData$date>=input$ndayaccounts_date_range,]

		highchart() %>% 
		hc_title(text="Evolution active 30-day accounts vs active 90-day accounts") %>%
		hc_xAxis(categories= nDayAccountData$date) %>%
		hc_add_series(data=nDayAccountData[["Active, 30-day"]], type="column", name="Active, 30-day") %>%
		hc_add_series(data=nDayAccountData[["Active, 90-day"]], type="column", name="Active, 90-day") %>%
		hc_plotOptions(column=list(stacking=""))
	})

	output$registeredAccountsAgents <- renderHighchart({
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
		registeredAccountsAgents <- registeredAccountsAgents[registeredAccountsAgents$date>=input$registeredaccag_date_range,]

		highchart() %>% 
		hc_title(text="Evolution registered accounts vs registered agents") %>%
		hc_xAxis(categories= registeredAccountsAgents$date) %>% 
		hc_add_series(data=registeredAccountsAgents[["Registered Accounts"]], type="line", name="Registered Accounts") %>% 
		hc_add_series(data=registeredAccountsAgents[["Registered Agents"]], type="line", name="Registered Agents")
	})

}

# Run the application 
shinyApp(ui = ui, server = server)
