
library(wqTools)
library(magrittr)
library(plotly)


ui <-fluidPage(
	
	# Header
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="High frequency data dashboard")
	),

	# Input widgets
	fluidRow(
		column(4,
			column(12,h4("Click a site"),
				shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7")
			)
		),
		column(8, h4("Plot window"),plotlyOutput("hf_plot"),
				tabsetPanel(id='tabs',
				  tabPanel("Assessments", column(12, div(DT::dataTableOutput("table0"), style = "font-size:100%"))),
					tabPanel("Summary stats", column(12, div(DT::dataTableOutput("table1"), style = "font-size:100%"))),
					tabPanel("Numeric criteria % exc", column(12, div(DT::dataTableOutput("table2"), style = "font-size:100%"))),					
					tabPanel("Criteria % exc", column(12, div(DT::dataTableOutput("table3"), style = "font-size:100%")))
				)
		)
	)
)

server <- function(input, output, session){

	## Loading modal to keep user out of trouble while map draws...
	#showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))

	## Remove modal when app is ready
	#observe({
	#	req(map)
	#	removeModal()
	#})

	# Load data
	load("data/all_hfdo_assessments.Rdata")
	master_site=readxl::read_excel("./data/IR_translation_workbook_working_2022.xlsx", sheet = "masterSiteTable")
	
	# Roll up to site
	site_use_asmt = all_HFDO_asmnts[,c("IR_MLID","BeneficialUse","IR_Cat")]
	site_use_asmt$rank = 1
	site_use_asmt$rank[site_use_asmt$IR_Cat=="NS"] = 2
	
	site_use_agg = aggregate(rank~IR_MLID+BeneficialUse, data = site_use_asmt, FUN = max)
	names(site_use_agg)[names(site_use_agg)=="rank"] = "IR_Cat"
	site_use_agg$IR_Cat[site_use_agg$IR_Cat==1] = "FS"
	site_use_agg$IR_Cat[site_use_agg$IR_Cat==2] = "NS"
	
	# Extract daily values
	daily_values=all_daily_values
	
	# Extract site locations
	sites=master_site[master_site$MonitoringLocationIdentifier %in% daily_values$IR_MLID,]
	
	# Match AU polygons to those present in sites
	data(au_poly)
	au_poly=au_poly[au_poly$ASSESS_ID %in% sites$ASSESS_ID,]
	
	# Extract 30 & 7 d means, expand to all site, date, & name combinations
	d307means=all_thirty_seven_means
	d307means$mid_date=(d307means$start_date+(d307means$AsmntAggPeriod)/2)
	d307means$name=paste(d307means$AsmntAggPeriod,d307means$AsmntAggPeriodUnit,d307means$AsmntAggFun)
	d307means=unique(d307means[,c("IR_MLID","mid_date","name","mean")])
	
	# mid_date=(seq(min(raw_data$ActivityStartDate), max(raw_data$ActivityStartDate), 1))
	# mid_date=data.frame(mid_date)
	# mid_date=merge(mid_date,sites$IR_MLID,all=T)
	# names(mid_date)[names(mid_date)=="y"]="IR_MLID"
	# mid_date=merge(mid_date, unique(d307means$name), all=T)
	# names(mid_date)[names(mid_date)=="y"]="name"
	# d307means1=merge(mid_date, d307means , all=T)

	
	# Empty reactive values object
	reactive_objects=reactiveValues()

	# Resources for returning site info on click:
	## https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
	## https://stackoverflow.com/questions/42613984/how-to-implement-inputmap-marker-click-correctly?noredirect=1&lq=1


	# Select map set up
    map = leaflet::createLeafletMap(session, 'map')

    session$onFlushed(once = T, function() {
		output$map <- leaflet::renderLeaflet({
			buildMap(sites=sites,au_poly=au_poly)
		})
    })
	
	
	# Map marker click (to identify selected site)
	observe({
		#req(profiles_long)
		site_click <- input$map_marker_click
		if (is.null(site_click)){return()}
		siteid=site_click$id
		reactive_objects$sel_mlid=siteid
	})

	# Grab data & available dates for selected site
	observe({
		req(reactive_objects$sel_mlid)
	  withProgress(message = "Generating raw data", value = 0.1,{
	    raw_df = do.call(rbind.data.frame, all_raw_data[lapply(all_raw_data, function(x) reactive_objects$sel_mlid %in% unique(x$IR_MLID))==TRUE])
	    setProgress(message = "Subsetting to selected MLID", value = 0.6)
	    raw_data = raw_df[raw_df$IR_MLID==reactive_objects$sel_mlid,]
	    raw_data$datetime=as.POSIXct(paste(raw_data$ActivityStartDate, raw_data$ActivityStartTime.Time), format="%Y-%m-%d %H:%M")
	    setProgress(message = "Creating reactive objects", value = 0.8)
	    reactive_objects$raw_data_sel = raw_data
	    #reactive_objects$raw_data_sel=raw_data[raw_data$IR_MLID %in% reactive_objects$sel_mlid,]
	    reactive_objects$d307means_sel=d307means[d307means$IR_MLID %in% reactive_objects$sel_mlid,]
	    reactive_objects$daily_values_sel=daily_values[daily_values$IR_MLID %in% reactive_objects$sel_mlid,]
	    reactive_objects$date_range=c(min(reactive_objects$raw_data_sel$ActivityStartDate),max(reactive_objects$raw_data_sel$ActivityStartDate))
	  })
	  	})
	
	## Date slider selection
	#output$date_slider <- renderUI({
	#	req(reactive_objects$date_range)
	#	date_min=reactive_objects$date_range[1]
	#	date_max=reactive_objects$date_range[2]
	#	sliderInput("date_slider", "Date range:", min=date_min, max=date_max, value=c(date_min,date_max))
	#})
	
	output$hf_plot=renderPlotly({
		req(reactive_objects$daily_values_sel)
		isolate({
			withProgress(message="Prepping raw data...", value=0.25,{
			raw_data_plot=unique(reactive_objects$raw_data_sel[,c("IR_MLID","ActivityStartDate","datetime","IR_Value")])
				raw_data_plot$key=raw_data_plot$datetime
				setProgress(message="Drawing plot...", value=0.65)
				raw_data_plot=raw_data_plot[order(raw_data_plot$datetime),]	
				criteria=unique(reactive_objects$raw_data_sel[,c("IR_MLID","ActivityStartDate","DailyAggFun","NumericCriterion","AsmntAggPeriod","AsmntAggPeriodUnit","ParameterQualifier","BeneficialUse")])
				criteria$ParameterQualifier[criteria$ParameterQualifier=="early life stages are present"] = "ELS"
				criteria$ParameterQualifier[criteria$ParameterQualifier=="other life stages present"] = "OLS"
				criteria$ParameterQualifier[is.na(criteria$ParameterQualifier)] = ""
				criteria$name=paste(criteria$BeneficialUse,criteria$AsmntAggPeriod,criteria$AsmntAggPeriodUnit,criteria$DailyAggFun, criteria$ParameterQualifier,"criterion")
				criteria$NumericCriterion=as.numeric(criteria$NumericCriterion)
				reactive_objects$criteria=criteria
				reactive_objects$applicable_crit=criteria
				daily_values=unique(reactive_objects$daily_values_sel[,c("IR_MLID","ActivityStartDate","DailyAggFun","IR_Value")])
				daily_means=daily_values[daily_values$DailyAggFun=="mean",]
				daily_mins=daily_values[daily_values$DailyAggFun=="min",]
				title=paste(reactive_objects$sel_mlid)
								
				setProgress(value=0.9)
				plot_ly(raw_data_plot, x=~datetime, source="a") %>%
					config(displaylogo = FALSE, collaborate = FALSE) %>% 
					#add_trace(type='scatter', y = ~IR_Value, name = '"Raw data', mode = 'lines+markers') %>%
					add_lines(y=~IR_Value, name="Raw data", line=list(color = 'rgb(180, 180, 180, 0.5)')) %>%
					add_markers(data=daily_means, x=~ActivityStartDate, y=~IR_Value, name="Daily mean", marker=list(color = 'rgba(45, 100, 185,0.5)', line=list(color = 'rgb(45, 100, 185)', width=2))) %>%
					add_markers(data=daily_mins, x=~ActivityStartDate, y=~IR_Value, name="Daily min", marker=list(color = 'rgba(230, 150, 0, 0.5)', line=list(color = 'rgb(230, 150, 0)', width=2))) %>%
					add_trace(type="scatter",data=reactive_objects$d307means_sel, x=~mid_date, y=~mean, split=~name, connectgaps=FALSE, marker = list(size=1), line = list(width = 5), mode = 'lines+markers') %>%
					add_lines(data=criteria, x=~ActivityStartDate, y=~NumericCriterion, split=~name, connectgaps=FALSE) %>%
					layout(title = title,
						xaxis = list(title = ""),
						yaxis = list(side = 'left', title = 'Dissolved oxygen (mg/l)')
					) %>% 
				config(displaylogo = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'hoverClosestCartesian',
						'hoverCompareCartesian',
						'lasso2d'
					))
			})
		
		
		})
	})
	
	# Generate selected data from plot for tables
	observe({
		select_data  <- event_data("plotly_selected", source="a")
		if(!is.null(select_data) & any(!is.na(select_data$x)) & any(!is.na(select_data$y))){
			x_range=list(min(select_data$x),max(select_data$x))
			y_range=list(min(select_data$y),max(select_data$y))
			
			daily_values_for_table=unique(reactive_objects$daily_values_sel[,c("IR_MLID","ActivityStartDate","DailyAggFun","IR_Value")])
			daily_values_for_table=daily_values_for_table[
				daily_values_for_table$ActivityStartDate >= x_range[[1]] & daily_values_for_table$ActivityStartDate <= x_range[[2]] &
				daily_values_for_table$IR_Value >= y_range[[1]]*0.95 & daily_values_for_table$IR_Value <= y_range[[2]]*1.05
			,]
			d307_means_for_table=na.omit(reactive_objects$d307means_sel)
			d307_means_for_table=d307_means_for_table[
				d307_means_for_table$mid_date >= x_range[[1]] & d307_means_for_table$mid_date <= x_range[[2]] &
				d307_means_for_table$mean >= y_range[[1]]*0.95 & d307_means_for_table$mean <= y_range[[2]]
			,]
		}else{
			daily_values_for_table=unique(reactive_objects$daily_values_sel[,c("IR_MLID","ActivityStartDate","DailyAggFun","IR_Value")])
			d307_means_for_table=reactive_objects$d307means_sel
		}

		reactive_objects$daily_values_for_table=daily_values_for_table
		reactive_objects$d307_means_for_table=d307_means_for_table
	})
	
	
	# Generate table data
	observe({
		req(reactive_objects$daily_values_for_table,reactive_objects$d307_means_for_table)
		dv=reactive_objects$daily_values_for_table
		dv$name=paste("Daily", dv$DailyAggFun)
		dv=dv[,c("name","IR_Value")]
		names(dv)[names(dv)=="IR_Value"]="value"
		d307means=reactive_objects$d307_means_for_table
		d307means=d307means[,c("name","mean")]
		names(d307means)[names(d307means)=="mean"]="value"
		table1_data=na.omit(rbind(dv,d307means))
		names(table1_data)[names(table1_data)=="name"]="Attribute"
		#table1_data <<- table1_data
		#criteria <<- reactive_objects$criteria
		t2data=table1_data
		criteria=reactive_objects$criteria
		t2criteria=unique(criteria$NumericCriterion)
		t2data=merge(t2data,t2criteria, all.x=T)
		names(t2data)[names(t2data)=="y"]="Criterion"
		t2data$exc=ifelse(t2data$value<t2data$Criterion,1,0)
		exc=aggregate(exc~Criterion+Attribute, t2data, FUN='sum')
		cnt=aggregate(exc~Criterion+Attribute, t2data, FUN='length')
		names(cnt)[names(cnt)=="exc"]="cnt"
		table2_data=merge(exc,cnt)
		table2_data$pct_exc=table2_data$exc/table2_data$cnt *100
		table2_data$Criterion=paste(table2_data$Criterion,"mg/l")
		
		t3data=table1_data
		t3criteria=unique(criteria[,c("name","NumericCriterion")])
		t3data=merge(t3data,t3criteria, all.x=T)
		t3data$exc=ifelse(t3data$value<t3data$NumericCriterion,1,0)
		exc=aggregate(exc~name+Attribute+NumericCriterion, t3data, FUN='sum')
		cnt=aggregate(exc~name+Attribute+NumericCriterion, t3data, FUN='length')
		names(cnt)[names(cnt)=="exc"]="cnt"
		table3_data=merge(exc,cnt)
		
		table3_data=within(table3_data,{
			pct_exc=exc/cnt *100
			Attribute[Attribute=="Daily min"]="1 day min"
			crit=name
			pq = ""
			pq[grepl("ELS", table3_data$crit)] = "Early Life Stages"
			pq[grepl("OLS", table3_data$crit)] = "Other Life Stages"
			crit=gsub(" ELS criterion","",crit)
			crit=gsub(" OLS criterion","",crit)
			crit=gsub(" criterion","",crit)
			crit=substring(crit, 4)
			crit=gsub("min ","min",crit)
			crit=gsub("mean ","mean",crit)
			keep=ifelse(Attribute==crit,1,0)
			})
		
		data_0 = site_use_agg[site_use_agg$IR_MLID==reactive_objects$sel_mlid,]
		table0_data = reshape2::dcast(data_0, IR_MLID~BeneficialUse, value.var = "IR_Cat")
		
		table3_data=table3_data[table3_data$keep==1,]
		reactive_objects$table0_data=table0_data
		reactive_objects$table1_data=table1_data
		reactive_objects$table2_data=table2_data
		reactive_objects$table3_data=table3_data
	})
	
	# Table 0
	output$table0=DT::renderDataTable({
	  req(reactive_objects$table0_data)
	  reactive_objects$table0_data %>%
	    DT::datatable(selection='none', rownames=F,
	                  options = list(scrollY = '250px', paging = FALSE, scrollX = TRUE, searching=F, dom = 't'))
	})

	# Table 1
	output$table1=DT::renderDataTable({
		req(reactive_objects$table1_data)
		reactive_objects$table1_data %>%
			group_by(Attribute) %>%
			dplyr::summarize_all(c(Min="min",Mean="mean",Med="median",Max="max")) %>%
			DT::datatable(selection='none', rownames=F,
			options = list(scrollY = '250px', paging = FALSE, scrollX = TRUE, searching=F, dom = 't')) %>%
				DT::formatRound(c(2:5), 2)
	})
	
	# Table 2
	output$table2=DT::renderDataTable({
		req(reactive_objects$table2_data)
		table2_data=reactive_objects$table2_data
		table2=reshape2::dcast(table2_data, Attribute~Criterion, value.var="pct_exc")
		DT::datatable(table2, selection='none', rownames=F,
			options = list(scrollY = '200px', paging = FALSE, scrollX = TRUE, searching=F, dom = 't')) %>%
				DT::formatRound(c(2:dim(table2)[2]), 2)
	})
	
	## Table 3
	output$table3=DT::renderDataTable({
		req(reactive_objects$table3_data)
		table3_data=reactive_objects$table3_data
		table3_data$name = paste0(table3_data$name,"-",table3_data$NumericCriterion)
		table3=reshape2::dcast(table3_data, Attribute~name, value.var="pct_exc")
		DT::datatable(table3, selection='none', rownames=F,
			options = list(scrollY = '200px', paging = FALSE, scrollX = TRUE, searching=F, dom = 't')) %>%
				DT::formatRound(c(2:dim(table3)[2]), 2)
	})


}

## run app
shinyApp(ui = ui, server = server)
