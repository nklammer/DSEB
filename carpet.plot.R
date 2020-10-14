# AREN 5030 DSEB
# Gregor P. Henze
# October 2017

##################################################################################
#									         			   
#	carpet.plot(from, to, data, cs) -> graphs a carpet plot for a specified      
#							timeframe              	               		           
#	                                                                                                                                                           
#	- Required are packages "lattice" and "grid", by default they should       
#	  be included in folder "library" and are loaded within carpet.plot        
#	- First column of data has to be Time, showing 1 hour     	               											    	
#	  measurements, format has to be "m.d.YYYY hh:mm" 		                  
#	  e.g "1/1/2011 0:00"                                                   
#	- from <- "1/1/2011 0:00" has to start at midnight                      
#	- to   <- "1/31/2011 23:00" until 23th hour of day                        
#	- cs has to be in the form: c(number, number,......,number) with           
#	  number being the columns of the data table that should be graphed                     
#	- name of graphed variable should be e.g: T_a_?C, will be used as 	   
#	  graph description   		                                         	   	
#				                                                 	   	
# Function is called e.g. like this:                                             
#                                                                                 
#	carpet.plot("2007.11.22 00:00","2008.06.25 23:00", hourly_d, c(2,3) )                          
#													   				    	
#	                                                                     	    	 		    
##################################################################################

carpet.plot <- function(from, to, data, cs){
	#Select time frame specified with "from" and "to"
	if(length(grep(" 0:00",from)) != 1) {
			print("selected start time doesn't begin at 00:00 o'clock")
			}else{
			
	if(length(grep(" 23:00",to)) != 1) {
			print("selected end time doesn't stop at 23:00 o'clock")
			}else{
    data <- data[c(grep(from,as.character(data[,2])):grep(to,
						as.character(data[,2]))),]
	
	#Extract the time format
	d <- strptime(data[,2], "%m/%d/%Y %H:%M")
	day_stamp <- format(d, "%Y.%m.%d")
	n <- length(cs)
	
	#Extract the amount of days 
	t <- unique(day_stamp)
	days <- NROW(t)

	#Extract days as POSIXct format, is x-axis
	t.1 <- strptime(t,"%Y.%m.%d")
	x <- as.POSIXct(t.1)

	#b is amount of tick marks on x-axis
	b <- days/15 

	#Make color platte
	self.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
  			"#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
	
	#loading necessary packages
	library(lattice)
	library(grid)
	
	#specifiying graphics device parameters if more than one graph per page
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(n,1)))

	#Creating Matrix M for plotting, n is amount of columns specified with cs()
	for(i in 1:n){
	M <- t(array(data[,cs[i]], dim=c(24,days)))	

	#Specifiying position of graph if more than one plot per page
	pushViewport(viewport(layout.pos.col=1, layout.pos.row=i)) 

	#actual graph command including color scheme, x-axis, labels
	print(levelplot(M,row.values = x,  aspect="fill",contour=F, region=T, 
		col.regions= self.colors(20), xlab=
		names(data)[cs[i]], 
		ylab="Hour", 
		scales=list( x=list(
			at = seq(min(x), max(x), by = paste(round(b,0), 
			"day", sep =" ")), format="%d.%b")) ),
		newpage = F) 

	#lifting Vieport 1 level up, necessary for subsequent graphs
	popViewport(1)	
	}
	popViewport()
}}}