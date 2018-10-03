 
#	Import the Bollywood data set in Rstudio in a variable named bollywood

  bollywood <- read.csv("bollywood.csv")

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()

  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
     
	last_10 <- bollywood$Movie[(length(bollywood$Movie)-9):length(bollywood$Movie)]
	 

#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
	na_bollywood <- sum(is.na(bollywood))
	  

#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
  top_movie <-bollywood$Movie[which.max(bollywood$Tcollection)]

  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  rank<-sort(bollywood$Tcollection,decreasing = TRUE)
  top_2_movie <- bollywood[which(rank[2]==bollywood$Tcollection),1]
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

  View(shahrukh)
  View(akshay)
  View(amitabh)
  
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
  shahrukh_collection <- sum(shahrukh$Tcollection)
    
	akshay_collection <- sum(akshay$Tcollection)
    
	amitabh_collection <- sum(amitabh$Tcollection)
    
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

Number_Flop_Movies<-length(which(bollywood$Verdict=="Flop"))
Number_Average_Movies<-length(which(bollywood$Verdict=="Average"))
Number_Hit_Movies<-length(which(bollywood$Verdict=="Hit"))
Number_Superhit_Movies<-length(which(bollywood$Verdict=="Super Hit"))

#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
max_value<- sapply(bollywood[ , 4:7], max, na.rm=TRUE)

#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

#Function returns the name of the movie with maximum collection basis Ocollection, Wcollection, Fwcollecion or Tcollection
  
  movie_name<-function(p)           
  {
    maximum<-max(bollywood[ ,p],na.rm=TRUE)
    for (i in 1:nrow(bollywood)) 
    {
      if(is.na(bollywood[i,p])!=TRUE)
      {
        a<-bollywood[i,p]
      }
      if(maximum==a)
      {
        movie<-bollywood[i,1]}
      }
    return(movie)
  }
  
  movie_result <-sapply(4:7, movie_name)
    
    
    