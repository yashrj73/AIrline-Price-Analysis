                           #SIX AIRLINES DATA

#Reading the data into R
SixAirlines.df <- read.csv(paste("SixAirlinesData.csv"))

#Summary statistics
summary(SixAirlines.df)

                            #AIRFRANCE AIRLINES

#Creating subset of AirFrance airlines
AirFrance.df <- subset(SixAirlines.df, Airline=="AirFrance")

#Summary statistics
summary(AirFrance.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft,data= AirFrance.df)
summary(fit)

#Significant Factor(s): Fraction of Premium Seats, Flight Duration, Total Seats and Type of Aircraft
#Scatter plot of significant factors vs Relative Price:

#PriceRelative vs FractionPremiumSeats
plot(PriceRelative~FractionPremiumSeats,data = AirFrance.df,ylim = c(0,2),ylab="Relative Price")

#PriceRelative vs FlightDuration  
plot(PriceRelative~FlightDuration,data = AirFrance.df,ylim = c(0,2),ylab="Relative Price")

#PriceRelative vs SeatsTotal
plot(PriceRelative~SeatsTotal,data = AirFrance.df,ylim = c(0,2),ylab="Relative Price",xlim=c(100,500))

#PriceRelative vs Aircraft
plot(PriceRelative~Aircraft,data = AirFrance.df,ylim = c(0,2),ylab="Relative Price")

                           #BRITISH AIRLINES

#Creating subset of British airlines
British.df <- subset(SixAirlines.df, Airline=="British")

#Summary statistics
summary(British.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft,data= British.df)
summary(fit)

#Significant Factor(s): Flight Duration, Total Seats, Type of Aircraft.
#Scatter plot of significant factors vs Relative Price:
  
#PriceRelative vs FlightDuration
plot(PriceRelative~FlightDuration,data = British.df,ylim = c(0,2),ylab="Relative Price")

#PriceRelative vs SeatsTotal
plot(PriceRelative~SeatsTotal,data = British.df,ylim = c(0,2),ylab="Relative Price",xlim=c(100,400)

#PriceRelative vs Aircraft
plot(PriceRelative~Aircraft,data = British.df,ylim = c(0,1.5),ylab="Relative Price")
     
                           #DELTA AIRLINES

#Creating subset of Delta airlines
Delta.df <- subset(SixAirlines.df, Airline=="Delta")

#Summary statistics
summary(Delta.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft, x8=IsInternational
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft+IsInternational,data= Delta.df)
summary(fit)

#Significant Factor(s): Width Difference, Flight Duration and Travel Month.
#Scatter plot of significant factors vs Relative Price:

#PriceRelative vs WidthDifference
plot(PriceRelative~WidthDifference,data = Delta.df,ylim = c(0,0.6),ylab="Relative Price")

#PriceRelative vs FlightDuration
plot(PriceRelative~FlightDuration,data = Delta.df,ylim = c(0,0.6),ylab="Relative Price")

#PriceRelative vs TravelMonth
plot(PriceRelative~TravelMonth,data = Delta.df,ylim = c(0,0.6),ylab="Relative Price",xlim=c(0,10))

                           #JET AIRLINES

#Creating subset of Jet airlines
Jet.df <- subset(SixAirlines.df, Airline=="Jet")

#Summary statistics
summary(Jet.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft 
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft,data= Jet.df)
summary(fit)

#Significant Factor(s): Width Difference, Fraction of Premium Seats, Flight Duration and Total Seats.
##Scatter plot of significant factors vs Relative Price:

#PriceRelative vs WidthDifference
plot(PriceRelative~WidthDifference,data = Jet.df,ylim = c(0,2),ylab="Relative Price")

#PriceRelative vs FractionPremiumSeats
plot(PriceRelative~FractionPremiumSeats,data = Jet.df,ylim = c(0,2),ylab="Relative Price")

#PriceRelative vs FlightDuration
plot(PriceRelative~FlightDuration,data = Jet.df,ylim = c(0,2),ylab="Relative Price",xlim=c(0,10))

#PriceRelative vs SeatsTotal
plot(PriceRelative~SeatsTotal,data = Jet.df,ylim = c(0,2),ylab="Relative Price",xlim=c(0,200))

                          #SINGAPORE AIRLINES

#Creating subset of Singapore airlines
Singapore.df <- subset(SixAirlines.df, Airline=="Singapore")

#Summary statistics
summary(Singapore.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft,data= Singapore.df)
summary(fit)

#Significant Factor(s): Fraction of Premium Seats.
#Scatter plot of significant factors vs Relative Price:

#PriceRelative vs FractionPremiumSeats
plot(PriceRelative~FractionPremiumSeats,data = Singapore.df,ylim = c(0,1.5),ylab="Relative Price")

                        #VIRGIN AIRLINES

#Creating subset of Virgin airlines
Virgin.df <- subset(SixAirlines.df, Airline=="Virgin")

#Summary statistics
summary(Virgin.df)

#Formulating a regression model: y=b0+b1*x1+b2*x2+..
#where y= PriceRelative
#x1= WidthDifference, x2= PitchDifference, x3= FractionPremiumSeats, x4= FlightDuration
#x5= FlightDuration, x6= TravelMonth, x7= Aircraft
#Fitting a Linear Regression Model using lm()
fit <-lm(PriceRelative~WidthDifference+PitchDifference+FractionPremiumSeats+FlightDuration+SeatsTotal+TravelMonth+Aircraft,data= Virgin.df)
summary(fit)

#Significant Factor(s): Null

                           #Additional Analysis

#Box plot of Flight duration vs Airlines
boxplot(SixAirlines.df$FlightDuration~SixAirlines.df$Airline, ylim = c(0,15),xlab="Airlines", ylab="Flight Duration")

#Bar plot of Total no of flights v Airlines
plot(SixAirlines.df$Airline, ylim = c(0,200),xlab="Airlines", ylab="Total no. of flights")

#Bar plot of Number of aircrafts v Aircraft manufacturers
plot(SixAirlines.df$Aircraft, ylim = c(0,400),xlab="Aircraft Manufacturers", ylab="No of aircrafts")

#Bar plot of No of flights vs Type of flight
plot(SixAirlines.df$IsInternational, ylim = c(0,500),xlab="Type of flight", ylab="No of flights")
