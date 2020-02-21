# Here's a function to practice minimizing. The minimum is at (0,0)
XYSquared<-function(x)
{
  return ((x[1]^2+x[2]^2))
}

set.seed(345)   # don't forget to set the random nuber generator seed

# pass this finction the name of the function you are trying to minimize ("Fn"), the starting coordinates
# ("StartPoint"), the initial temperature, funal temperature, and proportion by which to decrease the temperature
# each iteration
SimAnneal2D<-function(Fn,StartPoint,InitialTemp,FinalTemp,TempDecreaseRate){
  StepSize<-0.5   # how much are we going to move by for each step?
  X<-StartPoint[1]
  Y<-StartPoint[2]
  # record the value the function takes at this point (call it FnVal, say)
  # ADD code to calculate FnVal here

  # Set the temperature (Temp) to = InitialTemp,
  # ADD CODE to set TEMP here

  plot(X,Y,xlim=c(-2*X,2*X),ylim=c(-2*Y,2*Y)) # set up a plot
  points(0,0,pch=19,col="red")  # plot the Minimum - this is where you are hoping to end up
  while (Temp>FinalTemp){  # we keep going until things cool down
    #propose new point
    NewX<-X+rnorm(1,mean=0,sd=StepSize)  # We use a random walk in which we add a normal rv with mean 0 to each coordinate
    NewY<-Y+rnorm(1,mean=0,sd=StepSize)
    New<-c(NewX,NewY)
    #decide whether to move
    NewVal<- Fn(New)  # the value the function takes at the new point

    # Calculate the value of H
    # ADD CODE FOR H HERE

        p<-runif(1)  # the random number that is going to help us decide whether to move
    if (p<h){ # we move
       # add an arrow to the plot showing where we moved:
      arrows(X,Y,NewX,NewY,length=0.05)
      # update your records of where we are:...X<-NewX   Y<-NewY
      # Add code here

      # pause for a bit - otherwise the plots flash by too quickly to see properly
      Start.Time<-Sys.time()
      while (Sys.time()<Start.Time+0.02){}   # there is probably a better way of doing this
      # plot the path you are taking
      points(X,Y,pch='.',col="blue")

      # update the record of the function value:
      FnVal<-NewVal
    }
    #reduce temperature
    Temp<-Temp*(1-TempDecreaseRate)

  }
  points(X,Y,pch=19,col="blue")   # a big blue point to show where we ended up
  return (c(X,Y,Fn(X,Y)))  # return the coordinates of our final resting place, and the function value
}

# Call the function like this (for this example we start at the point (5,5), with a temperature going from 10 to 0.1)
SimAnneal2D(XYSquared,c(5,5),10,0.1,0.02)
