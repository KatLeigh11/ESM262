# the function kat_football_stats calculates the catch percentage and catches per minute of a round of receiver practice for Kat. To account for variations in wind speed, wind direction, and light between various days, correction factors are included (default values of 0.03, -0.05, and 0.05 respectively).

kat_football_stats = function(throws, catches, minutes, wind_speed = 0.03, wind_direction = -0.05, light = 0.05)
  {
  throws = ifelse((throws < 0), return("throws must be greater than zero"), throws)
  catches = ifelse((catches <0), return("catches must be greter than or equal to zero"), catches)
  minutes = ifelse((minutes <0), return("minutes must be greater than zero"), minutes)
  wind_speed = ifelse((wind_speed >1), return("speed must be less than one"), wind_speed)
  wind_direction = ifelse((abs(wind_direction) >1), return("absolute value of direction must be less than one"), wind_direction)
  light = ifelse((!(light %in% c(0.05, 0))), return("light must be a value of 0.05 or zero"), light)
  catch_percentage = ((catches+wind_speed+wind_direction+light)/throws)*100
  catches_per_minute = (catches+wind_speed+wind_direction+light)/minutes
  
  return(c(catch_percentage, catches_per_minute))
}

kat_football_stats(100,20,50, light = 5)
