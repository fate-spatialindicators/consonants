#PlotOutNC_MainFunction.r


library(ncdf4)

  ThisNC.nc<-nc_open("nanophytoplankton_bottom_era5_monthly_1980_2010.nc") 
  
  save(ThisNC.nc,file="nanophytoplankton_bottom_era5_monthly_1980_2010.Rdata")
  
  # JUST LIST ALL THE VARIABLES IN THE NCDF FILE
  # THIS CODE JUST PRINTS VARIABLE NAMES TO SCREEN, BUT CAN BE USEFUL FOR MAKING THE .CSV INPUT FILES. 
  print('LISTING ALL VARIABLE NAMES IN THE NETCDF FILE')
  numVarsInNC<-length(ThisNC.nc$var)
  ListOfVarsFromNC<-matrix(numVarsInNC,1)
  
  for (groupIndex in 1:numVarsInNC )   
  {
    thisVar <- ThisNC.nc$var[[groupIndex]]  
    #print(thisVar$name)    
    ListOfVarsFromNC[groupIndex]<-thisVar$name
  }
  write.csv(ListOfVarsFromNC,file="ListOfVarsFromNC.csv")
  print('THE LISTING ABOVE IS ALL VARIABLE NAMES IN CDF')
  print('THIS MAY BE USEFUL FOR MAKING .CSV INPUT FILES')
  print(' THE LIST OF VARIABLES IS SAVED AS ListOfVarsFromNC.csv')
  
  # variables are: 
  #nanophytoplankton
  #lon
  #lat
  #depth
  #year
  #month
  
  
  lat <- ncvar_get( ThisNC.nc,"lat") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  latDims<-dim(lat)  # Just use volume to see how many time steps are in the data

  lon<- ncvar_get( ThisNC.nc,"lon") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  lonDims<-dim(lon)  # Just use volume to see how many time steps are in the data
  
  nanophytoplankton<- ncvar_get( ThisNC.nc,"nanophytoplankton") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  nanophytoplanktonDims<-dim(nanophytoplankton)  # Just use volume to see how many time steps are in the data
  
  year<- ncvar_get( ThisNC.nc,"year") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  yearDims<-dim(year)  # Just use volume to see how many time steps are in the data
  
  
  month<- ncvar_get( ThisNC.nc,"month") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  monthDims<-dim(month)  # Just use volume to see how many time steps are in the data
  
  source("PlotNCmap.r")
  PlotNCmap(nanophytoplankton[,,6],"NanoPhytoPlankton",lon,lat)
    
  
  
  
  
  
  #library(sm)  # for pause()


#library(chron) #added 4/30/12
#remove.packages("compositions") # for mean.row
#remove.packages("fields") # for image.plot()
#library(graphics)
#remove.packages("MASS") # for write.matrix()
#remove.packages("matlab")
#library(grDevices)

#---------------------
# USER CAN DEFINE INPUT FILES, STARTYEAR, TIMESTEP, SAVE DIRECTORY, ETC. HERE:  
#Here are some things that are hardcoded, but change here if you want. 
#boxesToUse<-7

nc_close(ThisNC.nc)

                    







