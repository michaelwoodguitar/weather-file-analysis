mod.gen <- function(p,fileName, base8){

  
    # base8 <- 'baseV8_DSY_test_office.idf' # I've already added this in now
    # fileName <- 'newfile.idf'
    
    file.remove(fileName) # remove any previous versions of the file  
    # Dummy variables - Need to delete!
    nominalPower = 1
    # p = rnorm(100, 20, 1) # this is just a randomenum
    
    # [ ] Need to find out what the function 'copyfile()' does.... (does this make a copy of the basefile.idf)
    # [ ] Need to update the 'writeCoordinates' function (this is an alfonso function I think) - note that there are
    #     several instances of the writeCoordinates function
    # 
    # 
    # fileConn<-file(target.location) # Open file connection
    # writeLines(file.edited, con=fileConn,sep="\n") # write to the file
    # close(fileConn) # close the connection
  
    e = define_e()
    site = generateSiteValues(p)

    if (e$Sun == 0){
        sunExp <- 'noSun'
    } else {
        sunExp <- 'SunExposed'
    }

    if (e$Wind == 0){
        windExp = 'noWind'
    } else {
        windExp = 'windExposed'
    }

    
    
    Rroof = 0.0127/0.84 + 0.0001/1 + 0.0125/0.21 + (0.04+0.17) #  %0.35171;
    
    nominalPower = 2*40*site$totalUA

    # %generating the points of the box

    # %      8  0--------0 7    
    # %       / |       /|
    # %    5 0---------06|
    # %      |  |      | |
    # %      |4 0------|-0 3      
    # %      | /       |/      
    # %    1 0---------0 2
    # %         /
    # %  +X -------
    # %       /
    # %      S=-Y (a)

    a = site$a
    b = site$b
    
    p1 = c(-a/2, -b/2,  0)
    p2 = c( a/2, -b/2,  0)
    p3 = c( a/2,  b/2,  0)
    p4 = c(-a/2,  b/2,  0)

    p5 = c(-a/2, -b/2,  2.8*p[3])
    p6 = c( a/2, -b/2,  2.8*p[3])
    p7 = c( a/2,  b/2,  2.8*p[3])
    p8 = c(-a/2,  b/2,  2.8*p[3])

   # % base8 = 'E:\MaTLAB_ME_toolbox\Toolbox\ForMatt_run_EP_Alfonso\code\Sensitivity_in_U_Value_calculations\Building_generator/baseV8.idf';
    # %Inserting surfaces
    # str = paste(fileName,'/',fileName, sep='')
    # str <- fileName
    
    # %delete(str
    # %system(str);
    # %str = sprintf('copy baseV8.idf %s.idf',fileName);
    
    # copyfile(base8,str) # this is Matt's old code...
    file.copy(base8, fileName) # makes a copy of the base file.
              
              
   # % system(str);
   # % str = sprintf('%s.idf',fileName);
    # fileName = fopen(str,'a');
    # fileConn<-file(str) # Open file connection
    # write('bood',file=fileName,append=T)
    
    
    # writeLines('test', con=fileConn,sep="\n")
    write(file=fileName,sprintf('\r\n\r\n'), append = T)

    
   # % Adding heating system
   # % fprintf(fileName,'ZoneHVAC:Baseboard:Convective:Electric,Electric heater,Always on,%f,1;\r\n',nominalPower);                       
   # % fprintf(fileName,'ZoneHVAC:Baseboard:Convective:Electric,Electric heater,Binary_Occupancy,%f,1;\r\n',nominalPower);                       
    write(file=fileName,sprintf('ZoneHVAC:Baseboard:Convective:Electric,Electric heater,Occupancy binary year,%f,1;\r\n',nominalPower), append = T)       
    
    
    # %Sky temperature
    if (e$sky == 0){
        write(file=fileName,'Schedule:Constant,skyT,, 10;\r\n', append=T)
        write(file=fileName,'WeatherProperty:SkyTemperature,Year,ScheduleValue,skyT;\r\n', append=T)
    }
    
    # %make tiles change solar absorption
    write(file=fileName,sprintf('Material,Tiles,MediumRough,0.0127,0.84,1900,800,,%f,;\r\n\r\n',p[32]), append=T)
    
    # %resistance of walls
    write(file=fileName,sprintf('Material,Concrete,MediumRough,%f,1.4,2100,840,,,;\r\n\r\n',p[31]), append = T)
        
    Rwall <- 0.1016/0.89 + p[31]/1.4  + (0.04+0.13) # %+ 0.059524%Km2/K + surface resistance
    
    # %Calculating the insulation to calculate the U-Value
    t_insulation_walls = 0.03*((1/p[6]-Rwall))

    write(file=fileName,sprintf('Material,InsulationWalls,MediumRough,%f,0.03,43,1210,,,;\r\n\r\n',t_insulation_walls), append = T)

    t_insulation_roof = 0.03*((1/p[7]-Rroof))

    write(file=fileName,sprintf('Material,InsulationRoof,MediumRough,%f,0.03,43,1210,,,;',t_insulation_roof), append = T)

    if (e$radiation == 0){
        if (e$dynamics==T){
            write(file=fileName,sprintf('Material,M01 100mm brick,MediumRough,0.1016,0.89,1920,790,0.001,0.001,0.001;\r\n'), append = T)
        } else {
            write(file=fileName,sprintf('Material,M01 100mm brick,MediumRough,0.1016,0.89,1,100,0.001,0.001,0.001;\r\n'), append = T)
        }
        write(file=fileName,sprintf('WindowMaterial:Glazing,CLEAR 6MM,SpectralAverage,,0.006,0.775,0.071,0.071,0.881,0.080,0.080,0.0,0.001,0.001,0.9,,,,;\r\n'), append = T)
        write(file=fileName,sprintf('WindowMaterial:Glazing,CLEAR 3MM,SpectralAverage,,0.003,0.837,0.075,0.075,0.898,0.081,0.081,0.0,0.001,0.001,0.9,,,,;\r\n'), append = T)
        write(file=fileName,sprintf('WindowMaterial:Glazing,LoE CLEAR 3MM Rev,SpectralAverage,,0.003,0.630,0.220,0.190,0.850,0.079,0.056,0.0,0.001,0.001,0.9,,,,;\r\n'), append = T)
    } else {
        if (e$dynamics==T){
            write(file=fileName,sprintf('Material,M01 100mm brick,MediumRough,0.1016,0.89,1920,790,,,;\r\n'), append = T)
        } else {
            write(file=fileName,sprintf('Material,M01 100mm brick,MediumRough,0.1016,0.89,100,1,,,;\r\n'), append = T)
        }
        write(file=fileName,sprintf('WindowMaterial:Glazing,CLEAR 6MM,SpectralAverage,,0.006,0.775,0.071,0.071,0.881,0.080,0.080,0.0,0.84,0.84,0.9,,,,;\r\n'), append = T)
        write(file=fileName,sprintf('WindowMaterial:Glazing,CLEAR 3MM,SpectralAverage,,0.003,0.837,0.075,0.075,0.898,0.081,0.081,0.0,0.84,0.84,0.9,,,,;\r\n'), append = T)
        write(file=fileName,sprintf('WindowMaterial:Glazing,LoE CLEAR 3MM Rev,SpectralAverage,,0.003,0.630,0.220,0.190,0.850,0.079,0.056,0.0,0.10,0.84,0.9,,,,;\r\n'), append = T)
        write(file=fileName,sprintf('WindowMaterial:Glazing,LoE CLEAR 6MM,SpectralAverage,,0.006,0.630,0.220,0.190,0.850,0.079,0.056,0.0,0.84,0.1,0.9,,,,;\r\n'), append = T)
    }

    # %Convection
    if (e$convection == 0){
        write(file=fileName,sprintf('SurfaceProperty:ConvectionCoefficients:MultipleSurface,AllExteriorSurfaces,Outside,Value, 25,,,Inside,Value, 7,,;\r\n'), append = T)
    }

    # %floor
    if (p[1] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,Floor,floor,floorConstruction,Main,Adiabatic,,NoSun,NoWind,autocalculate,autocalculate,'), append = T)
    } else {
        write(file=fileName,sprintf('BuildingSurface:Detailed,Floor,floor,floorConstruction,Main,Ground,,NoSun,NoWind,autocalculate,autocalculate,'), append = T)
    }
    
    writeCoordinates(fileName,p4,p3,p2,p1) # ***************************************************** TO DO

    # %Roof
    if (p[1] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,Roof,roof,roofConstruction,Main,Adiabatic,,NoSun,NoWind,autocalculate,autocalculate,'), append = T)  
    } else {
        write(file=fileName,sprintf('BuildingSurface:Detailed,Roof,Roof,roofConstruction,Main,outdoors,,%s,%s,autocalculate,autocalculate,',sunExp,windExp), append=T)
    }
    writeCoordinates(fileName,p5,p6,p7,p8) # ***************************************************** TO DO

    # %Calculating the walls that are adiabatic
    adiabatic = c(0, 0, 0, 0) #% N S E W
    if (p[1] == 2){
        adiabatic[p[4]] <- 1 # ***************************************************** TO DO
    }
    
    if (p[1] == 3){
        adiabatic(p[4]) = 1 # ***************************************************** TO DO
        if (p[4] > 2){
            adiabatic[p[4]-2] <- 1
        } else {
            adiabatic[p[4]+2] <- 1
        }
    }

    # %North face
    if (adiabatic[1] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,NorthWall,wall,WallConstruction,Main,Adiabatic,,NoSun,NoWind,0,autocalculate,'), append = T)
    } else {
        write(file=fileName,sprintf('BuildingSurface:Detailed,NorthWall,wall,WallConstruction,Main,outdoors,,%s,%s,0,autocalculate,',sunExp,windExp), append = T)
    }
    
    writeCoordinates(fileName,p3,p4,p8,p7) # ***************************************************** TO DO

    # %South face
    if (adiabatic[3] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,SouthWall,wall,WallConstruction,Main,Adiabatic,,NoSun,NoWind,0,autocalculate,'),append = T)
    } else {
        write(file=fileName,sprintf('BuildingSurface:Detailed,SouthWall,wall,WallConstruction,Main,outdoors,,%s,%s,0,autocalculate,',sunExp,windExp), append = T)
    }
    writeCoordinates(fileName,p1,p2,p6,p5) # ***************************************************** TO DO


    # %East face
    if (adiabatic[2] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,EastWall,wall,WallConstruction,Main,Adiabatic,,NoSun,NoWind,0,autocalculate,'), append = T)
    } else {
        write(file=fileName,sprintf('BuildingSurface:Detailed,EastWall,wall,WallConstruction,Main,outdoors,,%s,%s,0,autocalculate,',sunExp,windExp), append = T)
    }
    writeCoordinates(fileName,p2,p3,p7,p6)  # ***************************************************** TO DO

    # %West face
    if (adiabatic[4] == 1){
        write(file=fileName,sprintf('BuildingSurface:Detailed,WestWall,wall,WallConstruction,Main,Adiabatic,,NoSun,NoWind,0,autocalculate,'), append = T)
    } else{
        write(file=fileName,sprintf('BuildingSurface:Detailed,WestWall,wall,WallConstruction,Main,outdoors,,%s,%s,0,autocalculate,',sunExp,windExp), append = T)
    }
    writeCoordinates(fileName,p1,p5,p8,p4) # ***************************************************** TO DO
    # %Alfonso
    # %creating windows as a scaled version of the walls
    # %windowSize = p(9)*p(2)/(4-sum(adiabatic));

    # %me
    # %creating windows as a scaled version of the walls
    windowSizeN <- p[9]*a*2.8##%*p(2)*2.8
    windowSizeS <- p[10]*a*2.8#;%*p(2)*2.8
    windowSizeE <- p[11]*b*2.8#;%*p(2)*2.8
    windowSizeW <- p[12]*b*2.8#;%*p(2)*2.8
    
    # %Selecting the window
    # switch p(14)
        if (p[14]==1){
            windowConstruction <- 'Sgl Clr 6mm'
        } else if (p[14]==2){
            windowConstruction <- 'Dbl loE Clr 6mm/13mm Argon' #  %%'Dbl Clr 6mm/13mm Air'
        } else if (p[14]==3){
            windowConstruction <- 'Trp LoE (e5=.1) Clr 3mm/13mm Arg'
        } else {
            cat('Something went wrong selecting the construction of the window\r\n')
        }
    

      if (p[30]==1){
            shadingDevice <- 'MEDIUM REFLECT - LOW TRANS SHADE'
        } else if (p[30]==2){
            shadingDevice <- 'MEDIUM REFLECT - MEDIUM TRANS SHADE'
        } else if (p[30]==3){
            shadingDevice <- 'LOW REFLECT - HIGH TRANS SHADE'
        } else {
            cat('Something went wrong selecting the shading of the window\r\n')

        }
    
    if (e$shading & sum(p[9:12]) > 0){
        write(file=fileName,sprintf('WindowProperty:ShadingControl,WinShading,InteriorShade,,OnIfHighOutdoorAirTemperature,On,28,Yes,No,%s,FixedSlatAngle,,;\r\n',shadingDevice), append=T)
        shadingDevice = 'WinShading'
    } else {
        shadingDevice = ' '
    }

    # North window
    if (adiabatic[1] == 0 & e$windows & windowSizeN > 0){
        write(file=fileName,sprintf('FenestrationSurface:Detailed,NorthWindow,Window,%s,NorthWall,,0,%s,FRAME, 1,autocalculate,', windowConstruction, shadingDevice), append = T)
        Ncoord = windowCoordinates(windowSizeN,p3,p4,p8,p7) # ***************************************************** TO DO
        writeCoordinates(fileName,Ncoord$pw1, Ncoord$pw2, Ncoord$pw3, Ncoord$pw4) # ***************************************************** TO DO
    }

    # South window
    if (adiabatic[3] == 0 & e$windows &  windowSizeS > 0){
        write(file = fileName,sprintf('FenestrationSurface:Detailed,SouthWindow,Window,%s,SouthWall,,0,%s,FRAME, 1,autocalculate,', windowConstruction, shadingDevice), append=T)
        Scoord = windowCoordinates(windowSizeS,p1,p2,p6,p5) # ***************************************************** TO DO
        writeCoordinates(fileName,Scoord$pw1, Scoord$pw2, Scoord$pw3, Scoord$pw4) # ***************************************************** TO DO
    }

    # East window
    if (adiabatic[2] == 0 & e$windows & windowSizeE > 0){
        write(file = fileName,sprintf('FenestrationSurface:Detailed,EastWindow,Window,%s,EastWall,,0,%s,FRAME, 1,autocalculate,', windowConstruction, shadingDevice), append = T)
        Ecoord = windowCoordinates(windowSizeE,p2,p3,p7,p6) # ***************************************************** TO DO
        writeCoordinates(fileName,Ecoord$pw1, Ecoord$pw2, Ecoord$pw3, Ecoord$pw4) # ***************************************************** TO DO
    }

    # West window
    if (adiabatic[4] == 0 && e$windows & windowSizeW > 0){
        write(file=fileName,sprintf('FenestrationSurface:Detailed,WestWindow,Window,%s,WestWall,,0,%s,FRAME, 1,autocalculate,', windowConstruction, shadingDevice), append = T)
        Wcoord = windowCoordinates(windowSizeW,p1,p5,p8,p4) # ***************************************************** TO DO
        writeCoordinates(fileName,Wcoord$pw1, Wcoord$pw2, Wcoord$pw3, Wcoord$pw4) # ***************************************************** TO DO
    }


    # %Introducing internal mass
    # %Definning the material
    if (e$dynamics==T){
        write(file=fileName,sprintf('Material,Partition brick,MediumRough,%f,0.89,1920,790,,,;\r\n',p[15]/1000), append = T)
    } else {
        write(file=fileName,sprintf('Material,Partition brick,MediumRough,%f,0.89,1,100,,,;\r\n',p[15]/1000), append = T)
    }

    # Defining the surfaces
    IMarea = a*b*p[3]
    IMarea = IMarea + a*2.8*p[3] 
    IMarea = IMarea + b*2.8*p[3]
    
    if (e$partitions==T){
        write(file=fileName,sprintf('InternalMass,Partitions,Partitions,Main, %f;\r\n',IMarea), append = T)
        write(file=fileName,sprintf('Construction, Partitions, Plasterboard, Partition brick, Plasterboard;\r\n'), append = T)
    }

    # Introducing the obstacles
    if (e$obstacles==T){
        
        distance = 2.8*p[3]
        
        if (p[18] > 0){
            write(file=fileName,sprintf('Shading:Building,NorthObstacle, 0, 90,%f,%f, 0, %f,%f;\r\n',a/2+distance, -b/2-distance, a+2*distance, p[18]*2.8*p[3]), append = T)
        }
        
        if (p[19] > 0){
            write(file=fileName,sprintf('Shading:Building,SouthObstacle, 180, 90,%f, %f, 0, %f,%f;\r\n',-a/2-distance, b/2+distance, a+2*distance, p[19]*2.8*p[3]), append = T)
        }
        
        if (p[20] > 0){
            write(file=fileName,sprintf('Shading:Building,EastObstacle, 90, 90,%f,%f, 0, %f,%f;\r\n',-a/2-distance, -b/2-distance, b+2*distance, p[20]*2.8*p[3]), append = T)
        }
        
        if (p[21] > 0){
            write(file=fileName,sprintf('Shading:Building,WestObstacle, 270, 90, %f,%f, 0, %f,%f;\r\n', a/2+distance, b/2+distance, b+2*distance, p[21]*2.8*p[3]), append = T)
        }
    }

    # Adding the people
    write(file=fileName,sprintf('Schedule:Constant,activity,Activity-limits,%f;\r\n',p[17]), append = T)
    
    numberOfPeople = floor(p[2]*0.025)
    numberOfPeople = floor(p[2]/16)
    
    if (numberOfPeople == 0){
        numberOfPeople = 1
    }
    
    if (e$Metabolic==T){
        write(file=fileName,sprintf('People,People,Main,Occupancy Year,People, %d,,, 0,autocalculate,activity, .0000000382,No,ZoneAveraged,,,,,,,,,;\r\n',numberOfPeople), append = T)
    }
    
    # Adding the DHW
    DHW_E = p[16] * a*b*p[3] # kWh
    # %now in terms of occupancy as per report
    DHW_P = p[16] * (1.8 + numberOfPeople)/2.6 # kw
    # %Into Jules
# %    DHW_E = DHW_E*1000*3600;
 # %   DHW_P = DHW_E/(5*60);

#  %     if e.DHW
# %         fprintf(fileName,'HotWaterEquipment,domestic_hot_water,Main,DHW,EquipmentLevel,%f,,,0.0216,0,0.957,General;\r\n',DHW_P*1000);
# %     end
    
    # %Adding infiltration and ventilation
    if (e$Infiltration==T){
        # %fprintf(fileName,'ZoneInfiltration:DesignFlowRate,Infiltration,Main,Always on,AirChanges/Hour,,,,%f, 1,,,;\r\n',p(8));
        write(file=fileName,sprintf('ZoneInfiltration:DesignFlowRate,Infiltration,Main,on,AirChanges/Hour,,,,%f, 1,,,;\r\n',p[8]), append = T)
        # %This produces an stimate, but a more accurate ventilation considering when the people is in will be done later
    }
    
    # Adding the electricity use
    if (e$Electricity==T){
      #  fprintf(fileName,'ElectricEquipment,Electricity, Main, Equipment Year,  EquipmentLevel, %f, , , , , , General;\r\n',p(2)*p(22)/0.0238);
        write(file=fileName,sprintf('ElectricEquipment,Electricity, Main, Equipment Year,  Watts/Area, , ,%f , , , , General;\r\n',p[22]), append = T)
    }
    
    # remove here as necessary
    write(file=fileName,sprintf('Lights,  Lighting,Main, Lighting Year,Watts/Area, ,5,,0,0,0,1,General,No,,;\r\n'), append = T)
   
    if (e$Ventilation==T){
        # %Adding ventilation to cool down the building
        if (windowSizeN > 0){
            # %fprintf(fileName,'ZoneVentilation:WindandStackOpenArea,Natural ventilation1,Main,%f,Occupancy,0.1,,%f,0.6,%f,, 100,,-100,,2,, 30,, 20;\r\n',0.05*windowSizeN*numberOfPeople,sqrt(windowSizeN),28);
            
            DH = (Ncoord$pw3[3]-Ncoord$pw1[3])
            write(file=fileName,sprintf('ZoneVentilation:WindandStackOpenArea,Natural ventilation1,Main,%f,Ventilation year,autocalculate,,%f,0.6,%f,, 100,,0,,2,, 50,, 20;\r\n',p[25]*windowSizeN,DH*(1-p[25]/2),p[29]), append =T)
        }
      
        if (windowSizeS > 0){
            DH = (Scoord$pw3[3]-Scoord$pw1[3])
            
            write(file=fileName,sprintf('ZoneVentilation:WindandStackOpenArea,Natural ventilation2,Main,%f,Ventilation year,autocalculate,,%f,0.6,%f,, 100,,0,,2,, 50,, 20;\r\n',p[26]*windowSizeS,DH*(1-p[26]/2),p[29]), append = T)
        }
      
        if (windowSizeE > 0){
            DH = (Ecoord$pw3[3]-Ecoord$pw1[3])
            write(file=fileName,sprintf('ZoneVentilation:WindandStackOpenArea,Natural ventilation3,Main,%f,Ventilation year,autocalculate,,%f,0.6,%f,, 100,,0,,2,, 50,, 20;\r\n',p[27]*windowSizeE,DH*(1-p[27]/2),p[29]), append = T)
        }
      
        if (windowSizeW > 0){
            DH = (Wcoord$pw3[3]-Wcoord$pw1[3])
            write(file=fileName,sprintf('ZoneVentilation:WindandStackOpenArea,Natural ventilation4,Main,%f,Ventilation year,autocalculate,,%f,0.6,%f,, 100,,0,,2,, 50,, 20;\r\n',p[28]*windowSizeW,DH*(1-p[28]/2),p[29]), append = T)
        }
      
        # % Window opening for air renewal
        eff_vent =  p[23]*numberOfPeople - p[8]*site$vol/3600
# %         fprintf('Max vent for air renewal: %f\n', p(21)*numberOfPeople);
# %         fprintf('Vent due to infiltration: %f\n', p(8)*site.vol/3600);
# %         fprintf('Inf = %f ach\n', p(8));
# %         fprintf('eff_vent = %f m3/s\n', eff_vent);
        if (eff_vent < 0){
            eff_vent = 0
        }
       # % fprintf(fileName,'ZoneVentilation:DesignFlowRate,Ventilation,Main,Occupancy,Flow/Person,,,%f,,Natural,,1,1,,,,-100,,100,,-100,,-100,,100,,40;\r\n',eff_vent);
       # % fprintf(fileName,'ZoneVentilation:DesignFlowRate,Ventilation,Main,Occupancy Year,Flow/Person,,,%f,,Natural,,1,1,,,,-100,,100,,-100,,-100,,100,,40;\r\n',eff_vent);
    }
    
    if (p[33]>0 & windowSizeN>0){
        write(file=fileName,sprintf('Shading:Overhang,ShadeN,NorthWindow,0.01,90,0,0,%f;\r\n',p[33]), append = T)
    }
    
    if (p[34]>0 & windowSizeS>0){
        write(file=fileName,sprintf('Shading:Overhang,ShadeS,SouthWindow,0.01,90,0,0,%f;\r\n',p[34]), append = T)
    }
    
    if (p[35]>0 & windowSizeE>0){
        write(file=fileName,sprintf('Shading:Overhang,ShadeE,EastWindow,0.01,90,0,0,%f;\r\n',p[35]), append = T)
    }
    if (p[36]>0 & windowSizeW>0){
        write(file=fileName,sprintf('Shading:Overhang,ShadeW,WestWindow,0.01,90,0,0,%f;\r\n',p[36]), append = T)
    }

    write(file=fileName,sprintf('Schedule:Compact,HeatingSetpoint,Heating limits,Through: 6/30, For: AllDays, Until: 24:00,%f,Through: 8/30, For: AllDays, Until: 24:00, 16,Through: 12/31, For: AllDays, Until: 24:00, %f;\r\n',21,21), append = T)

    write(file=fileName,sprintf('Site:GroundTemperature:BuildingSurface,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f;\r\n',15,16,17,18,18,18,18,18,18,17,16,15), append = T)

    # This is a hack to get the file to work properly! 
    filetext <- readLines(con = fileName, n = -1L, ok = TRUE, skipNul = FALSE)
    fileConn<-file(fileName) # Open file connection
    writeLines(filetext, con=fileConn,sep="\n")
    close(fileConn)
    
} 
    


