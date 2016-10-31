PMV_BSI <- function(TA,TR,VEL,RH) {    # Input CLO at some point
# PMV <- PMV_BSI(data(:,3),data(:,2),data(:,2)*0+0.1,data(:,4)) << This is the input from the previous function in R.

    if (length(TA) != length(TR) | length(TA) != length(VEL) | length(TA) != length(RH)){
        # if this is true then matrices will be wrong size
        PMV = -999
        PPD = -999
    } else {
  
          MET = 1.20   # metabolic rate equivalent to 70Wm-2
          WME = 0 # work rate
          CLO = cos(TA/15)+0.5
          CLO[CLO<0.5] = 0.5
          # %CLO = 0.5
          # %TA = 20    %air temp
          # %TR = 19    %radiant temp
          # %VEL = 0.1    %air velocity
          # %CLO = 1.0    %clothing layer
          ICL = CLO*0.155 # clothing insulation
          M = MET*58.15
          W = WME*58.15
          MW = M-W # internal heat production in human body
          # %RH = 40     %relative humidity as a %
          
          # [ ] check the way this is working:
          PA = 10*RH*exp(16.6536-(4030.183/(TA+235))) # water vapour pressure
        
          XN = rep(0, length(TA))  # XN = zeros(size(TA,1),1)
          TCL = XN
          # %functions
        
          FCL = 1.05+(0.645*ICL)
          FCL[ICL<0.078] = 1+(1.29*ICL[ICL<0.078])
          
          # %             
          # %             if (ICL<0.078)
          #   %                 FCL = 1+(1.29*ICL)
          # %             else
          #   %                 FCL = 1.05+(0.645*ICL)
          # %             end
        
          #  Up to here...
          HCF = 12.1*VEL^0.5
          TAA = TA+273
          TRA = TR+273     
        
          # %  TCLA = TAA+(35.5-TA)./(3.5*(6.45*ICL+0.1))
          TCLA = TAA+(35.5-TA)/(3.5*ICL+0.1)
          P1 = ICL*FCL
          P2 = P1*3.96
          P3 = P1*100
          P4 = P1*TAA
          P5 = 308.7-0.028*MW+P2*((TRA/100)^4)
          HC = P1*0
        
        for (i in 1:length(TA)){
            
            condition = 0
            n=0
            XN[i] = TCLA[i]/100
            XF = XN[i]
            
            while (condition == 0){
            
                XF = (XF+XN[i])/2
                HCN = 2.38*abs(100*XF-TAA[i])^0.25
                
                if (HCF[i]>HCN){
                  HC[i] = HCF[i]
                } else {
                  HC[i] = HCN
                }
        
                XN[i] = (P5[i]+P4[i]*HC[i]-P2[i]*XF^4)/(100+P3[i]*HC[i])
                
                n = n+1
        
                if (abs(XN[i]-XF) < 0.00015){
                  condition = 1
                }
            
                if (n == 150){
                  condition = 1
                }
            }
        
            TCL[i] = (100*XN[i])-273
        }
        
        HL1 = 0.00305*(5733-6.99*MW-PA)
        
          if (MW>58.15){
            HL2 = 0.42*(MW-58.15)
          } else {
            HL2 = 0
          }
        
        HL3 = 0.000017*M*(5867-PA)
        HL4 = 0.0014*M*(34-TA)
        # %HL5 = 3.96*FCL*(XN.^4-(TRA/100).^4)
        HL5 = 3.96e-8*FCL*((TCL+273)^4-(TRA)^4)
        HL6 = FCL*HC*(TCL-TA)
        
        TS = 0.303*exp(-.036*M)+0.028
        
        # PMV[,1] = TS*(MW-HL1-HL2-HL3-HL4-HL5-HL6)
        PMV = TS*(MW-HL1-HL2-HL3-HL4-HL5-HL6)
        
        # PPD[,1] = 100-95*exp(-0.03353*PMV^4-0.2179*PMV^2)
        PPD = 100-95*exp(-0.03353*PMV^4-0.2179*PMV^2)
    }
  
  
PMV[TA==-999] = -999
PMV[TR==-999] = -999
PMV[VEL==-999] = -999
PMV[RH==-999] = -999

# outputs should be:[PMV,PPD] 

output <- list(PMV, PPD)
names(output) <- c('PMV', 'PPD')

return(output)

}

