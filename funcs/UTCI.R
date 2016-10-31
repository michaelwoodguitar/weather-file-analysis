UTCI_matt2 <- function(Ta1,Tr1,RH1,Va1){

    Ta <- Ta1
    Tmrt <- Tr1
    va <- Va1
    va[va<0.5] <- 0.5

    SVP <- svpt_UTCI(Ta)
    RH <- RH1
    
    Pa <- SVP*RH/10
    D_Tmrt <- Tmrt - Ta
    
    UTCI_Temperature <- Ta  
    UTCI_Temperature <- UTCI_Temperature + ( 6.07562052e-1 ) 
    UTCI_Temperature <- UTCI_Temperature + ( -2.27712343e-2 ) * Ta
    UTCI_Temperature <- UTCI_Temperature + ( 8.06470249e-4 ) * Ta*Ta
    UTCI_Temperature <- UTCI_Temperature + ( -1.54271372e-4 ) * Ta*Ta*Ta
    UTCI_Temperature <- UTCI_Temperature + ( -3.24651735e-6 ) * Ta*Ta*Ta*Ta
    UTCI_Temperature <- UTCI_Temperature + ( 7.32602852e-8 ) * Ta*Ta*Ta*Ta*Ta
    UTCI_Temperature <- UTCI_Temperature + ( 1.35959073e-9 ) * Ta*Ta*Ta*Ta*Ta*Ta
    UTCI_Temperature <- UTCI_Temperature + ( -2.25836520e0 ) * va
    UTCI_Temperature <- UTCI_Temperature + ( 8.80326035e-2 ) * Ta*va
    UTCI_Temperature <- UTCI_Temperature + ( 2.16844454e-3 ) * Ta*Ta*va
    UTCI_Temperature <- UTCI_Temperature + ( -1.53347087e-5 ) * Ta*Ta*Ta*va
    UTCI_Temperature <- UTCI_Temperature + ( -5.72983704e-7 ) * Ta*Ta*Ta*Ta*va
    UTCI_Temperature <- UTCI_Temperature + ( -2.55090145e-9 ) * Ta*Ta*Ta*Ta*Ta*va
    UTCI_Temperature <- UTCI_Temperature + ( -7.51269505e-1 ) * va*va
    UTCI_Temperature <- UTCI_Temperature + ( -4.08350271e-3) * Ta*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -5.21670675e-5 ) * Ta*Ta*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 1.94544667e-6 ) * Ta*Ta*Ta*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 1.14099531e-8 ) * Ta*Ta*Ta*Ta*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 1.58137256e-1 ) * va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -6.57263143e-5 ) * Ta*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 2.22697524e-7 ) * Ta*Ta*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -4.16117031e-8 ) * Ta*Ta*Ta*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -1.27762753e-2 ) * va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 9.66891875e-6 ) * Ta*va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 2.52785852e-9 ) * Ta*Ta*va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 4.56306672e-4 ) * va*va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -1.74202546e-7 ) * Ta*va*va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( -5.91491269e-6 ) * va*va*va*va*va*va
    UTCI_Temperature <- UTCI_Temperature + ( 3.98374029e-1 ) * D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.83945314e-4 ) * Ta*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.73754510e-4 ) * Ta*Ta*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -7.60781159e-7 ) * Ta*Ta*Ta*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 3.77830287e-8 ) * Ta*Ta*Ta*Ta*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 5.43079673e-10 ) * Ta*Ta*Ta*Ta*Ta*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -2.00518269e-2 ) * va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 8.92859837e-4 ) * Ta*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 3.45433048e-6 ) * Ta*Ta*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -3.77925774e-7 ) * Ta*Ta*Ta*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.69699377e-9 ) * Ta*Ta*Ta*Ta*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.69992415e-4 ) * va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -4.99204314e-5 ) * Ta*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 2.47417178e-7 ) * Ta*Ta*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.07596466e-8 ) * Ta*Ta*Ta*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 8.49242932e-5 ) * va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.35191328e-6 ) * Ta*va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -6.21531254e-9 ) * Ta*Ta*va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -4.99410301e-6 ) * va*va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.89489258e-8 ) * Ta*va*va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 8.15300114e-8 ) * va*va*va*va*va*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 7.55043090e-4 ) * D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -5.65095215e-5 ) * Ta*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -4.52166564e-7 ) * Ta*Ta*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 2.46688878e-8 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 2.42674348e-10 ) * Ta*Ta*Ta*Ta*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.54547250e-4 ) * va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 5.24110970e-6 ) * Ta*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -8.75874982e-8 ) * Ta*Ta*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.50743064e-9 ) * Ta*Ta*Ta*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.56236307e-5 ) * va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.33895614e-7 ) * Ta*va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 2.49709824e-9 ) * Ta*Ta*va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 6.51711721e-7 ) * va*va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.94960053e-9 ) * Ta*va*va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.00361113e-8 ) * va*va*va*va*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.21206673e-5 ) * D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -2.18203660e-7 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 7.51269482e-9 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 9.79063848e-11 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.25006734e-6 ) * va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.81584736e-9 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -3.52197671e-10 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -3.36514630e-8 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.35908359e-10 ) * Ta*va*va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 4.17032620e-10 ) * va*va*va*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -1.30369025e-9 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 4.13908461e-10 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 9.22652254e-12 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -5.08220384e-09 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -2.24730961e-11 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.17139133e-10 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 6.62154879e-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 4.03863260e-13 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 1.95087203e-12 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( -4.73602469e-12 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt
    UTCI_Temperature <- UTCI_Temperature + ( 5.12733497e0 ) * Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.12788561e-1 ) * Ta*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.96701861e-2 ) * Ta*Ta*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 9.99690870e-4 ) * Ta*Ta*Ta*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 9.51738512e-6 ) * Ta*Ta*Ta*Ta*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.66426341e-7 ) * Ta*Ta*Ta*Ta*Ta*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 5.48050612e-1 ) * va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.30552823e-3 ) * Ta*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.64119440e-3 ) * Ta*Ta*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -5.16670694e-6 ) * Ta*Ta*Ta*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 9.52692432e-7 ) * Ta*Ta*Ta*Ta*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.29223622e-2 ) * va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 5.00845667e-3 ) * Ta*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.00601257e-6 ) * Ta*Ta*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.81748644e-6 ) * Ta*Ta*Ta*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.25813502e-3 ) * va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.79330391e-4 ) * Ta*va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.34994441e-6 ) * Ta*Ta*va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.29735808e-4 ) * va*va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.29064870e-6 ) * Ta*va*va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.28558686e-6 ) * va*va*va*va*va*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.69476348e-2 ) * D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.62325322e-3 ) * Ta*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.1427968e-5 ) * Ta*Ta*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.59835559e-6 ) * Ta*Ta*Ta*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.77136523e-8 ) * Ta*Ta*Ta*Ta*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 8.64203390e-3 ) * va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.87405181e-4 ) * Ta*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -9.13863872e-6 ) * Ta*Ta*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 5.15916806e-7 ) * Ta*Ta*Ta*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.59217476e-5 ) * va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.28696511e-5 ) * Ta*va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.10542454e-7 ) * Ta*Ta*va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.24382300e-5 ) * va*va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.38584400e-9 ) * Ta*va*va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.20609296e-7 ) * va*va*va*va*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.32469180e-4 ) * D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.87381964e-5 ) * Ta*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 4.80925239e-6 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -8.75492040e-8 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.77862930e-5 ) * va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -5.06004592e-6 ) * Ta*va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.14325367e-7 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.53016723e-6 ) * va*va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.72857035e-8 ) * Ta*va*va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.95079398e-8 ) * va*va*va*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.59413173e-7 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 7.04388046e-7 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.89309167e-8 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.79768731e-7 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 7.96079978e-9 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.62897058e-9 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.94367674e-8 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.18566247e-9 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.34678041e-10 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.15606447e-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.80626406e0 ) * Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 5.48712484e-1 ) * Ta*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.99428410e-3 ) * Ta*Ta*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -9.54009191e-4 ) * Ta*Ta*Ta*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.93090978e-5 ) * Ta*Ta*Ta*Ta*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.08806365e-1 ) * va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.16952364e-2 ) * Ta*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 4.95271903e-4 ) * Ta*Ta*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.90710882e-5 ) * Ta*Ta*Ta*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.10787756e-3 ) * va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.98445738e-4 ) * Ta*va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.30109073e-5 ) * Ta*Ta*va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 4.17856590e-4 ) * va*va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.27043871e-5 ) * Ta*va*va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.04620472e-6 ) * va*va*va*va*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 5.14507424e-2 ) * D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.32510997e-3 ) * Ta*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 8.99281156e-5 ) * Ta*Ta*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.14663943e-7 ) * Ta*Ta*Ta*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.66016305e-4 ) * va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.63789586e-4 ) * Ta*va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.01199003e-6 ) * Ta*Ta*va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.06823306e-4 ) * va*va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.61341136e-6 ) * Ta*va*va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.29748967e-7 ) * va*va*va*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.04788893e-4 ) * D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.42070836e-5 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.16257971e-6 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 7.68023384e-6 ) * va*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -5.47446896e-7 ) * Ta*va*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.59937910e-8 ) * va*va*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.36497725e-6 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.68737969e-7 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.67489271e-8 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.23926897e-9 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.53874123e-2 ) * Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.21201190e-1 ) * Ta*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.55126038e-2 ) * Ta*Ta*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.63917279e-4 ) * Ta*Ta*Ta*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 4.53433455e-2 ) * va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.32943862e-3 ) * Ta*va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.45389826e-4 ) * Ta*Ta*va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.17508610e-4 ) * va*va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.66724702e-5 ) * Ta*va*va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.33217140e-5 ) * va*va*va*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -2.26921615e-3 ) * D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.80261982e-4 ) * Ta*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -5.45314314e-9 ) * Ta*Ta*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -7.96355448e-4 ) * va*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.53458034e-5 ) * Ta*va*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.31223658e-6 ) * va*va*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.02122035e-4 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.77403547e-6 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.73825715e-6 ) * va*D_Tmrt*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.09087898e-7 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 6.14155345e-1 ) * Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.16755931e-2 ) * Ta*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.33374846e-3 ) * Ta*Ta*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 3.55375387e-3 ) * va*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -5.13027851e-4 ) * Ta*va*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.02449757e-4 ) * va*va*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -1.48526421e-3 ) * D_Tmrt*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -4.11469183e-5 ) * Ta*D_Tmrt*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -6.80434415e-6 ) * va*D_Tmrt*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -9.77675906e-6 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 8.82773108e-2 ) * Pa*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( -3.01859306e-3 ) * Ta*Pa*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.04452989e-3 ) * va*Pa*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 2.47090539e-4 ) * D_Tmrt*Pa*Pa*Pa*Pa*Pa
    UTCI_Temperature <- UTCI_Temperature + ( 1.48348065e-3 ) * Pa*Pa*Pa*Pa*Pa*Pa
    
    UTCI_Temperature[Ta == -999] = -999
    UTCI_Temperature[Tmrt == -999] = -999
    UTCI_Temperature[Pa == -999] = -999
    UTCI_Temperature[RH == -999] = -999
    
    
    return(UTCI_Temperature)
    
}

# %saturation_vap_pressure_temp
svpt_UTCI <- function(TA){
  
    g = c(-2.8365744e3, -6.028076559e3, 1.954263612e1, -2.737830188e-2, 1.6261698e-5, 7.0229056e-10, -1.8680009e-13, 2.7150305)
    TK = TA + 273.15 		# ! air temp in K
    ES = g[8]*log(TK)
    for (i in 1:7){
        ES = ES + g[i]*TK^(i-3)  
    }
    
    ES = exp(ES)*0.01	# %! *0.01: convert Pa to hPa
    
    return(ES)
    
}