printOR=function(effect,se, digits=3){
    paste0(
        sprintf(paste0("%.", digits,"f"), 
                round(exp(effect),digits)), " (",  
        sprintf(paste0("%.", digits,"f"), 
                round(exp(effect-1.96*se),digits)), ", ", 
        sprintf(paste0("%.", digits,"f"), 
                round(exp(effect+1.96*se),digits)),")"
    )
}
