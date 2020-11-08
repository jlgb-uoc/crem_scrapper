getnt <- function(n2, ww) {
       nt = 4
       if (n2==10) urlBase=paste0(urlBase,"/eaid18")
       if (n2==10) nt=5
       if (n2==30 & ww==1) nt=3
       if (n2==30 & ww==2) nt=4
       if (n2==31 & ww==2) nt=5
       if (n2==32 & ww==1) nt=5
       if (n2==32 & ww==2) nt=6

       if (n2==33 & ww==1) nt=6
       if (n2==33 & ww==2) nt=7
       if (n2==35 & ww==2) nt=3
       if (n2==35 & ww==3) nt=6
       if (n2==36 & ww==1) nt=3
       if (n2==36 & ww==2) nt=2
       if (n2==36 & ww==3) nt=7
       if (n2==50 & ww==1) nt=5
       if (n2==50 & ww==2) nt=6
       if (n2==51 & ww==2) nt=4
       if (n2==51 & ww==3) nt=5
       if (n2==54 & ww==3) nt=5
       if (n2==74 & ww==2) nt=5
       if (n2==75 & ww==1) nt=5
       if (n2==75 & ww==2) nt=6

       if (n2==77 & ww==2) nt=3
       if (n2==77 & ww==3) nt=4
       if (n2==77 & ww==4) nt=5
       if (n2==78 & ww==3) nt=5

       if (n2==90 & ww==2) nt=5
       if (n2==94 & ww==2) nt=5

       if (n2==95 & ww==1) nt=5
       if (n2==95 & ww==2) nt=6

       if (n2==96 & ww==1) nt=6
       if (n2==96 & ww==2) nt=7

       if (n2==97 & ww==2) nt=9
       if (n2==97 & ww==3) nt=10

       if (n2==119 & ww==1) nt=3
       if (n2==119 & ww==2) nt=4
       if (n2==119 & ww==3) nt=5
       if (n2==120 & ww==2) nt=5
       if (n2==120 & ww==3) nt=6

       if (n2 %in% c(167,172:173)) nt=ww+1
       if (n2 %in% c(121:127,129,140,151,168,174,266,232,288,299)) nt=ww+2
 
       if (n2==128 & ww==2) nt=5     
       if (n2 %in% c(130:132,141,144,152,169,223,228,230,233,243,265,270,289:291,351)) nt=ww+3
       if (n2 %in% c(133,142,145,170,229,231,234,266,278,287,301)) nt=ww+4
       if (n2 %in% c(153,171,224)) nt=ww+5  
       if (n2 %in% c(225)) nt=ww+7  

       return(nt)
}
