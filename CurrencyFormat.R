CurrencyFormat <-function (number,rounding=F) 
{
    first <- TRUE
    lut <- c( 1, 100, 1000, 1000000, 1000000000,1000000000000 )
    pre <- c("", "H", "K", "M", "B", "T")
    if (length(number) > 1) {
        for (cnt in 1:length(number)){        
            ix <- findInterval(number[cnt], lut)
            print(number[cnt])
            print(cnt)
            if (ix != 0 | ix != 1){
                if (rounding==T) {
                    sistring <- paste(round(number[cnt]/lut[ix]), pre[ix])
                }
                else {
                    sistring <- paste(signif(number[cnt]/lut[ix],digits=5), pre[ix])
                }
                if (first){
                    tnumber <- sistring
                    fnumber <- tnumber 
                    first <- FALSE
                }
                else
                    fnumber <- cbind(tnumber, fnumber)
            }
            else {
                sistring <- number[cnt]
            }
        }
        return(fnumber)
    }
    else
        return(number)
    
}