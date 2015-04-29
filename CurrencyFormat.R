CurrencyFormat <-function (number,rounding=F) 
{
    first <- TRUE
    lut <- c( 1, 1000, 1000000, 1000000000,1000000000000 )
    pre <- c("", "K", "M", "B", "T")
    if (length(number) > 1) {
        for (cnt in 1:length(number)){        
            ix <- findInterval(number[cnt], lut)
#             print(number[cnt])
#             print(cnt)
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
                    fnumber <- append(fnumber, sistring)
            }
            else {
                sistring <- number[cnt]
                if (first){
                    tnumber <- sistring
                    fnumber <- tnumber
                    first <- FALSE
                }
                else
                    fnumber <- append(fnumber, sistring)
            }
#         print(paste("PMC Length : ", length(fnumber), sep=""))
        }
#          print(paste("Final PMC Length", length(fnumber), class(fnumber), sep=" : "))        
        return(fnumber)
    }
    else{
        ix <- findInterval(number, lut)
        if (ix != 0 | ix != 1){
            if (rounding==T) {
                sistring <- paste(round(number/lut[ix]), pre[ix])
            }
            else {
                sistring <- paste(signif(number/lut[ix],digits=5), pre[ix])
            }
#             print("format 1 number")
            return(sistring)
        }    
        else
#             print("no format")
            return(number)
    }
}