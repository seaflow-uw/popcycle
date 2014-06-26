cytodiv <- function(opp, para, Ncat){
        
        require(plyr)
        
        # bin the data
        xx <- data.frame(apply(as.matrix(opp[,para]),2,function(x) x%/%(10^3.5/Ncat)))
        
        # calculate frequency in each bin
        ff <- count(xx, para)
        
        # calculate diversity indices
        N0 = dim(ff)[1] # richness
        p_i = ff$freq/sum(ff$freq)
        N1 = exp(-sum(p_i*log(p_i)))
        H = log(N1) # Shannon-Wiener's Index - use natural log
        J = log(N1)/log(N0) # Evenness
        opp_red = sum(opp$fsc_small)/1000 # Estimates of bulk Chlorophyll from fsc_small
        # output
        indices <- data.frame(cbind(N0, N1, H, J, opp_red))
        return(indices)
        
        
    }
