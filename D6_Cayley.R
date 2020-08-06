#A5 in terms of generators and relations


# If I knew what CSS was this part might be less ugly.

# Bold serif fonts for headings, like "Cayley Graph" or "Permutations"
kahua.s <- "font-weight: bold; font-family: 'Liberation Serif', 'Nimbus Roman No 9 L', 'Hoefler Text', 'Times', 'Times New Roman', serif"

# Decent-sized sans serif fonts for subheadings like "Rewrite Rules" or "Defining Relations"
kahua   <- "font-size:large; font-family: 'Liberation Sans', 'Nimbus Sans L', 'Lucida Sans Unicode', 'Lucida Grande', 'FreeSans', 'Helvetica Neue', 'Helvetica', sans-serif"

# Formatting on the generators; 'r' gets royalblue, 'f' gets darkorange, both fixed-width fonts
kahuaf <- "<span style = \"color:darkorange; font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">f</span>"
kahuar <- "<span style = \"color:royalblue; font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">r</span>"
identiteit <- "<span style = \"font-weight: bold; font-family: 'Liberation Serif', 'Nimbus Roman No 9 L', 'Hoefler Text', 'Times', 'Times New Roman', serif\">I</span>"
# here's some fixed-width whitespace so that we can kludge in some text alignment between lines
whitespace <- "<span style = \"font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">&nbsp;</span>"

# sadly rep(whitespace,n) does not do what I want so here is a silly function
# If I ever had delusions of being a programmer this should put those notions to rest
# If I remembered how sprintf() works this could be much cleaner
Leerraum <- function(n){ #it takes an integer n and returns a string of n non-breaking spaces
  peaCrabs <- ""
  for (i in 1:n){
    peaCrabs <- paste0(peaCrabs,whitespace)
  }
  return(peaCrabs)
}

# Let's see if we can make a function that takes a word made out of r and f
# and returns some HTML we can render for a prettier word display
# 'sana' is Finnish for 'group element expressed in terms of generato elements'

CD6.manifestWord <- function(sana){ # we get ourselves a string of 'r's and 'f's
  separo <- unlist(strsplit(sana, split="")) # we explode the string 'sana' into a vector of single characters 'separo'
  slovo <- ""
  if (length(separo)==0) return(identiteit) # in the dataframe the identity element is "" but when we display it we call it "I"
  for (i in 1:length(separo)){
    slovo <- paste0(slovo,ifelse(separo[i]=="f",kahuaf,kahuar)) # and we paste in the html over and over
  }
  return(slovo)
}

# here we start doing math




CD6.makeDataFrame <- function() {
  r1 <- 3.7 #distance to outer pentagon
  V <- matrix(nrow = 12, ncol = 2)
  V[1:6,1] <- c(2, 5, 6, 5, 2, 1)
  V[1:6,2] <- c(0, 0, 2, 4, 4, 2)
  r2 <- 3 #distance to inner pentagon
  V[7:12,1] <- c(2.7, 4.3, 4.8, 4.3, 2.7, 2.2)
  V[7:12,2] <- c(1, 1, 2, 3, 3, 2)
  
  # magic numbers; the function tables for r and f
  rdest <- c(2,3,4,5,6,1,12,7,8,9,10,11)
  fdest <- c(7,8,9,10,11,12,1,2,3,4,5,6)
  
  # 60
  N <- 12
  
  DF <- data.frame(x=V[1:N,1],
                   y=V[1:N,2],
                   rdest=rdest,
                   fdest=fdest,
                   rsrc=integer(12),
                   label=as.character(1:N),
                   sana=rep("",N),
                   color =rep("lightgray",N),
                   perm=rep("",N),
                   stringsAsFactors=FALSE)
  
  
  # insane code for creating the canonical sequence of r and f to label each vertex of the graph.
  # Should probably come up with a more elegant way to do this
  while (min(nchar(DF$sana)) == 0){
    for (indeksi in 1:12){
      if (indeksi == 1 | nchar(DF$sana[indeksi])>0){
        rtarget <- DF$rdest[indeksi]
        rsana <- paste("r",DF$sana[indeksi],sep="")
        ftarget <- DF$fdest[indeksi]
        fsana <- paste("f",DF$sana[indeksi],sep="")
        if (nchar(DF$sana[rtarget])==0){
          DF$sana[rtarget] <- rsana
        }
        if (nchar(DF$sana[ftarget])==0){
          DF$sana[ftarget] <- fsana
        }
      }
    }
  }
  
  # and now that we have some sort of sequence of r and f that gets to each node, multiply by I
  # because that will put everything into our standard form.
  for (xarisxis in 1:12){
    DF$sana[xarisxis] <- CD6.multiply(DF$sana[xarisxis],"")
  }
  
  # Time to fill in the rsrc column
  for (xarisxis in 1:12){
    DF$rsrc[xarisxis]<-which(DF$rdest==xarisxis)
  }
  return(DF)
}








uqqw<-integrate(function(x) 1/log(x)+1/(1-x),0,1);xvc<-uqqw$value;cvx<-1-xvc # yup
i_to_the_i <- exp(-pi/2) # i to the i is real and equals sqrt(exp(-pi))






# OK time to draw some graph
CD6.drawGraph <- function(DF, permlabel=FALSE) { # not using the permlabel argument but leaving it in because hope springs eternal
  xr0 <- DF[DF$rsrc,1]; yr0 <- DF[DF$rsrc,2]  # the coordinates for where an r is coming from
  xr1 <- DF[DF$rdest,1];yr1 <- DF[DF$rdest,2] # the coordinates for where r takes us
  xf1 <- DF[DF$fdest,1];yf1 <- DF[DF$fdest,2] # the coordinates for where f takes us
  xv1 <- xr0 - DF$x; yv1 <- yr0 - DF$y # vector components for incoming r vector
  xv2 <- xr1 - DF$x; yv2 <- yr1 - DF$y # vector components for outgoing r vector
  xv3 <- xf1 - DF$x; yv3 <- yf1 - DF$y # vector components for f vector
  norme1 <- sqrt(xv1^2+yv1^2); norme2 <- sqrt(xv2^2+yv2^2);norme3 <- sqrt(xv3^2+yv3^2) # some Euclidean norms
  xv1 <- xv1/norme1; yv1 <- yv1/norme1 # normalize the vectors now
  xv2 <- xv2/norme2; yv2 <- yv2/norme2 # I hope no one but me ever has to debug this
  xv3 <- xv3/norme3; yv3 <- yv3/norme3 # because this is bad even for me
  ks12 <- xv1*xv2+yv1*yv2; ks13<- xv1*xv3+yv1*yv3; ks23 <- xv2*xv3+yv2*yv3 # ksab is the product of vector a with vector b
  for (i in 1:12){ # I should really figure out how to vectorize this
    smks <- min(ks12[i],ks13[i],ks23[i]) # the widest angle has the smallest cosine (not true in general but true for our diagram)
    if (ks12[i]==smks){
      xv3[i] <- -2*xv3[i]
      yv3[i] <- -2*yv3[i]; next # I am going to programmer hell
    }
    if (ks13[i]==smks){
      xv2[i] <- -2*xv2[i]
      yv2[i] <- -2*yv2[i]; next
    }
    xv1[i] <- -2*xv1[i] 
    yv1[i] <- -2*yv1[i] 
  }
  offx <- xv1 + xv2 + xv3; offy <- yv1 + yv2 + yv3
  xoffset <- offx*i_to_the_i/sqrt(offx^2+offy^2);yoffset <- offy*i_to_the_i/sqrt(offx^2+offy^2) # this is going to be hard to debug
  par(mar=c(.1,.1,.1,.1))  #narrow margins
  # start an empty plot, just big enough, and make sure the aspect ratio is forced to 1
  plot(NULL,xlim = c(1.05*min(DF$x),1.05*max(DF$x)),ylim = c(1.05*min(DF$y),1.05*max(DF$y)),axes = FALSE, asp = 1)
  arrows(DF$x,DF$y,(xvc*xr1+cvx*DF$x),(xvc*yr1+cvx*DF$y),length = 0.1,col="DarkSlateBlue") # draw blue arrows
  segments((xvc*xr1+cvx*DF$x),(xvc*yr1+cvx*DF$y),xr1,yr1,col="DarkSlateBlue") # these represent multiplication by r
  segments(DF$x,DF$y,xf1,yf1,col = "lightsalmon", lwd = 2) # no arrowheads because f is order 2
  points(DF$x,DF$y,pch=21,bg = DF$color, col = "black", cex=2) # and now we draw the vertices, after the lines
  text(DF$x+xoffset,DF$y+yoffset,DF$label) # now we actually label
}



CD6.multiply <- function(a,b) {
  x <- paste("rrrrrr",a,b,sep="") # collapsing it down to standard form is messy so don't question me
  oldx <- "xyzzy" # initialize previous non-standard generator form with nonsense
  while (x!=oldx){ # I could have left it as a for loop but I thought it would be more elegant like this
    oldx <- x # before we mess with it, keep a copy for reference
    x <- sub("rrrrrr","",x)
    x <- sub("ff","",x) # two flips equals nothing
    x <- sub("frfr", "", x)
    x <- sub("rf","frrrrr",x) # flip,rotate,flip = counter-rotate,flip,counter-rotate
    x <- sub("rrf","frrrr",x) # flip,counter-rotate,flip = rotate,flip,rotate
    x <- sub("rrrf","frrr",x) # flip,counter-rotate,flip = rotate,flip,rotate
    x <- sub("rrrrf","frr",x) # flip,counter-rotate,flip = rotate,flip,rotate
    x <- sub("rrrrrf","fr",x) # flip,counter-rotate,flip = rotate,flip,rotate
  }
  return(x)
}








CD6.power <- function(a,n){
  x <-a # it does pretty much what you'd expect
  while (n>1){
    x <- CD6.multiply(x,a) # it's multiplying
    n <- n-1 # n times
  }
  return(x)
}







CD6.markVertex <- function(DF,x,color) {
  if (is.character(x)){
    x <- CD6.multiply(x,"")
    x <- which(DF$sana == x)
  }
  DF$color[x] <- color # replaced hardcoded column number with column name
  return(DF)
} 







D6DF <- CD6.makeDataFrame() #this seems like a weird place to put this but whatever

# A5DF <- CA5.markVertex(A5DF,"frrrf","orangered")
# A5DF <- CA5.markVertex(A5DF,59,"cornflowerblue")
CD6.drawGraph(D6DF)





CD6.makePerm <- function(DF,sana,IVert ="I", rSana = "(123456)", fSana = "(12)(34)(56)"){
  product = IVert # start with the identity
  n <- nchar(sana) # how many elements are in the generator word
  if (n > 0) # as long as it's not stupid,
    for (i in (1:nchar(sana))){ # loop through the word
      g <- substr(sana,i,i) # we might be able to do this with strsplit()
      product <- Perm.multiply(product,ifelse(g=="f",fSana,rSana)) # use Perm.multiply to multiply perms
    }
  return (product)
}






CD6.makePerms <- function(DF,rSana = "(123456)", fSana = "(12)(34)(56)"){
  for (i in (1:nrow(DF))){
    DF$perm[i] <- CD6.makePerm(DF,DF$sana[i],"I",rSana,fSana)
    DF$label[i] <- DF$perm[i] # This will work for now but once you make perms you can never go back
  }
  return (DF)
}





D6DF <- CD6.makePerms(D6DF)
# CA5.drawGraph(A5DF)


# findClosestVertex takes an x and y coordinate
# and unsurprisingly enough, finds the closest vertex to that position
# through the straightforward approach of calculating all the distances
CD6.findClosestVertex <- function(x,y){
  DF <- CD6.makeDataFrame()
  distsq <- numeric(6)
  for (i in 1:12){
    distsq[i] <- (DF[i,1]-x)^2+(DF[i,2]-y)^2
  }
  k <- which(distsq == min(distsq))
  stopifnot(length(k)==1)
  return(k)
}
