# Simple scatterplot

testdf <- data.frame(x=1:10,y=11:20)

plot(testdf[["x"]], testdf[["y"]], main="Simple Plot", xlab="x", ylab="y")
