kleene xs = []:[y++[x] | y<-(kleene xs), x<-xs]
