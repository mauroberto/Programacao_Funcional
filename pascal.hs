binomio n k = (product [1..n])/((product [1..k]) * (product[1..(n-k)]))

pascal = [[round(binomio n k) | k <- [0..n]] | n<-[0..]]

pascal2 = [[ if k == 0 || k == n then 1 else (pascal2 !! (n-1) !! (k-1)) + (pascal2 !! (n-1) !! k)| k<-[0..n] ]| n<-[0..]] 
