geom_progressia :: Float -> Float -> Float -> Float
geom_progressia b1 q 1 = b1
geom_progressia b1 q n = if n<1 then error "Not Positive!" else b1*(q**(n-1))+(geom_progressia b1 q (n-1))

geom_progressia2 :: Float -> Float -> Float
geom_progressia2 b1 q = if q>(-1) && q<1 then b1/(1-q) else error "q<=-1 or q>=1"

print1 :: Float-> Float
print1 n = n

geom_progressia3 :: Float -> Float -> Float -> Float -> Float
geom_progressia3 b1 q n e = if abs(geom_progressia b1 q n - geom_progressia2 b1 q)>=e then geom_progressia3 b1 q (n+1) e else print1 n

f b1 q e = geom_progressia3 b1 q 1 e