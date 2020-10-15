timeMinus :: (Num a,Integral a)=> (a,a) -> (a,a) -> (a,a)
timeMinus (h1,m1) (h2,m2) = (mod (h1 - h2 + (div (m1 - m2) 60)) 24, mod (m1-m2) 60)

zeroToN :: (Enum a,Num a) => a -> [a]
zeroToN a  = [0..a]

--String: Data típusosztályban van, karaktereket fűz össze 
--Word: Int méretű előjel nélküli változó