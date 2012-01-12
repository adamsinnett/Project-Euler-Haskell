-- from eulerThree.hs

primeFactors n = factor n primes
	where factor n (p:ps)
		| p * p > n			= [n]
		| n `mod` p == 0	= p : factor (n `div` p) (p:ps)
		| otherwise			= factor n ps

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

eulerSeven n = primes !! (n-1)