eulerTwo n = sum [ m | m <- takeWhile(<=n) fibs, even m]
	where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)