fibonacci = 1 : 2 : [f n | n <- [3..]]
	where 
		f 0 = 1
		f 1 = 1
		f n = if n > 1 then f (n-1) + f (n-2) else 0


answer = sum [x | x <- fibonacci, even x, x <= n]
    where n = 4000000