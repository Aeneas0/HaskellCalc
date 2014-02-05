main = do
	putStrLn "Please enter an expression to evaluate."
	seq <- getLine
	print $ readRPN $ shuntingYard $ words seq
	
isNum :: String -> Bool
isNum str
	| str == "-" = False
	| otherwise = all isDigit str
	where isDigit n
		| n `elem` ['0'..'9'] = True
		| n == '-'	  	  	  = isNum $ tail str
		| n == '.'			  = isNum $ tail str
		| otherwise 		  = False

isOp :: String -> Bool
isOp n = n `elem` ["+", "-", "*", "/", "^"]

isLeftAssoc :: String -> Bool
isLeftAssoc "^" = False
isLeftAssoc  _  = True

getPrecedence :: String -> Integer
getPrecedence n
	| n `elem` ["+", "-"] = 0
	| n `elem` ["*", "/"] = 1
	| n `elem` ["^"]	  = 2
	| otherwise			  = -1
	
shuntingYard :: [String] -> [String]
shuntingYard xs = reverse $ parse xs ([], [])
	where 
		parse [] ([], outs) = outs
		parse [] ((o:ops), outs) = parse [] (ops, (o:outs))	
		parse (x:xs) ([], [])
			| isNum x = parse (xs) ([], [x])
			| isOp x  = parse (xs) ([x], [])
		parse (x:xs) ((ops), outs)
			| isNum x = parse xs (ops, (x:outs))
			| isOp x = parse xs $ reconfigStack (x:ops, outs)
			| x == "(" = parse xs (x:ops, outs)
			| x == ")" = parse xs $ handleParens (ops, outs)
			
handleParens :: ([String], [String]) -> ([String], [String])
handleParens ([], _) = error "Unmatched parentheses."
handleParens (x:ops, outs)
	| x == "(" = (ops, outs)
	| otherwise = handleParens (ops, x:outs)
			
reconfigStack :: ([String], [String]) -> ([String], [String])
reconfigStack (o1:[], outs) = ([o1], outs)
reconfigStack (o1:o2:xs, outs)
	| (isLeftAssoc o1 && (getPrecedence o1 == getPrecedence o2)) = reconfigStack (o1:xs, o2:outs)
	| (getPrecedence o1 < getPrecedence o2) = reconfigStack (o1:xs, o2:outs)
	| otherwise = (o1:o2:xs, outs)
	
readRPN :: [String] -> Double
readRPN = head . foldl foldingFunction []
	where 
		foldingFunction (x:y:ys) "*" = (x * y):ys
		foldingFunction (x:y:ys) "+" = (x + y):ys
		foldingFunction (x:y:ys) "-" = (y - x):ys
		foldingFunction (x:y:ys) "/" = (y / x):ys
		foldingFunction (x:y:ys) "^" = (y ** x):ys
		foldingFunction xs numberString = (read numberString :: Double):xs
