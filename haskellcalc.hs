isNum :: String -> Bool --Note: Returns false for all negative integers.
isNum = all isDigit
	where isDigit n
		| n `elem` ['0'..'9'] = True
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
shuntingYard xs = reverse $ parse xs ([], []) --is reverse necessary?
	where 
		parse [] ([], outs) = outs
		parse [] ((o:ops), outs) = parse [] (ops, (o:outs))	
		parse (x:xs) ([], [])
			| isNum x = parse (xs) ([], [x])
			| isOp x  = parse (xs) ([x], [])
		parse (x:xs) ((ops), outs)
			| isNum x = parse xs (ops, (x:outs))
			| isOp x = parse xs $ reconfigStack (x:ops, outs)
			
reconfigStack :: ([String], [String]) -> ([String], [String])
reconfigStack (o1:[], outs) = ([o1], outs)
reconfigStack (o1:o2:xs, outs)
	| (isLeftAssoc o1 && (getPrecedence o1 == getPrecedence o2)) = reconfigStack (o1:xs, o2:outs)
	| (getPrecedence o1 < getPrecedence o2) = reconfigStack (o1:xs, o2:outs)
	| otherwise = (o1:o2:xs, outs)
