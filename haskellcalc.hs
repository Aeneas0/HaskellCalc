isNum :: String -> Bool --Note: Returns false for all negative integers.
isNum = all isDigit
	where isDigit n
		| n `elem` ['0'..'9'] = True
		| otherwise 		  = False

isOp :: String -> Bool
isOp n = n `elem` ["+", "-", "*", "/", "^"]

getPrecedence :: String -> Integer
getPrecedence n
	| n `elem` ["+", "-"] = 0
	| n `elem` ["*", "/"] = 1
	| n `elem` ["^"]	  = 2
	| otherwise			  = -1
	
shuntingYard :: [String] -> [String]
shuntingYard xs = parse xs [] []
	where 
		parse [] [] outs = outs
		parse [] (o:ops) outs = parse [] ops (o:outs)
		parse (x:xs) (o:ops) outs
			| isNum x = parse (x:xs) (o:ops) (x:outs)
			| isOp x = undefined