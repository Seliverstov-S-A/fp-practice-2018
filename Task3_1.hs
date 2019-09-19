module Task3_1 where


data WeirdPeanoNumber = Zero 
                        | Succ (WeirdPeanoNumber) 
						| Pred (WeirdPeanoNumber)

instance Show WeirdPeanoNumber where
    show a = show (int' a)
	
instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.int'

instance Real WeirdPeanoNumber where
    toRational x = toRational (int' x)	
				
instance Eq WeirdPeanoNumber where
    (==) a b = deq' (dec' a) (dec' b)
	where
    		deq' Zero Zero = True
    		deq' Zero _    = False
    		deq' _ Zero    = False
   		deq' (Pred a) (Pred b) = deq' a b
    		deq' (Succ a) (Succ b) = deq' a b
    		deq' _ _               = False
				
int' :: WeirdPeanoNumber -> Integer
int' Zero = 0
int' (Succ a) = 1 + (int' a)
int' (Pred a) = (int' a) - 1

num' :: Integer -> WeirdPeanoNumber
num' x
  | x > 0 = Succ (num' (x - 1))
  | x < 0 = Pred (num' (x + 1))
  | otherwise = Zero

dec' Zero = Zero
dec' (Succ (Pred a)) = dec' a
dec' (Pred (Succ a)) = dec' a
dec' (Succ a) = let dec = dec' a in 
                 case dec of (Pred b) -> b 
                             _        -> Succ $ dec
dec' (Pred a) = let dec = dec' a in
                 case dec of (Succ b) -> b
                             _        -> Pred $ dec				 
							 
instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) (Pred a) b = (+) a (Pred b)
    (+) (Succ a) b = (+) a (Succ b)

    signum a = case dec' a of 
                      Succ _ -> Succ Zero
                      Pred _ -> Pred Zero
                      _      -> Zero

    negate Zero = Zero
    negate (Pred a) = Succ ( negate a)
    negate (Succ a) = Pred ( negate a)
	
    abs a = if signum a < Zero then negate a else a

    (*) a b  = dcMlt (dec' a) (dec' b)
		where
			dcMlt Zero _ = Zero
                        dcMlt _ Zero = Zero
			dcMlt (Succ a) (Succ b) = dec' ( Succ ( a * b + a + b))
                        dcMlt (Pred a) (Pred b) = dec' (Succ ( a * b - a - b))
                        dcMlt (Succ a) (Pred b) = dec' (Pred ( a * b - a + b))
                        dcMlt (Pred a) (Succ b) = dec' (Pred ( a * b + a - b))

    fromInteger = num'

instance Ord WeirdPeanoNumber where
    (<=) a b = decOrd' (dec' a) (dec' b)
	where
		decOrd' (Pred a) (Pred b) = decOrd' a b
		decOrd' (Succ a) (Succ b) = decOrd' a b
		decOrd' Zero a  = case a of
                                    Pred _ -> False
                                    _      -> True
		decOrd' a Zero  = case a of
                                    Succ _ -> False
                                    _      -> True

instance Integral WeirdPeanoNumber where
    toInteger = int'
    quotRem a b 
	      | signum a == signum b = divCnt (abs b) (Zero, (abs a)) 
          | otherwise  = (\(a,b) -> (dec' $ negate a, dec' b)) (divCnt (abs b) (Zero, abs a))
                   where divCnt b  res@(quot, rem) | rem >= b = divCnt b (quot + 1, rem - b)
                                                   | otherwise = res
-- Test:												   
-- show $ fst ( quotRem (num' (-7)) (num' 8) ) + (num' 2) + fst ( quotRem (num' (-7)) (num' 8) )												   
												   