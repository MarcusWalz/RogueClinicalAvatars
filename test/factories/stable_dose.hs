{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid 
import Data.List (zip4)
import System.Random

data Race = Asian | Black | White
  deriving(Show, Eq)

data VKORC1G = AA | AG | GG
  deriving(Eq)

instance Show VKORC1G where
  show AA = "A/A"
  show AG = "A/G"
  show GG = "G/G"

data VKORC1T = CC | CT | TT
  deriving(Eq)
  
instance Show VKORC1T where
  show CC = "C/C"
  show CT = "C/T"
  show TT = "T/T"

data CYP2C9 = OneOne | OneTwo | OneThree | TwoOne | TwoTwo | TwoThree | ThreeThree
  deriving(Eq)

instance Show CYP2C9 where
  show OneOne     = "*1/*1"
  show OneTwo     = "*1/*2"
  show OneThree   = "*1/*3"
  show TwoOne     = "*2/*1"
  show TwoTwo     = "*2/*2"
  show TwoThree   = "*2/*3"
  show ThreeThree = "*3/*3"

data Gender = Male | Female 
  deriving(Show, Eq)

type INR  = Double
type Dose = Double

data Avatar = Avatar 
  { race      :: Maybe Race
  , vkorc1g   :: Maybe VKORC1G
  , vkorc1t   :: Maybe VKORC1T
  , cyp2c9    :: Maybe CYP2C9
  , gender    :: Maybe Gender
  , enzyme    :: Bool
  , smoker    :: Bool
  , dvt       :: Bool
  , amiodaron :: Bool
  , age       :: Double
  , height    :: Double
  , weight    :: Double
  , tinr      :: INR
  } deriving(Show, Eq)

sampleAvatar = Avatar (Just White) (Just AA) (Just CC) (Just OneOne) (Just Male)
  False False False False 40 121 31 3.4

data Sim = Sim 
  { days :: Int
  } deriving(Show, Eq)

data SimDay = SimDay
  { day   :: Int 
  , inr   :: INR 
  , dose  :: Dose 
  , check :: Bool 
  } deriving(Show, Eq)

type SimOut = [SimDay]


arrayToSimOut :: [INR] -> [Dose] -> [Bool] -> SimOut
arrayToSimOut inr dose check = map (\(a,b,c,d)->SimDay a b c d) $ zip4 [1..] inr dose check

newtype Sim_Factory g a = Sim_Factory (g -> Avatar -> Sim -> [a])

instance Num a => Monoid (Sim_Factory g a) where
  mempty  = Sim_Factory $ \gen av sim -> replicate (days sim) 0
  mappend (Sim_Factory f) (Sim_Factory g) =
    Sim_Factory $ \gen av sim -> zipWith (+) (f gen av sim) (g gen av sim) 

instance Monoid (Sim_Factory g Bool) where   
  mempty  = Sim_Factory $ \gen av sim -> replicate (days sim) False
  mappend (Sim_Factory f) (Sim_Factory g) =
    Sim_Factory $ \gen av sim -> zipWith (||) (f gen av sim) (g gen av sim) 
    
inr_constant :: INR -> Sim_Factory g INR
inr_constant inr = Sim_Factory $ \g av sim -> replicate (days sim) inr

inr_target :: Sim_Factory g INR
inr_target = Sim_Factory $ \g av sim -> replicate (days sim) (tinr av)

-- inr_spike :: Int -> Int -> Double -> Sim_Factory g INR

testData = arrayToSimOut [1,2,3,4,5,1,2,3,4,3,3,4] [10, 10, 11, 12, 13, 10, 10,10,10,10] [True, True, True, False, False, False, False, False, False, True, False, False, False, False, True, True]


to_split :: SimOutOps -> SimOutOps
to_split (Filter f) = Split $ \sim_out -> return $ f sim_out
to_split (Split  f) = Split f

-- | Operation to apply to Simulation Outputs
-- We can pull things apart but never back together. 
data SimOutOps = 
    Split  (SimOut -> [SimOut])
  | Filter (SimOut -> SimOut)

instance Monoid SimOutOps where
  mempty
    = Split $ \x -> return x
  mappend a b
    = Split $ \sim_out -> concatMap g $ f sim_out
    where (Split f) = to_split a
          (Split g) = to_split b

execute :: SimOutOps -> SimOut -> [SimOut]
execute s input = let (Split f) = to_split s 
                in filter (/=[]) $ f input

splitByDose :: SimOutOps
splitByDose = Split $ \out -> splitByDose' out

splitByDose' ([]) = []
splitByDose' (l@(x:_)) = 
  takeWhile (\s_out -> (dose s_out) == (dose x)) l :
  (splitByDose' $ dropWhile (\s_out -> (dose s_out) == (dose x)) l)

splitByInrStability :: (Double -> Bool) -> SimOutOps
splitByInrStability f = Split $ \out ->  splitByInrStability' f out
splitByInrStability' f ([]) = []
splitByInrStability' f (l@(x:_)) = 
  takeWhile (\s_out -> (f $ inr s_out) == (f $ inr x)) l :
  (splitByDose' $ dropWhile (\s_out -> (f $ inr s_out) == (f $ inr x)) l)

-- 2 lines
filterInr :: (Double -> Bool) -> SimOutOps
filterInr f = Filter $ \sim_out -> filter (\sim_day -> f $ inr sim_day) sim_out

stableStreaks :: (Double -> Bool) -> SimOutOps
stableStreaks f =  splitByDose <> splitByInrStability(f) <> filterInr(f)

-- | Reduction steps
data Reducer = Reducer (SimOut -> SimOut -> SimOut)

exeReduce :: Reducer -> [SimOut] -> Maybe SimOut
exeReduce _ []        = Nothing
exeReduce (Reducer r) xs = Just $ foldl1 r xs

-- | return the biggest table w/r/t the number of rows. When in doubt
-- return the oldest
longest :: Reducer
longest = Reducer $ \a b -> 
  if length a > length b then a else b 

-- | return the longest table w/r/t to time elapsed. useful when only looking at checks.
longest_elapsed :: Reducer 
longest_elapsed = Reducer $ \a b ->
  if (days_elapsed a) > (days_elapsed b) then a else b
  where days_elapsed sim_out = day ( last sim_out ) - day ( head sim_out ) + 1

-- | Calculate the dose based on the winning SimOut table 
data DoseFunction = DoseFunction (SimOut -> Maybe INR)

my_exe :: SimOutOps -> Reducer -> DoseFunction -> SimOut -> Maybe INR
my_exe simOutOps r (DoseFunction f) sim_out =
  (exeReduce r $ (execute simOutOps) sim_out) >>= \a -> f a

firstDose = DoseFunction $ \sim_out -> Just $ dose $ head sim_out

newtype StableDose = StableDose (Avatar -> SimOut -> Maybe Dose)

main = do
  print $ my_exe (stableStreaks ((==) 1)) longest firstDose testData 
