{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude

import System.Environment
import HsShellScript
-- import System.IO

import Data.List hiding (group)
import Data.List.NonEmpty (groupAllWith,toList,NonEmpty(..),head,group)
import Data.Eq.HT
import Data.Ord
import Data.Maybe
import Data.Function

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- import Control.Applicative

import Data.Array.ST
import Control.Monad.ST
import Data.Array.MArray


class Translate d where
  translate :: String -> Maybe d

data Rating = Rating { ratedUserID::Int
                     , ratedMovieID::Int
                     , score::Int
                     , ratedTime::Int
                     } deriving (Eq,Show)

data Gender = M | F deriving (Eq,Show)

data Age = Age1 | Age18 | Age25 | Age35 | Age45 | Age50 | Age56 deriving (Eq,Show)

data Occupation = Other
                | AcademicOrEducator
                | Artist
                | ClericalOrAdmin
                | CollegeOrGradStudent
                | CustomerService
                | DoctorOrHealthCare
                | ExecutiveOrManagerial
                | Farmer
                | Homemaker
                | K12Student
                | Lawyer
                | Programmer
                | Retired
                | SalesOrMarketing
                | Scientist
                | SelfEmployed
                | TechnicianOrEngineer
                | TradesmanOrCraftsman
                | Unemployed
                | Writer
                deriving (Eq,Show)
                
data User = User { userID::Int
                 , gender::Gender
                 , age::Age
                 , occupation::Occupation
                 , zipCode::Int
                 } deriving (Eq,Show)

data Gener = Action     
           | Adventure  
           | Animation  
           | Children's 
           | Comedy     
           | Crime      
           | Documentary
           | Drama      
           | Fantasy    
           | FilmNoir  
           | Horror     
           | Musical    
           | Mystery    
           | Romance    
           | SciFi     
           | Thriller   
           | War        
           | Western
           deriving (Eq,Show)

data Movie = Movie { movieID::Int
                   , name::String
                   , year::Int
                   , geners::[Gener]
                   } deriving (Eq,Show)


instance Translate Int where
  translate = (fmap $ foldl (\i d -> 10*i+d) 0)
              . sequenceA
              . fmap (\c -> elemIndex c "0123456789")

instance Translate Rating where
  translate = (ratingOfList =<<) . sequenceA . fmap translate . split where
    ratingOfList (user:movie:score:time:[]) = Just (Rating user movie score time)
    ratingOfList _ = Nothing

instance Translate Gender where
  translate "M" = Just M
  translate "F" = Just F
  translate _ = Nothing

instance Translate Age where
  translate "1" = Just Age1
  translate "18" = Just Age18
  translate "25" = Just Age25
  translate "35" = Just Age35
  translate "45" = Just Age45
  translate "50" = Just Age50
  translate "56" = Just Age56
  translate _ = Nothing

instance Translate Occupation where
  translate "0" = Just Other
  translate "1" = Just AcademicOrEducator
  translate "2" = Just Artist
  translate "3" = Just ClericalOrAdmin
  translate "4" = Just CollegeOrGradStudent
  translate "5" = Just CustomerService
  translate "6" = Just DoctorOrHealthCare
  translate "7" = Just ExecutiveOrManagerial
  translate "8" = Just Farmer
  translate "9" = Just Homemaker
  translate "10" = Just K12Student
  translate "11" = Just Lawyer
  translate "12" = Just Programmer
  translate "13" = Just Retired
  translate "14" = Just SalesOrMarketing
  translate "15" = Just Scientist
  translate "16" = Just SelfEmployed
  translate "17" = Just TechnicianOrEngineer
  translate "18" = Just TradesmanOrCraftsman
  translate "19" = Just Unemployed
  translate "20" = Just Writer
  translate _ = Nothing

instance Translate User where
  translate = userOfList . split where
    userOfList (id:gender:age:occ:zc:[]) = User
                                           <$> translate id
                                           <*> translate gender
                                           <*> translate age
                                           <*> translate occ
                                           <*> translate zc
    userOfList _ = Nothing

instance Translate Gener where
  translate "Action" = Just Action
  translate "Adventure" = Just Adventure
  translate "Animation" = Just Animation
  translate "Children's" = Just Children's
  translate "Comedy" = Just Comedy
  translate "Crime" = Just Crime
  translate "Documentary" = Just Documentary
  translate "Drama" = Just Drama
  translate "Fantasy" = Just Fantasy
  translate "Film-Noir" = Just FilmNoir
  translate "Horror" = Just Horror
  translate "Musical" = Just Musical
  translate "Mystery" = Just Mystery
  translate "Romance" = Just Romance
  translate "Sci-Fi" = Just SciFi
  translate "Thriller" = Just Thriller
  translate "War" = Just War
  translate "Western" = Just Western
  translate _ = Nothing

instance Translate Movie where
  translate = movieOfList . split where
    movieOfList (id:title:generes:[]) = do
      (name,year) <- nameYear title
      Movie <$> translate id <*> pure name <*> pure year <*> generesOf generes where
        generesOf = sequenceA . fmap translate . split where
          split ('|':cs) = [] : split cs
          split (c:cs) = extendSplit $ split cs where
            extendSplit [] = [[c]]
            extendSplit (cs:css) = (c:cs):css
          split "" = []
        nameYear (' ':'(':cs) = fmap ( "" , ) $ translate =<< end cs
        nameYear (c:cs) = do
          (name,year) <- nameYear cs
          return (c:name,year)
        nameYear [] = Nothing
        end ")" = Just ""
        end (c:cs) = (c:) <$> end cs
        end [] = Nothing
    movieOfList _ = Nothing


split [] = []
split (':':':':cs) = [] : split cs
split (c:cs) = splitExtend $ split cs where
  splitExtend [] = [[c]]
  splitExtend (cs:css) = (c:cs):css

nth _ [] = Nothing
nth 0 (x:_) = Just x
nth n (_:xs) = nth (n-1) xs


filterRatingUsers :: [Rating] -> [Rating]
filterRatingUsers = mconcat . fmap toList
                    . filter ((>=20) . length) . groupAllWith ratedUserID

filterRatingMovies :: [Rating] -> [Rating]
filterRatingMovies = mconcat . fmap toList
                     . filter ((>=10) . length)
                     . groupAllWith ratedMovieID

alignBy filters =
  fst . Data.List.head . dropWhile (uncurry (/=)) . (\xs -> zip xs $ tail xs)
  . flip (scanl (flip ($))) (repeat $ foldl (.) id filters)

alignRatings = alignBy [filterRatingUsers,filterRatingMovies]

usersAmount :: [Rating] -> Int
usersAmount = length . group . sort . fmap ratedUserID

watchesProbs :: [Rating] -> [(Int,Double)]
watchesProbs ratings =
  fmap moviesToProb . group . sort . fmap ratedMovieID $ ratings
  where
    moviesToProb :: NonEmpty Int -> (Int,Double)
    moviesToProb (r :| rs) =
      ( r , (1 + length rs) // usersAmount ratings )

watchesDualProbs :: [Rating] -> [(Int,Int,Double)]
watchesDualProbs ratings =
  fmap movie2sToProb
  . group . sort
  . mconcat
  . fmap (allCombinations . sort . fmap ratedMovieID . toList)
  . groupAllWith ratedUserID
  $ ratings
  where
    movie2sToProb ((r1,r2) :| rs) = (r1,r2,(1+length rs) // usersAmount ratings)

allCombinations xs = [ (x1,x2) | x1 <- xs , x2 <- xs , x1 < x2 ]

(//) = (/) `on` fromIntegral

data Sign = Pos | Neg deriving (Show)

watchesCorelationSign :: [Rating] -> [(Int,Int,Sign)]
watchesCorelationSign ratings =
  let duals = watchesDualProbs ratings
      allComb = allCombinations $ watchesProbs ratings
  in consume duals allComb where
    consume (d@(m1,m2,p12):ds) (c@((m1',p1),(m2',p2)):cs)
      | (m1,m2) <  (m1',m2') = consume ds     (c:cs)
      | (m1,m2) >  (m1',m2') = consume (d:ds) cs
      | (m1,m2) == (m1',m2') = ( m1 , m2 , sign $ p12-p1*p2 ) : consume ds cs
    consume _ _ = []
    sign x | x>0  = Pos
           | x<=0 = Neg


getMoviesFromArgs :: MaybeT IO [Int]
getMoviesFromArgs = do
  moviesFile <- MaybeT $ fmap (nth 2) getArgs
  movies <- lift . contents $ moviesFile
  MaybeT . return . sequenceA . fmap translate . lines $ movies

getRatingsFromArgs :: MaybeT IO String
getRatingsFromArgs = do
  dir <- MaybeT $ fmap (nth 0) getArgs
  lift . contents $ dir <> "./ratings.dat"

maybeHead [] = Nothing
maybeHead (x:_) = Just x



main :: IO ()
main = do
  let x = runST $ modify_x
  print x
    where
      modify_x :: ST s Int
      modify_x = do
        x :: <- newListArray (1,1000000000) [1..1000000000]
        traverse (\i -> writeArray x i (-i)) [1..10000000]
        readArray x 10
  
  -- x <- runMaybeT (do ratings <- getRatingsFromArgs
  --                    translated <- MaybeT . pure . traverse translate . lines $ ratings
  --                    lift . print . length . watchesCorelationSign
  --                      $ (translated :: [Rating])
  --                )
  -- print $ case x of Just _ -> 1
  --                   Nothing -> 2
  
  -- pure ()
  -- sequence_ $ do
  --   ratings <- ratings
  --   translated <- traverse translate . lines $ ratings
  --   let watches =  watchesCorelationSign $ translated
  --   pure . print $ 123
    
    
    
--  sequenceA $ fmap (print . watchesCorelationSign) $ sequenceA . fmap (translate =<<) . lines $ ratings
  -- return ()

   
