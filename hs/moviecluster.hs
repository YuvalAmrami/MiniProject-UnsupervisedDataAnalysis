{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module MovieClusters where

import Prelude

import System.Environment
import HsShellScript
-- import System.IO

import Data.List
import Data.Eq.HT
import Data.Ord

-- import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- import Control.Applicative

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
              . map (\c -> elemIndex c "0123456789")

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

chunksFilterBy by pred = mconcat . filter pred
                         . groupBy (equating by)
                         . sortBy (comparing by)

filterRatingUsers = chunksFilterBy ratedUserID $ (>=20) . length

filterRatingMovies = chunksFilterBy ratedMovieID $ (>=10) . length

alignBy filters =
  fst . head . dropWhile (uncurry (==)) . (\xs -> zip xs $ tail xs)
  . flip (scanl (flip ($))) (cycle filters)

alignRatings = alignBy [filterRatingUsers,filterRatingMovies]

-- [(movie,user)] -> [(movie,prob)]
probabilities :: Ord o => [o] -> (o,Double) List
probabilities watches = fmap (fmap (/ total)) occurence where
  occurence = catMaybes
              . fmap (\case []       -> Nothing
                            xs@(x:_) -> Some (x, length xs))
              . group . sort $ watches
  total = sum (fmap snd) occurence

-- [(x,y)] -> [((x1,x2),y)] where x1 /= x2 and y1 == y2
mutual :: (Ord x,Eq y) => [(x,y)] -> [(x,x),y]
mutual xys = do
  (x1,y1) <- xys
  (x2,y2) <- xys
  guard (x1 < x2 && y1 == y2)
  ((x1,x2),y1)

getMoviesFromArgs :: MaybeT IO [Int]
getMoviesFromArgs = do
  moviesFile <- MaybeT $ fmap (nth 2) getArgs
  movies <- lift . contents $ moviesFile
  MaybeT . return . sequenceA . fmap translate . lines $ movies

getRatingsFromArgs :: MaybeT IO String
getRatingsFromArgs = do
  dir <- MaybeT $ fmap (nth 0) getArgs
  lift . contents $ dir <> "./ratings.dat"


  

  
