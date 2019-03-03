module Photo where

type Id = Int
type Tag = String
data Photo = Photo {
  photoId :: Id,
  isVertical :: Bool,
  tags :: [Tag]
} deriving (Show, Eq)

getPhotos :: [String] -> [Photo]
getPhotos = zipWith getPhoto [1..] . map words
  where
    getPhoto i (x:_:xs) = Photo i (x == "V") xs

inputToPhotos :: String -> [Photo]
inputToPhotos = getPhotos . tail . lines
