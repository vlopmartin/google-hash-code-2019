module Slide
  ( module Slide
  , module Photo
  ) where

import Data.List
import Photo

type Score = Int
data Slide = Single Photo | Double Photo Photo deriving (Show, Eq)
-- This distinction is important, not all lists of slides are a valid slideshow
type Slideshow = [Slide]

-- List of tags of a slide
slideTags :: Slide -> [Tag]
slideTags (Single h) = tags h
slideTags (Double v1 v2) = tags v1 `union` tags v2

-- Score given by the transition between two slides
transitionScore :: Slide -> Slide -> Score
transitionScore s1 s2 = commonTags `min` s1Tags `min` s2Tags
  where
    commonTags = length (slideTags s1 `intersect` slideTags s2)
    s1Tags = length (slideTags s1) - commonTags
    s2Tags = length (slideTags s2) - commonTags

-- Total score of a slideshow
totalScore :: Slideshow -> Score
totalScore xs = sum $ scores xs
  where
    scores xs = zipWith transitionScore xs (drop 1 xs)

-- Out of a list of slides, the one that maximizes the transition score with the given slide
bestMatch :: Slide -> [Slide] -> Slide
bestMatch x = maximumBy (higherScore x)
  where
    higherScore s s1 s2 = compare (transitionScore s s1) (transitionScore s s2)

-- Out of a list of slides, the first one that has a score higher than the given score with the given slide
firstMatchHigherThan :: Score -> Slide -> [Slide] -> Slide
firstMatchHigherThan n x = first ((> n) . transitionScore x)
  where
    first p xs
     | null (filter p xs) = head xs
     | otherwise          = head (filter p xs)

-- Out of a list of slides, the first one that has nonzero score with the given slide
firstNonzeroMatch :: Slide -> [Slide] -> Slide
firstNonzeroMatch = firstMatchHigherThan 0

-- Whether the two given slides have photos in common (only applies to double slides)
haveCommonPhotos :: Slide -> Slide -> Bool
haveCommonPhotos (Double p1 p2) (Double p3 p4) = (p1 == p3) || (p1 == p4) || (p2 == p3) || (p2 == p4)
haveCommonPhotos _ _ = False

-- List of slides remaining when you remove a given slide
-- (If the removed slide is vertical, then other slides need to be removed too!)
removeSlide :: Slide -> [Slide] -> [Slide]
removeSlide s@(Single _) = delete s
removeSlide s@(Double _ _) = filter (not . haveCommonPhotos s)

-- Gets all the photo IDs in a slide
getPhotoIds :: Slide -> [Id]
getPhotoIds (Single p) = [photoId p]
getPhotoIds (Double p1 p2) = [photoId p1, photoId p2]

-- Converts a slideshow into the required output format
slideshowToOutput :: Slideshow -> String
slideshowToOutput ss = unlines $ ((show $ length ss):) $ map (unwords . map show . getPhotoIds) ss
