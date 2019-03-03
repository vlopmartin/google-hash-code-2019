module Strategies
  ( module Strategies
  , module Slide
  ) where

import Slide
import Debug.Trace

-- Every possible horizontal and vertical slide out of a list of photos
allPossibleSlides :: [Photo] -> [Slide]
allPossibleSlides = horizontalSlides <> verticalSlides
  where
    horizontalSlides = map Single . filter (not . isVertical)
    verticalSlides = pairsWithFirst . filter (isVertical)
    pairsWithFirst [] = []
    pairsWithFirst (p:ps) = zipWith Double (repeat p) ps ++ pairsWithFirst ps

-- Vertical slides get paired with their closest vertical neighbor
verticalPairedSlides :: [Photo] -> [Slide]
verticalPairedSlides = horizontalSlides <> verticalSlides
  where
    horizontalSlides = map Single . filter (not . isVertical)
    verticalSlides = pairs . filter (isVertical)
    pairs [] = []
    pairs [_] = []
    pairs (p1:p2:ps) = (Double p1 p2):(pairs ps)


-- Recursively appends the highest scoring slide to the slideshow
appendBestSlides :: Slideshow -> [Slide] -> Slideshow
appendBestSlides xs []      = xs
appendBestSlides [] (y:ys)  = appendBestSlides [y] ys
appendBestSlides (x:xs) ys  =
  --trace ("Score: " <> show (transitionScore s x)) $
  appendBestSlides (s:x:xs) (removeSlide s ys)
  where
    s = bestMatch x ys

-- Recursively appends the first nonzero scoring slide to the slideshow
appendFirstScoringSlides :: Slideshow -> [Slide] -> Slideshow
appendFirstScoringSlides xs []      = xs
appendFirstScoringSlides [] (y:ys)  = appendFirstScoringSlides [y] ys
appendFirstScoringSlides (x:xs) ys  =
  trace ("Score: " <> show (transitionScore s x)) $
  appendFirstScoringSlides (s:x:xs) (removeSlide s ys)
  where
    s = firstNonzeroMatch x ys
