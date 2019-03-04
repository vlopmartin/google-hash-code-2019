-- When scanning the list of slides, gets the first one with a nonzero score
-- Faster than 2, but still doesn't seem viable with large datasets

import System.Environment
import Data.List
import Debug.Trace
import Strategies
import Files

main = do
  args <- getArgs
  let dataset = read $ args!!0
  photos <- readPhotos dataset
  traceM "Calculating slides"
  let slides = getSlides photos
  traceM $ "Calculating solution (slides: " <> show (length slides) <> ")"
  let solution = buildSlideshow slides
  putStrLn $ "Total score: " <> show (totalScore solution)
  writeSlideshow dataset solution

-- Bruteforce solution: tries every combination
getSlides :: [Photo] -> [Slide]
getSlides = allPossibleSlides

-- Searches for the first nonzero scoring slide
buildSlideshow :: [Slide] -> Slideshow
buildSlideshow = appendFirstScoringSlides []

