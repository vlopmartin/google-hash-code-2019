-- Vertical slides get grouped up as they go, without caring if it's optimal
-- Dataset 2 seems to work but score isn't very high

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


-- Groups up vertical slides as they appear
getSlides :: [Photo] -> [Slide]
getSlides = verticalPairedSlides

-- Bruteforce solution: always searches for the best match
buildSlideshow :: [Slide] -> Slideshow
buildSlideshow = appendBestSlides []
