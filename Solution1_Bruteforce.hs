import System.Environment
import Slide
import Files
import Data.List
import Debug.Trace

filenames :: [String]
filenames = ["a_example", "b_lovely_landscapes"]

main = do
  args <- getArgs
  let dataset = read $ args!!0
  photos <- readPhotos dataset
  traceM "Calculating slides"
  let slides = allPossibleSlides photos
  traceM $ "Calculating solution (slides: " <> show (length slides) <> ")"
  let solution = buildSlideshow slides
  putStrLn $ "Total score: " <> show (totalScore solution)
  writeSlideshow dataset solution


-- Bruteforce solution: tries every combination
buildSlideshow :: [Slide] -> Slideshow
buildSlideshow = appendBestSlides []

-- Recursively appends the highest scoring slide to the slideshow
appendBestSlides :: Slideshow -> [Slide] -> Slideshow
appendBestSlides xs []      = xs
appendBestSlides [] (y:ys)  = appendBestSlides [y] ys
appendBestSlides (x:xs) ys  = trace ("Score: " <> show (transitionScore s x)) $ appendBestSlides (s:x:xs) (removeSlide s ys)
  where
    s = bestMatch x ys



