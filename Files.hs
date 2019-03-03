module Files where

import Slide
import System.Directory

filenames :: [String]
filenames = ["a_example", "b_lovely_landscapes", "c_memorable_moments", "d_pet_pictures", "e_shiny_selfies"]

inFilename :: Int -> String
inFilename n = "input/" <> (filenames!!n) <> ".txt"

outFilename :: Int -> String
outFilename n = "output/" <> (filenames!!n) <> ".out"

readPhotos :: Int -> IO [Photo]
readPhotos = fmap inputToPhotos . readFile . inFilename

writeSlideshow :: Int -> Slideshow -> IO ()
writeSlideshow n ss = createDirectoryIfMissing True "output" >> (writeFile (outFilename n) $ slideshowToOutput ss)
