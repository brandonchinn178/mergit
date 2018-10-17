import qualified Data.Map.Strict as Map
import Example (getContinents, runApp)

main :: IO ()
main = mapM_ printContinent . Map.toList =<< runApp getContinents
  where
    printContinent (continent, countries) = do
      putStrLn $ "Continent: " ++ show continent
      print countries
