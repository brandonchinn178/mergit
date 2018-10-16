import qualified Data.Map.Strict as Map
import Example (getCountries, runApp)

main :: IO ()
main = mapM_ printContinent . Map.toList =<< runApp getCountries
  where
    printContinent (continent, countries) = do
      putStrLn $ "Continent: " ++ show continent
      print countries
