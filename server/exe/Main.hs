import Network.Wai.Handler.Warp (run)

import MergeBot.Server (initApp)

main :: IO ()
main = run 3000 =<< initApp
