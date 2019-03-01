import Network.Wai.Handler.Warp (run)

import MergeBot.Server (initApp)

main :: IO ()
main = initApp >>= run 3000
