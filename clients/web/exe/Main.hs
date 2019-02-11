import Yesod (warp)

import MergeBot.Client (App(..))

main :: IO ()
main = warp 8080 App
