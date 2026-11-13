module Logger where

import Types

logOrderReceived :: OrderRequest -> IO ()
logOrderReceived o = putStrLn ("[RECV] " ++ show o)

logOrderFilled :: Fill -> IO ()
logOrderFilled f = putStrLn ("[FILL] " ++ show f)

