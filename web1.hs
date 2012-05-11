import Control.Monad
import Happstack.Server

main = simpleHTTP (nullConf {port = 80}) $ msum [dir "main" $ path serveMain, ok "Error, unknown path."]

serveMain path = ok path