import Control.Monad
import Happstack.Server

main = simpleHTTP (nullConf {port = 80}) $ msum [dir "main" $ ok "nar", ok "Error, unknown path."]