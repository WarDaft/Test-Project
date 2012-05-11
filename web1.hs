import Happstack.Server

main = simpleHTTP (nullConf {port = 80}) $ ok "Test."