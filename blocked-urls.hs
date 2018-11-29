import Data.Char
import System.FilePath

data U =
  U
    String
    [String]
  deriving (Eq, Ord, Show)

outname = "20161215-federal-court-block"

ulines ::
  String
  -> [U]
ulines =
  map uline . lines

uline ::
  String
  -> U
uline s =
  let (u, x) = break isSpace s
  in  U u (split (== ',') (dropWhile isSpace x))

split ::
  (a -> Bool)
  -> [a]
  -> [[a]]
split p s = 
  case  dropWhile p s of
          [] ->
            []
          t ->
            let (w, u) = break p t
            in  w : split p u

zoneFile ::
  U
  -> String
zoneFile (U u _) =
  concat
    [
      "zone \""
    , u
    , "\" {\n  type master;\n  file \"/etc/bind/zones/"
    , outname
    , "/"
    , u
    , ".db\";\n};\n"
    ]

dbFile ::
  U
  -> String
dbFile (U u x@(h:_)) =
  concat
    [
      "$TTL 14400\n@ IN SOA ns1.domain1.com. host.domain1.com. (\n201006601 ; Serial\n7200 ; Refresh\n120 ; Retry\n2419200 ; Expire\n604800) ; Default TTL\n;\n"
    , u
    , ". IN NS ns1.tmorris.intra.\n"
    , u
    , ". IN NS ns2.tmorris.intra.\n\n"
    , u
    , ". IN MX 10 thepiratebay.org.\n"
    , u
    , ". IN A 104.18.40.167\n"
    , u
    , ". IN A 104.18.41.167\n\n"
    , concatMap (\i -> "ns1 IN A " ++ i ++ "\n") x
    , "www IN CNAME "
    , u
    , ".\nmail IN A "
    , h
    , "\nftp IN CNAME "
    , u
    , ".\n"
    , u
    , ". IN TXT \"v=spf1 ip4:"
    , h
    , " a mx ~all\"\nmail IN TXT \"v=spf1 a -all\"\n"
    ]

writeOneU ::
  U
  -> IO ()
writeOneU v@(U u _) =
  do  writeFile (outname </> u ++ ".zone") (zoneFile v)
      writeFile (outname </> u ++ ".db") (dbFile v)
      appendFile (outname ++ ".conf") ("include \"/etc/bind/zones/" ++ outname ++ "/" ++ u ++ ".zone\";\n")

main =
  do  f <- readFile "blocked-urls"
      let u = ulines f
      mapM_ writeOneU u

