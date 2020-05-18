{- Taken from gist https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900, found at https://stackoverflow.com/questions/46789241/redirections-in-servant -}
{-
$ curl -X POST localhost:9876/dog -v
* Connected to localhost (127.0.0.1) port 9876 (#0)
> POST /dog HTTP/1.1
> Host: localhost:9876
> User-Agent: curl/7.55.1
> Accept: */*
> 
< HTTP/1.1 301 Moved Permanently
< Transfer-Encoding: chunked
< Date: Tue, 17 Oct 2017 12:08:01 GMT
< Server: Warp/3.2.13
< Content-Type: application/json;charset=utf-8
< Location: https://google.com/?q=dog
< 
* Connection #0 to host localhost left intact
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module ServantExtensions where

import GHC.TypeLits
import Servant

type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)

type RedirectHandler loc = Handler (Headers '[Header "Location" loc] NoContent)

redirect
  :: ToHttpApiData loc
  => loc -- ^ what to put in the 'Location' header
  -> RedirectHandler loc
redirect a = return (addHeader a NoContent)