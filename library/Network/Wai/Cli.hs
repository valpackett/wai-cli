{-# LANGUAGE CPP, OverloadedStrings, UnicodeSyntax #-}

module Network.Wai.Cli where

import           Network.Wai (responseLBS, Application)
import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp hiding (run)
#ifdef WaiCliTLS
import           Network.Wai.Handler.WarpTLS
#endif
#ifdef WaiCliFastCGI
import qualified Network.Wai.Handler.FastCGI as FCGI
#endif
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.HTTP.Types (serviceUnavailable503)
import           Network.Socket.Activation
import qualified Network.Socket as S
import           GHC.Conc (getNumCapabilities, forkIO)
import           System.Posix.Internals (setNonBlockingFD)
import           System.Posix.Signals (installHandler, sigTERM, Handler(CatchOnce))
import           Data.Streaming.Network (bindPath, bindPortTCP)
import           System.Console.ANSI
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Exception (bracket)
import           Options
import           Data.List (intercalate)
import           Data.String (fromString)
import           Data.IP (fromHostAddress, fromHostAddress6)

data GracefulMode = ServeNormally | Serve503

data WaiOptions = WaiOptions
  { wHttpPort                ∷ Int
  , wHttpHost                ∷ String
  , wUnixSock                ∷ String
  , wProtocol                ∷ String
#ifdef WaiCliTLS
  , wTlsKeyFile              ∷ String
  , wTlsCertFile             ∷ String
#endif
  , wGracefulMode            ∷ String
  , wDevlogging              ∷ Maybe Bool }

instance Options WaiOptions where
  defineOptions = pure WaiOptions
    <*> simpleOption "port"              3000                "The port the app should listen for connections on (for http)"
    <*> simpleOption "host"              "*4"                "Host preference (for http)"
    <*> simpleOption "socket"            "wai.sock"          "The UNIX domain socket path the app should listen for connections on (for unix)"
    <*> simpleOption "protocol"          "http"              ("The protocol for the server. One of: " ++ availableProtocols)
#ifdef WaiCliTLS
    <*> simpleOption "tlskey"            ""                  "Path to the TLS private key file for +tls protocols"
    <*> simpleOption "tlscert"           ""                  "Path to the TLS certificate bundle file for +tls protocols"
#endif
    <*> simpleOption "graceful"          "serve-normally"    "Graceful shutdown mode. One of: none, serve-normally, serve-503"
    <*> simpleOption "devlogging"        Nothing             "Whether development logging should be enabled"
    where
      availableProtocols = intercalate ", " $ concat
        [coreProtocols, tlsProtocols, fcgiProtocol]
      coreProtocols = ["http", "unix", "activate", "cgi"]
      tlsProtocols =
#ifdef WaiCliTLS
        ["http+tls", "unix+tls", "activate+tls"]
#else
        []
#endif
      fcgiProtocol =
#ifdef WaiCliFastCGI
        ["fastcgi"]
#else
        []
#endif


runActivated ∷ (Settings → S.Socket → Application → IO ()) → Settings → Application → IO ()
runActivated run warps app = do
  sockets ← getActivatedSockets
  case sockets of
    Just socks →
      void $ forM socks $ \sock → do
        setNonBlockingFD (S.fdSocket sock) True
        forkIO $ run warps sock app
    Nothing → putStrLn "No sockets to activate"

runGraceful ∷ GracefulMode → (Settings → Application → IO ()) → Settings → Application → IO ()
runGraceful mode run warps app = do
  -- based on https://gist.github.com/NathanHowell/5435345
  -- XXX: an option to stop accepting (need change stuff inside Warp?)
  shutdown ← newEmptyTMVarIO
  activeConnections ← newTVarIO (0 ∷ Int)
  _ ← installHandler sigTERM (CatchOnce $ atomically $ putTMVar shutdown ()) Nothing
  let warps' = setOnOpen  (\_ → atomically (modifyTVar' activeConnections (+1)) >> return True) $
               setOnClose (\_ → atomically (modifyTVar' activeConnections (subtract 1)) >> return ()) warps
  let app' = case mode of
        ServeNormally → app
        Serve503 → \req respond → do
          shouldRun ← liftIO . atomically $ isEmptyTMVar shutdown
          if shouldRun then app req respond else respond $ responseLBS serviceUnavailable503 [] ""
  void $ forkIO $ run warps' app'
  atomically $ do
    takeTMVar shutdown
    conns ← readTVar activeConnections
    when (conns /= 0) retry

-- | Adjusts 'WaiOptions' with an address assigned to a newly created
-- server socket, uses those to set a "before main loop" function in
-- Warp 'Settings', which are then used to run an application.
runWarp :: (WaiOptions -> IO ())
        -- ^ A "before main loop" function
        -> WaiOptions
        -- ^ Original options
        -> (Settings -> S.Socket -> Application -> IO ())
        -- ^ A function such as 'runSettingsSocket'
        -> Settings -> Application -> IO ()
runWarp putListening opts runSocket set app = S.withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) S.close $ \s -> do
  sa <- S.getSocketName s
  S.setCloseOnExecIfNeeded $ S.fdSocket s
  runSocket (setBeforeMainLoop (putListening $ updateOptions sa opts) set) s app
  where
    updateOptions :: S.SockAddr -> WaiOptions -> WaiOptions
    updateOptions (S.SockAddrInet pn ha) opt =
      opt { wHttpPort = fromIntegral pn, wHttpHost = show (fromHostAddress ha) }
    updateOptions (S.SockAddrInet6 pn _flow ha _scope) opt =
      opt { wHttpPort = fromIntegral pn, wHttpHost = show (fromHostAddress6 ha) }
    updateOptions _ opt = opt

waiMain ∷ (WaiOptions → IO ()) → (WaiOptions → IO ()) → Application → IO ()
waiMain putListening putWelcome app = runCommand $ \opts _ → do
#ifdef WaiCliTLS
  let tlss = tlsSettings (wTlsCertFile opts) (wTlsKeyFile opts)
#endif
  let warps = setBeforeMainLoop (putListening opts) $ setPort (wHttpPort opts) $
              setHost (fromString $ wHttpHost opts) defaultSettings
  let app' = if wDevlogging opts == Just True then logStdoutDev app else app
  case wProtocol opts of
     "cgi" → CGI.run app'
#ifdef WaiCliFastCGI
     "fastcgi" → FCGI.run app'
#endif
     _ → do
       let run = case wProtocol opts of
             "http" → runWarp putListening opts runSettingsSocket
             "unix" → \warps' app'' → bracket (bindPath $ wUnixSock opts) S.close (\sock → runSettingsSocket warps' sock app'')
             "activate" → runActivated runSettingsSocket
#ifdef WaiCliTLS
             "http+tls" → runWarp putListening opts (runTLSSocket tlss)
             "unix+tls" → \warps' app'' → bracket (bindPath $ wUnixSock opts) S.close (\sock → runTLSSocket tlss warps' sock app'')
             "activate+tls" → runActivated (runTLSSocket tlss)
#endif
             x → \_ _ → putStrLn $ "Unsupported protocol: " ++ x
       putWelcome opts
       case wGracefulMode opts of
             "none" → run warps app'
             "serve-normally" → runGraceful ServeNormally run warps app'
             "serve-503" → runGraceful Serve503 run warps app'
             x  → putStrLn $ "Unsupported graceful mode: " ++ x

defPutListening ∷ WaiOptions → IO ()
defPutListening opts = getNumCapabilities >>= putMain
  where putMain cpus = reset "Running on " >> blue (wProtocol opts) >> putProto >> reset " with " >> green (show cpus ++ " CPUs") >> setReset >> putStrLn ""
        putProto = case wProtocol opts of
                     "http" → reset " host " >> boldMagenta (wHttpHost opts)
                              >> reset ", port " >> boldMagenta (show $ wHttpPort opts)
                     "unix" → reset " socket " >> boldMagenta (show $ wUnixSock opts)
                     "activate" → reset " activated socket"
#ifdef WaiCliTLS
                     "http+tls" → reset " (TLS) port "   >> boldMagenta (show $ wHttpPort opts)
                     "unix+tls" → reset " (TLS) socket " >> boldMagenta (show $ wUnixSock opts)
                     "activate+tls" → reset " (TLS) activated socket"
#endif
                     _      → setReset
        setReset = setSGR [ Reset ]
        boldMagenta x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta ] >> putStr x
        green x = setReset >> setSGR [ SetColor Foreground Dull Green ] >> putStr x
        blue  x = setReset >> setSGR [ SetColor Foreground Dull Blue ] >> putStr x
        reset x = setReset >> putStr x

defWaiMain ∷ Application → IO ()
defWaiMain = waiMain defPutListening (\_ → return ())
