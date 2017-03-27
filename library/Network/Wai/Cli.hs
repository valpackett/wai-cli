{-# LANGUAGE CPP, OverloadedStrings, UnicodeSyntax #-}

module Network.Wai.Cli where

import           Network.Wai (responseLBS, Application)
import qualified Network.Wai.Handler.CGI as CGI
import           Network.Wai.Handler.Warp hiding (run)
#ifdef WaiCliTLS
import           Network.Wai.Handler.WarpTLS
#endif
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.HTTP.Types (serviceUnavailable503)
import           Network.Socket.Activation
import qualified Network.Socket as S
import           GHC.Conc (getNumCapabilities, forkIO)
import           System.Posix.Internals (setNonBlockingFD)
import           System.Posix.Signals (installHandler, sigTERM, Handler(CatchOnce))
import           Data.Streaming.Network (bindPath)
import           System.Console.ANSI
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Exception (bracket)
import           Options

data GracefulMode = ServeNormally | Serve503

data WaiOptions = WaiOptions
  { port                     ∷ Int
  , socket                   ∷ String
  , protocol                 ∷ String
#ifdef WaiCliTLS
  , tlsKeyFile               ∷ String
  , tlsCertFile              ∷ String
#endif
  , gracefulMode             ∷ String
  , devlogging               ∷ Maybe Bool }

instance Options WaiOptions where
  defineOptions = pure WaiOptions
    <*> simpleOption "port"              3000                "The port the app should listen for connections on (for http)"
    <*> simpleOption "socket"            "wai.sock"          "The UNIX domain socket path the app should listen for connections on (for unix)"
#ifdef WaiCliTLS
    <*> simpleOption "protocol"          "http"              "The protocol for the server. One of: http, http+tls, unix, unix+tls, activate, activate+tls, cgi"
    <*> simpleOption "tlskey"            ""                  "Path to the TLS private key file for +tls protocols"
    <*> simpleOption "tlscert"           ""                  "Path to the TLS certificate bundle file for +tls protocols"
#else
    <*> simpleOption "protocol"          "http"              "The protocol for the server. One of: http, unix, activate, cgi"
#endif
    <*> simpleOption "graceful"          "serve-normally"    "Graceful shutdown mode. One of: none, serve-normally, serve-503"
    <*> simpleOption "devlogging"        Nothing             "Whether development logging should be enabled"

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

waiMain ∷ (WaiOptions → IO ()) → (WaiOptions → IO ()) → Application → IO ()
waiMain putListening putWelcome app = runCommand $ \opts _ → do
#ifdef WaiCliTLS
  let tlss = tlsSettings (tlsCertFile opts) (tlsKeyFile opts)
#endif
  let warps = setBeforeMainLoop (putListening opts) $ setPort (port opts) defaultSettings
  let app' = if devlogging opts == Just True then logStdoutDev app else app
  if protocol opts == "cgi"
     then CGI.run app'
     else do
       let run = case protocol opts of
             "http" → runSettings
             "unix" → \warps' app'' → bracket (bindPath $ socket opts) S.close (\sock → runSettingsSocket warps' sock app'')
             "activate" → runActivated runSettingsSocket
#ifdef WaiCliTLS
             "http+tls" → runTLS tlss
             "unix+tls" → \warps' app'' → bracket (bindPath $ socket opts) S.close (\sock → runTLSSocket tlss warps' sock app'')
             "activate+tls" → runActivated (runTLSSocket tlss)
#endif
             x → \_ _ → putStrLn $ "Unsupported protocol: " ++ x
       putWelcome opts
       case gracefulMode opts of
             "none" → run warps app'
             "serve-normally" → runGraceful ServeNormally run warps app'
             "serve-503" → runGraceful Serve503 run warps app'
             x  → putStrLn $ "Unsupported graceful mode: " ++ x

defPutListening ∷ WaiOptions → IO ()
defPutListening opts = getNumCapabilities >>= putMain
  where putMain cpus = reset "Running on " >> blue (protocol opts) >> putProto >> reset " with " >> green (show cpus ++ " CPUs") >> putStrLn ""
        putProto = case protocol opts of
                     "http" → reset " port "   >> boldMagenta (show $ port opts)
                     "unix" → reset " socket " >> boldMagenta (show $ socket opts)
                     "activate" → reset " activated socket"
#ifdef WaiCliTLS
                     "http+tls" → reset " (TLS) port "   >> boldMagenta (show $ port opts)
                     "unix+tls" → reset " (TLS) socket " >> boldMagenta (show $ socket opts)
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
