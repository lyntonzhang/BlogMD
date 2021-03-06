{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------
import Snap hiding (Handler)
import Snap.Util.FileServe
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.FilePath.Posix
import Heist
import qualified Heist.Compiled as C
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import Control.Monad.Trans.Either
import Data.Maybe
import qualified Blaze.ByteString.Builder as BBB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Monoid
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as PU8
import Control.Monad.Trans.Reader
import Data.List.Split (splitOn)


-------------------------------------------------------------------
                      --------------
                      ---- Blog ----
                      --------------
{-

    1. Read the blog directory, generate PostEntry.
    2. According to PostEntry, generate navSplice
    3. According to PostEntry, generate post templates with pandoc
    4. Serve the snap

    Todo:
    1. Lens
    2. User Authentication
    3. Unicode for FilePath of PostEntry
    4. Recursive postNav
    5. Discussion and message
    6. Command arg to choose posts and templates
   
-}

-------------------------------------------------------------------
                      -------------------
                      ---- PostEntry ----
                      -------------------

data PostEntry = PostFile FilePath T.Text | PostDir FilePath [PostEntry] 

archiveHtml :: String -> PostEntry -> T.Text
archiveHtml category (PostDir dirname childs) = 
    mconcat [ "<li>"
            , T.pack dirname
            , "</li>"
            , archiveHtmlList (category </> dirname) childs
            ]
archiveHtml url (PostFile tplname title) =
    mconcat [ "<li><a href=\""
            , T.pack $ url </> nomd tplname
            , "\">"
            , title
            , "</a></li>"
            ]
  where
    head' :: [String] -> String
    head' [] = ""   
    head' s = head s
    nomd :: String -> String 
    nomd = head' . splitOn "."

archiveHtmlList :: String -> [PostEntry] -> T.Text
archiveHtmlList _ [] = ""
{-
archiveHtmlList url postEntries = concatMap wrapEntry postEntries
  where
    wrapEntry postEntry = mconcat [ "\n"
                                  , archiveHtml url postEntry
                                  ]
-}
archiveHtmlList url (x:xs) =
        T.concat $ ["\n"
                   , archiveHtml url x
                   , archiveHtmlList url xs
                   ]

getValidContents :: FilePath -> IO [FilePath]
getValidContents pathName =
    filter (`notElem` [".",".."]) <$> getDirectoryContents pathName

genPostDir :: FilePath -> FilePath -> IO PostEntry
genPostDir cdir dirName = do 
    cnts <- getValidContents $ cdir </> dirName
    childs <- forM cnts $ \pathName -> do
        t <- isSubDir pathName
        case t of
             True -> genPostDir (cdir </> dirName) pathName
             False -> do
                 title <- postTitle $ cdir </> dirName </> pathName 
                 return $ PostFile pathName title
    return $ PostDir dirName childs
  where
    isSubDir :: FilePath -> IO Bool
    isSubDir fpath = doesDirectoryExist $ cdir </> dirName </> fpath
    postTitle :: FilePath -> IO T.Text
    postTitle fpath = do 
            print fpath
            md <- PU8.readFile fpath
            -- TODO Exception, not every markdown doc start with head 1
            let Pandoc _ bs = readMarkdown def md
                Header _ _ inlines = bs !! 0
                Str s = inlines !! 0
            putStrLn s
            return $ T.pack s 

 ----------------------------------------------------------------------
                      ---------------------------
                      ----  App and Handler  ----
                      ---------------------------

data App = App 
    { hs :: HeistState Handler
    , pe :: PostEntry 
    }

-- | Use type instead of newtype so don't have to derive the classes.
-- newtype Handler a = Handler {unHandler :: ReaderT App Snap a }
type Handler = ReaderT App Snap

initApp :: FilePath -> FilePath -> FilePath -> IO App
initApp cdir posts templates  = do
    p <- genPostDir cdir posts
    genPostTpl "" p
    h <- load templates splices
    return $ App h p 
  where
    splices :: Splices (C.Splice Handler)
    splices = do
        "postNav" ## navSplice
    tplName :: FilePath -> FilePath
    tplName s = splitOn "." s !! 0 ++ ".tpl"
    mdToHtml :: String -> String
    mdToHtml md = mconcat [ "<apply template=\"layout\">"
                          , writeHtmlString def $ readMarkdown def md
                          , "</apply>" ]
    -- TODO Nonlazy IO
    genPostTpl :: FilePath -> PostEntry -> IO ()
    genPostTpl dpath (PostDir dname childs) = do
        createDirectoryIfMissing False 
            $ cdir </> templates </> dpath </> dname
        mapM_ (genPostTpl $ dpath </> dname) childs
    genPostTpl dpath (PostFile fname ftitle) = do
        md <- PU8.readFile $ cdir </> dpath </> fname
        PU8.writeFile (cdir </> templates </> dpath </> (tplName fname))
            $ mdToHtml md

----------------------------------------------------------------------
                      ---------------------------
                      ----      Splices     ----
                      ---------------------------

load :: MonadIO n => FilePath
                  -> Splices (C.Splice n)
                  -> IO (HeistState n)
load baseDir splices = do
    tmap <- runEitherT $ do
        let t = loadTemplates baseDir
            hc = HeistConfig  
                    defaultInterpretedSplices  
                    defaultLoadTimeSplices 
                    splices 
                    mempty
                    [t]
        initHeist hc 
    either (error . concat) return tmap
            
-- TODO Error Exception, unicod
navSplice :: C.Splice Handler
navSplice = return $ C.yieldRuntimeText $ do
    app <- lift ask
    case pe app of
        PostDir s childs -> 
             return $ archiveHtmlList "/posts" childs
        _ -> return $ T.pack $ show "error"

----------------------------------------------------------------------
                      ---------------------------
                      ---- Handler and Snaps ----
                      ---------------------------

postHandler :: Handler ()
postHandler = do
    liftIO $ print "postHandler"
    app <- ask 
    liftSnap $ do
        tag <- getParam "tag"
        title <- getParam "title"
        let runtime =fromJust $ do
            t <- tag
            tt <- title
            C.renderTemplate (hs app) $ posttpl t tt
        builder <- runReaderT (fst runtime) app
        writeBuilder builder 
  where
    -- TODO "/" cross platform 
    posttpl :: B.ByteString -> B.ByteString -> B.ByteString
    posttpl t tt = B.concat [ "posts/" :: B.ByteString
                            , t
                            , "/" :: B.ByteString
                            , tt ]

indexHandler :: Handler ()
indexHandler = do
    app <- ask 
    liftSnap $ do
        let runtime = fromJust $ C.renderTemplate (hs app) "index"
        builder <- runReaderT (fst runtime) app
        writeBuilder builder

site :: App -> Snap ()
site app = 
    ifTop indexSnap <|>
    route [("posts/:tag/:title",postSnap)] <|>
    dir "static" (serveDirectory "static")
  where
    postSnap :: Snap ()
    postSnap = runReaderT postHandler app
    indexSnap :: Snap ()
    indexSnap = runReaderT indexHandler app

----------------------------------------------------------------------
                      ---------------------------
                      ----------  Main  ---------
                      ---------------------------

main :: IO ()
main = do
    cdir <- getCurrentDirectory 
    app <- initApp cdir "posts" "template"
    httpServe (setPort 8017 mempty) $ site app
    
