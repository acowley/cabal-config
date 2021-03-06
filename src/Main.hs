module Main (main) where
import Control.Applicative
import Control.Monad ((>=>), filterM)
import Data.List (sort, intercalate, isPrefixOf)
import Data.Maybe (listToMaybe)
import Distribution.InstalledPackageInfo
import Options.Applicative
import System.Directory
import System.FilePath

defaultGhcVersion :: String
defaultGhcVersion = "7.6.1"

-- pkgDir :: IO FilePath
-- pkgDir = (</>".ghc/x86_64-darwin-7.6.1/package.conf.d/") <$> getHomeDirectory

findPkgDir :: String -> IO (Maybe FilePath)
findPkgDir version = 
  do d <- (</>".ghc") <$> getHomeDirectory
     ghcs <- filterM (doesDirectoryExist . (d</>)) =<<
             (filter (not . isPrefixOf ".")
             <$> getDirectoryContents d)
     return . fmap (d</>) . listToMaybe $ 
       filter ((== version) . last . splitOn '-') ghcs

-- Find the package.conf.d associated the base.
findBasePkgDir :: String -> IO FilePath
findBasePkgDir version =
    do d <- (</>"lib") <$> getHomeDirectory
       ghcs <- filterM (doesDirectoryExist . (d</>)) =<<
               (filter (not . isPrefixOf "."))
               <$> getDirectoryContents d
       return . (d</>) . head $
         filter ((== version) . last . splitOn '-') ghcs
     
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = go
  where go [] = []
        go xs = let (a, xs') = break (== x) xs
                in a : if null xs' then [] else go (tail xs')

findNewest :: [FilePath] -> String -> Maybe FilePath
findNewest fs pkgName = listToMaybe . reverse . sort $ 
                        filter ((pkgName ==) . getPkgName) fs

getPkgName :: FilePath -> String
getPkgName = aux . reverse . splitOn '-' . takeFileName
  where aux (_hash : _version : name) = intercalate "-" (reverse name)
        aux x = error $ "Unexpected .conf file name " ++ show x

spaceSep :: [String] -> String
spaceSep = intercalate " " . filter (not . null)

getLibFlags :: Args -> FilePath -> IO String
getLibFlags args = readFile >=> return . aux . parseInstalledPackageInfo
  where aux (ParseFailed err) = error $ show err
        aux (ParseOk _ pkg) = spaceSep [getDirs pkg, getLibs pkg, getRpath pkg]
        getDirs = intercalate " " . map ("-L"++) . libraryDirs
        getLibs = intercalate " " . map (("-l"++) . (++"-ghc"++ghcVersion args))
                . hsLibraries
        getRpath | genRpath args = getrpathFlags
                 | otherwise = const ""

getrpathFlags :: InstalledPackageInfo -> String
getrpathFlags = intercalate " " . map ("-Wl,-rpath,"++) . libraryDirs

data Args = Args { ghcVersion   :: String
                 , genRpath     :: Bool
                 , packageNames :: [String] }

getConfs :: FilePath -> IO [FilePath]
getConfs d = map (d</>) 
           . filter (not . isPrefixOf "builtin_rts")
           .  filter ((== ".conf") . takeExtension)
          <$> getDirectoryContents d

getFlags :: Args -> IO ()
getFlags args = 
  do d' <- findPkgDir (ghcVersion args) 
     db <- (</> "package.conf.d") <$> findBasePkgDir (ghcVersion args)
     let d = case d' of
               Nothing -> error $ "No package database for GHC version "++
                          ghcVersion args
               Just x -> x </> "package.conf.d"
     --confs <- filter ((==".conf") . takeExtension) <$> getDirectoryContents d
     confs <- (++) <$> getConfs d <*> getConfs db
     case sequence (map (findNewest confs) (packageNames args)) of
       Nothing -> putStrLn "No packages parsed"
       Just pkgs' -> mapM (getLibFlags args) pkgs' >>= 
       --Just pkgs' -> mapM (getLibFlags args . (d</>)) pkgs' >>= 
                     putStrLn . spaceSep

argParser :: Parser Args
argParser = Args <$> strOption 
                     (long "ghc" 
                      <> metavar "GHC_VERSION" 
                      <> value defaultGhcVersion
                      <> help "Version of GHC to use; default is 7.6.1")
                 <*> switch
                     (long "rpath"
                      <> help "Emit -Wl,-rpath flags in addition to -L/-l")
                 <*> arguments1 Just (metavar "PackageNames")
                              
main :: IO ()
main = execParser opts >>= getFlags
  where opts = info (helper <*> argParser)
                    (fullDesc 
                     <> progDesc "Generate linker flags for requested packages."
                     <> header "cabal-config - polyglot tool chain helper")
