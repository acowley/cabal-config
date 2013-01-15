module Main (main) where
import Control.Applicative
import Control.Monad ((>=>), filterM)
import Data.List (sort, intercalate, isPrefixOf)
import Data.Maybe (listToMaybe)
import Distribution.InstalledPackageInfo
import Options.Applicative
import System.Directory
import System.Environment
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
     
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = go
  where go [] = []
        go xs = let (a, xs') = break (== x) xs
                in a : if null xs' then [] else go (tail xs')

findNewest :: [FilePath] -> String -> Maybe FilePath
findNewest fs pkgName = listToMaybe . reverse . sort $ 
                        filter ((pkgName ==) . getPkgName) fs

getPkgName :: String -> String
getPkgName = aux . reverse . splitOn '-'
  where aux (_hash : _version : name) = intercalate "-" (reverse name)
        aux _ = error "Unexpected .conf file name"

spaceSep :: [String] -> String
spaceSep = intercalate " " . filter (not . null)

getLibFlags :: Args -> FilePath -> IO String
getLibFlags args = readFile >=> return . aux . parseInstalledPackageInfo
  where aux (ParseFailed err) = error $ show err
        aux (ParseOk _ pkg) = spaceSep [getDirs pkg, getLibs pkg]
        getDirs = intercalate " " . map ("-L"++) . libraryDirs
        getLibs = intercalate " " . map (("-l"++) . (++"-ghc"++ghcVersion args))
                . hsLibraries

data Args = Args { ghcVersion   :: String
                 , packageNames :: [String] }

getFlags :: Args -> IO ()
getFlags args = 
  do d' <- findPkgDir (ghcVersion args) 
     let d = case d' of
               Nothing -> error $ "No package database for GHC version "++
                          ghcVersion args
               Just x -> x </> "package.conf.d"
     confs <- filter ((==".conf") . takeExtension) <$> getDirectoryContents d
     case sequence (map (findNewest confs) (packageNames args)) of
       Nothing -> putStrLn "No packages parsed"
       Just pkgs' -> mapM (getLibFlags args . (d</>)) pkgs' >>= 
                     putStrLn . spaceSep

argParser :: Parser Args
argParser = Args <$> strOption 
                     (long "ghc" 
                      <> metavar "GHC_VERSION" 
                      <> value defaultGhcVersion
                      <> help "Version of GHC to use; default is 7.6.1")
                 <*> arguments1 Just (metavar "PackageNames")
                              
main :: IO ()
main = execParser opts >>= getFlags
  where opts = info (helper <*> argParser)
                    (fullDesc 
                     <> progDesc "Generate linker flags for requested packages."
                     <> header "cabal-config - polyglot tool chain helper")
