module FileIO where

-- takes a path and a file name and loads the data set as a list of transactions, where transactions are a list of strings
importData :: String -> String -> IO [[String]]
importData path file = do
    fileString <- readFile (path ++ "/" ++ file)
    return (map words $ lines fileString)

exportResult :: String -> String -> String -> IO ()
exportResult path file res = do writeFile (path ++ "/res_" ++ file) res
