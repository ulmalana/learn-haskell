import Data.List (break)

x -: f = f x

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
    Folder "root"
        [ File "goat_yelling.mp3" "weeeee"
        , File "pope.avi" "good god"
        , Folder "pics"
            [ File "ape_throwup.jpg" "huok"
            , File "semangka.png" "merah"
            , File "skull.gif" "hehehe"]
        , File "baguette.pdf" "crisp"
        , Folder "programs"
            [ File "install.exe" "game"
            , File "config.sh" "shell"
            , File "py.py" "python"
            , Folder "src"
                [ File "main.rs" "rust bin"
                , File "lib.rs" "rust lib"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) = 
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _ ) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs)


