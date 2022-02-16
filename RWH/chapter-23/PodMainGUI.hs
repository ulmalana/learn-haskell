-- this is the updated version of PodMainGUI.hs of the book
-- credit to Adam Yin who provided this version in the comment section

module PodMainGUI where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket (withSocketsDo)

import Graphics.UI.Gtk hiding (disconnect, add)
import Graphics.UI.Gtk.Builder

import Control.Concurrent

-- GUI type
data GUI = GUI {
    mainWin :: Window,
    mwAddBt :: Button,
    mwUpdateBt :: Button,
    mwDownloadBt :: Button,
    mwFetchBt :: Button,
    mwExitBt :: Button,
    statusWin :: Dialog,
    swOKBt :: Button,
    swCancelBt :: Button,
    swLabel :: Label,
    addWin :: Dialog,
    awOKBt :: Button,
    awCancelBt :: Button,
    awEntry :: Entry
}

main :: FilePath -> IO ()
main gladepath = withSocketsDo $ do 
    -- initialize GTK engine
    initGUI

    timeoutAddFull (yield >> return True)
                     priorityDefaultIdle 100

    -- load glade file
    gui <- loadGlade gladepath

    dbh <- connect "pod.db"

    connectGui gui dbh 

    mainGUI
    disconnect dbh

loadGlade :: FilePath -> IO GUI 
loadGlade gladepath = do 
    builder <- builderNew
    builderAddFromFile builder gladepath
    mw <- builderGetObject builder castToWindow "mainWindow"

    -- load all buttons
    [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
     auOk, auCancel] <- mapM (builderGetObject builder castToButton)
                            ["addButton", "updateButton", "downloadButton", "fetchButton",
                             "exitButton", "okButton", "cancelButton", "auOk", "auCancel"]

    sw <- builderGetObject builder castToDialog "statusDialog"
    swl <- builderGetObject builder castToLabel "statusLabel"

    au <- builderGetObject builder castToDialog "addDialog"
    aue <- builderGetObject builder castToEntry "auEntry"

    return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit
                sw swOK swCancel swl au auOk auCancel aue                        

connectGui :: IConnection conn => GUI -> conn -> IO ()
connectGui gui dbh = do 
    onDestroy (mainWin gui) mainQuit

    -- main window buttons
    onClicked (mwAddBt gui) (guiAdd gui dbh)
    onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
    onClicked (mwDownloadBt gui) (guiDownload gui dbh)
    onClicked (mwFetchBt gui) (guiFetch gui dbh)
    onClicked (mwExitBt gui) mainQuit
    return ()

guiAdd :: IConnection conn => GUI -> conn -> IO ()
guiAdd gui dbh = do 
    entrySetText (awEntry gui) ""
    onClicked (awCancelBt gui) (widgetHide $ addWin gui)
    onClicked (awOKBt gui) procOK

    -- show the add url window
    windowPresent $ addWin gui

  where 
    procOK = do 
        url <- entryGetText $ awEntry gui 
        widgetHide $ addWin gui 
        add dbh url


statusWindow :: IConnection conn => GUI 
                                 -> conn 
                                 -> String 
                                 -> ((String -> IO ()) -> IO ())
                                 -> IO ()
statusWindow gui dbh title func = do 
    labelSetText (swLabel gui) ""
    widgetSetSensitivity (swOKBt gui) False 
    widgetSetSensitivity (swCancelBt gui) True 

    windowSetTitle (statusWin gui) title 

    childThread <- forkIO childTasks

    onClicked (swCancelBt gui) (cancelChild childThread)

    windowPresent $ statusWin gui 
  where
    childTasks = do 
        updateLabel "Starting thread..."
        func updateLabel
        enableOK

    enableOK = do
        widgetSetSensitivity (swCancelBt gui) False 
        widgetSetSensitivity (swOKBt gui) True 
        onClicked (swOKBt gui) (widgetHide $ statusWin gui)
        return ()

    updateLabel text = labelSetText (swLabel gui) text

    cancelChild childThread = do 
        killThread childThread
        yield
        updateLabel "Action has been cancelled"
        enableOK

guiUpdate, guiDownload, guiFetch :: IConnection conn => GUI -> conn -> IO ()
guiUpdate gui dbh = statusWindow gui dbh "Pod: Update" (update dbh)

guiDownload gui dbh = statusWindow gui dbh "Pod: Download" (download dbh)

guiFetch gui dbh = statusWindow gui dbh "Pod: Fetch" (\logf -> update dbh logf >> download dbh logf)

add :: IConnection conn => conn -> String -> IO ()
add dbh url = do 
    addPodcast dbh pc 
    commit dbh
  where
    pc = Podcast {castId = 0,  castURL = url}


update :: IConnection conn => conn -> (String -> IO ()) -> IO ()
update dbh logf = do 
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    logf "Update complete"
  where
    procPodcast pc = do 
        logf $ "Updating from " ++ (castURL pc)
        updatePodcastFromFeed dbh pc


download :: IConnection conn => conn -> (String -> IO ()) -> IO ()
download dbh logf = do 
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
  where 
    procPodcast pc = do 
        logf $ "Considering " ++ (castURL pc)
        episodelist <- getPodcastEpisodes dbh pc
        let dleps = filter (\ep -> epDone ep == False)
                             episodelist
        mapM_ procEpisode dleps
    procEpisode ep = do 
        logf $ "Downloading " ++ (epURL ep)
        getEpisode dbh ep 
