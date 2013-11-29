jspreadsheet
============

Web-based collaborative spreadsheets

Websocket backend is written in Haskell,
spreadsheets are created using Handsontable 
(see http://handsontable.com).

This is a sample application which was written 
to learn more about the following aspects of Haskell:

    * Software Transactional Memory (Control.Concurrent.STM)
    * JSON serialization (Data.Aeson)
    * Sets with multiple indexes (Data.IxSet)
    * Testing (Test.QuickCheck)
    * Websocket server/client implementations (Network.WebSockets)
    and more

Installation
============
    
    * Git checkout to any directory
    * cabal sandbox init  in the directory
    * cabal install
    
    * Point your nginx instance to the public/ directory (example nginx.conf is provided)
      and setup nginx reverse proxy for websocket server (port 1313 by default
      
    * Run .cabal-sandbox/bin/jServer
    
    
    * Open <yourserver>/html/main.html in multiple browsers and type something into spreadsheet
