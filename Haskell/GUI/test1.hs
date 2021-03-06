import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do f    <- frame [text := "Hello"]
              inp <- entry f []
              out <- entry f []
              but <- button f [ text := "Hello”
                                     , on command := do s <- get inp text
                                 set out [text := "Hello "++s]
                                     ]
              set f [ layout := floatCentre $ column 5 
                [ label "What is your name?”
                                         , widget inp
                                         , widget but
                                         , widget out
                                         ] ]
