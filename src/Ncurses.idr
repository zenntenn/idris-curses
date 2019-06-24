||| State based wraper for the Ncurses C library
module Ncurses

import Control.ST

%lib C "ncurses"
%include C "Ncurses/ncurses_extra.h"
%link C "Ncurses/ncurses_extra.o"

%access public export
%default total

private
cBool : Bool -> Int
cBool True = 1
cBool False = 0

data Window = WindowPtr Ptr

-- Commenting out the ones that I don't need yet, as Idris currently gets very slow if there are too many of these functions defined
interface Ncurses (m : Type -> Type) where
{
  ||| Outputs a string, without trailing newline 
  putStr : Window -> (str : String) -> STrans m () xs (const xs)
  ||| Gets the most recent line entered 
  getStr : Window -> STrans m String xs (const xs)
  ||| Outputs a string, with a trailing newline
  putStrLn : Window -> (str : String) -> STrans m () xs (const xs)
  ||| Outputs a single character
  putChar : Window -> (char : Char) -> STrans m () xs (const xs)
  ||| Gets the next character, after a newline 
  getChar : Window -> STrans m Char xs (const xs)
  ||| Gets a single character, without waiting for a newline 
  getCh : Window -> STrans m Char xs (const xs)
  -- lines : STrans m Int xs (const xs)
  -- cols : STrans m Int xs (const xs)
  -- initscr : STrans m Window xs (const xs)
  -- endwin : STrans m Int xs (const xs)
  -- refresh : Window -> STrans m Int xs (const xs)
  -- cbreak : STrans m Int xs (const xs)
  -- nocbreak : STrans m Int xs (const xs)
  -- echo : STrans m Int xs (const xs)
  -- noecho : STrans m Int xs (const xs)
  -- halfdelay : STrans m Int xs (const xs)
  -- intrflush : Window -> Bool -> STrans m Int xs (const xs)
  -- keypad : Window -> Bool -> STrans m Int xs (const xs)
  -- meta : Window -> Bool -> STrans m Int xs (const xs)
  -- nodelay : Window -> Bool -> STrans m Int xs (const xs)
  -- raw : STrans m Int xs (const xs)
  -- noraw : STrans m Int xs (const xs)
  -- noqiflush : STrans m Int xs (const xs)
  -- qiflush : STrans m Int xs (const xs)
  -- notimeout : Window -> Bool -> STrans m Int xs (const xs)
  -- timeout : Int -> STrans m () xs (const xs)
  -- wtimeout : Window -> Int -> STrans m () xs (const xs)
  -- typeahead : Nat -> STrans m Int xs (const xs)
  -- clearok : Window -> Bool -> STrans m Int xs (const xs)
  -- idlok : Window -> Bool -> STrans m Int xs (const xs)
  -- idcok : Window -> Bool -> STrans m Int xs (const xs)
  -- immedok : Window -> Bool -> STrans m Int xs (const xs)
  -- leaveok : Window -> Bool -> STrans m Int xs (const xs)
  -- setscrreg : (top : Int) -> (bottom : Int) -> STrans m Int xs (const xs)
  -- wsetscrreg : Window -> (top : Int) -> (bottom : Int) -> STrans m Int xs (const xs)
  -- scrollok : Window -> Bool -> STrans m Int xs (const xs)
  -- nl : STrans m Int xs (const xs)
  -- nonl : STrans m Int xs (const xs)
}

private total
intReturn : (str : String) -> Ncurses IO => STrans IO Int xs (const xs)
intReturn str = lift $ foreign FFI_C str (IO Int)

private total
windowBoolIntReturn : (str : String) -> Window -> Bool -> Ncurses IO => STrans IO Int xs (const xs) 
windowBoolIntReturn str (WindowPtr p) a = lift $ foreign FFI_C str (Ptr -> Int -> IO Int) p (cBool a)

Ncurses IO where
{
  putStr (WindowPtr p) str = lift $ foreign FFI_C "wprintw" (Ptr -> String -> IO ()) p str
  putStrLn w str = putStr w (str ++ "\n")
  getStr (WindowPtr p) = lift $ foreign FFI_C "getStr" (Ptr -> IO String) p
  putChar (WindowPtr p) char = lift $ foreign FFI_C "putChar" (Ptr -> Char -> IO()) p char
  getChar (WindowPtr p) = lift $ foreign FFI_C "getChar" (Ptr -> IO Char) p
  getCh (WindowPtr p) = lift $ foreign FFI_C "wgetch" (Ptr -> IO Char) p
  -- lines = intReturn "getLines"
  -- cols = intReturn "getCols"
  -- initscr = lift $ map WindowPtr $ foreign FFI_C "initscr" (IO Ptr)
  -- endwin = intReturn "endwin"
  -- refresh (WindowPtr p) = lift $ foreign FFI_C "wrefresh" (Ptr -> IO Int) p
  -- cbreak = intReturn "cbreak"
  -- nocbreak = intReturn "nocbreak"
  -- echo = intReturn "echo"
  -- noecho = intReturn "noecho"
  -- halfdelay = intReturn "halfdelay"
  -- intrflush w a = windowBoolIntReturn "intrflush" w a
  -- keypad w a = windowBoolIntReturn "keypad" w a
  -- meta w a = windowBoolIntReturn "meta" w a
  -- nodelay w a = windowBoolIntReturn "nodelay" w a
  -- raw = intReturn "raw"
  -- noraw = intReturn "noraw"
  -- noqiflush = intReturn "noqiflush"
  -- qiflush = intReturn "qiflush"
  -- notimeout w a = windowBoolIntReturn "notimeout" w a
  -- timeout k = lift $ foreign FFI_C "timeout" (Int -> IO ()) k
  -- wtimeout (WindowPtr p) k = lift $ foreign FFI_C "wtimeout" (Ptr -> Int -> IO ()) p k
  -- typeahead fileDescriptor = lift $ foreign FFI_C "typeahead" (Int -> IO Int) (toIntNat fileDescriptor)
  -- clearok w a = windowBoolIntReturn "clearok" w a
  -- idlok w a = windowBoolIntReturn "idlok" w a
  -- idcok w a = windowBoolIntReturn "idcok" w a
  -- immedok w a = windowBoolIntReturn "immedok" w a
  -- leaveok w a = windowBoolIntReturn "leaveok" w a
  -- setscrreg top bottom = lift $ foreign FFI_C "setscrreg" (Int -> Int -> IO Int) top bottom
  -- wsetscrreg (WindowPtr ptr) top bottom = lift $ foreign FFI_C "wsetscrreg" (Ptr -> Int -> Int -> IO Int) ptr top bottom
  -- scrollok w a = windowBoolIntReturn "scrollok" w a
  -- nl = intReturn "nl"
  -- nonl = intReturn "nonl"
}


{-
private
liftError : IO Int -> NcursesIO ()
liftError code = MkNcursesIO (map liftErr code) where
  liftErr code = if code == -1 then Left NullWindow else Right ()

runNcurses : (NcursesError -> IO b) -> (a -> IO b) -> NcursesIO a -> IO b
runNcurses f g nio = do
  e <- unliftIO nio
  case e of
    Left err => f err
    Right a => g a

ncursesMain : NcursesIO () -> IO ()
ncursesMain = runNcurses logError pure where
  logError NullWindow = putStrLn "NULL window"

-}