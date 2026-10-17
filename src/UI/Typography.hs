{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Typography where

import Data.FileEmbed
import Relude hiding (one)

zero, one, two, three, four, five, six, seven, eight, nine, dots :: Text
zero = $(embedStringFile =<< makeRelativeToProject "assets/0.txt")
one = $(embedStringFile =<< makeRelativeToProject "assets/1.txt")
two = $(embedStringFile =<< makeRelativeToProject "assets/2.txt")
three = $(embedStringFile =<< makeRelativeToProject "assets/3.txt")
four = $(embedStringFile =<< makeRelativeToProject "assets/4.txt")
five = $(embedStringFile =<< makeRelativeToProject "assets/5.txt")
six = $(embedStringFile =<< makeRelativeToProject "assets/6.txt")
seven = $(embedStringFile =<< makeRelativeToProject "assets/7.txt")
eight = $(embedStringFile =<< makeRelativeToProject "assets/8.txt")
nine = $(embedStringFile =<< makeRelativeToProject "assets/9.txt")
dots = $(embedStringFile =<< makeRelativeToProject "assets/dots.txt")

renderDigit :: Int -> Text
renderDigit 0 = zero
renderDigit 1 = one
renderDigit 2 = two
renderDigit 3 = three
renderDigit 4 = four
renderDigit 5 = five
renderDigit 6 = six
renderDigit 7 = seven
renderDigit 8 = eight
renderDigit 9 = nine
renderDigit _ = ""
