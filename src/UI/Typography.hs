{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Typography where

import Relude

renderDigit :: Int -> Text
renderDigit 0 =
  """
  ◢◼◼◼◣
  ◼   ◼
  ◼   ◼
  ◼   ◼
  ◥◼◼◼◤
  """
renderDigit 1 =
  """
  ◢◼
   ◼
   ◼
   ◼
   ▼
  """
renderDigit 2 =
  """
  ◢◼◼◼◣
      ◼
  ◢◼◼◼◤
  ◼
  ◥◼◼◼◤
  """
renderDigit 3 =
  """
  ◢◼◼◼◣
      ◼
  ◼◼◼◼◼
      ◼   
  ◥◼◼◼◤
  """
renderDigit 4 =
  """
  ◢   ◣
  ◼   ◼
  ◼◼◼◼◼
      ◼   
      ◤
  """
renderDigit 5 =
  """
  ◢◼◼◼◣
  ◼
  ◥◼◼◼◣
      ◼
  ◥◼◼◼◤
  """
renderDigit 6 =
  """
  ◢◼◼◼◣
  ◼
  ◼◼◼◼◣
  ◼   ◼
  ◥◼◼◼◤
  """
renderDigit 7 =
  """
  ◢◼◼◼◣
      ◼
      ◼
      ◼
      ◤
  """
renderDigit 8 =
  """
  ◢◼◼◼◣
  ◼   ◼
  ◼◼◼◼◼
  ◼   ◼
  ◥◼◼◼◤
  """
renderDigit 9 =
  """
  ◢◼◼◼◣
  ◼   ◼
  ◼◼◼◼◼
      ◼
  ◥◼◼◼◤
  """
renderDigit _ = ""

dots :: Text
dots =
  """

  ◼

  ◼

  """
