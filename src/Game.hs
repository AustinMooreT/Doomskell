module Game (Game) where

data Weapon =
  Weapon
  {
    weaponName :: String,
    weaponDmg :: Integer,
    weaponAmmo :: Integer
  }

data Player =
  Player
  {
    
  }

data Game =
  Game
  {
    gameTime :: Integer,
    player :: Player,
  }


