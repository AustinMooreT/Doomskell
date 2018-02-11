module Game (Game) where


-- | The model represents the state of the game.
data Model =
  Model
  {
  }

-- | Msg represents any time of externel event that can be applied to the game engine.
data Msg =
  Right       |
  Left        |
  Forward     |
  Backward    |
  StrafeLeft  |
  StrafeRight |
  FireWeapon  |
  Use         |
  ChangeWeapon Integer



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


