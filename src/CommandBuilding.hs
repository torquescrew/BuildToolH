module CommandBuilding where
import Entities


--updateCommand :: Entity -> Entity
--updateCommand (Command stats e remaining miners)
--        | remaining == 0 = Command stats Nil 0 (create e:miners)

-- TODO: improve this
mineralsMined :: Double -> Double
mineralsMined wrkrs | wrkrs >= 24 = 816.0 / 60.0
mineralsMined wrkrs | wrkrs <= 16 = (40.0 * wrkrs) / 60.0
mineralsMined wrkrs               = (672.0 / 60.0) + ((wrkrs-16) * 18.0) / 60.0


-- Entity -> GameState -> GameState
{- 
  protected def mineralsMined(workers: Int) = {
    var mined = 0.0
    if (workers <= 16) {
      mined = (40.0 * workers) / 60.0
    }
    else {
      var n = workers - 16
      if (n >= 8) {
        mined = 816.0 / 60.0
      }
      else {
        mined += 672.0 / 60.0
        mined += (n * 18.0) / 60.0
      }
    }
    mined
  }
  
  
-}