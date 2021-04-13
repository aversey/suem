module Video (ConfigSocket(..), video) where

import Graphics.Gloss
import Data.ByteString (pack)

data ConfigSocket = ConfigInet String | ConfigUnix String

video :: ConfigSocket -> IO ()
video _ = do
    let pic = replicate (256 * 256 * 4) 255
        bmp = bitmapOfByteString 256 256 (BitmapFormat TopToBottom PxRGBA)
                                 (pack pic) False
    display (InWindow "Suem Video" (256, 256) (0, 0)) black bmp
