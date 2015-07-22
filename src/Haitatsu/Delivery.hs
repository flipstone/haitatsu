module Haitatsu.Delivery where

import            Haitatsu.AWS
import            Haitatsu.Types

deliver :: Haitatsu ()
deliver = do
  initialStatus
  taskRev <- registerTask
  updateService taskRev
  healthCheck taskRev

