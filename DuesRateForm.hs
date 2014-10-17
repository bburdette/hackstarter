module DuesRateForm where

import Import

duesRateForm :: Maybe DuesRate -> Form DuesRate
duesRateForm dr = renderDivs $ DuesRate
 <$> areq textField "Dues rate name" (duesRateName <$> dr) 
 <*> areq intField "Dues rate amount" (duesRateAmount <$> dr)

