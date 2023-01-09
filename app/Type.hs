module Type (SchemeValue (..)) where

data SchemeValue
  = SchemeEmpty
  | SchemeBool Bool
  | SchemeIdentifier String
  | SchemeNum Integer -- NOTE: No floats yet
  | SchemeString String -- NOTE: no escaping support
  | SchemeQuote SchemeValue
  | SchemeCons {car :: SchemeValue, cdr :: SchemeValue}
  deriving (Show, Eq)
