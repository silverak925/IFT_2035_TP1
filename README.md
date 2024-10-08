# IFT_2035_TP1
mkPrim f = Vprim (\(Vnum x) -> Vprim (\(Vnum y) -> Vnum (f x y)))
