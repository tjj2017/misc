package Multi_Type_Own_Pragma is
   pragma Classic_Own ((V with Own_Type => Integer),
                       (W with Own_Type => Character),
                       (X with Own_Type => Boolean));
   V : Integer;
end Multi_Type_Own_Pragma;
