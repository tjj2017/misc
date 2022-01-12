package Multi_Type_Own_Pragma is
   pragma Classic_Own ((((Plain => V) with Own_Type => Integer),
                       ((Plain => W) with Own_Type => Character),
                       ((Plain => X) with Own_Type => Boolean)));
   V : Integer;
end Multi_Type_Own_Pragma;
