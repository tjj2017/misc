package Not_First_Item is
   V : Integer;
   pragma Classic_Own (((Plain => V) with Own_Type => Integer));
end Not_First_Item;
