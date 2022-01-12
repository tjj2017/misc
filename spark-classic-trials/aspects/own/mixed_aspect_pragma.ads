package Mixed_Aspect_Pragma
with Classic_Initializes => ((V)),
     Classic_Pre => True
is
   pragma Classic_Own ((Plain => V));
   V : Integer;
private
   PV : Integer;
end Mixed_Aspect_Pragma;
